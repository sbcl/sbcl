;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was written at
;;;; Carnegie Mellon University and released into the public domain. The
;;;; software is in the public domain and is provided with absolutely no
;;;; warranty. See the COPYING and CREDITS files for more information.

(in-package :sb-concurrency-test)

(deftest mailbox-trivia.1
    (values (mailboxp (make-mailbox))
            (mailboxp 42))
  t
  nil)

(deftest mailbox-trivia.2
    (let ((mbox1 (make-mailbox :name "foof"))
          (mbox2 (make-mailbox)))
      (values (mailbox-name mbox1)
              (mailbox-name mbox2)))
  "foof"
  nil)

(deftest mailbox-trivia.3
    (flet ((test (initial-contents)
             (let ((mbox (make-mailbox :initial-contents initial-contents)))
               (list (mailbox-count mbox)
                     (mailbox-empty-p mbox)
                     (list-mailbox-messages mbox)
                     (eq (list-mailbox-messages mbox) initial-contents)))))
      (values (test '(1 2 3))
              (test #(1 2 3))
              (test "123")
              (test nil)))
  (3 nil (1 2 3) nil)
  (3 nil (1 2 3) nil)
  (3 nil (#\1 #\2 #\3) nil)
  (0 t nil t))

#+sb-thread
(deftest mailbox-timeouts
    (let* ((mbox (make-mailbox))
           (writers (loop for i from 1 upto 20
                          collect (make-thread
                                   (lambda (x)
                                     (loop repeat 50
                                           do (send-message mbox x)
                                              (sleep 0)))
                                   :arguments i)))
           (readers (loop repeat 10
                          collect (make-thread
                                   (lambda ()
                                     (loop while (receive-message mbox :timeout 0.5)
                                           count t))))))
      (mapc #'join-thread writers)
      (apply #'+ (mapcar #'join-thread readers)))
  1000)

;;; FIXME: Several tests disabled on SunOS due to hangs.
;;;
;;; The issues don't seem to have anything to do with mailboxes
;;; per-se, but are rather related to our usage of signal-unsafe
;;; pthread functions inside signal handlers.
;;;
;;; INTERRUPT-THREAD is not reliable on Windows
#+(and sb-thread (not sunos) (not win32))
(progn

;; Dummy struct for ATOMIC-INCF to work.
(defstruct counter
  (ref 0 :type sb-vm:word))

(defun receiver-distribution (n-receivers)
  (let* ((aux              (floor n-receivers 2))
         (n-recv-msg       (- n-receivers aux))
         (n-recv-pend-msgs (floor aux 3))
         (n-recv-msg-n-h   (- aux n-recv-pend-msgs)))
    (values n-recv-msg
            n-recv-msg-n-h
            n-recv-pend-msgs)))

(defun test-mailbox-producers-consumers
    (&key n-senders n-receivers n-messages interruptor)
  (let ((mbox    (make-mailbox))
        (counter (make-counter))
        (+sleep+  0.0001)
        (+fin-token+ :finish) ; end token for receivers to stop
        (+blksize+ 5))        ; "block size" for RECEIVE-PENDING-MESSAGES
    (multiple-value-bind (n-recv-msg
                          n-recv-msg-n-h
                          n-recv-pend-msgs)
        ;; We have three groups of receivers, one using
        ;; RECEIVE-MESSAGE, one RECEIVE-MESSAGE-NO-HANG, and
        ;; another one RECEIVE-PENDING-MESSAGES.
        (receiver-distribution n-receivers)
      (let ((senders
             (make-threads n-senders "SENDER"
                           #'(lambda ()
                               (dotimes (i n-messages t)
                                 (send-message mbox i)
                                 (sleep (random +sleep+))))))
            (receivers
             (flet ((process-msg (msg out)
                      (cond
                        ((eq msg +fin-token+)
                         (funcall out t))
                        ((not (< -1 msg n-messages))
                         (funcall out nil))
                        (t
                         (sb-ext:atomic-incf (counter-ref counter))))))
               (append
                (make-threads n-recv-msg "RECV-MSG"
                  #'(lambda ()
                      (sleep (random +sleep+))
                      (loop (process-msg (receive-message mbox)
                                         #'(lambda (x) (return x))))))
                (make-threads n-recv-pend-msgs "RECV-PEND-MSGS"
                  #'(lambda ()
                      (loop
                        (sleep (random +sleep+))
                        (mapc #'(lambda (msg)
                                  (process-msg msg #'(lambda (x) (return x))))
                              (receive-pending-messages mbox +blksize+)))))
                (make-threads n-recv-msg-n-h "RECV-MSG-NO-HANG"
                  #'(lambda ()
                      (loop
                        (sleep (random +sleep+))
                        (multiple-value-bind (msg ok)
                            (receive-message-no-hang mbox)
                          (when ok
                            (process-msg msg #'(lambda (x)
                                                 (return x))))))))))))

        (when interruptor
          (funcall interruptor (append receivers senders)))
        (let ((garbage  0)
              (errors   0)
              (timeouts 0))
          (flet ((wait-for (threads)
                   (mapc #'(lambda (thread)
                             (ecase (timed-join-thread thread)
                               ((t))
                               ((nil)      (incf garbage))
                               ((:aborted) (incf errors))
                               ((:timeout) (incf timeouts)
                                           (kill-thread thread))))
                         threads)))
            ;; First wait until all messages are propagating.
            (wait-for senders)
            ;; Senders are finished, inform and wait for the
            ;; receivers.
            (loop repeat (+ n-recv-msg
                            n-recv-msg-n-h
                            (* n-recv-pend-msgs +blksize+))
                  ;; The number computed above is an upper bound; if
                  ;; we send as many FINs as that, we can be sure that
                  ;; every receiver must have got at least one FIN.
                  do (send-message mbox +fin-token+))
            (wait-for receivers)
            ;; We may in fact have sent too many FINs, so make sure
            ;; it's only FINs in the mailbox now.
            (mapc #'(lambda (msg) (unless (eq msg +fin-token+)
                                    (incf garbage)))
                  (list-mailbox-messages mbox))
            (values  `(:received . ,(counter-ref counter))
                     `(:garbage  . ,garbage)
                     `(:errors   . ,errors)
                     `(:timeouts . ,timeouts))))))))

(deftest mailbox.single-producer-single-consumer
    (test-mailbox-producers-consumers :n-senders 1
                                      :n-receivers 1
                                      :n-messages 1000)
  (:received . 1000)
  (:garbage  . 0)
  (:errors   . 0)
  (:timeouts . 0))

(deftest mailbox.single-producer-multiple-consumers
    (test-mailbox-producers-consumers :n-senders 1
                                      :n-receivers (if (> *cpus* 1) 100 50)
                                      :n-messages 1000)
  (:received . 1000)
  (:garbage  . 0)
  (:errors   . 0)
  (:timeouts . 0))

(deftest mailbox.multiple-producers-single-consumer
    (test-mailbox-producers-consumers :n-senders 10
                                      :n-receivers 1
                                      :n-messages 100)
  (:received . 1000)
  (:garbage  . 0)
  (:errors   . 0)
  (:timeouts . 0))

(deftest mailbox.multiple-producers-multiple-consumers
    (test-mailbox-producers-consumers :n-senders 50
                                      :n-receivers 50
                                      :n-messages 1000)
  (:received . 50000)
  (:garbage  . 0)
  (:errors   . 0)
  (:timeouts . 0))

(deftest mailbox.interrupts-safety.1
    (multiple-value-bind (received garbage errors timeouts)
        (test-mailbox-producers-consumers
         :n-senders 50
         :n-receivers 50
         :n-messages 1000
         :interruptor #'(lambda (threads &aux (n (length threads)))
                          ;; 99 so even in the unlikely case that only
                          ;; receivers (or only senders) are shot
                          ;; dead, there's still one that survives to
                          ;; properly end the test.
                          (loop repeat 99
                                for victim = (nth (random n) threads)
                                do (kill-thread victim)
                                   (sleep (random 0.0001)))))
      (values
       ;; We may have killed a receiver before it got to incrementing
       ;; the counter.
       (if (<= (cdr received) 1000000)
           `(:received . :ok)
           received)
       garbage
       ;; we may have gotten errors due to our killing spree.
       timeouts))
  (:received . :ok)
  (:garbage  . 0)
  (:timeouts . 0))

) ; #+sb-thread (progn ...

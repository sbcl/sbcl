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
(progn

;; Dummy struct for ATOMIC-INCF to work.
(defstruct counter
  (ref 0 :type sb-vm:word))

(defun test-mailbox-producers-consumers
    (&key n-senders n-receivers n-messages mailbox interruptor)
  (let* ((cnt  (make-counter))
         (mbox (or mailbox (make-mailbox)))
         (senders
          (make-threads n-senders "SENDER"
            #'(lambda ()
                (dotimes (i n-messages)
                  (send-message mbox i)
                  (sleep (random 0.001))))))
         (receivers
          ;; We have three groups of receivers, one using
          ;; RECEIVE-MESSAGE, one RECEIVE-MESSAGE-NO-HANG, and another
          ;; one RECEIVE-PENDING-MESSAGES.
          (let* ((aux              (floor n-receivers 2))
                 (n-recv-msg       (- n-receivers aux))
                 (n-recv-pend-msgs (floor aux 3))
                 (n-recv-msg-n-h   (- aux n-recv-pend-msgs)))
            (append
             (make-threads n-recv-msg "RECV-MSG"
               #'(lambda ()
                   (sleep (random 0.001))
                   (handler-case
                       (loop
                         (sb-sys:with-deadline (:seconds 1.0)
                           (let ((msg (receive-message mbox)))
                             (sb-ext:atomic-incf (counter-ref cnt))
                             (unless (< -1 msg n-messages)
                               (hang)))))
                     (sb-ext:timeout ()))))
             (make-threads n-recv-pend-msgs "RECV-PEND-MSGS"
               #'(lambda ()
                   (sleep (random 0.001))
                   (dotimes (i 10)
                     (thread-yield)
                     (let ((msgs (receive-pending-messages mbox (random 5))))
                       (mapc #'(lambda (msg)
                                 (sb-ext:atomic-incf (counter-ref cnt))
                                 (unless (< -1 msg n-messages)
                                   (hang)))
                             msgs)))))
             (make-threads n-recv-msg-n-h "RECV-MSG-NO-HANG"
               #'(lambda ()
                   (sleep (random 0.001))
                   (dotimes (i 30)
                     (thread-yield)
                     (multiple-value-bind (msg ok)
                         (receive-message-no-hang mbox)
                       (when ok
                         (sb-ext:atomic-incf (counter-ref cnt))
                         (unless (< -1 msg n-messages)
                           (hang))))))))))
         (threads (append receivers senders)))
    (when interruptor (funcall interruptor threads))
    (mapc #'timed-join-thread threads)
    (values mbox (counter-ref cnt) (* n-senders n-messages))))

(deftest mailbox.single-producer-single-consumer
    (multiple-value-bind (mbox received total)
        (test-mailbox-producers-consumers :n-senders 1
                                          :n-receivers 1
                                          :n-messages 10000)
      (values
       (= received total)
       (mailbox-count mbox)
       (list-mailbox-messages mbox)))
  t
  0
  nil)

(deftest mailbox.single-producer-multiple-consumers
    (multiple-value-bind (mbox received total)
        (test-mailbox-producers-consumers :n-senders 1
                                          :n-receivers 100
                                          :n-messages 10000)
      (values
       (= received total)
       (mailbox-count mbox)
       (list-mailbox-messages mbox)))
  t
  0
  nil)

(deftest mailbox.multiple-producers-single-consumer
    (multiple-value-bind (mbox received total)
        (test-mailbox-producers-consumers :n-senders 100
                                          :n-receivers 10
                                          :n-messages 1000)
      (values
       (= received total)
       (mailbox-count mbox)
       (list-mailbox-messages mbox)))
  t
  0
  nil)

(deftest mailbox.multiple-producers-multiple-consumers
    (multiple-value-bind (mbox received total)
        (test-mailbox-producers-consumers :n-senders 100
                                          :n-receivers 100
                                          :n-messages 1000)
      (values
       (= received total)
       (mailbox-count mbox)
       (list-mailbox-messages mbox)))
  t
  0
  nil)

(deftest mailbox.interrupts-safety.1
    (multiple-value-bind (mbox received total)
        (test-mailbox-producers-consumers
         :n-senders 100
         :n-receivers 100
         :n-messages 1000
         :interruptor #'(lambda (threads)
                          (let ((n (length threads)))
                            ;; 99 so even in the unlikely case that only
                            ;; receivers (or only senders) are shot
                            ;; dead, there's still one that survives to
                            ;; properly end the test.
                            (loop repeat 99 do
                              (kill-thread (nth (random n) threads))))))
      (values
       ;; We may have killed a receiver before it got to incrementing
       ;; the counter.
       (<= received total)
       (mailbox-count mbox)
       (list-mailbox-messages mbox)))
  t
  0
  nil)

) ; #+sb-thread (progn ...
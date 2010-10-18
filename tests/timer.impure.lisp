;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; While most of SBCL is derived from the CMU CL system, the test
;;;; files (like this one) were written from scratch after the fork
;;;; from CMU CL.
;;;;
;;;; This software is in the public domain and is provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for
;;;; more information.

(in-package "CL-USER")

(use-package :test-util)

(with-test (:name :heap)
  (let* ((size 1000)
         (heap (make-array size :adjustable t :fill-pointer 0))
         (unsorted (loop for i below size collect (random size)))
         (sorted (sort (copy-list unsorted) #'>=))
         heap-sorted)
    (map nil #'(lambda (val) (sb-impl::heap-insert heap val)) unsorted)
    (setf heap-sorted (loop for i below size
                            collect (sb-impl::heap-extract-maximum heap)))
    (unless (equal sorted heap-sorted)
      (error "Heap sort failure ~S" heap-sorted))))

(sb-alien:define-alien-routine "check_deferrables_blocked_or_lose"
    void
  (where sb-alien:unsigned-long))
(sb-alien:define-alien-routine "check_deferrables_unblocked_or_lose"
    void
  (where sb-alien:unsigned-long))

(defun make-limited-timer (fn n &rest args)
  (let (timer)
    (setq timer
          (apply #'sb-ext:make-timer
                 (lambda ()
                   (sb-sys:without-interrupts
                     (decf n)
                     (cond ((minusp n)
                            (warn "Unscheduling timer ~A ~
                                   upon reaching run limit. System too slow?"
                                  timer)
                            (sb-ext:unschedule-timer timer))
                           (t
                            (sb-sys:allow-with-interrupts
                              (funcall fn))))))
                 args))))

(defun make-and-schedule-and-wait (fn time)
  (let ((finishedp nil))
    (sb-ext:schedule-timer (sb-ext:make-timer
                            (lambda ()
                              (sb-sys:without-interrupts
                                (unwind-protect
                                     (sb-sys:allow-with-interrupts
                                       (funcall fn))
                                  (setq finishedp t)))))
                           time)
    (loop until finishedp)))

#-win32
(with-test (:name (:timer :deferrables-blocked))
  (make-and-schedule-and-wait (lambda ()
                                (check-deferrables-blocked-or-lose 0))
                              (random 0.1))
  (check-deferrables-unblocked-or-lose 0))

#-win32
(with-test (:name (:timer :deferrables-unblocked))
  (make-and-schedule-and-wait (lambda ()
                                (sb-sys:with-interrupts
                                  (check-deferrables-unblocked-or-lose 0)))
                              (random 0.1))
  (check-deferrables-unblocked-or-lose 0))

#-win32
(with-test (:name (:timer :deferrables-unblocked :unwind))
  (catch 'xxx
    (make-and-schedule-and-wait (lambda ()
                                  (check-deferrables-blocked-or-lose 0)
                                  (throw 'xxx nil))
                                (random 0.1))
    (sleep 1))
  (check-deferrables-unblocked-or-lose 0))

(defmacro raises-timeout-p (&body body)
  `(handler-case (progn (progn ,@body) nil)
    (sb-ext:timeout () t)))

#-win32
(with-test (:name (:timer :relative)
            :fails-on '(and :sparc :linux))
  (let* ((has-run-p nil)
         (timer (make-timer (lambda () (setq has-run-p t))
                            :name "simple timer")))
    (schedule-timer timer 0.5)
    (sleep 0.2)
    (assert (not has-run-p))
    (sleep 0.5)
    (assert has-run-p)
    (assert (zerop (length (sb-impl::%pqueue-contents sb-impl::*schedule*))))))

#-win32
(with-test (:name (:timer :absolute)
            :fails-on '(and :sparc :linux))
  (let* ((has-run-p nil)
         (timer (make-timer (lambda () (setq has-run-p t))
                            :name "simple timer")))
    (schedule-timer timer (+ 1/2 (get-universal-time)) :absolute-p t)
    (sleep 0.2)
    (assert (not has-run-p))
    (sleep 0.5)
    (assert has-run-p)
    (assert (zerop (length (sb-impl::%pqueue-contents sb-impl::*schedule*))))))

#+sb-thread
(with-test (:name (:timer :other-thread))
  (let* ((thread (sb-thread:make-thread (lambda () (sleep 2))))
         (timer (make-timer (lambda ()
                              (assert (eq thread sb-thread:*current-thread*)))
                            :thread thread)))
    (schedule-timer timer 0.1)))

#+sb-thread
(with-test (:name (:timer :new-thread))
  (let* ((original-thread sb-thread:*current-thread*)
         (timer (make-timer
                 (lambda ()
                   (assert (not (eq original-thread
                                    sb-thread:*current-thread*))))
                 :thread t)))
    (schedule-timer timer 0.1)))

#-win32
(with-test (:name (:timer :repeat-and-unschedule)
            :fails-on '(and :sparc :linux))
  (let* ((run-count 0)
         timer)
    (setq timer
          (make-timer (lambda ()
                        (when (= 5 (incf run-count))
                          (unschedule-timer timer)))))
    (schedule-timer timer 0 :repeat-interval 0.2)
    (assert (timer-scheduled-p timer :delta 0.3))
    (sleep 1.3)
    (assert (= 5 run-count))
    (assert (not (timer-scheduled-p timer)))
    (assert (zerop (length (sb-impl::%pqueue-contents sb-impl::*schedule*))))))

#-win32
(with-test (:name (:timer :reschedule))
  (let* ((has-run-p nil)
         (timer (make-timer (lambda ()
                              (setq has-run-p t)))))
    (schedule-timer timer 0.2)
    (schedule-timer timer 0.3)
    (sleep 0.5)
    (assert has-run-p)
    (assert (zerop (length (sb-impl::%pqueue-contents sb-impl::*schedule*))))))

#-win32
(with-test (:name (:timer :stress))
  (let ((time (1+ (get-universal-time))))
    (loop repeat 200 do
             (schedule-timer (make-timer (lambda ())) time :absolute-p t))
    (sleep 2)
    (assert (zerop (length (sb-impl::%pqueue-contents sb-impl::*schedule*))))))

#-win32
(with-test (:name (:timer :stress2))
  (let ((time (1+ (get-universal-time)))
        (n 0))
    (loop for time-n from time upto (+ 1/10 time) by (/ 1/10 200)
          do (schedule-timer (make-timer (lambda ())) time-n :absolute-p t)
             (incf n))
    (sleep 2)
    (assert (zerop (length (sb-impl::%pqueue-contents sb-impl::*schedule*))))))

#-win32
(with-test (:name (:with-timeout :timeout))
  (assert (raises-timeout-p
           (sb-ext:with-timeout 0.2
             (sleep 1)))))

#-win32
(with-test (:name (:with-timeout :fall-through))
  (assert (not (raises-timeout-p
                (sb-ext:with-timeout 0.3
                  (sleep 0.1))))))

#-win32
(with-test (:name (:with-timeout :nested-timeout-smaller))
  (assert(raises-timeout-p
          (sb-ext:with-timeout 10
            (sb-ext:with-timeout 0.5
              (sleep 2))))))

#-win32
(with-test (:name (:with-timeout :nested-timeout-bigger))
  (assert(raises-timeout-p
          (sb-ext:with-timeout 0.5
            (sb-ext:with-timeout 2
              (sleep 2))))))

(defun wait-for-threads (threads)
  (loop while (some #'sb-thread:thread-alive-p threads) do (sleep 0.01)))

#+sb-thread
(with-test (:name (:with-timeout :many-at-the-same-time))
  (let ((ok t))
    (let ((threads (loop repeat 10 collect
                         (sb-thread:make-thread
                          (lambda ()
                            (handler-case
                                (sb-ext:with-timeout 0.5
                                  (sleep 5)
                                  (setf ok nil)
                                  (format t "~%not ok~%"))
                              (timeout ()
                                )))))))
      (assert (not (raises-timeout-p
                    (sb-ext:with-timeout 20
                      (wait-for-threads threads)))))
      (assert ok))))

#+sb-thread
(with-test (:name (:with-timeout :dead-thread))
  (sb-thread:make-thread
   (lambda ()
     (let ((timer (make-timer (lambda ()))))
       (schedule-timer timer 3)
       (assert t))))
  (sleep 6)
  (assert t))


(defun random-type (n)
  `(integer ,(random n) ,(+ n (random n))))

;;; FIXME: Since timeouts do not work on Windows this would loop
;;; forever.
#-win32
(with-test (:name (:hash-cache :interrupt))
  (let* ((type1 (random-type 500))
         (type2 (random-type 500))
         (wanted (subtypep type1 type2)))
    (dotimes (i 100)
      (block foo
        (sb-ext:schedule-timer (sb-ext:make-timer
                                (lambda ()
                                  (assert (eq wanted (subtypep type1 type2)))
                                    (return-from foo)))
                               0.05)
        (loop
           (assert (eq wanted (subtypep type1 type2))))))))

;;; Used to hang occasionally at least on x86. Two bugs caused it:
;;; running out of stack (due to repeating timers being rescheduled
;;; before they ran) and dying threads were open interrupts.
#+sb-thread
(with-test (:name (:timer :parallel-unschedule) :fails-on :ppc)
  #+darwin
  (error "Prone to hang on Darwin due to interrupt issues.")
  #+ppc
  (error "Prone to hang the host on linux/ppc for unknown reasons.")
  (let ((timer (sb-ext:make-timer (lambda () 42) :name "parallel schedulers"))
        (other nil))
    (flet ((flop ()
             (sleep (random 0.01))
             (loop repeat 10000
                   do (sb-ext:unschedule-timer timer))))
      (loop repeat 5
            do (mapcar #'sb-thread:join-thread
                       (loop for i from 1 upto 10
                             collect (let* ((thread (sb-thread:make-thread #'flop
                                                                           :name (format nil "scheduler ~A" i)))
                                            (ticker (make-limited-timer (lambda () 13)
                                                                               1000
                                                                               :thread (or other thread)
                                                                               :name (format nil "ticker ~A" i))))
                                       (setf other thread)
                                       (sb-ext:schedule-timer ticker 0 :repeat-interval 0.00001)
                                       thread)))))))

;;;; FIXME: OS X 10.4 doesn't like these being at all, and gives us a SIGSEGV
;;;; instead of using the Mach expection system! 10.5 on the other tends to
;;;; lose() here with interrupt already pending. :/
;;;;
;;;; Used to have problems in genereal, see comment on (:TIMER
;;;; :PARALLEL-UNSCHEDULE).
#-win32
(with-test (:name (:timer :schedule-stress))
  (flet ((test ()
         (let* ((slow-timers
                 (loop for i from 1 upto 1
                       collect (make-limited-timer
                                (lambda () 13)
                                1000
                                :name (format nil "slow ~A" i))))
                (fast-timer (make-limited-timer (lambda () 42) 1000
                                                :name "fast")))
           (sb-ext:schedule-timer fast-timer 0.0001 :repeat-interval 0.0001)
           (dolist (timer slow-timers)
             (sb-ext:schedule-timer timer (random 0.1)
                                    :repeat-interval (random 0.1)))
           (dolist (timer slow-timers)
             (sb-ext:unschedule-timer timer))
           (sb-ext:unschedule-timer fast-timer))))
  #+sb-thread
  (mapcar #'sb-thread:join-thread
          (loop repeat 10 collect (sb-thread:make-thread #'test)))
  #-sb-thread
  (loop repeat 10 do (test))))

#+sb-thread
(with-test (:name (:timer :threaded-stress))
  (let ((barrier (sb-thread:make-semaphore))
        (goal 100))
    (flet ((wait-for-goal ()
             (let ((*n* 0))
               (declare (special *n*))
               (sb-thread:signal-semaphore barrier)
               (loop until (eql *n* goal))))
           (one ()
             (declare (special *n*))
             (incf *n*)))
      (let ((threads (list (sb-thread:make-thread #'wait-for-goal)
                           (sb-thread:make-thread #'wait-for-goal)
                           (sb-thread:make-thread #'wait-for-goal))))
        (sb-thread:wait-on-semaphore barrier)
        (sb-thread:wait-on-semaphore barrier)
        (sb-thread:wait-on-semaphore barrier)
        (flet ((sched (thread)
                 (sb-thread:make-thread (lambda ()
                                          (loop repeat goal
                                                do (sb-ext:schedule-timer (make-timer #'one :thread thread) 0.001))))))
          (dolist (thread threads)
            (sched thread)))
        (mapcar #'sb-thread:join-thread threads)))))

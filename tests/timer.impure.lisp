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

(defmacro raises-timeout-p (&body body)
  `(handler-case (progn (progn ,@body) nil)
    (sb-ext:timeout () t)))

(with-test (:name (:timer :relative))
  (let* ((has-run-p nil)
         (timer (make-timer (lambda () (setq has-run-p t))
                            :name "simple timer")))
    (schedule-timer timer 0.5)
    (sleep 0.2)
    (assert (not has-run-p))
    (sleep 0.5)
    (assert has-run-p)
    (assert (zerop (length (sb-impl::%pqueue-contents sb-impl::*schedule*))))))

(with-test (:name (:timer :absolute))
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

(with-test (:name (:timer :repeat-and-unschedule))
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

(with-test (:name (:timer :reschedule))
  (let* ((has-run-p nil)
         (timer (make-timer (lambda ()
                              (setq has-run-p t)))))
    (schedule-timer timer 0.2)
    (schedule-timer timer 0.3)
    (sleep 0.5)
    (assert has-run-p)
    (assert (zerop (length (sb-impl::%pqueue-contents sb-impl::*schedule*))))))

(with-test (:name (:timer :stress))
  (let ((time (1+ (get-universal-time))))
    (loop repeat 200 do
          (schedule-timer (make-timer (lambda ())) time :absolute-p t))
    (sleep 2)
    (assert (zerop (length (sb-impl::%pqueue-contents sb-impl::*schedule*))))))

(with-test (:name (:with-timeout :timeout))
  (assert (raises-timeout-p
           (sb-ext:with-timeout 0.2
             (sleep 1)))))

(with-test (:name (:with-timeout :fall-through))
  (assert (not (raises-timeout-p
                (sb-ext:with-timeout 0.3
                  (sleep 0.1))))))

(with-test (:name (:with-timeout :nested-timeout-smaller))
  (assert(raises-timeout-p
          (sb-ext:with-timeout 10
            (sb-ext:with-timeout 0.5
              (sleep 2))))))

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

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

(with-test (:name (:timer :threaded-stress)
            :skipped-on (not :sb-thread)
            :broken-on :x86
            :fails-on :win32)
  #+win32
  (error "fixme")
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
        (loop for thread in threads
              do (sb-thread:join-thread thread :timeout 20))))))

;; A timer with a repeat interval can be configured to "catch up" in
;; case of missed calls.


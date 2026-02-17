;;;; Test errors signaled when saving a core fails early.

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

(with-test (:name (sb-ext:save-lisp-and-die error :multiple-threads)
            :skipped-on (:not :sb-thread))
  (let* ((mutex (sb-thread:make-mutex))
         (cvar (sb-thread:make-waitqueue))
         (donep nil)
         (thread (sb-thread:make-thread
                  (lambda ()
                    (sb-thread:with-mutex (mutex)
                      (loop until donep
                            do (sb-thread:condition-wait cvar mutex)))))))
    (assert-error (save-lisp-and-die "./test")
                  sb-impl::save-with-multiple-threads-error)
    (sb-thread:with-mutex (mutex)
      (setf donep t)
      (sb-thread:condition-notify cvar))
    (sb-thread:join-thread thread)))

(with-test (:name (sb-ext:save-lisp-and-die error :multiple-threads-2))
  (let* ((sem (sb-thread:make-semaphore))
         (thread (sb-thread:make-thread
                  (lambda () (sb-thread:wait-on-semaphore sem)))))
    (assert (eq (handler-case (save-lisp-and-die "expect_failure")
                  (sb-impl::save-with-multiple-threads-error () 'ok))
                'ok))
    (assert (sb-thread::thread-p sb-impl::*finalizer-thread*))
    (sb-thread:signal-semaphore sem)
    (sb-thread:join-thread thread)))

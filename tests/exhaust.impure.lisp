;;;; tests of the system's ability to catch resource exhaustion problems

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

#+interpreter (invoke-restart 'run-tests::skip-file)

(test-util::disable-profiling)

;;; Prior to sbcl-0.7.1.38, doing something like (RECURSE), even in
;;; safe code, would crash the entire Lisp process. Then the soft
;;; stack checking was introduced, which checked (in safe code) for
;;; stack exhaustion at each lambda.

;;; Post 0.7.6.1, this was rewritten to use mprotect()-based stack
;;; protection which does not require lisp code to check anything,
;;; and works at all optimization settings.  However, it now signals a
;;; STORAGE-CONDITION instead of an ERROR.

(defun recurse ()
  (recurse)
  (recurse))

(defvar *count* 100)

;; Don't want to keep seeing failures that happen under parallel-exec
;; (It's not even "random" now - it's pretty reliable)
;; Gotta do with fork() or something that I don't care to diagnose.
#+(and darwin x86-64 parallel-test-runner) (invoke-restart 'run-tests::skip-file)

(setf (extern-alien "lose_on_corruption_p" int) 0)

;;; Base-case: detecting exhaustion
(with-test (:name (:exhaust :basic))
  (assert (eq :exhausted
              (handler-case
                  (recurse)
                (storage-condition (c)
                  (declare (ignore c))
                  :exhausted)))))

;;; Regression test: we used to misconfigure the stack for new threads on win32.
#+sb-thread
(with-test (:name (:exhaust :basic :new-thread))
  (assert (eq :exhausted
              (sb-thread:join-thread
               (sb-thread:make-thread
                (lambda ()
                  (handler-case
                      (recurse)
                    (storage-condition (c)
                      (declare (ignore c))
                      :exhausted))))))))

;;; Check that non-local control transfers restore the stack
;;; exhaustion checking after unwinding -- and that previous test
;;; didn't break it.
(with-test (:name (:exhaust :non-local-control))
  (let ((exhaust-count 0)
        (recurse-count 0))
    (tagbody
     :retry
       (handler-bind ((storage-condition (lambda (c)
                                           (declare (ignore c))
                                           (if (= *count* (incf exhaust-count))
                                               (go :stop)
                                               (go :retry)))))
         (incf recurse-count)
         (recurse))
     :stop)
    (assert (= exhaust-count recurse-count *count*))))

;;; Check that we can safely use user-provided restarts to
;;; unwind.
(with-test (:name (:exhaust :restarts))
  (let ((exhaust-count 0)
        (recurse-count 0))
    (block nil
      (handler-bind ((storage-condition (lambda (c)
                                          (declare (ignore c))
                                          (if (= *count* (incf exhaust-count))
                                              (return)
                                              (invoke-restart (find-restart 'ok))))))
        (loop
           (with-simple-restart (ok "ok")
             (incf recurse-count)
             (recurse)))))
    (assert (= exhaust-count recurse-count *count*))))

;;; We can get away with writing to the stack on unwind as long as we don't
;;; write on the Windows-managed control stack guard page /after/ triggering the
;;; memory protection set up by win32_reset_stack_overflow_guard_page(). This is
;;; certainly not fool proof, though.
(with-test (:name (:exhaust :write-to-stack-on-unwind)
            :skipped-on (not :win32))
  (let ((count 0))
    (labels ((recurse-and-write-to-stack-on-unwind ()
               (let ((x (random 1.0)))
                 (unwind-protect
                      (recurse-and-write-to-stack-on-unwind)
                   (incf x (random 1.0))))))
      (block nil
        (handler-bind ((storage-condition (lambda (c)
                                            (format t "Got ~s, unwinding...~%" c)
                                            (incf count)
                                            (return))))
          (recurse-and-write-to-stack-on-unwind))))
    (assert (eql count 1))))

;;; On Windows, we can freely write to the stack while handling
;;; STORAGE-CONDITION because its equivalent to CONTROL_STACK_RETURN_GUARD_PAGE
;;; is only set up while unwinding from the STACK_OVERFLOW_EXCEPTION handler.
;;; On other platforms, while handling the initial STORAGE-CONDITION, this test
;;; will re-trigger the CONTROL_STACK_RETURN_GUARD_PAGE and
;;; CONTROL_STACK_GUARD_PAGE, and issue a CORRUPTION WARNING.
(with-test (:name (:exhaust :write-to-stack-in-handler)
            :skipped-on (not :win32))
  (labels ((recurse-and-write-to-stack-on-error ()
             (let ((x 0))
               (handler-bind ((storage-condition (lambda (c)
                                                   (declare (ignore c))
                                                   (setq x (random 1.0)))))
                 (recurse-and-write-to-stack-on-error)
                 x))))
    (handler-case (recurse-and-write-to-stack-on-error)
      (storage-condition ()))))

(with-test (:name (:exhaust :binding-stack))
  (let ((ok nil)
        (symbols (loop repeat 1024 collect (gensym)))
        (values (loop repeat 1024 collect nil)))
    (gc :full t)
    (labels ((exhaust-binding-stack (i)
               (progv symbols values
                 (exhaust-binding-stack (1+ i)))))
      (handler-case
          (exhaust-binding-stack 0)
        (sb-kernel::binding-stack-exhausted ()
          (setq ok t)))
      (assert ok))))

(with-test (:name (:exhaust :alien-stack)
                  :skipped-on (not :c-stack-is-control-stack))
  (let ((ok nil))
    (labels ((exhaust-alien-stack (i)
               (with-alien ((integer-array (array int 500)))
                 (+ (deref integer-array 0)
                    (exhaust-alien-stack (1+ i))))))
      (handler-case
          (exhaust-alien-stack 0)
        (sb-kernel::alien-stack-exhausted ()
          (setq ok t)))
      (assert ok))))

;;; OK!

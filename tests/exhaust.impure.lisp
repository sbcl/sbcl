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

#+interpreter (sb-ext:exit :code 104)

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

;;; Base-case: detecting exhaustion
(with-test (:name (:exhaust :basic))
  (assert (eq :exhausted
              (handler-case
                  (recurse)
                (storage-condition (c)
                  (declare (ignore c))
                  :exhausted)))))

;;; Check that non-local control transfers restore the stack
;;; exhaustion checking after unwinding -- and that previous test
;;; didn't break it.
(with-test (:name (:exhaust :non-local-control)
                  :skipped-on :win32)
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
(with-test (:name (:exhaust :restarts)
                  :skipped-on :win32)
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
                  :skipped-on (or (not :c-stack-is-control-stack)))
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

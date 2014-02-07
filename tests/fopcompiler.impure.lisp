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

;;; These tests don't need to be processed by the compiler before
;;; being executed.

(defvar *tmp-filename* "fopcompile-test.tmp")

;; Ensure we can get a style-warning about undefined functions from FOPCOMPILE.
(with-test (:name :fopcompiler-undefined-warning)
  (let ((form '(defvar *foo* (i-do-not-exist))))
    ;; Assert that the test case is handled by the fopcompiler.
    (let ((sb-c::*lexenv* (sb-kernel:make-null-lexenv)))
      (assert (sb-c::fopcompilable-p form)))
    ;; Make sure some wiseacre didn't defconstant *FOO*
    (assert (eq (sb-int:info :variable :kind '*foo*) :unknown))
    ;; ... or define the I-DO-NOT-EXIST function.
    (assert (eq (sb-int:info :function :where-from 'i-do-not-exist) :assumed))
    (with-open-file (stream *tmp-filename*
                            :direction :output :if-exists :supersede)
      (prin1 form stream))
    (multiple-value-bind (output warningp errorp)
        (compile-file *tmp-filename*)
      (when output
        (delete-file output))
      (assert (and warningp (not errorp))))))

(ignore-errors (delete-file *tmp-filename*))

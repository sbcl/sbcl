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
;;; being executed, in fact mustn't go in "fopcompiler.impure-cload.lisp"
;;; because the call to COMPILE-FILE needs to be wrapped in HANDLER-BIND.

(defvar *tmp-filename* (scratch-file-name))

;; Assert that FORM is handled by the fopcompiler, then compile it.
(defun assert-fopcompilable-and-compile-it (form)
  ;; Since FOPCOMPILABLE-P now expands compiler-macros, and the macro for
  ;; SOURCE-LOCATION expands to a literal structure, we end up calling
  ;; CONSTANT-FOPCOMPILABLE-P which needs *COMPILE-OBJECT* to be bound.
  (let ((sb-c::*compile-object*
         (sb-fasl::make-fasl-output :stream (make-broadcast-stream)))
        (sb-c::*lexenv* (sb-kernel:make-null-lexenv)))
    (assert (sb-c::fopcompilable-p form))
    (with-open-file (stream *tmp-filename*
                            :direction :output :if-exists :supersede)
      (prin1 form stream))
    (let (warning)
      (handler-bind ((warning
                      (lambda (c)
                        (when (null warning)
                          (setq warning c)
                          (muffle-warning)))))
        (multiple-value-bind (output warningp errorp)
            (compile-file *tmp-filename*)
          (when output
            (delete-file output))
          (if (and (not warningp) (not errorp))
              ;; return muffled warning, which didn't count as a warning
              warning))))))

;; Ensure we can get a style-warning about undefined functions from FOPCOMPILE.
(with-test (:name :fopcompiler-undefined-warning)
  ;; Make sure some wiseacre didn't defconstant *FOO*
  (assert (eq (sb-int:info :variable :kind '*foo*) :unknown))
  ;; ... or define the I-DO-NOT-EXIST function.
  (assert (eq (sb-int:info :function :where-from 'i-do-not-exist) :assumed))
  (let ((w (assert-fopcompilable-and-compile-it
            '(defvar *foo* (i-do-not-exist)))))
    (assert (and (typep w 'sb-int:simple-style-warning)
                 (eql (search "undefined"
                              (write-to-string w :escape nil))
                      0)))))

;; Note: This tests fails, but for a bad reason, as opposed to the wrong reason
;; (which was also bad). It used to fail because the name of the variable that it
;; used was completely removed, so it failed with "unknown variable".
;; Nobody noticed because the test got marked as failing when the variable was
;; removed. But now despite addition of a new deprecated variable, it fails
;; because it's *actually* failing, because the thing it tests got broken.
;; Ensure that FOPCOMPILE warns about deprecated variables.
(sb-int:define-deprecated-variable :late "1.1.4.9" *i-am-deprecated*)
(with-test (:name :fopcompiler-deprecated-var-warning
            :fails-on :sbcl)
  (assert (typep (assert-fopcompilable-and-compile-it
                  '(defvar *frob* (if *i-am-deprecated* 'yes 'no)))
                 'sb-ext:deprecation-condition)))

(ignore-errors (delete-file *tmp-filename*))

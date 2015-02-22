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

(load "assertoid.lisp")
(use-package '#:assertoid)

;;;; Helpers

(defun check-deprecated-thing (kind name state make-body)
  (flet ((search-string (string)
           (dolist (fragment `(,(string name)
                                "deprecated" "as" "of" "SBCL" "1.2.10"
                                "Use"
                                ,(format nil "~A.~A" name '#:replacement)
                                "instead"))
             (assert (search fragment string)))))
    ;; Check the signaled warning condition.
    (let* ((condition)
           (function (handler-bind
                         ((warning (lambda (c)
                                     (setf condition c)
                                     (muffle-warning))))
                       (compile nil `(lambda ()
                                       ,@(funcall make-body name))))))
      (assert (typep condition (ecase state
                                 (:early 'sb-int:early-deprecation-warning)
                                 (:late 'sb-int:late-deprecation-warning)
                                 (:final 'sb-int:final-deprecation-warning))))
      (search-string (princ-to-string condition))
      (ecase state
        ((:early :late)
         (assert (eq :deprecated (funcall function))))
        (:final
         (assert-error (funcall function) sb-int:deprecation-error))))
    ;; Check the documentation.
    (search-string (documentation name kind))))

;;;; Deprecated variables

(sb-impl::define-deprecated-variable :early "1.2.10"
  deprecated-variable.early
  :value :deprecated
  :replacement deprecated-variable.early.replacement)

(with-test (:name (sb-impl::define-deprecated-variable :early))
  (check-deprecated-thing 'variable 'deprecated-variable.early :early
                          (lambda (name) `(,name)))
  (check-deprecated-thing 'variable 'deprecated-variable.early :early
                          (lambda (name) `((symbol-value ',name))))
  (check-deprecated-thing 'variable 'deprecated-variable.early :early
                          (lambda (name) `((symbol-global-value ',name)))))

(sb-impl::define-deprecated-variable :late "1.2.10"
  deprecated-variable.late
  :value :deprecated
  :replacement deprecated-variable.late.replacement)

(with-test (:name (sb-impl::define-deprecated-variable :late))
  (check-deprecated-thing 'variable 'deprecated-variable.late :late
                          (lambda (name) `(,name)))
  (check-deprecated-thing 'variable 'deprecated-variable.late :late
                          (lambda (name) `((symbol-value ',name))))
  (check-deprecated-thing 'variable 'deprecated-variable.late :late
                          (lambda (name) `((symbol-global-value ',name)))))

(sb-impl::define-deprecated-variable :final "1.2.10"
  deprecated-variable.final
  :value :deprecated
  :replacement deprecated-variable.final.replacement)

(with-test (:name (sb-impl::define-deprecated-variable :final))
  (check-deprecated-thing 'variable 'deprecated-variable.final :final
                          (lambda (name) `(,name)))
  (check-deprecated-thing 'variable 'deprecated-variable.final :final
                          (lambda (name) `((symbol-value ',name))))
  (check-deprecated-thing 'variable 'deprecated-variable.final :final
                          (lambda (name) `((symbol-global-value ',name)))))


;;;; Deprecated functions

(sb-impl::define-deprecated-function :early "1.2.10"
    deprecated-function.early deprecated-function.early.replacement ()
  :deprecated)

(with-test (:name (sb-impl::define-deprecated-function :early))
  (check-deprecated-thing 'function 'deprecated-function.early :early
                          (lambda (name) `((,name)))))

(sb-impl::define-deprecated-function :late "1.2.10"
    deprecated-function.late deprecated-function.late.replacement ()
  :deprecated)

(with-test (:name (sb-impl::define-deprecated-function :late))
  (check-deprecated-thing 'function 'deprecated-function.late :late
                          (lambda (name) `((,name)))))

(sb-impl::define-deprecated-function :final "1.2.10"
    deprecated-function.final deprecated-function.final.replacement ()
  :deprecated)

(with-test (:name (sb-impl::define-deprecated-function :final))
  (check-deprecated-thing 'function 'deprecated-function.final :final
                          (lambda (name) `((,name)))))

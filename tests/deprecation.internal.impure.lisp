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

(defun check-deprecated-thing (kind name state make-body
                               &key (replacements
                                     (list (format nil "~A.~A"
                                                   name '#:replacement))))
  (flet ((search-string (string)
           (declare (ignorable string))
           (let ((start))
             (dolist (fragment `(,(string name)
                                  "deprecated" "as" "of" "SBCL" "1.2.10"
                                  "Use" ,@replacements "instead"))
               (let ((match (search fragment string :start2 (or start 0))))
                 (assert match)
                 (setf start (+ match (length fragment))))))))
    ;; Check the signaled warning condition.
    (let* ((condition)
           (count 0)
           (function (handler-bind
                         ((warning (lambda (c)
                                     (incf count)
                                     (setf condition c)
                                     (muffle-warning))))
                       (compile nil `(lambda ()
                                       ,@(funcall make-body name))))))
      (assert (= count 1))
      (assert (typep condition (ecase state
                                 (:early 'early-deprecation-warning)
                                 (:late 'late-deprecation-warning)
                                 (:final 'final-deprecation-warning))))
      (search-string (princ-to-string condition))
      (ecase state
        ((:early :late)
         (assert (eq :deprecated (funcall function))))
        (:final
         (assert-error (funcall function) deprecation-error))))
    ;; Check the documentation.
    #+sb-doc
    (search-string (documentation name kind))))

;;;; Deprecated variables

(sb-int:define-deprecated-variable :early "1.2.10"
  deprecated-variable.early
  :value :deprecated
  :replacement deprecated-variable.early.replacement)

(with-test (:name (sb-int:define-deprecated-variable :early))
  (check-deprecated-thing 'variable 'deprecated-variable.early :early
                          (lambda (name) `(,name)))
  (check-deprecated-thing 'variable 'deprecated-variable.early :early
                          (lambda (name) `((symbol-value ',name))))
  (check-deprecated-thing 'variable 'deprecated-variable.early :early
                          (lambda (name) `((symbol-global-value ',name)))))

(sb-int:define-deprecated-variable :late "1.2.10"
  deprecated-variable.late
  :value :deprecated
  :replacement deprecated-variable.late.replacement)

(with-test (:name (sb-int:define-deprecated-variable :late))
  (check-deprecated-thing 'variable 'deprecated-variable.late :late
                          (lambda (name) `(,name)))
  (check-deprecated-thing 'variable 'deprecated-variable.late :late
                          (lambda (name) `((symbol-value ',name))))
  (check-deprecated-thing 'variable 'deprecated-variable.late :late
                          (lambda (name) `((symbol-global-value ',name)))))

(sb-int:define-deprecated-variable :final "1.2.10"
  deprecated-variable.final
  :value :deprecated
  :replacement deprecated-variable.final.replacement)

(with-test (:name (sb-int:define-deprecated-variable :final))
  (check-deprecated-thing 'variable 'deprecated-variable.final :final
                          (lambda (name) `(,name)))
  (check-deprecated-thing 'variable 'deprecated-variable.final :final
                          (lambda (name) `((symbol-value ',name))))
  (check-deprecated-thing 'variable 'deprecated-variable.final :final
                          (lambda (name) `((symbol-global-value ',name)))))


;;;; Deprecated functions

(sb-int:define-deprecated-function :early "1.2.10"
    deprecated-function.early deprecated-function.early.replacement ()
  :deprecated)

(with-test (:name (sb-int:define-deprecated-function :early))
  (check-deprecated-thing 'function 'deprecated-function.early :early
                          (lambda (name) `((,name)))))

(sb-int:define-deprecated-function :late "1.2.10"
    deprecated-function.late deprecated-function.late.replacement ()
  :deprecated)

(with-test (:name (sb-int:define-deprecated-function :late))
  (check-deprecated-thing 'function 'deprecated-function.late :late
                          (lambda (name) `((,name)))))

(sb-int:define-deprecated-function :final "1.2.10"
    deprecated-function.final deprecated-function.final.replacement ()
  :deprecated)

(with-test (:name (sb-int:define-deprecated-function :final))
  (check-deprecated-thing 'function 'deprecated-function.final :final
                          (lambda (name) `((,name)))))

(sb-int:define-deprecated-function :early "1.2.10"
    deprecated-function.two-replacements
    (deprecated-function.two-replacements.replacement1
     deprecated-function.two-replacements.replacement2)
    ()
  :deprecated)

(with-test (:name (sb-int:define-deprecated-function :two-replacements))
  (check-deprecated-thing
   'function 'deprecated-function.two-replacements :early
   (lambda (name) `((,name)))
   :replacements '("DEPRECATED-FUNCTION.TWO-REPLACEMENTS.REPLACEMENT1"
                   "DEPRECATED-FUNCTION.TWO-REPLACEMENTS.REPLACEMENT2")))

(sb-int:define-deprecated-function :early "1.2.10"
    deprecated-function.three-replacements
    (deprecated-function.three-replacements.replacement1
     deprecated-function.three-replacements.replacement2
     deprecated-function.three-replacements.replacement3)
    ()
  :deprecated)

(with-test (:name (sb-int:define-deprecated-function :three-replacements))
  (check-deprecated-thing
   'function 'deprecated-function.three-replacements :early
   (lambda (name) `((,name)))
   :replacements '("DEPRECATED-FUNCTION.THREE-REPLACEMENTS.REPLACEMENT1"
                   "DEPRECATED-FUNCTION.THREE-REPLACEMENTS.REPLACEMENT2"
                   "DEPRECATED-FUNCTION.THREE-REPLACEMENTS.REPLACEMENT3")))

(with-test (:name :deftype-tricky-constant)
  (assert-signal (eval '(deftype cows () (if nil 'foo 'sb-thread::spinlock)))
                 warning))

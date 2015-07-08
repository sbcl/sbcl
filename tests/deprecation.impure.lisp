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
           #+sb-doc
           (dolist (fragment `(,(string name)
                                "deprecated" "as" "of" "SBCL" "1.2.10"
                                "Use"
                                ,@replacements
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

(sb-impl::define-deprecated-function :early "1.2.10"
    deprecated-function.two-replacements
    (deprecated-function.two-replacements.replacement1
     deprecated-function.two-replacements.replacement2)
    ()
  :deprecated)

(with-test (:name (sb-impl::define-deprecated-function :two-replacements))
  (check-deprecated-thing
   'function 'deprecated-function.two-replacements :early
   (lambda (name) `((,name)))
   :replacements '("DEPRECATED-FUNCTION.TWO-REPLACEMENTS.REPLACEMENT1"
                   "DEPRECATED-FUNCTION.TWO-REPLACEMENTS.REPLACEMENT2")))

(sb-impl::define-deprecated-function :early "1.2.10"
    deprecated-function.three-replacements
    (deprecated-function.three-replacements.replacement1
     deprecated-function.three-replacements.replacement2
     deprecated-function.three-replacements.replacement3)
    ()
  :deprecated)

(with-test (:name (sb-impl::define-deprecated-function :three-replacements))
  (check-deprecated-thing
   'function 'deprecated-function.three-replacements :early
   (lambda (name) `((,name)))
   :replacements '("DEPRECATED-FUNCTION.THREE-REPLACEMENTS.REPLACEMENT1"
                   "DEPRECATED-FUNCTION.THREE-REPLACEMENTS.REPLACEMENT2"
                   "DEPRECATED-FUNCTION.THREE-REPLACEMENTS.REPLACEMENT3")))


(sb-int:define-deprecated-function :early "1.2.10"
  please-dont-use-this moar-better-function (x) (identity x))
(sb-int:define-deprecated-function :late "1.2.10"
  really-dont-do-it use-other-thing-instead (x) (identity x))
(sb-int:define-deprecated-function :final "1.2.10"
  you-cant-use-this replacement-fn (x) (identity x))

(with-test (:name :introspect-deprecation-stage)
  (assert (eq (sb-int:deprecated-thing-p :function 'please-dont-use-this)
              :early))
  (assert (eq (sb-int:deprecated-thing-p :function 'really-dont-do-it)
              :late))
  (assert (eq (sb-int:deprecated-thing-p :function 'you-cant-use-this)
              :final)))

;; lp#1439151
(with-test (:name :late-deprecated-fun-doc :skipped-on '(not :sb-doc))
  (assert (string= (documentation 'you-cant-use-this 'function)
                   (documentation #'you-cant-use-this 'function)))
  (assert (string= (documentation 'deprecated-function.late 'function)
                   (documentation #'deprecated-function.late 'function)))
  (assert (string/= (documentation 'you-cant-use-this 'function)
                    (documentation 'deprecated-function.late 'function))))

(with-test (:name :load-time-deprecation-warning)
  (let ((source "load-test.tmp") fasl)
    (with-open-file (f source :direction :output
                       :if-does-not-exist :create :if-exists :supersede)
      (write-string "(defun a () (quit))" f)
      ;; a full warning even though the PLEASE-DONT- function is only :early
      (write-string "(defun b () (please-dont-use-this 1) (really-dont-do-it 2))" f)
      (write-string "(defun c () (you-cant-use-this 3))" f))
    ;; We expect four deprecation warnings from compiling the source
    ;; (four uses of deprecated things) and three from loading it
    ;; (loading three functions that contain uses of deprecated
    ;; things).
    (unwind-protect
         (progn (setq fasl
                      (assert-signal
                       (compile-file source :verbose nil :print nil)
                       (or early-deprecation-warning
                           late-deprecation-warning
                           final-deprecation-warning)
                       4))
                (assert-signal (load fasl) warning 3))
      (delete-file fasl)
      (delete-file source))))

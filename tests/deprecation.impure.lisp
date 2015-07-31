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

(defun check-deprecated-thing (namespace name state make-body
                               &key replacements
                                    (call t)
                                    (expected-warning-count '(eql 1))
                                    (check-describe t))
  (labels ((search-string (string fragments)
             (let ((start))
               (dolist (fragment fragments)
                 (let ((match (search fragment string :start2 (or start 0))))
                   (assert match)
                   (setf start (+ match (length fragment)))))))
           (search-string/documentation (string)
             (search-string
              string `(,(string-downcase namespace) ,(string name)
                        "deprecated" "as" "of" "some-lib" "version" "1.2.3"
                        "Use" ,@replacements "instead")))
           (search-string/describe (string)
             (search-string
              string `(,(string name) ,(string state)
                        "deprecation" "since" "some-lib" "version" "1.2.3"))))
    ;; Check the signaled warning condition.
    (let* ((condition)
           (count 0)
           (function (handler-bind
                         ((deprecation-condition (lambda (c)
                                                   (incf count)
                                                   (setf condition c)
                                                   (muffle-warning)))
                          (warning 'muffle-warning))
                       (compile nil `(lambda ()
                                       ,@(funcall make-body name))))))
      (assert (typep count expected-warning-count))
      (assert (typep condition (ecase state
                                 (:early 'early-deprecation-warning)
                                 (:late 'late-deprecation-warning)
                                 (:final 'final-deprecation-warning))))
      (search-string/documentation (princ-to-string condition))
      (when call
        (ecase state
          ((:early :late)
           (assert (eq :deprecated (funcall function))))
          (:final
           (assert-error (funcall function) deprecation-error)))))
    ;; Check DESCRIBE output.
    (when check-describe
      (search-string/describe (with-output-to-string (stream)
                                (describe name stream))))
    ;; Check DOCUMENTATION.
    (search-string/documentation (documentation name namespace))))

;;;; DEPRECATED declaration syntax

(with-test (:name (deprecated :declaration :syntax))
  ;; Some syntax errors.
  (mapc (lambda (declaration)
          (assert-error (proclaim declaration)))
        '((deprecated)
          (deprecated :early)
          (deprecated :early 1)
          (deprecated :early ("1"))
          (deprecated :early ("a" "b" "c"))
          (deprecated :early 1 (function))
          (deprecated :early 1 (unsupported-namespace name))))

  ;; These should work.
  (mapc (lambda (declaration)
          (assert-no-signal (proclaim declaration)))
        '((deprecated :early "1")
          (deprecated :early ("my-software" "1"))
          (deprecated :early "1" (variable deprecated.declaration.variable))
          (deprecated :early "1" (function deprecated.declaration.function))
          (deprecated :early "1" (function (setf deprecated.declaration.function)))
          (deprecated :early "1" (type     deprecated.declaration.type))
          (deprecated :early "1" (variable deprecated.declaration.thing1)
                                 (variable deprecated.declaration.thing2))
          (deprecated :early "1" (variable deprecated.declaration.replacement
                                  :replacement deprecated.declaration.replacement))
          (deprecated :early "1" (variable deprecated.declaration.replacement
                                  :replacement (deprecated.declaration.replacement1
                                                deprecated.declaration.replacement2))))))

;;;; Deprecated variables

(macrolet
    ((definition.undefined (variable-name)
       (declare (ignore variable-name))
       ())
     (definition.declaimed-special (variable-name)
       `(declaim (special ,variable-name)))
     (definition.defvar (variable-name)
       `(defvar ,variable-name :deprecated))
     (definition.defglobal (variable-name)
       `(defglobal ,variable-name :deprecated))
     (definition.defconstant (variable-name)
       `(defconstant ,variable-name :deprecated))
     (definition.define-symbol-macro (variable-name)
       `(define-symbol-macro ,variable-name :deprecated))
     (define-variable-tests (tag definition-name &rest args)
       (flet ((make-test-case (tag state
                                   &key
                                   (call t)
                                   (symbol-value t)
                                   (check-describe t))
                (let ((variable-name (sb-int::symbolicate
                                      '#:variable. tag '#:. state))
                      (replacement   'replacement))
                  `(,@(unless (eq state :final)
                        `((,definition-name ,variable-name)))
                    (declaim (deprecated
                              ,state ("some-lib" "1.2.3")
                              (variable ,variable-name
                                        :replacement ,replacement)))

                    (with-test (:name (deprecated variable ,tag ,state))
                      (check-deprecated-thing
                       'variable ',variable-name ,state
                       (lambda (name) `(,name))
                       :replacements   '(,(string replacement))
                       :call           ,call
                       :check-describe ,check-describe)
                      ,@(when symbol-value
                          `((check-deprecated-thing
                             'variable ',variable-name ,state
                             (lambda (name) `((symbol-value ',name)))
                             :replacements   '(,(string replacement))
                             :call           ,call
                             :check-describe ,check-describe)
                            (check-deprecated-thing
                             'variable ',variable-name ,state
                             (lambda (name) `((symbol-global-value ',name)))
                             :replacements   '(,(string replacement))
                             :call           ,call
                             :check-describe ,check-describe))))))))
         `(progn
            ,@(apply #'make-test-case tag :early args)
            ,@(apply #'make-test-case tag :late args)
            ,@(apply #'make-test-case tag :final :check-describe t args)))))

  (define-variable-tests :undefined          definition.undefined
    :call nil :check-describe nil)
  (define-variable-tests :declaimed-special  definition.declaimed-special
    :call nil)
  (define-variable-tests defvar              definition.defvar)
  (define-variable-tests defglobal           definition.defglobal)
  (define-variable-tests defconstant         definition.defconstant)
  (define-variable-tests define-symbol-macro definition.define-symbol-macro
    :symbol-value nil))
  
;;;; Deprecated functions

(macrolet
    ((definition.undefined (function-name)
       (declare (ignore function-name))
       ())
     (definition.declaimed-ftype (function-name)
       `(declaim (ftype (function () (values keyword &optional))
                        ,function-name)))
     (definition.defun (function-name)
       `(defun ,function-name () :deprecated))
     (definition.defmacro (function-name)
       `(defmacro ,function-name () :deprecated))
     (define-function-tests (tag definition-name &rest args)
       (flet ((make-test-case (tag state
                                   &key
                                   (call t)
                                   (check-describe t))
                (let ((function-name (sb-int::symbolicate
                                      '#:function. tag '#:. state))
                      (replacement   'replacement))
                  `(,@(unless (eq state :final)
                        `((,definition-name ,function-name)))
                    (declaim (deprecated
                              ,state ("some-lib" "1.2.3")
                              (function ,function-name
                                        :replacement ,replacement)))

                    (with-test (:name (deprecated function ,tag ,state))
                      (check-deprecated-thing
                       'function ',function-name ,state
                       (lambda (name) `((,name)))
                       :replacements   '(,(string replacement))
                       :call           ,call
                       :check-describe ,check-describe))))))
         `(progn
            ,@(apply #'make-test-case tag :early args)
            ,@(apply #'make-test-case tag :late args)
            ,@(apply #'make-test-case tag :final :check-describe t args)))))

  (define-function-tests :undefined       definition.undefined
    :call nil :check-describe nil)
  (define-function-tests :declaimed-ftype definition.declaimed-ftype
    :call nil :check-describe nil)
  (define-function-tests defun            definition.defun)
  (define-function-tests defmacro         definition.defmacro))

;;;; Deprecated types

(macrolet
    ((definition.undefined (type-name)
       (declare (ignore type-name))
       ())
     (definition.deftype.empty-body (type-name)
       `(deftype ,type-name ()))
     (definition.deftype.t-body (type-name)
       `(deftype ,type-name () t))
     (definition.defclass (type-name)
       `(defclass ,type-name () ()))
     (definition.defstruct (type-name)
       `(defstruct ,type-name))
     (definition.define-condition (type-name)
       `(define-condition ,type-name () ()))
     (define-type-tests (tag definition-name &rest args)
       (flet ((make-test-case (tag state &key check-describe)
                (let ((type-name (apply #'sb-int::symbolicate
                                        (append '(#:type.)
                                                (sb-int:ensure-list tag)
                                                (list '#:. state (gensym)))))
                      (replacement   'replacement))
                  `(,@(unless (eq state :final)
                        `((,definition-name ,type-name)))
                    (declaim (deprecated
                              ,state ("some-lib" "1.2.3")
                              (type ,type-name
                                    :replacement ,replacement)))

                    (test-util:with-test (:name (deprecated type
                                                 ,@(sb-int:ensure-list tag)
                                                 ,state))
                      (check-deprecated-thing
                       'type ',type-name ,state
                       (lambda (name)
                         `((let ((x))
                             (declare (type (or null ,name) x)))
                           (typep nil ',name)
                           (defmethod ,(gensym) ((x ,name)))
                           (defclass ,(gensym) (,name) ())))
                       :replacements           '(,(string replacement))
                       :call                   nil
                       :expected-warning-count '(integer 4 6)
                       :check-describe         ,check-describe))))))
         `(progn
            ,@(apply #'make-test-case tag :early args)
            ,@(apply #'make-test-case tag :late  args)
            ,@(apply #'make-test-case tag :final :check-describe t args)))))

  (define-type-tests :undefined            definition.undefined
    :check-describe nil)
  (define-type-tests (deftype :empty-body) definition.deftype.empty-body)
  (define-type-tests (deftype :t-body)     definition.deftype.t-body)
  (define-type-tests defclass              definition.defclass)
  (define-type-tests defstruct             definition.defstruct)
  (define-type-tests define-condition      definition.define-condition))

;;;; Loader deprecation warnings

(defun please-dont-use-this (x)
  (identity x))
(declaim (deprecated :early "1.2.10"
                     (function please-dont-use-this
                               :replacement moar-better-function)))

(defun really-dont-do-it (x)
  (identity x))
(declaim (deprecated :late "1.2.10"
                     (function really-dont-do-it
                               :replacement use-other-thing-instead)))

(defun you-cant-use-this (x)
  (identity x))
(declaim (deprecated :final "1.2.10"
                     (function you-cant-use-this
                               :replacement replacement-fn)))

(with-test (:name :introspect-deprecation-stage)
  (assert (eq (sb-int:deprecated-thing-p 'function 'please-dont-use-this)
              :early))
  (assert (eq (sb-int:deprecated-thing-p 'function 'really-dont-do-it)
              :late))
  (assert (eq (sb-int:deprecated-thing-p 'function 'you-cant-use-this)
              :final)))

(with-test (:name (:late-deprecated-fun-doc :bug-1439151)
                  :skipped-on '(not :sb-doc))
  (assert (string= (documentation 'you-cant-use-this 'function)
                   (documentation #'you-cant-use-this 'function)))
  (assert (string= (documentation 'function.defun.late 'function)
                   (documentation #'function.defun.late 'function)))
  (assert (string/= (documentation 'you-cant-use-this 'function)
                    (documentation 'function.defun.late 'function))))

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

;;;; basic environmental stuff

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.

;;;; This software is derived from software originally released by Xerox
;;;; Corporation. Copyright and release statements follow. Later modifications
;;;; to the software are in the public domain and are provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for more
;;;; information.

;;;; copyright information from original PCL sources:
;;;;
;;;; Copyright (c) 1985, 1986, 1987, 1988, 1989, 1990 Xerox Corporation.
;;;; All rights reserved.
;;;;
;;;; Use and copying of this software and preparation of derivative works based
;;;; upon this software are permitted. Any distribution of this software or
;;;; derivative works must comply with all applicable United States export
;;;; control laws.
;;;;
;;;; This software is made available AS IS, and Xerox Corporation makes no
;;;; warranty about the software, its performance or its conformity to any
;;;; specification.

(in-package "SB-PCL")

;;;; MAKE-LOAD-FORM

;; Overwrite the old bootstrap non-generic MAKE-LOAD-FORM function with a
;; shiny new generic function.
(fmakunbound 'make-load-form)
(defgeneric make-load-form (object &optional environment))

(defun !install-cross-compiled-methods (gf-name &key except)
  (assert (generic-function-p (fdefinition gf-name)))
  (dolist (method (cdr (assoc gf-name *!deferred-methods* :test #'equal)))
    (destructuring-bind (qualifiers specializers fmf lambda-list source-loc)
        method
      (unless (member (first specializers) except)
        (let ((arg-info
                (if (equal gf-name '(setf documentation))
                    '(:arg-info (3))
                    (case gf-name
                      (print-object
                       '(:arg-info (2)))
                      ((make-load-form close)
                       '(:arg-info (1 . t)))
                      ((documentation)
                       '(:arg-info (2)))
                      (t
                       '(:arg-info (1)))))))
          (load-defmethod
           'standard-method gf-name
           qualifiers (mapcar (lambda (x)
                                (if (typep x '(cons (eql eql) (cons t null)))
                                    (intern-eql-specializer (constant-form-value (second x)))
                                    (find-class x)))
                              specializers)
           lambda-list
           `(:function
             ,(let ((mf (%make-method-function fmf)))
                (setf (%funcallable-instance-fun mf)
                      (method-function-from-fast-function fmf arg-info))
                mf)
             plist ,arg-info simple-next-method-call t)
           source-loc))))))
(!install-cross-compiled-methods 'make-load-form
                                 :except '(layout sb-alien-internals:alien-type))

(defmethod make-load-form ((class class) &optional env)
  ;; FIXME: should we not instead pass ENV to FIND-CLASS?  Probably
  ;; doesn't matter while all our environments are the same...
  (declare (ignore env))
  (let ((name (class-name class)))
    (if (and name (eq (find-class name nil) class))
        `(find-class ',name)
        (error "~@<Can't use anonymous or undefined class as constant: ~S~:@>"
               class))))

(defmethod make-load-form ((object layout) &optional env)
  (declare (ignore env))
  (let ((pname (classoid-proper-name (layout-classoid object))))
    (unless pname
      (error "can't dump wrapper for anonymous class:~%  ~S"
             (layout-classoid object)))
    `(classoid-layout (find-classoid ',pname))))

(defmethod make-load-form ((object sb-alien-internals:alien-type) &optional env)
  (or (sb-alien::make-type-load-form object)
      (make-load-form-saving-slots object :environment env)))

;; FIXME: this seems wrong. NO-APPLICABLE-METHOD should be signaled.
(defun dont-know-how-to-dump (object)
  (error "~@<don't know how to dump ~S (default ~S method called).~>"
         object 'make-load-form))

(macrolet ((define-default-make-load-form-method (class)
             `(defmethod make-load-form ((object ,class) &optional env)
                (declare (ignore env))
                (dont-know-how-to-dump object))))
  (define-default-make-load-form-method structure-object)
  (define-default-make-load-form-method standard-object)
  (define-default-make-load-form-method condition))

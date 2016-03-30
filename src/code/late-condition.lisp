;;;; Condition support in target lisp

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-KERNEL")

(fmakunbound 'install-condition-slot-reader)
(fmakunbound 'install-condition-slot-writer)

(defmacro standard-method-function (lambda &environment env)
  (let ((proto-gf (load-time-value
                   (ensure-generic-function (gensym)))))
    (multiple-value-bind (lambda initargs)
        (sb-mop:make-method-lambda
         proto-gf
         (sb-mop:class-prototype (sb-mop:generic-function-method-class proto-gf))
         lambda
         env) ; FIXME: coerce to lexenv?
      `(values #',lambda ',initargs))))

(defun install-condition-slot-reader (name condition slot-name)
  (let ((gf (if (fboundp name)
                (ensure-generic-function name)
                (ensure-generic-function name :lambda-list '(condition)))))
    (if (and (eq (class-of gf) (find-class 'standard-generic-function))
             (eq (sb-mop:generic-function-method-class gf)
                 (find-class 'standard-method)))
        (multiple-value-bind (method-fun initargs)
              (standard-method-function
               (lambda (condition)
                 (condition-reader-function condition slot-name)))
          (let ((method (apply #'make-instance
                               'standard-method
                               :specializers (list (find-class condition))
                               :lambda-list '(condition)
                               :function method-fun
                               initargs)))
            (add-method gf method)
            method))
        (eval
         `(defmethod ,name ((condition ,condition))
            (condition-reader-function condition ',slot-name))))))

(defun install-condition-slot-writer (name condition slot-name)
  (let ((gf (if (fboundp name)
                (ensure-generic-function name)
                (ensure-generic-function name :lambda-list '(new-value condition)))))
    (if (and (eq (class-of gf) (find-class 'standard-generic-function))
             (eq (sb-mop:generic-function-method-class gf)
                 (find-class 'standard-method)))
        (multiple-value-bind (method-fun initargs)
              (standard-method-function
               (lambda (new-value condition)
                 (condition-writer-function condition new-value slot-name)))
          (let ((method (apply #'make-instance
                               'standard-method
                               :specializers (list (find-class t)
                                                   (find-class condition))
                               :lambda-list '(new-value condition)
                               :function method-fun
                               initargs)))
            (add-method gf method)
            method))
        (eval
         `(defmethod ,name (new-value (condition ,condition))
            (condition-writer-function condition new-value ',slot-name))))))

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

(flet ((install-condition-slot-accessor
           (name slot-name lambda-list specializers
            access-function-name method-function method-initargs)
         (let ((gf (apply #'ensure-generic-function name
                          (unless (fboundp name) (list :lambda-list lambda-list)))))
           (if (and (eq (class-of gf) (find-class 'standard-generic-function))
                    (eq (sb-mop:generic-function-method-class gf)
                        (find-class 'standard-method)))
               (let ((method (apply #'make-instance
                                    'standard-method
                                    :specializers (mapcar #'find-class specializers)
                                    :lambda-list lambda-list
                                    :function method-function
                                    method-initargs)))

                 (add-method gf method)
                 method)
               (eval
                (let ((specialized-lambda-list
                       (mapcar (lambda (parameter specializer)
                                 (if (eq specializer t)
                                     parameter
                                     `(,parameter ,specializer)))
                               lambda-list specializers)))
                  `(defmethod ,name ,specialized-lambda-list
                     (,access-function-name ,@lambda-list ',slot-name))))))))

  (macrolet
      ((standard-method-function (lambda &environment env)
         (binding* ((proto-gf (load-time-value (ensure-generic-function (gensym))))
                    (proto-method (sb-mop:class-prototype
                                   (sb-mop:generic-function-method-class proto-gf)))
                    ((lambda initargs) (sb-mop:make-method-lambda
                                        proto-gf proto-method lambda env))) ; FIXME: coerce to lexenv?
           `(values #',lambda ',initargs))))

    (defun install-condition-slot-reader (name condition slot-name)
      (multiple-value-call #'install-condition-slot-accessor
        name slot-name '(condition) (list condition)
        'condition-slot-value
        (standard-method-function
         (lambda (condition)
           (condition-slot-value condition slot-name)))))

    (defun install-condition-slot-writer (name condition slot-name)
      (multiple-value-call #'install-condition-slot-accessor
        name slot-name '(new-value condition) (list t condition)
        'set-condition-slot-value
        (standard-method-function
         (lambda (new-value condition)
           (set-condition-slot-value condition new-value slot-name)))))))

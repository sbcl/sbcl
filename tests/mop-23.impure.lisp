;;;; miscellaneous side-effectful tests of the MOP

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

;;; Extending MAKE-METHOD-LAMBDA, and making sure that the resulting
;;; method functions compile without warnings.

(defclass verbose-generic-function (standard-generic-function) ()
  (:metaclass sb-mop:funcallable-standard-class))
(defmethod sb-mop:make-method-lambda
    ((gf verbose-generic-function) method lambda env)
  (multiple-value-bind (lambda initargs)
      (call-next-method)
    (values
     `(lambda (args next-methods)
       (format *trace-output* "Called a method!")
       (,lambda args next-methods))
     initargs)))

(defgeneric foo (x)
  (:generic-function-class verbose-generic-function))

(handler-bind ((warning #'error))
  (eval '(defmethod foo ((x integer)) (1+ x))))

(with-test (:name (:mop-23 sb-mop:make-method-lambda  1))
  (assert (string= (with-output-to-string (*trace-output*)
                     (assert (= (foo 3) 4)))
                   "Called a method!")))

(defclass super () ((a :initarg :a)))
(defclass sub (super) (b))

(handler-bind ((warning #'error))
  (eval '(defmethod foo ((x sub)) (slot-boundp x 'b)))
  (eval '(defmethod foo :around ((x super))
          (list (slot-value x 'a) (call-next-method)))))

(with-test (:name (:mop-23 sb-mop:make-method-lambda 3))
  (assert (string= (with-output-to-string (*trace-output*)
                     (assert (equal (foo (make-instance 'sub :a 4))
                                    '(4 nil))))
                   "Called a method!Called a method!")))

(defclass super ()
  ((b :initform 3)
   (a :initarg :a)))

(with-test (:name (:mop-23 sb-mop:make-method-lambda 3))
  (assert (string= (with-output-to-string (*trace-output*)
                     (assert (equal (foo (make-instance 'sub :a 5))
                                    '(5 t))))
                   "Called a method!Called a method!")))

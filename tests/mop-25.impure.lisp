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

;;; be sure that the :FUNCTION initarg to initialize methods overrides
;;; any system-provided function.

(defclass typechecking-reader-method (standard-reader-method)
  ())

(defmethod initialize-instance
    ((method typechecking-reader-method) &rest initargs &key slot-definition)
  (let ((name (sb-mop:slot-definition-name slot-definition))
        (type (sb-mop:slot-definition-type slot-definition)))
    (apply #'call-next-method method
           :function #'(lambda (args next-methods)
                         (declare (ignore next-methods))
                         (apply #'(lambda (instance)
                                    (let ((value (slot-value instance name)))
                                      (unless (typep value type)
                                        (error "Slot ~S of ~S is not of type ~S: ~S"
                                               name instance type value))
                                      value))
                                args))
           initargs)))
(defclass typechecking-reader-class (standard-class)
  ())

(defmethod sb-mop:validate-superclass ((c1 typechecking-reader-class) (c2 standard-class))
  t)

(defmethod reader-method-class
    ((class typechecking-reader-class) direct-slot &rest args)
  (declare (ignore args))
  (find-class 'typechecking-reader-method))

(defclass testclass25 ()
  ((pair :type (cons symbol (cons symbol null)) :initarg :pair :accessor testclass25-pair))
  (:metaclass typechecking-reader-class))

(with-test (:name (:mop-24))
  (let ((p (list 'abc 'def))
        (x (make-instance 'testclass25)))
    (assert-no-signal (make-instance 'testclass25 :pair '(seventeen 17)))
    (assert-no-signal (setf (testclass25-pair x) p))
    (assert-no-signal (setf (second p) 456))
    (assert-no-signal (testclass25-pair x))
    (assert-no-signal (slot-value x 'pair))))

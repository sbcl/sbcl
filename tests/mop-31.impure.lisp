;;;; CHANGE-CLASS, obsolete instances, and methods on SLOT-VALUE-USING-CLASS

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

(defclass metaclass (sb-mop:standard-class) ())

(defmethod sb-mop:validate-superclass ((class metaclass) (superclass t))
  NIL)

(defmethod sb-mop:validate-superclass ((class standard-class) (superclass metaclass))
  T)

(defmethod sb-mop:validate-superclass ((class metaclass) (superclass standard-class))
  T)

(defmethod sb-mop:validate-superclass ((class metaclass) (superclass metaclass))
  T)

(defmethod sb-mop:direct-slot-definition-class ((class metaclass) &rest initargs)
  (find-class 'sb-mop:standard-direct-slot-definition))

(defmethod sb-mop:effective-slot-definition-class ((class metaclass) &rest initargs)
  (find-class 'sb-mop:standard-effective-slot-definition))

(defmethod sb-mop:slot-value-using-class ((class metaclass) object slot)
  (if (eq 'test (sb-mop:slot-definition-name slot))
      (* 2 (call-next-method))
      (call-next-method)))

(defmethod (setf sb-mop:slot-value-using-class) (value (class metaclass) object slot)
  (if (eq 'test (sb-mop:slot-definition-name slot))
      (call-next-method (/ value 2) class object slot)
      (call-next-method)))

(defclass stdclass ()
  ((test :initform 2))
  (:metaclass metaclass))

(let ((a (make-instance 'stdclass)))
  (assert (= 2 (slot-value a 'test)))
  (change-class a 'stdclass)
  (assert (= 2 (slot-value a 'test))))

;;; check obsolete instance handling
(defvar *a* (make-instance 'stdclass))

(defclass stdclass ()
  ((test :initform 2)
   (newtest :initform 4))
  (:metaclass metaclass))

(assert (= 2 (slot-value *a* 'test)))
(assert (= 4 (slot-value *a* 'newtest)))

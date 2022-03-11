;;;; Handling errors in UPDATE-INSTANCE-FOR-REDEFINED-CLASS

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

(defclass foo-class (standard-class) ())

(defmethod sb-mop:validate-superclass ((c foo-class) (s standard-class)) t)

(defclass foo-object (standard-object) ())

(defmethod shared-initialize :around ((class foo-class) slot-names
                                      &rest rest
                                      &key direct-superclasses)
  (apply #'call-next-method
         class slot-names
         :direct-superclasses
         (append (remove (find-class 'standard-object) direct-superclasses)
                 (list (find-class 'foo-object)))
         rest))

(defmethod update-instance-for-redefined-class :before
    ((instance foo-object)
     added-slots discarded-slots
     property-list
     &rest initargs)
  (declare (ignore initargs))
  ;; This U-I-F-R-C is meant to always signal an error.
  (error "expected failure"))

;;; Define FOO.
(defclass foo () () (:metaclass foo-class))

;;; Make an instance of FOO.
(defparameter *foo* (make-instance 'foo))

;;; Redefine FOO, causing *FOO* to become obsolete.
(defclass foo ()
  ((slot :initform 42))
  (:metaclass foo-class))

;;; This should result in an "expected failure" error, because
;;; the instance is obsolete.
(multiple-value-bind (result error)
    (ignore-errors (slot-value *foo* 'slot))
  (assert (null result))
  (assert (string= (princ-to-string error) "expected failure")))

;;; This should *also* result in an "expected failure" error, because after
;;; the previous U-I-F-R-C call made a non-local exit, the instance should be
;;; automatically made obsolete once more.
(multiple-value-bind (result error)
    (ignore-errors (slot-value *foo* 'slot))
  (assert (null result))
  (assert (string= (princ-to-string error) "expected failure")))

;;; Redefine the U-I-F-R-C method to no longer signal an error.
(defmethod update-instance-for-redefined-class :before
    ((instance foo-object)
     added-slots discarded-slots
     property-list
     &rest initargs)
  (declare (ignore initargs)))

;;; Instance is now updateable. It should now be possible to access the new slot
;;; and fetch its initform-initialized value.
(assert (= 42 (slot-value *foo* 'slot)))

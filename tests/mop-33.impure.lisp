;;;; Handling errors in UPDATE-INSTANCE-FOR-DIFFERENT-CLASS

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

(defclass foo () ())

(defclass bar () ((slot :initform 42)))

(defmethod update-instance-for-different-class :before
    ((foo foo) (bar bar) &rest initargs)
  (declare (ignore initargs))
  ;; This U-I-F-D-C is meant to always signal an error.
  (error "expected failure"))

;;; Make an instance of FOO.
(defparameter *foo* (make-instance 'foo))

;;; This should result in an "expected failure" error.
(multiple-value-bind (result error)
    (ignore-errors (change-class *foo* 'bar))
  (assert (null result))
  (assert (string= (princ-to-string error) "expected failure")))

;;; This should *also* result in an "expected failure" error, because after
;;; the previous U-I-F-D-C call made a non-local exit, the instance should be
;;; automatically restored to its previous class.
(multiple-value-bind (result error)
    (ignore-errors (change-class *foo* 'bar))
  (assert (null result))
  (assert (string= (princ-to-string error) "expected failure")))

;;; Redefine the U-I-F-D-C method to no longer signal an error.
(defmethod update-instance-for-different-class :before
    ((foo foo) (bar bar) &rest initargs)
  (declare (ignore initargs)))

;;; It is now possible to change the instance's class.
(change-class *foo* 'bar)

;;; It should now be possible to access the new slot and fetch its
;;; initform-initialized value.
(assert (= 42 (slot-value *foo* 'slot)))

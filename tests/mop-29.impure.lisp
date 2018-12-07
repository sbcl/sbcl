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

;;; a test that metaclasses can be instantiated even if there are
;;; applicable methods for SLOT-VALUE-USING-CLASS with specialized
;;; arguments that invoke slot readers.  (Previously the PV
;;; optimization for slot readers caused the new class's wrapper and
;;; effective slot definitions to be available during class
;;; finalization)

(defclass my-class (standard-class)
  ())

(defmethod sb-mop:validate-superclass ((class my-class) (super-class standard-class))
  t)

(defvar *foo*)

;;; the specialization of OBJECT here triggers the PV optimization;
;;; with an unspecialized argument, the SLOT-VALUE is not optimized.
(defmethod sb-mop:slot-value-using-class
    ((class my-class) (object standard-object) eslotd)
  (if *foo*
      (setf (slot-value object 'id) 42)
      (call-next-method)))

(defclass my-object ()
  ((id :type integer :reader id-of))
  (:metaclass my-class))

;;; the first patch failed on code like this, because the STD-P field
;;; of the accessor information was also computed lazily, but it is
;;; needed in order to real with accessor cache misses.
(defun test-global-accessors ()
  (let ((object (make-instance 'my-object)))
    (setf (slot-value object 'id) 13)
    (let ((*foo* nil))
      (assert (= (id-of object) 13))
      (assert (= (slot-value object 'id) 13)))
    (let ((*foo* t))
      (assert (= (id-of object) 42))
      (assert (= (slot-value object 'id) 42)))
    (let ((*foo* nil))
      (assert (= (id-of object) 42))
      (assert (= (slot-value object 'id) 42)))))
(compile 'test-global-accessors)

(with-test (:name (:mop-29))
  (test-global-accessors))

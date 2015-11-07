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

;;; this file tests the accessor method class portion of the protocol
;;; for Initialization of Class Metaobjects.

(defpackage "MOP-19"
  (:use "CL" "SB-MOP"))

(in-package "MOP-19")

(defclass my-class (standard-class) ())
(defmethod validate-superclass ((a my-class) (b standard-class)) t)

(defclass my-reader (standard-reader-method) ())
(defclass my-writer (standard-writer-method) ())

(defvar *calls* nil)

(defmethod reader-method-class ((c my-class) s &rest initargs)
  (declare (ignore initargs))
  (push (cons (slot-definition-name s) 'reader) *calls*)
  (find-class 'my-reader))
(defmethod writer-method-class ((c my-class) s &rest initargs)
  (declare (ignore initargs))
  (push (cons (slot-definition-name s) 'writer) *calls*)
  (find-class 'my-writer))

(defclass foo ()
  ((a :reader a)
   (b :writer b)
   (c :accessor c))
  (:metaclass my-class))

(assert (= (length *calls*) 4))
(assert (= (count 'a *calls* :key #'car) 1))
(assert (= (count 'b *calls* :key #'car) 1))
(assert (= (count 'c *calls* :key #'car) 2))
(assert (= (count 'reader *calls* :key #'cdr) 2))
(assert (= (count 'writer *calls* :key #'cdr) 2))
(let ((method (find-method #'a nil (list (find-class 'foo)))))
  (assert (eq (class-of method) (find-class 'my-reader))))
(let ((method (find-method #'b nil (list (find-class t) (find-class 'foo)))))
  (assert (eq (class-of method) (find-class 'my-writer))))

(defclass my-other-class (my-class) ())
(defmethod validate-superclass ((a my-other-class) (b standard-class)) t)

(defclass my-other-reader (standard-reader-method) ())

(defclass my-direct-slot-definition (standard-direct-slot-definition) ())

(defmethod direct-slot-definition-class ((c my-other-class) &rest args)
  (declare (ignore args))
  (find-class 'my-direct-slot-definition))

(defmethod reader-method-class :around
    (class (s my-direct-slot-definition) &rest initargs)
  (declare (ignore initargs))
  (find-class 'my-other-reader))

(defclass bar ()
  ((d :reader d)
   (e :writer e))
  (:metaclass my-other-class))

(let ((method (find-method #'d nil (list (find-class 'bar)))))
  (assert (eq (class-of method) (find-class 'my-other-reader))))
(let ((method (find-method #'e nil (list (find-class t) (find-class 'bar)))))
  (assert (eq (class-of method) (find-class 'my-writer))))

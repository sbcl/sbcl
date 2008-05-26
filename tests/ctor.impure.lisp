;;;; gray-box testing of the constructor optimization machinery

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

(defpackage "CTOR-TEST"
  (:use "CL"))

(in-package "CTOR-TEST")

(defclass no-slots () ())

(defun make-no-slots ()
  (make-instance 'no-slots))
(compile 'make-no-slots)

(defmethod update-instance-for-redefined-class
    ((object no-slots) added discarded plist &rest initargs)
  (declare (ignore initargs))
  (error "Called U-I-F-R-C on ~A" object))

(assert (typep (make-no-slots) 'no-slots))

(make-instances-obsolete 'no-slots)

(assert (typep (make-no-slots) 'no-slots))
(assert (typep (funcall #'(sb-pcl::ctor no-slots nil)) 'no-slots))

(defclass one-slot ()
  ((a :initarg :a)))

(defun make-one-slot-a (a)
  (make-instance 'one-slot :a a))
(compile 'make-one-slot-a)
(defun make-one-slot-noa ()
  (make-instance 'one-slot))
(compile 'make-one-slot-noa)

(defmethod update-instance-for-redefined-class
    ((object one-slot) added discarded plist &rest initargs)
  (declare (ignore initargs))
  (error "Called U-I-F-R-C on ~A" object))

(assert (= (slot-value (make-one-slot-a 3) 'a) 3))
(assert (not (slot-boundp (make-one-slot-noa) 'a)))

(make-instances-obsolete 'one-slot)

(assert (= (slot-value (make-one-slot-a 3) 'a) 3))
(assert (= (slot-value (funcall #'(sb-pcl::ctor one-slot nil :a sb-pcl::\.p0.) 4) 'a) 4))
(assert (not (slot-boundp (make-one-slot-noa) 'a)))
(assert (not (slot-boundp (funcall #'(sb-pcl::ctor one-slot nil)) 'a)))

(defclass one-slot-superclass ()
  ((b :initarg :b)))
(defclass one-slot-subclass (one-slot-superclass)
  ())

(defun make-one-slot-subclass (b)
  (make-instance 'one-slot-subclass :b b))
(compile 'make-one-slot-subclass)

(defmethod update-instance-for-redefined-class
    ((object one-slot-superclass) added discarded plist &rest initargs)
  (declare (ignore initargs))
  (error "Called U-I-F-R-C on ~A" object))

(assert (= (slot-value (make-one-slot-subclass 2) 'b) 2))

(make-instances-obsolete 'one-slot-subclass)

(assert (= (slot-value (make-one-slot-subclass 2) 'b) 2))
(assert (= (slot-value (funcall #'(sb-pcl::ctor one-slot-subclass nil :b sb-pcl::\.p0.) 3) 'b) 3))
(make-instances-obsolete 'one-slot-superclass)

(assert (= (slot-value (make-one-slot-subclass 2) 'b) 2))
(assert (= (slot-value (funcall #'(sb-pcl::ctor one-slot-subclass nil :b sb-pcl::\.p0.) 4) 'b) 4))

;;;; success

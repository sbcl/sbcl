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

;;; this file contains tests of (SETF CLASS-NAME) and (SETF
;;; GENERIC-FUNCTION-NAME)

(defpackage "MOP-9"
  (:use "CL" "SB-MOP" "TEST-UTIL"))

(in-package "MOP-9")

(defclass metaclass/ri (standard-class)
  ())
(defmethod validate-superclass ((c metaclass/ri) (s standard-class))
  t)
(defclass class/ri ()
  ()
  (:metaclass metaclass/ri))
(defvar *class/ri-args* nil)
(defmethod reinitialize-instance :after ((o metaclass/ri) &rest initargs)
  (setf *class/ri-args* initargs))
(with-test (:name ((setf class-name) reinitialize-instance))
  (let ((class (find-class 'class/ri)))
    (setf (class-name class) 'name)
    (assert (equal *class/ri-args* '(:name name)))
    (setf (class-name class) 'class/ri)
    (assert (equal *class/ri-args* '(:name class/ri)))))

(defclass dependent ()
  ((slot :initform nil :accessor dependent-slot)))
(defclass class/dependent ()
  ())
(defvar *dependent* (make-instance 'dependent))
(defmethod update-dependent ((object standard-class) (dependent dependent)
                             &rest args)
  (setf (dependent-slot dependent) args))
(with-test (:name ((setf class-name) update-dependent))
  (let ((class (find-class 'class/dependent)))
    (add-dependent class *dependent*)
    (setf (class-name class) 'name)
    (assert (equal (dependent-slot *dependent*) '(:name name)))
    (remove-dependent class *dependent*)
    (setf (class-name class) 'name)
    (assert (equal (dependent-slot *dependent*) '(:name name)))))

(defclass gfc/ri (standard-generic-function)
  ()
  (:metaclass funcallable-standard-class))
(defgeneric gf/ri ()
  (:generic-function-class gfc/ri))
(defvar *gf/ri-args* nil)
(defmethod reinitialize-instance :after ((o gfc/ri) &rest initargs)
  (setf *gf/ri-args* initargs))
(with-test (:name ((setf generic-function-name) reinitialize-instance))
  (let ((gf #'gf/ri))
    (setf (generic-function-name gf) 'name)
    (assert (equal *gf/ri-args* '(:name name)))
    (setf (generic-function-name gf) 'gf/ri)
    (assert (equal *gf/ri-args* '(:name gf/ri)))))

(defgeneric gf/dependent ())
(defmethod update-dependent ((object standard-generic-function)
                             (dependent dependent)
                             &rest args)
  (setf (dependent-slot dependent) args))
(with-test (:name ((setf generic-function-name) update-dependent))
  (let ((gf (find-class 'class/dependent)))
    (add-dependent gf *dependent*)
    (setf (generic-function-name gf) 'gf/name)
    (assert (equal (dependent-slot *dependent*) '(:name gf/name)))
    (remove-dependent gf *dependent*)
    (setf (generic-function-name gf) 'gf/dependent)
    (assert (equal (dependent-slot *dependent*) '(:name gf/name)))))

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

;;; this file tests the protocol for Reinitialization of Class Metaobjects

(defpackage "MOP-18"
  (:use "CL" "SB-MOP"))

(in-package "MOP-18")

(defvar *in-reinitialize-instance* nil)

(defvar *finalized-class* nil)

(defclass test-standard-class (standard-class) ())

(defmethod validate-superclass
    ((class test-standard-class) (superclass standard-class))
  t)

(defmethod finalize-inheritance :before ((class test-standard-class))
  (when *in-reinitialize-instance*
    (setf *finalized-class* class)))

(defmethod reinitialize-instance :around
    ((class test-standard-class) &key &allow-other-keys)
  (let ((*in-reinitialize-instance* t))
    (call-next-method)))

(defclass test-standard-object () ((slot))
  (:metaclass test-standard-class))

(unless (class-finalized-p (find-class 'test-standard-object))
  (finalize-inheritance (find-class 'test-standard-object)))

(assert (class-slots (find-class 'test-standard-object)))
(assert (null *finalized-class*))
(reinitialize-instance (find-class 'test-standard-object) :direct-slots nil)
(assert (eq *finalized-class* (find-class 'test-standard-object)))
(assert (null (class-slots (find-class 'test-standard-object))))

(defclass test-funcallable-standard-class (funcallable-standard-class) ())

(defmethod validate-superclass
    ((class test-funcallable-standard-class)
     (superclass funcallable-standard-class))
  t)

(defmethod finalize-inheritance :before
    ((class test-funcallable-standard-class))
  (when *in-reinitialize-instance*
    (setf *finalized-class* class)))

(defmethod reinitialize-instance :around
    ((class test-funcallable-standard-class) &key &allow-other-keys)
  (let ((*in-reinitialize-instance* t))
    (call-next-method)))

(defclass test-funcallable-standard-object () ((slot))
  (:metaclass test-funcallable-standard-class))

(unless (class-finalized-p (find-class 'test-funcallable-standard-object))
  (finalize-inheritance (find-class 'test-funcallable-standard-object)))

(assert (class-slots (find-class 'test-funcallable-standard-object)))
(assert (eq *finalized-class* (find-class 'test-standard-object)))
(reinitialize-instance (find-class 'test-funcallable-standard-object)
                       :direct-slots nil)
(assert (eq *finalized-class* (find-class 'test-funcallable-standard-object)))
(assert (null (class-slots (find-class 'test-funcallable-standard-object))))

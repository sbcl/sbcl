;;;; To test the IGNORE/IGNORABLE behavior in CLOS, run COMPILE-FILE on
;;;; this file and look at the output (warnings, etc.).
;;;;
;;;; (In sbcl-0.6.8.25, the handling of IGNORE and IGNORABLE in
;;;; DEFMETHOD forms was rewritten to systematize the old PCL behavior.
;;;; Now all required variables are IGNORABLE by default.)

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

(in-package :cl-user)

(defgeneric foo (x y &key &allow-other-keys))

;;; should have no STYLE-WARNINGs (e.g. about unused vars)
(defmethod foo ((x t) (y t))
  nil)

;;; should have no STYLE-WARNINGs
(defmethod foo ((x t) (y t) &key &allow-other-keys)
  (declare (ignore x)))

;;; should have no STYLE-WARNINGs
(defmethod foo ((x t) (y t) &key &allow-other-keys)
  (declare (ignorable x y))
  (declare (ignore y)))

;;; should have no STYLE-WARNINGs
(defmethod foo ((x t) (y t) &key &allow-other-keys)
  x)

;;; should have a STYLE-WARNING: using an IGNOREd variable
(defmethod foo ((x t) (y t) &key &allow-other-keys)
  (declare (ignore x y))
  x)

;;; should have no STYLE-WARNINGs
(defmethod foo (x y &key &allow-other-keys)
  (declare (ignore x y))
  (call-next-method))

;;; should have no STYLE-WARNINGs
(defmethod foo ((x integer) (y t) &key &allow-other-keys)
  (declare (ignore x y))
  (call-next-method))

;;; should have no STYLE-WARNINGs
(defmethod foo ((x integer) (y t) &key &allow-other-keys)
  (declare (ignore x))
  (call-next-method))

;;; should have a STYLE-WARNING: Z is unused.
(defmethod foo ((x t) (y integer) &key z)
  nil)

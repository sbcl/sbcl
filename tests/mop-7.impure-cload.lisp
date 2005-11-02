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

;;; This file contains the simplest test that the multiple subclasses
;;; of generic function metacircle is gone.

(defpackage "MOP-7"
  (:use "CL" "SB-MOP" "TEST-UTIL"))

(in-package "MOP-7")

(defclass g1 (standard-generic-function)
  ()
  (:metaclass funcallable-standard-class))
(defclass g2 (standard-generic-function)
  ()
  (:metaclass funcallable-standard-class))

(defgeneric f1 ()
  (:generic-function-class g1))
(defgeneric f2 ()
  (:generic-function-class g2))

(print #'f1)
(print #'f2)

;;;; floating-point-related tests with no side effects

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

(cl:in-package :cl-user)

(let ((+ifni single-float-positive-infinity)
      (-ifni single-float-negative-infinity))
  (assert (= (* +ifni 1) +ifni))
  (assert (= (* +ifni -0.1) -ifni))
  (assert (= (+ +ifni -0.1) +ifni))
  (assert (= (- +ifni -0.1) +ifni))
  (assert (= (sqrt +ifni) +ifni))
  (assert (= (* -ifni -14) +ifni))
  (assert (= (/ -ifni 0.1) -ifni))
  (assert (= (/ -ifni 100/3) -ifni))
  (assert (< -ifni +ifni))
  ;; FIXME: Reenable this when bug 92 is fixed.
  ;; (assert (not (< +ifni 100)))
  (assert (not (< +ifni 100.0)))
  (assert (not (< +ifni -ifni))))

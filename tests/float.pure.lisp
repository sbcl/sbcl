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

(dolist (ifnis (list (cons single-float-positive-infinity
			   single-float-negative-infinity)
		     (cons double-float-positive-infinity
			   double-float-negative-infinity)))
  (destructuring-bind (+ifni . -ifni) ifnis
    (assert (= (* +ifni 1) +ifni))
    (assert (= (* +ifni -0.1) -ifni))
    (assert (= (+ +ifni -0.1) +ifni))
    (assert (= (- +ifni -0.1) +ifni))
    (assert (= (sqrt +ifni) +ifni))
    (assert (= (* -ifni -14) +ifni))
    (assert (= (/ -ifni 0.1) -ifni))
    (assert (= (/ -ifni 100/3) -ifni))
    (assert (not (= +ifni -ifni)))
    (assert (= -ifni -ifni))
    (assert (not (= +ifni 100/3)))
    (assert (not (= -ifni -1.0 -ifni)))
    (assert (not (= -ifni -17/02 -ifni)))
    (assert (< -ifni +ifni))
    (assert (not (< +ifni 100)))
    (assert (not (< +ifni 100.0)))
    (assert (not (< +ifni -ifni)))
    (assert (< 100 +ifni))
    (assert (< 100.0 +ifni))
    (assert (>= 100 -ifni))
    (assert (not (<= 6/7 (* 3 -ifni))))
    (assert (not (> +ifni +ifni)))))

;;; ANSI: FLOAT-RADIX should signal an error if its argument is not a
;;; float.
;;;
;;; (Peter Van Eynde's ansi-test suite caught this, and Eric Marsden
;;; reported a fix for CMU CL, which was ported to sbcl-0.6.12.35.)
(assert (typep (nth-value 1 (ignore-errors (float-radix "notfloat")))
	       'type-error))

(assert (typep (nth-value 1 (ignore-errors
                              (funcall (fdefinition 'float-radix) "notfloat")))
               'type-error))

;;; Before 0.8.2.14 the cross compiler failed to work with
;;; denormalized numbers
(when (subtypep 'single-float 'short-float)
  (assert (eql least-positive-single-float least-positive-short-float)))

#+nil ; bug 269
(let ((f (eval 'least-positive-double-float)))
  (assert (eql (multiple-value-bind (signif expon sign)
                   (integer-decode-float f)
                 (scale-float (float signif f) expon))
               f)))

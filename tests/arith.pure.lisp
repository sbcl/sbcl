;;;; arithmetic tests with no side effects

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

;;; Once upon a time, in the process of porting CMUCL's SPARC backend
;;; to SBCL, multiplications were excitingly broken.  While it's
;;; unlikely that anything with such fundamental arithmetic errors as
;;; these are going to get this far, it's probably worth checking.
(macrolet ((test (op res1 res2)
	     `(progn
	       (assert (= (,op 4 2) ,res1))
	       (assert (= (,op 2 4) ,res2))
	       (assert (= (funcall (compile nil (lambda (x y) (,op x y))) 4 2) 
			,res1))
	       (assert (= (funcall (compile nil (lambda (x y) (,op x y))) 2 4) 
			,res2)))))
  (test + 6 6)
  (test - 2 -2)
  (test * 8 8)
  (test / 2 1/2)
  (test expt 16 16))

;;; In a bug reported by Wolfhard Buss on cmucl-imp 2002-06-18 (BUG
;;; 184), sbcl didn't catch all divisions by zero, notably divisions
;;; of bignums and ratios by 0.  Fixed in sbcl-0.7.6.13.
(macrolet ((test (form) `(multiple-value-bind (val cond)
			     (ignore-errors ,form)
			   (assert (null val))
			   (assert (typep cond 'division-by-zero)))))
  (test (/ 2/3 0))
  (test (/ (1+ most-positive-fixnum) 0)))

;;; In a bug reported by Raymond Toy on cmucl-imp 2002-07-18, (COERCE
;;; <RATIONAL> '(COMPLEX FLOAT)) was failing to return a complex
;;; float; a patch was given by Wolfhard Buss cmucl-imp 2002-07-19.
(assert (= (coerce 1 '(complex float)) #c(1.0 0.0)))
(assert (= (coerce 1/2 '(complex float)) #c(0.5 0.0)))
(assert (= (coerce 1.0d0 '(complex float)) #c(1.0d0 0.0d0)))

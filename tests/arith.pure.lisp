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
(assert (raises-error? (/ 2/3 0) division-by-zero))
(assert (raises-error? (/ (1+ most-positive-fixnum) 0) division-by-zero))

;;; In a bug reported by Raymond Toy on cmucl-imp 2002-07-18, (COERCE
;;; <RATIONAL> '(COMPLEX FLOAT)) was failing to return a complex
;;; float; a patch was given by Wolfhard Buss cmucl-imp 2002-07-19.
(assert (= (coerce 1 '(complex float)) #c(1.0 0.0)))
(assert (= (coerce 1/2 '(complex float)) #c(0.5 0.0)))
(assert (= (coerce 1.0d0 '(complex float)) #c(1.0d0 0.0d0)))

;;; COERCE also sometimes failed to verify that a particular coercion
;;; was possible (in particular coercing rationals to bounded float
;;; types.
(assert (raises-error? (coerce 1 '(float 2.0 3.0)) type-error))
(assert (raises-error? (coerce 1 '(single-float -1.0 0.0)) type-error))
(assert (eql (coerce 1 '(single-float -1.0 2.0)) 1.0))

;;; ANSI says MIN and MAX should signal TYPE-ERROR if any argument
;;; isn't REAL. SBCL 0.7.7 didn't in the 1-arg case. (reported as a
;;; bug in CMU CL on #lisp IRC by lrasinen 2002-09-01)
(assert (null (ignore-errors (min '(1 2 3)))))
(assert (= (min -1) -1))
(assert (null (ignore-errors (min 1 #(1 2 3)))))
(assert (= (min 10 11) 10))
(assert (null (ignore-errors (min (find-package "CL") -5.0))))
(assert (= (min 5.0 -3) -3))
(assert (null (ignore-errors (max #c(4 3)))))
(assert (= (max 0) 0))
(assert (null (ignore-errors (max "MIX" 3))))
(assert (= (max -1 10.0) 10.0))
(assert (null (ignore-errors (max 3 #'max))))
(assert (= (max -3 0) 0))

;;; (CEILING x 2^k) was optimized incorrectly
(loop for divisor in '(-4 4)
   for ceiler = (compile nil `(lambda (x)
                                (declare (fixnum x))
                                (declare (optimize (speed 3)))
                                (ceiling x ,divisor)))
   do (loop for i from -5 to 5
         for exact-q = (/ i divisor)
         do (multiple-value-bind (q r)
                (funcall ceiler i)
              (assert (= (+ (* q divisor) r) i))
              (assert (<= exact-q q))
              (assert (< q (1+ exact-q))))))

;;; CEILING had a corner case, spotted by Paul Dietz
(assert (= (ceiling most-negative-fixnum (1+ most-positive-fixnum)) -1))

;;; give any optimizers of constant multiplication a light testing.
;;; 100 may seem low, but (a) it caught CSR's initial errors, and (b)
;;; before checking in, CSR tested with 10000.  So one hundred
;;; checkins later, we'll have doubled the coverage.
(dotimes (i 100)
  (let* ((x (random most-positive-fixnum))
	 (x2 (* x 2))
	 (x3 (* x 3)))
    (let ((fn (handler-bind ((sb-ext:compiler-note #'error))
		(compile nil
			 `(lambda (y)
			    (declare (optimize speed) (type (integer 0 3) y))
			    (* y ,x))))))
      (unless (and (= (funcall fn 0) 0)
		   (= (funcall fn 1) x)
		   (= (funcall fn 2) x2)
		   (= (funcall fn 3) x3))
	(error "bad results for ~D" x)))))

;;; Bugs reported by Paul Dietz:

;;; (GCD 0 x) must return (abs x)
(dolist (x (list -10 (* 3 most-negative-fixnum)))
  (assert (= (gcd 0 x) (abs x))))
;;; LCM returns a non-negative number
(assert (= (lcm 4 -10) 20))

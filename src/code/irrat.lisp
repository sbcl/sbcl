;;;; This file contains all the irrational functions. (Actually, most
;;;; of the work is done by calling out to C.)

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!KERNEL")

(file-comment
  "$Header$")

;;;; miscellaneous constants, utility functions, and macros

(defconstant pi 3.14159265358979323846264338327950288419716939937511L0)
;(defconstant e 2.71828182845904523536028747135266249775724709369996L0)

;;; Make these INLINE, since the call to C is at least as compact as a Lisp
;;; call, and saves number consing to boot.
;;;
;;; FIXME: This should be (EVAL-WHEN (COMPILE-EVAL) (SB!XC:DEFMACRO ..)),
;;; I think.
(defmacro def-math-rtn (name num-args)
  (let ((function (intern (concatenate 'simple-string
				       "%"
				       (string-upcase name)))))
    `(progn
       (proclaim '(inline ,function))
       (let ((sb!int::*rogue-export* "DEF-MATH-RTN"))
         (export ',function))
       (sb!alien:def-alien-routine (,name ,function) double-float
	 ,@(let ((results nil))
	     (dotimes (i num-args (nreverse results))
	       (push (list (intern (format nil "ARG-~D" i))
			   'double-float)
		     results)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)

(defun handle-reals (function var)
  `((((foreach fixnum single-float bignum ratio))
     (coerce (,function (coerce ,var 'double-float)) 'single-float))
    ((double-float)
     (,function ,var))))

) ; EVAL-WHEN

;;;; stubs for the Unix math library

;;; Please refer to the Unix man pages for details about these routines.

;;; Trigonometric.
#!-x86 (def-math-rtn "sin" 1)
#!-x86 (def-math-rtn "cos" 1)
#!-x86 (def-math-rtn "tan" 1)
(def-math-rtn "asin" 1)
(def-math-rtn "acos" 1)
#!-x86 (def-math-rtn "atan" 1)
#!-x86 (def-math-rtn "atan2" 2)
(def-math-rtn "sinh" 1)
(def-math-rtn "cosh" 1)
(def-math-rtn "tanh" 1)
(def-math-rtn "asinh" 1)
(def-math-rtn "acosh" 1)
(def-math-rtn "atanh" 1)

;;; Exponential and Logarithmic.
#!-x86 (def-math-rtn "exp" 1)
#!-x86 (def-math-rtn "log" 1)
#!-x86 (def-math-rtn "log10" 1)
(def-math-rtn "pow" 2)
#!-x86 (def-math-rtn "sqrt" 1)
(def-math-rtn "hypot" 2)
#!-(or hpux x86) (def-math-rtn "log1p" 1)

#!+x86 ;; These are needed for use by byte-compiled files.
(progn
  (defun %sin (x)
    (declare (double-float x)
	     (values double-float))
    (%sin x))
  (defun %sin-quick (x)
    (declare (double-float x)
	     (values double-float))
    (%sin-quick x))
  (defun %cos (x)
    (declare (double-float x)
	     (values double-float))
    (%cos x))
  (defun %cos-quick (x)
    (declare (double-float x)
	     (values double-float))
    (%cos-quick x))
  (defun %tan (x)
    (declare (double-float x)
	     (values double-float))
    (%tan x))
  (defun %tan-quick (x)
    (declare (double-float x)
	     (values double-float))
    (%tan-quick x))
  (defun %atan (x)
    (declare (double-float x)
	     (values double-float))
    (%atan x))
  (defun %atan2 (x y)
    (declare (double-float x y)
	     (values double-float))
    (%atan2 x y))
  (defun %exp (x)
    (declare (double-float x)
	     (values double-float))
    (%exp x))
  (defun %log (x)
    (declare (double-float x)
	     (values double-float))
    (%log x))
  (defun %log10 (x)
    (declare (double-float x)
	     (values double-float))
    (%log10 x))
  #+nil ;; notyet
  (defun %pow (x y)
    (declare (type (double-float 0d0) x)
	     (double-float y)
	     (values (double-float 0d0)))
    (%pow x y))
  (defun %sqrt (x)
    (declare (double-float x)
	     (values double-float))
    (%sqrt x))
  (defun %scalbn (f ex)
    (declare (double-float f)
	     (type (signed-byte 32) ex)
	     (values double-float))
    (%scalbn f ex))
  (defun %scalb (f ex)
    (declare (double-float f ex)
	     (values double-float))
    (%scalb f ex))
  (defun %logb (x)
    (declare (double-float x)
	     (values double-float))
    (%logb x))
  (defun %log1p (x)
    (declare (double-float x)
	     (values double-float))
    (%log1p x))
  ) ; progn

;;;; power functions

(defun exp (number)
  #!+sb-doc
  "Return e raised to the power NUMBER."
  (number-dispatch ((number number))
    (handle-reals %exp number)
    ((complex)
     (* (exp (realpart number))
	(cis (imagpart number))))))

;;; INTEXP -- Handle the rational base, integer power case.

;;; FIXME: As long as the
;;; system dies on stack overflow or memory exhaustion, it seems reasonable
;;; to have this, but its default should be NIL, and when it's NIL,
;;; anything should be accepted.
(defparameter *intexp-maximum-exponent* 10000)

;;; This function precisely calculates base raised to an integral power. It
;;; separates the cases by the sign of power, for efficiency reasons, as powers
;;; can be calculated more efficiently if power is a positive integer. Values
;;; of power are calculated as positive integers, and inverted if negative.
(defun intexp (base power)
  (when (> (abs power) *intexp-maximum-exponent*)
    ;; FIXME: should be ordinary error, not CERROR. (Once we set the
    ;; default for the variable to NIL, the un-continuable error will
    ;; be less obnoxious.)
    (cerror "Continue with calculation."
	    "The absolute value of ~S exceeds ~S."
	    power '*intexp-maximum-exponent* base power))
  (cond ((minusp power)
	 (/ (intexp base (- power))))
	((eql base 2)
	 (ash 1 power))
	(t
	 (do ((nextn (ash power -1) (ash power -1))
	      (total (if (oddp power) base 1)
		     (if (oddp power) (* base total) total)))
	     ((zerop nextn) total)
	   (setq base (* base base))
	   (setq power nextn)))))

;;; If an integer power of a rational, use INTEXP above. Otherwise, do
;;; floating point stuff. If both args are real, we try %POW right off,
;;; assuming it will return 0 if the result may be complex. If so, we call
;;; COMPLEX-POW which directly computes the complex result. We also separate
;;; the complex-real and real-complex cases from the general complex case.
(defun expt (base power)
  #!+sb-doc
  "Returns BASE raised to the POWER."
  (if (zerop power)
      (1+ (* base power))
    (labels (;; determine if the double float is an integer.
	     ;;  0 - not an integer
	     ;;  1 - an odd int
	     ;;  2 - an even int
	     (isint (ihi lo)
	       (declare (type (unsigned-byte 31) ihi)
			(type (unsigned-byte 32) lo)
			(optimize (speed 3) (safety 0)))
	       (let ((isint 0))
		 (declare (type fixnum isint))
		 (cond ((>= ihi #x43400000)	; exponent >= 53
			(setq isint 2))
		       ((>= ihi #x3ff00000)
			(let ((k (- (ash ihi -20) #x3ff)))	; exponent
			  (declare (type (mod 53) k))
			  (cond ((> k 20)
				 (let* ((shift (- 52 k))
					(j (logand (ash lo (- shift))))
					(j2 (ash j shift)))
				   (declare (type (mod 32) shift)
					    (type (unsigned-byte 32) j j2))
				   (when (= j2 lo)
				     (setq isint (- 2 (logand j 1))))))
				((= lo 0)
				 (let* ((shift (- 20 k))
					(j (ash ihi (- shift)))
					(j2 (ash j shift)))
				   (declare (type (mod 32) shift)
					    (type (unsigned-byte 31) j j2))
				   (when (= j2 ihi)
				     (setq isint (- 2 (logand j 1))))))))))
		 isint))
	     (real-expt (x y rtype)
	       (let ((x (coerce x 'double-float))
		     (y (coerce y 'double-float)))
		 (declare (double-float x y))
		 (let* ((x-hi (sb!kernel:double-float-high-bits x))
			(x-lo (sb!kernel:double-float-low-bits x))
			(x-ihi (logand x-hi #x7fffffff))
			(y-hi (sb!kernel:double-float-high-bits y))
			(y-lo (sb!kernel:double-float-low-bits y))
			(y-ihi (logand y-hi #x7fffffff)))
		   (declare (type (signed-byte 32) x-hi y-hi)
			    (type (unsigned-byte 31) x-ihi y-ihi)
			    (type (unsigned-byte 32) x-lo y-lo))
		   ;; y==zero: x**0 = 1
		   (when (zerop (logior y-ihi y-lo))
		     (return-from real-expt (coerce 1d0 rtype)))
		   ;; +-NaN return x+y
		   (when (or (> x-ihi #x7ff00000)
			     (and (= x-ihi #x7ff00000) (/= x-lo 0))
			     (> y-ihi #x7ff00000)
			     (and (= y-ihi #x7ff00000) (/= y-lo 0)))
		     (return-from real-expt (coerce (+ x y) rtype)))
		   (let ((yisint (if (< x-hi 0) (isint y-ihi y-lo) 0)))
		     (declare (type fixnum yisint))
		     ;; special value of y
		     (when (and (zerop y-lo) (= y-ihi #x7ff00000))
		       ;; y is +-inf
		       (return-from real-expt
			 (cond ((and (= x-ihi #x3ff00000) (zerop x-lo))
				;; +-1**inf is NaN
				(coerce (- y y) rtype))
			       ((>= x-ihi #x3ff00000)
				;; (|x|>1)**+-inf = inf,0
				(if (>= y-hi 0)
				    (coerce y rtype)
				    (coerce 0 rtype)))
			       (t
				;; (|x|<1)**-,+inf = inf,0
				(if (< y-hi 0)
				    (coerce (- y) rtype)
				    (coerce 0 rtype))))))

		     (let ((abs-x (abs x)))
		       (declare (double-float abs-x))
		       ;; special value of x
		       (when (and (zerop x-lo)
				  (or (= x-ihi #x7ff00000) (zerop x-ihi)
				      (= x-ihi #x3ff00000)))
			 ;; x is +-0,+-inf,+-1
			 (let ((z (if (< y-hi 0)
				      (/ 1 abs-x)	; z = (1/|x|)
				      abs-x)))
			   (declare (double-float z))
			   (when (< x-hi 0)
			     (cond ((and (= x-ihi #x3ff00000) (zerop yisint))
				    ;; (-1)**non-int
				    (let ((y*pi (* y pi)))
				      (declare (double-float y*pi))
				      (return-from real-expt
					(complex
					 (coerce (%cos y*pi) rtype)
					 (coerce (%sin y*pi) rtype)))))
				   ((= yisint 1)
				    ;; (x<0)**odd = -(|x|**odd)
				    (setq z (- z)))))
			   (return-from real-expt (coerce z rtype))))

		       (if (>= x-hi 0)
			   ;; x>0
			   (coerce (sb!kernel::%pow x y) rtype)
			   ;; x<0
			   (let ((pow (sb!kernel::%pow abs-x y)))
			     (declare (double-float pow))
			     (case yisint
			       (1 ; Odd
				(coerce (* -1d0 pow) rtype))
			       (2 ; Even
				(coerce pow rtype))
			       (t ; Non-integer
				(let ((y*pi (* y pi)))
				  (declare (double-float y*pi))
				  (complex
				   (coerce (* pow (%cos y*pi)) rtype)
				   (coerce (* pow (%sin y*pi)) rtype)))))))))))))
      (declare (inline real-expt))
      (number-dispatch ((base number) (power number))
	(((foreach fixnum (or bignum ratio) (complex rational)) integer)
	 (intexp base power))
	(((foreach single-float double-float) rational)
	 (real-expt base power '(dispatch-type base)))
	(((foreach fixnum (or bignum ratio) single-float)
	  (foreach ratio single-float))
	 (real-expt base power 'single-float))
	(((foreach fixnum (or bignum ratio) single-float double-float)
	  double-float)
	 (real-expt base power 'double-float))
	((double-float single-float)
	 (real-expt base power 'double-float))
	(((foreach (complex rational) (complex float)) rational)
	 (* (expt (abs base) power)
	    (cis (* power (phase base)))))
	(((foreach fixnum (or bignum ratio) single-float double-float)
	  complex)
	 (if (and (zerop base) (plusp (realpart power)))
	     (* base power)
	     (exp (* power (log base)))))
	(((foreach (complex float) (complex rational))
	  (foreach complex double-float single-float))
	 (if (and (zerop base) (plusp (realpart power)))
	     (* base power)
	     (exp (* power (log base)))))))))

(defun log (number &optional (base nil base-p))
  #!+sb-doc
  "Return the logarithm of NUMBER in the base BASE, which defaults to e."
  (if base-p
      (if (zerop base)
	  base				; ANSI spec
	  (/ (log number) (log base)))
      (number-dispatch ((number number))
	(((foreach fixnum bignum ratio))
	 (if (minusp number)
	     (complex (log (- number)) (coerce pi 'single-float))
	     (coerce (%log (coerce number 'double-float)) 'single-float)))
	(((foreach single-float double-float))
	 ;; Is (log -0) -infinity (libm.a) or -infinity + i*pi (Kahan)?
	 ;; Since this doesn't seem to be an implementation issue
	 ;; I (pw) take the Kahan result.
	 (if (< (float-sign number)
		(coerce 0 '(dispatch-type number)))
	     (complex (log (- number)) (coerce pi '(dispatch-type number)))
	     (coerce (%log (coerce number 'double-float))
		     '(dispatch-type number))))
	((complex)
	 (complex-log number)))))

(defun sqrt (number)
  #!+sb-doc
  "Return the square root of NUMBER."
  (number-dispatch ((number number))
    (((foreach fixnum bignum ratio))
     (if (minusp number)
	 (complex-sqrt number)
	 (coerce (%sqrt (coerce number 'double-float)) 'single-float)))
    (((foreach single-float double-float))
     (if (minusp number)
	 (complex-sqrt number)
	 (coerce (%sqrt (coerce number 'double-float))
		 '(dispatch-type number))))
     ((complex)
      (complex-sqrt number))))

;;;; trigonometic and related functions

(defun abs (number)
  #!+sb-doc
  "Returns the absolute value of the number."
  (number-dispatch ((number number))
    (((foreach single-float double-float fixnum rational))
     (abs number))
    ((complex)
     (let ((rx (realpart number))
	   (ix (imagpart number)))
       (etypecase rx
	 (rational
	  (sqrt (+ (* rx rx) (* ix ix))))
	 (single-float
	  (coerce (%hypot (coerce rx 'double-float)
			  (coerce ix 'double-float))
		  'single-float))
	 (double-float
	  (%hypot rx ix)))))))

(defun phase (number)
  #!+sb-doc
  "Returns the angle part of the polar representation of a complex number.
  For complex numbers, this is (atan (imagpart number) (realpart number)).
  For non-complex positive numbers, this is 0. For non-complex negative
  numbers this is PI."
  (etypecase number
    (rational
     (if (minusp number)
	 (coerce pi 'single-float)
	 0.0f0))
    (single-float
     (if (minusp (float-sign number))
	 (coerce pi 'single-float)
	 0.0f0))
    (double-float
     (if (minusp (float-sign number))
	 (coerce pi 'double-float)
	 0.0d0))
    (complex
     (atan (imagpart number) (realpart number)))))

(defun sin (number)
  #!+sb-doc
  "Return the sine of NUMBER."
  (number-dispatch ((number number))
    (handle-reals %sin number)
    ((complex)
     (let ((x (realpart number))
	   (y (imagpart number)))
       (complex (* (sin x) (cosh y))
		(* (cos x) (sinh y)))))))

(defun cos (number)
  #!+sb-doc
  "Return the cosine of NUMBER."
  (number-dispatch ((number number))
    (handle-reals %cos number)
    ((complex)
     (let ((x (realpart number))
	   (y (imagpart number)))
       (complex (* (cos x) (cosh y))
		(- (* (sin x) (sinh y))))))))

(defun tan (number)
  #!+sb-doc
  "Return the tangent of NUMBER."
  (number-dispatch ((number number))
    (handle-reals %tan number)
    ((complex)
     (complex-tan number))))

(defun cis (theta)
  #!+sb-doc
  "Return cos(Theta) + i sin(Theta), AKA exp(i Theta)."
  (if (complexp theta)
      (error "Argument to CIS is complex: ~S" theta)
      (complex (cos theta) (sin theta))))

(defun asin (number)
  #!+sb-doc
  "Return the arc sine of NUMBER."
  (number-dispatch ((number number))
    ((rational)
     (if (or (> number 1) (< number -1))
	 (complex-asin number)
	 (coerce (%asin (coerce number 'double-float)) 'single-float)))
    (((foreach single-float double-float))
     (if (or (> number (coerce 1 '(dispatch-type number)))
	     (< number (coerce -1 '(dispatch-type number))))
	 (complex-asin number)
	 (coerce (%asin (coerce number 'double-float))
		 '(dispatch-type number))))
    ((complex)
     (complex-asin number))))

(defun acos (number)
  #!+sb-doc
  "Return the arc cosine of NUMBER."
  (number-dispatch ((number number))
    ((rational)
     (if (or (> number 1) (< number -1))
	 (complex-acos number)
	 (coerce (%acos (coerce number 'double-float)) 'single-float)))
    (((foreach single-float double-float))
     (if (or (> number (coerce 1 '(dispatch-type number)))
	     (< number (coerce -1 '(dispatch-type number))))
	 (complex-acos number)
	 (coerce (%acos (coerce number 'double-float))
		 '(dispatch-type number))))
    ((complex)
     (complex-acos number))))

(defun atan (y &optional (x nil xp))
  #!+sb-doc
  "Return the arc tangent of Y if X is omitted or Y/X if X is supplied."
  (if xp
      (flet ((atan2 (y x)
	       (declare (type double-float y x)
			(values double-float))
	       (if (zerop x)
		   (if (zerop y)
		       (if (plusp (float-sign x))
			   y
			   (float-sign y pi))
		       (float-sign y (/ pi 2)))
		   (%atan2 y x))))
	(number-dispatch ((y number) (x number))
	  ((double-float
	    (foreach double-float single-float fixnum bignum ratio))
	   (atan2 y (coerce x 'double-float)))
	  (((foreach single-float fixnum bignum ratio)
	    double-float)
	   (atan2 (coerce y 'double-float) x))
	  (((foreach single-float fixnum bignum ratio)
	    (foreach single-float fixnum bignum ratio))
	   (coerce (atan2 (coerce y 'double-float) (coerce x 'double-float))
		   'single-float))))
      (number-dispatch ((y number))
	(handle-reals %atan y)
	((complex)
	 (complex-atan y)))))

;; It seems that everyone has a C version of sinh, cosh, and
;; tanh. Let's use these for reals because the original
;; implementations based on the definitions lose big in round-off
;; error. These bad definitions also mean that sin and cos for
;; complex numbers can also lose big.

#+nil
(defun sinh (number)
  #!+sb-doc
  "Return the hyperbolic sine of NUMBER."
  (/ (- (exp number) (exp (- number))) 2))

(defun sinh (number)
  #!+sb-doc
  "Return the hyperbolic sine of NUMBER."
  (number-dispatch ((number number))
    (handle-reals %sinh number)
    ((complex)
     (let ((x (realpart number))
	   (y (imagpart number)))
       (complex (* (sinh x) (cos y))
		(* (cosh x) (sin y)))))))

#+nil
(defun cosh (number)
  #!+sb-doc
  "Return the hyperbolic cosine of NUMBER."
  (/ (+ (exp number) (exp (- number))) 2))

(defun cosh (number)
  #!+sb-doc
  "Return the hyperbolic cosine of NUMBER."
  (number-dispatch ((number number))
    (handle-reals %cosh number)
    ((complex)
     (let ((x (realpart number))
	   (y (imagpart number)))
       (complex (* (cosh x) (cos y))
		(* (sinh x) (sin y)))))))

(defun tanh (number)
  #!+sb-doc
  "Return the hyperbolic tangent of NUMBER."
  (number-dispatch ((number number))
    (handle-reals %tanh number)
    ((complex)
     (complex-tanh number))))

(defun asinh (number)
  #!+sb-doc
  "Return the hyperbolic arc sine of NUMBER."
  (number-dispatch ((number number))
    (handle-reals %asinh number)
    ((complex)
     (complex-asinh number))))

(defun acosh (number)
  #!+sb-doc
  "Return the hyperbolic arc cosine of NUMBER."
  (number-dispatch ((number number))
    ((rational)
     ;; acosh is complex if number < 1
     (if (< number 1)
	 (complex-acosh number)
	 (coerce (%acosh (coerce number 'double-float)) 'single-float)))
    (((foreach single-float double-float))
     (if (< number (coerce 1 '(dispatch-type number)))
	 (complex-acosh number)
	 (coerce (%acosh (coerce number 'double-float))
		 '(dispatch-type number))))
    ((complex)
     (complex-acosh number))))

(defun atanh (number)
  #!+sb-doc
  "Return the hyperbolic arc tangent of NUMBER."
  (number-dispatch ((number number))
    ((rational)
     ;; atanh is complex if |number| > 1
     (if (or (> number 1) (< number -1))
	 (complex-atanh number)
	 (coerce (%atanh (coerce number 'double-float)) 'single-float)))
    (((foreach single-float double-float))
     (if (or (> number (coerce 1 '(dispatch-type number)))
	     (< number (coerce -1 '(dispatch-type number))))
	 (complex-atanh number)
	 (coerce (%atanh (coerce number 'double-float))
		 '(dispatch-type number))))
    ((complex)
     (complex-atanh number))))

;;; HP-UX does not supply a C version of log1p, so
;;; use the definition.

#!+hpux
#!-sb-fluid (declaim (inline %log1p))
#!+hpux
(defun %log1p (number)
  (declare (double-float number)
	   (optimize (speed 3) (safety 0)))
  (the double-float (log (the (double-float 0d0) (+ number 1d0)))))


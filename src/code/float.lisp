;;;; This file contains the definitions of float-specific number
;;;; support (other than irrational stuff, which is in irrat.) There is
;;;; code in here that assumes there are only two float formats: IEEE
;;;; single and double. (LONG-FLOAT support has been added, but bugs
;;;; may still remain due to old code which assumes this dichotomy.)

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!KERNEL")

;;;; utilities

(eval-when (:compile-toplevel :load-toplevel :execute)

;;; These functions let us create floats from bits with the
;;; significand uniformly represented as an integer. This is less
;;; efficient for double floats, but is more convenient when making
;;; special values, etc.
(defun single-from-bits (sign exp sig)
  (declare (type bit sign) (type (unsigned-byte 24) sig)
	   (type (unsigned-byte 8) exp))
  (make-single-float
   (dpb exp sb!vm:single-float-exponent-byte
	(dpb sig sb!vm:single-float-significand-byte
	     (if (zerop sign) 0 -1)))))
(defun double-from-bits (sign exp sig)
  (declare (type bit sign) (type (unsigned-byte 53) sig)
	   (type (unsigned-byte 11) exp))
  (make-double-float (dpb exp sb!vm:double-float-exponent-byte
			  (dpb (ash sig -32)
			       sb!vm:double-float-significand-byte
			       (if (zerop sign) 0 -1)))
		     (ldb (byte 32 0) sig)))
#!+(and long-float x86)
(defun long-from-bits (sign exp sig)
  (declare (type bit sign) (type (unsigned-byte 64) sig)
	   (type (unsigned-byte 15) exp))
  (make-long-float (logior (ash sign 15) exp)
		   (ldb (byte 32 32) sig)
		   (ldb (byte 32 0) sig)))

) ; EVAL-WHEN

;;;; float parameters

(defconstant least-positive-single-float (single-from-bits 0 0 1))
(defconstant least-positive-short-float (single-from-bits 0 0 1))
(defconstant least-negative-single-float (single-from-bits 1 0 1))
(defconstant least-negative-short-float (single-from-bits 1 0 1))
(defconstant least-positive-double-float (double-from-bits 0 0 1))
#!-long-float
(defconstant least-positive-long-float (double-from-bits 0 0 1))
#!+(and long-float x86)
(defconstant least-positive-long-float (long-from-bits 0 0 1))
(defconstant least-negative-double-float (double-from-bits 1 0 1))
#!-long-float
(defconstant least-negative-long-float (double-from-bits 1 0 1))
#!+(and long-float x86)
(defconstant least-negative-long-float (long-from-bits 1 0 1))

(defconstant least-positive-normalized-single-float
  (single-from-bits 0 sb!vm:single-float-normal-exponent-min 0))
(defconstant least-positive-normalized-short-float
  least-positive-normalized-single-float)
(defconstant least-negative-normalized-single-float
  (single-from-bits 1 sb!vm:single-float-normal-exponent-min 0))
(defconstant least-negative-normalized-short-float
  least-negative-normalized-single-float)
(defconstant least-positive-normalized-double-float
  (double-from-bits 0 sb!vm:double-float-normal-exponent-min 0))
#!-long-float
(defconstant least-positive-normalized-long-float
  least-positive-normalized-double-float)
#!+(and long-float x86)
(defconstant least-positive-normalized-long-float
  (long-from-bits 0 sb!vm:long-float-normal-exponent-min
		  (ash sb!vm:long-float-hidden-bit 32)))
(defconstant least-negative-normalized-double-float
  (double-from-bits 1 sb!vm:double-float-normal-exponent-min 0))
#!-long-float
(defconstant least-negative-normalized-long-float
  least-negative-normalized-double-float)
#!+(and long-float x86)
(defconstant least-negative-normalized-long-float
  (long-from-bits 1 sb!vm:long-float-normal-exponent-min
		  (ash sb!vm:long-float-hidden-bit 32)))

(defconstant most-positive-single-float
  (single-from-bits 0 sb!vm:single-float-normal-exponent-max
		    (ldb sb!vm:single-float-significand-byte -1)))
(defconstant most-positive-short-float most-positive-single-float)
(defconstant most-negative-single-float
  (single-from-bits 1 sb!vm:single-float-normal-exponent-max
		    (ldb sb!vm:single-float-significand-byte -1)))
(defconstant most-negative-short-float most-negative-single-float)
(defconstant most-positive-double-float
  (double-from-bits 0 sb!vm:double-float-normal-exponent-max
		    (ldb (byte sb!vm:double-float-digits 0) -1)))
#!-long-float
(defconstant most-positive-long-float most-positive-double-float)
#!+(and long-float x86)
(defconstant most-positive-long-float
  (long-from-bits 0 sb!vm:long-float-normal-exponent-max
		  (ldb (byte sb!vm:long-float-digits 0) -1)))
(defconstant most-negative-double-float
  (double-from-bits 1 sb!vm:double-float-normal-exponent-max
		    (ldb (byte sb!vm:double-float-digits 0) -1)))
#!-long-float
(defconstant most-negative-long-float most-negative-double-float)
#!+(and long-float x86)
(defconstant most-negative-long-float
  (long-from-bits 1 sb!vm:long-float-normal-exponent-max
		  (ldb (byte sb!vm:long-float-digits 0) -1)))

;;; We don't want to do these DEFCONSTANTs at cross-compilation time,
;;; because the cross-compilation host might not support floating
;;; point infinities. Putting them inside a LET removes
;;; toplevel-formness, so that any EVAL-WHEN trickiness in the
;;; DEFCONSTANT forms is suppressed.
(let ()
(defconstant single-float-positive-infinity
  (single-from-bits 0 (1+ sb!vm:single-float-normal-exponent-max) 0))
(defconstant short-float-positive-infinity single-float-positive-infinity)
(defconstant single-float-negative-infinity
  (single-from-bits 1 (1+ sb!vm:single-float-normal-exponent-max) 0))
(defconstant short-float-negative-infinity single-float-negative-infinity)
(defconstant double-float-positive-infinity
  (double-from-bits 0 (1+ sb!vm:double-float-normal-exponent-max) 0))
#!+(not long-float)
(defconstant long-float-positive-infinity double-float-positive-infinity)
#!+(and long-float x86)
(defconstant long-float-positive-infinity
  (long-from-bits 0 (1+ sb!vm:long-float-normal-exponent-max)
		  (ash sb!vm:long-float-hidden-bit 32)))
(defconstant double-float-negative-infinity
  (double-from-bits 1 (1+ sb!vm:double-float-normal-exponent-max) 0))
#!+(not long-float)
(defconstant long-float-negative-infinity double-float-negative-infinity)
#!+(and long-float x86)
(defconstant long-float-negative-infinity
  (long-from-bits 1 (1+ sb!vm:long-float-normal-exponent-max)
		  (ash sb!vm:long-float-hidden-bit 32)))
) ; LET-to-suppress-possible-EVAL-WHENs

(defconstant single-float-epsilon
  (single-from-bits 0 (- sb!vm:single-float-bias
			 (1- sb!vm:single-float-digits)) 1))
(defconstant short-float-epsilon single-float-epsilon)
(defconstant single-float-negative-epsilon
  (single-from-bits 0 (- sb!vm:single-float-bias sb!vm:single-float-digits) 1))
(defconstant short-float-negative-epsilon single-float-negative-epsilon)
(defconstant double-float-epsilon
  (double-from-bits 0 (- sb!vm:double-float-bias
			 (1- sb!vm:double-float-digits)) 1))
#!-long-float
(defconstant long-float-epsilon double-float-epsilon)
#!+(and long-float x86)
(defconstant long-float-epsilon
  (long-from-bits 0 (- sb!vm:long-float-bias (1- sb!vm:long-float-digits))
		  (+ 1 (ash sb!vm:long-float-hidden-bit 32))))
(defconstant double-float-negative-epsilon
  (double-from-bits 0 (- sb!vm:double-float-bias sb!vm:double-float-digits) 1))
#!-long-float
(defconstant long-float-negative-epsilon double-float-negative-epsilon)
#!+(and long-float x86)
(defconstant long-float-negative-epsilon
  (long-from-bits 0 (- sb!vm:long-float-bias sb!vm:long-float-digits)
		  (+ 1 (ash sb!vm:long-float-hidden-bit 32))))

;;;; float predicates and environment query

#!-sb-fluid
(declaim (maybe-inline float-denormalized-p float-infinity-p float-nan-p
		       float-trapping-nan-p))

(defun float-denormalized-p (x)
  #!+sb-doc
  "Return true if the float X is denormalized."
  (number-dispatch ((x float))
    ((single-float)
     (and (zerop (ldb sb!vm:single-float-exponent-byte (single-float-bits x)))
	  (not (zerop x))))
    ((double-float)
     (and (zerop (ldb sb!vm:double-float-exponent-byte
		      (double-float-high-bits x)))
	  (not (zerop x))))
    #!+(and long-float x86)
    ((long-float)
     (and (zerop (ldb sb!vm:long-float-exponent-byte (long-float-exp-bits x)))
	  (not (zerop x))))))

(defmacro !define-float-dispatching-function
    (name doc single double #!+(and long-float x86) long)
  `(defun ,name (x)
    ,doc
    (number-dispatch ((x float))
     ((single-float)
      (let ((bits (single-float-bits x)))
	(and (> (ldb sb!vm:single-float-exponent-byte bits)
		sb!vm:single-float-normal-exponent-max)
	     ,single)))
     ((double-float)
      (let ((hi (double-float-high-bits x))
	    (lo (double-float-low-bits x)))
	(declare (ignorable lo))
	(and (> (ldb sb!vm:double-float-exponent-byte hi)
		sb!vm:double-float-normal-exponent-max)
	     ,double)))
     #!+(and long-float x86)
     ((long-float)
      (let ((exp (long-float-exp-bits x))
	    (hi (long-float-high-bits x))
	    (lo (long-float-low-bits x)))
	(declare (ignorable lo))
	(and (> (ldb sb!vm:long-float-exponent-byte exp)
		sb!vm:long-float-normal-exponent-max)
	     ,long))))))

(!define-float-dispatching-function float-infinity-p
  "Return true if the float X is an infinity (+ or -)."
  (zerop (ldb sb!vm:single-float-significand-byte bits))
  (and (zerop (ldb sb!vm:double-float-significand-byte hi))
       (zerop lo))
  #!+(and long-float x86)
  (and (zerop (ldb sb!vm:long-float-significand-byte hi))
       (zerop lo)))

(!define-float-dispatching-function float-nan-p
  "Return true if the float X is a NaN (Not a Number)."
  (not (zerop (ldb sb!vm:single-float-significand-byte bits)))
  (or (not (zerop (ldb sb!vm:double-float-significand-byte hi)))
      (not (zerop lo)))
  #!+(and long-float x86)
  (or (not (zerop (ldb sb!vm:long-float-significand-byte hi)))
      (not (zerop lo))))

(!define-float-dispatching-function float-trapping-nan-p
  "Return true if the float X is a trapping NaN (Not a Number)."
  (zerop (logand (ldb sb!vm:single-float-significand-byte bits)
		 sb!vm:single-float-trapping-nan-bit))
  (zerop (logand (ldb sb!vm:double-float-significand-byte hi)
		 sb!vm:double-float-trapping-nan-bit))
  #!+(and long-float x86)
  (zerop (logand (ldb sb!vm:long-float-significand-byte hi)
		 sb!vm:long-float-trapping-nan-bit)))

;;; If denormalized, use a subfunction from INTEGER-DECODE-FLOAT to find the
;;; actual exponent (and hence how denormalized it is), otherwise we just
;;; return the number of digits or 0.
#!-sb-fluid (declaim (maybe-inline float-precision))
(defun float-precision (f)
  #!+sb-doc
  "Return a non-negative number of significant digits in its float argument.
  Will be less than FLOAT-DIGITS if denormalized or zero."
  (macrolet ((frob (digits bias decode)
	       `(cond ((zerop f) 0)
		      ((float-denormalized-p f)
		       (multiple-value-bind (ignore exp) (,decode f)
			 (declare (ignore ignore))
			 (truly-the fixnum
				    (+ ,digits (1- ,digits) ,bias exp))))
		      (t
		       ,digits))))
    (number-dispatch ((f float))
      ((single-float)
       (frob sb!vm:single-float-digits sb!vm:single-float-bias
	 integer-decode-single-denorm))
      ((double-float)
       (frob sb!vm:double-float-digits sb!vm:double-float-bias
	 integer-decode-double-denorm))
      #!+long-float
      ((long-float)
       (frob sb!vm:long-float-digits sb!vm:long-float-bias
	 integer-decode-long-denorm)))))

(defun float-sign (float1 &optional (float2 (float 1 float1)))
  #!+sb-doc
  "Return a floating-point number that has the same sign as
   FLOAT1 and, if FLOAT2 is given, has the same absolute value
   as FLOAT2."
  (declare (float float1 float2))
  (* (if (etypecase float1
	   (single-float (minusp (single-float-bits float1)))
	   (double-float (minusp (double-float-high-bits float1)))
	   #!+long-float
	   (long-float (minusp (long-float-exp-bits float1))))
	 (float -1 float1)
	 (float 1 float1))
     (abs float2)))

(defun float-format-digits (format)
  (ecase format
    ((short-float single-float) sb!vm:single-float-digits)
    ((double-float #!-long-float long-float) sb!vm:double-float-digits)
    #!+long-float
    (long-float sb!vm:long-float-digits)))

#!-sb-fluid (declaim (inline float-digits float-radix))

(defun float-digits (f)
  (number-dispatch ((f float))
    ((single-float) sb!vm:single-float-digits)
    ((double-float) sb!vm:double-float-digits)
    #!+long-float
    ((long-float) sb!vm:long-float-digits)))

(defun float-radix (x)
  #!+sb-doc
  "Return (as an integer) the radix b of its floating-point argument."
  (declare (ignore x))
  2)

;;;; INTEGER-DECODE-FLOAT and DECODE-FLOAT

#!-sb-fluid
(declaim (maybe-inline integer-decode-single-float
		       integer-decode-double-float))

;;; Handle the denormalized case of INTEGER-DECODE-FLOAT for SINGLE-FLOAT.
(defun integer-decode-single-denorm (x)
  (declare (type single-float x))
  (let* ((bits (single-float-bits (abs x)))
	 (sig (ash (ldb sb!vm:single-float-significand-byte bits) 1))
	 (extra-bias 0))
    (declare (type (unsigned-byte 24) sig)
	     (type (integer 0 23) extra-bias))
    (loop
      (unless (zerop (logand sig sb!vm:single-float-hidden-bit))
	(return))
      (setq sig (ash sig 1))
      (incf extra-bias))
    (values sig
	    (- (- sb!vm:single-float-bias)
	       sb!vm:single-float-digits
	       extra-bias)
	    (if (minusp (float-sign x)) -1 1))))

;;; Handle the single-float case of INTEGER-DECODE-FLOAT. If an infinity or
;;; NaN, error. If a denorm, call i-d-s-DENORM to handle it.
(defun integer-decode-single-float (x)
  (declare (single-float x))
  (let* ((bits (single-float-bits (abs x)))
	 (exp (ldb sb!vm:single-float-exponent-byte bits))
	 (sig (ldb sb!vm:single-float-significand-byte bits))
	 (sign (if (minusp (float-sign x)) -1 1))
	 (biased (- exp sb!vm:single-float-bias sb!vm:single-float-digits)))
    (declare (fixnum biased))
    (unless (<= exp sb!vm:single-float-normal-exponent-max)
      (error "can't decode NaN or infinity: ~S" x))
    (cond ((and (zerop exp) (zerop sig))
	   (values 0 biased sign))
	  ((< exp sb!vm:single-float-normal-exponent-min)
	   (integer-decode-single-denorm x))
	  (t
	   (values (logior sig sb!vm:single-float-hidden-bit) biased sign)))))

;;; like INTEGER-DECODE-SINGLE-DENORM, only doubly so
(defun integer-decode-double-denorm (x)
  (declare (type double-float x))
  (let* ((high-bits (double-float-high-bits (abs x)))
	 (sig-high (ldb sb!vm:double-float-significand-byte high-bits))
	 (low-bits (double-float-low-bits x))
	 (sign (if (minusp (float-sign x)) -1 1))
	 (biased (- (- sb!vm:double-float-bias) sb!vm:double-float-digits)))
    (if (zerop sig-high)
	(let ((sig low-bits)
	      (extra-bias (- sb!vm:double-float-digits 33))
	      (bit (ash 1 31)))
	  (declare (type (unsigned-byte 32) sig) (fixnum extra-bias))
	  (loop
	    (unless (zerop (logand sig bit)) (return))
	    (setq sig (ash sig 1))
	    (incf extra-bias))
	  (values (ash sig (- sb!vm:double-float-digits 32))
		  (truly-the fixnum (- biased extra-bias))
		  sign))
	(let ((sig (ash sig-high 1))
	      (extra-bias 0))
	  (declare (type (unsigned-byte 32) sig) (fixnum extra-bias))
	  (loop
	    (unless (zerop (logand sig sb!vm:double-float-hidden-bit))
	      (return))
	    (setq sig (ash sig 1))
	    (incf extra-bias))
	  (values (logior (ash sig 32) (ash low-bits (1- extra-bias)))
		  (truly-the fixnum (- biased extra-bias))
		  sign)))))

;;; like INTEGER-DECODE-SINGLE-FLOAT, only doubly so
(defun integer-decode-double-float (x)
  (declare (double-float x))
  (let* ((abs (abs x))
	 (hi (double-float-high-bits abs))
	 (lo (double-float-low-bits abs))
	 (exp (ldb sb!vm:double-float-exponent-byte hi))
	 (sig (ldb sb!vm:double-float-significand-byte hi))
	 (sign (if (minusp (float-sign x)) -1 1))
	 (biased (- exp sb!vm:double-float-bias sb!vm:double-float-digits)))
    (declare (fixnum biased))
    (unless (<= exp sb!vm:double-float-normal-exponent-max)
      (error "Can't decode NaN or infinity: ~S." x))
    (cond ((and (zerop exp) (zerop sig) (zerop lo))
	   (values 0 biased sign))
	  ((< exp sb!vm:double-float-normal-exponent-min)
	   (integer-decode-double-denorm x))
	  (t
	   (values
	    (logior (ash (logior (ldb sb!vm:double-float-significand-byte hi)
				 sb!vm:double-float-hidden-bit)
			 32)
		    lo)
	    biased sign)))))

#!+(and long-float x86)
(defun integer-decode-long-denorm (x)
  (declare (type long-float x))
  (let* ((high-bits (long-float-high-bits (abs x)))
	 (sig-high (ldb sb!vm:long-float-significand-byte high-bits))
	 (low-bits (long-float-low-bits x))
	 (sign (if (minusp (float-sign x)) -1 1))
	 (biased (- (- sb!vm:long-float-bias) sb!vm:long-float-digits)))
    (if (zerop sig-high)
	(let ((sig low-bits)
	      (extra-bias (- sb!vm:long-float-digits 33))
	      (bit (ash 1 31)))
	  (declare (type (unsigned-byte 32) sig) (fixnum extra-bias))
	  (loop
	    (unless (zerop (logand sig bit)) (return))
	    (setq sig (ash sig 1))
	    (incf extra-bias))
	  (values (ash sig (- sb!vm:long-float-digits 32))
		  (truly-the fixnum (- biased extra-bias))
		  sign))
	(let ((sig (ash sig-high 1))
	      (extra-bias 0))
	  (declare (type (unsigned-byte 32) sig) (fixnum extra-bias))
	  (loop
	    (unless (zerop (logand sig sb!vm:long-float-hidden-bit))
	      (return))
	    (setq sig (ash sig 1))
	    (incf extra-bias))
	  (values (logior (ash sig 32) (ash low-bits (1- extra-bias)))
		  (truly-the fixnum (- biased extra-bias))
		  sign)))))

#!+(and long-float x86)
(defun integer-decode-long-float (x)
  (declare (long-float x))
  (let* ((hi (long-float-high-bits x))
	 (lo (long-float-low-bits x))
	 (exp-bits (long-float-exp-bits x))
	 (exp (ldb sb!vm:long-float-exponent-byte exp-bits))
	 (sign (if (minusp exp-bits) -1 1))
	 (biased (- exp sb!vm:long-float-bias sb!vm:long-float-digits)))
    (declare (fixnum biased))
    (unless (<= exp sb!vm:long-float-normal-exponent-max)
      (error "can't decode NaN or infinity: ~S" x))
    (cond ((and (zerop exp) (zerop hi) (zerop lo))
	   (values 0 biased sign))
	  ((< exp sb!vm:long-float-normal-exponent-min)
	   (integer-decode-long-denorm x))
	  (t
	   (values (logior (ash hi 32) lo) biased sign)))))

;;; Dispatch to the correct type-specific i-d-f function.
(defun integer-decode-float (x)
  #!+sb-doc
  "Return three values:
   1) an integer representation of the significand.
   2) the exponent for the power of 2 that the significand must be multiplied
      by to get the actual value. This differs from the DECODE-FLOAT exponent
      by FLOAT-DIGITS, since the significand has been scaled to have all its
      digits before the radix point.
   3) -1 or 1 (i.e. the sign of the argument.)"
  (number-dispatch ((x float))
    ((single-float)
     (integer-decode-single-float x))
    ((double-float)
     (integer-decode-double-float x))
    #!+long-float
    ((long-float)
     (integer-decode-long-float x))))

#!-sb-fluid (declaim (maybe-inline decode-single-float decode-double-float))

;;; Handle the denormalized case of DECODE-SINGLE-FLOAT. We call
;;; INTEGER-DECODE-SINGLE-DENORM and then make the result into a float.
(defun decode-single-denorm (x)
  (declare (type single-float x))
  (multiple-value-bind (sig exp sign) (integer-decode-single-denorm x)
    (values (make-single-float
	     (dpb sig sb!vm:single-float-significand-byte
		  (dpb sb!vm:single-float-bias
		       sb!vm:single-float-exponent-byte
		       0)))
	    (truly-the fixnum (+ exp sb!vm:single-float-digits))
	    (float sign x))))

;;; Handle the single-float case of DECODE-FLOAT. If an infinity or NaN,
;;; error. If a denorm, call d-s-DENORM to handle it.
(defun decode-single-float (x)
  (declare (single-float x))
  (let* ((bits (single-float-bits (abs x)))
	 (exp (ldb sb!vm:single-float-exponent-byte bits))
	 (sign (float-sign x))
	 (biased (truly-the single-float-exponent
			    (- exp sb!vm:single-float-bias))))
    (unless (<= exp sb!vm:single-float-normal-exponent-max)
      (error "can't decode NaN or infinity: ~S" x))
    (cond ((zerop x)
	   (values 0.0f0 biased sign))
	  ((< exp sb!vm:single-float-normal-exponent-min)
	   (decode-single-denorm x))
	  (t
	   (values (make-single-float
		    (dpb sb!vm:single-float-bias
			 sb!vm:single-float-exponent-byte
			 bits))
		   biased sign)))))

;;; like DECODE-SINGLE-DENORM, only doubly so
(defun decode-double-denorm (x)
  (declare (double-float x))
  (multiple-value-bind (sig exp sign) (integer-decode-double-denorm x)
    (values (make-double-float
	     (dpb (logand (ash sig -32) (lognot sb!vm:double-float-hidden-bit))
		  sb!vm:double-float-significand-byte
		  (dpb sb!vm:double-float-bias
		       sb!vm:double-float-exponent-byte 0))
	     (ldb (byte 32 0) sig))
	    (truly-the fixnum (+ exp sb!vm:double-float-digits))
	    (float sign x))))

;;; like DECODE-SINGLE-FLOAT, only doubly so
(defun decode-double-float (x)
  (declare (double-float x))
  (let* ((abs (abs x))
	 (hi (double-float-high-bits abs))
	 (lo (double-float-low-bits abs))
	 (exp (ldb sb!vm:double-float-exponent-byte hi))
	 (sign (float-sign x))
	 (biased (truly-the double-float-exponent
			    (- exp sb!vm:double-float-bias))))
    (unless (<= exp sb!vm:double-float-normal-exponent-max)
      (error "can't decode NaN or infinity: ~S" x))
    (cond ((zerop x)
	   (values 0.0d0 biased sign))
	  ((< exp sb!vm:double-float-normal-exponent-min)
	   (decode-double-denorm x))
	  (t
	   (values (make-double-float
		    (dpb sb!vm:double-float-bias
			 sb!vm:double-float-exponent-byte hi)
		    lo)
		   biased sign)))))

#!+(and long-float x86)
(defun decode-long-denorm (x)
  (declare (long-float x))
  (multiple-value-bind (sig exp sign) (integer-decode-long-denorm x)
    (values (make-long-float sb!vm:long-float-bias (ash sig -32)
			     (ldb (byte 32 0) sig))
	    (truly-the fixnum (+ exp sb!vm:long-float-digits))
	    (float sign x))))

#!+(and long-float x86)
(defun decode-long-float (x)
  (declare (long-float x))
  (let* ((hi (long-float-high-bits x))
	 (lo (long-float-low-bits x))
	 (exp-bits (long-float-exp-bits x))
	 (exp (ldb sb!vm:long-float-exponent-byte exp-bits))
	 (sign (if (minusp exp-bits) -1l0 1l0))
	 (biased (truly-the long-float-exponent
			    (- exp sb!vm:long-float-bias))))
    (unless (<= exp sb!vm:long-float-normal-exponent-max)
      (error "can't decode NaN or infinity: ~S" x))
    (cond ((zerop x)
	   (values 0.0l0 biased sign))
	  ((< exp sb!vm:long-float-normal-exponent-min)
	   (decode-long-denorm x))
	  (t
	   (values (make-long-float
		    (dpb sb!vm:long-float-bias sb!vm:long-float-exponent-byte
			 exp-bits)
		    hi
		    lo)
		   biased sign)))))

;;; Dispatch to the appropriate type-specific function.
(defun decode-float (f)
  #!+sb-doc
  "Return three values:
   1) a floating-point number representing the significand. This is always
      between 0.5 (inclusive) and 1.0 (exclusive).
   2) an integer representing the exponent.
   3) -1.0 or 1.0 (i.e. the sign of the argument.)"
  (number-dispatch ((f float))
    ((single-float)
     (decode-single-float f))
    ((double-float)
     (decode-double-float f))
    #!+long-float
    ((long-float)
     (decode-long-float f))))

;;;; SCALE-FLOAT

#!-sb-fluid (declaim (maybe-inline scale-single-float scale-double-float))

;;; Handle float scaling where the X is denormalized or the result is
;;; denormalized or underflows to 0.
(defun scale-float-maybe-underflow (x exp)
  (multiple-value-bind (sig old-exp) (integer-decode-float x)
    (let* ((digits (float-digits x))
	   (new-exp (+ exp old-exp digits
		       (etypecase x
			 (single-float sb!vm:single-float-bias)
			 (double-float sb!vm:double-float-bias))))
	   (sign (if (minusp (float-sign x)) 1 0)))
      (cond
       ((< new-exp
	   (etypecase x
	     (single-float sb!vm:single-float-normal-exponent-min)
	     (double-float sb!vm:double-float-normal-exponent-min)))
	(when (sb!vm:current-float-trap :inexact)
	  (error 'floating-point-inexact :operation 'scale-float
		 :operands (list x exp)))
	(when (sb!vm:current-float-trap :underflow)
	  (error 'floating-point-underflow :operation 'scale-float
		 :operands (list x exp)))
	(let ((shift (1- new-exp)))
	  (if (< shift (- (1- digits)))
	      (float-sign x 0.0)
	      (etypecase x
		(single-float (single-from-bits sign 0 (ash sig shift)))
		(double-float (double-from-bits sign 0 (ash sig shift)))))))
       (t
	(etypecase x
	  (single-float (single-from-bits sign new-exp sig))
	  (double-float (double-from-bits sign new-exp sig))))))))

;;; Called when scaling a float overflows, or the original float was a
;;; NaN or infinity. If overflow errors are trapped, then error,
;;; otherwise return the appropriate infinity. If a NaN, signal or not
;;; as appropriate.
(defun scale-float-maybe-overflow (x exp)
  (cond
   ((float-infinity-p x)
    ;; Infinity is infinity, no matter how small...
    x)
   ((float-nan-p x)
    (when (and (float-trapping-nan-p x)
	       (sb!vm:current-float-trap :invalid))
      (error 'floating-point-invalid-operation :operation 'scale-float
	     :operands (list x exp)))
    x)
   (t
    (when (sb!vm:current-float-trap :overflow)
      (error 'floating-point-overflow :operation 'scale-float
	     :operands (list x exp)))
    (when (sb!vm:current-float-trap :inexact)
      (error 'floating-point-inexact :operation 'scale-float
	     :operands (list x exp)))
    (* (float-sign x)
       (etypecase x
	 (single-float single-float-positive-infinity)
	 (double-float double-float-positive-infinity))))))

;;; Scale a single or double float, calling the correct over/underflow
;;; functions.
(defun scale-single-float (x exp)
  (declare (single-float x) (fixnum exp))
  (let* ((bits (single-float-bits x))
	 (old-exp (ldb sb!vm:single-float-exponent-byte bits))
	 (new-exp (+ old-exp exp)))
    (cond
     ((zerop x) x)
     ((or (< old-exp sb!vm:single-float-normal-exponent-min)
	  (< new-exp sb!vm:single-float-normal-exponent-min))
      (scale-float-maybe-underflow x exp))
     ((or (> old-exp sb!vm:single-float-normal-exponent-max)
	  (> new-exp sb!vm:single-float-normal-exponent-max))
      (scale-float-maybe-overflow x exp))
     (t
      (make-single-float (dpb new-exp
			      sb!vm:single-float-exponent-byte
			      bits))))))
(defun scale-double-float (x exp)
  (declare (double-float x) (fixnum exp))
  (let* ((hi (double-float-high-bits x))
	 (lo (double-float-low-bits x))
	 (old-exp (ldb sb!vm:double-float-exponent-byte hi))
	 (new-exp (+ old-exp exp)))
    (cond
     ((zerop x) x)
     ((or (< old-exp sb!vm:double-float-normal-exponent-min)
	  (< new-exp sb!vm:double-float-normal-exponent-min))
      (scale-float-maybe-underflow x exp))
     ((or (> old-exp sb!vm:double-float-normal-exponent-max)
	  (> new-exp sb!vm:double-float-normal-exponent-max))
      (scale-float-maybe-overflow x exp))
     (t
      (make-double-float (dpb new-exp sb!vm:double-float-exponent-byte hi)
			 lo)))))

#!+(and x86 long-float)
(defun scale-long-float (x exp)
  (declare (long-float x) (fixnum exp))
  (scale-float x exp))

;;; Dispatch to the correct type-specific scale-float function.
(defun scale-float (f ex)
  #!+sb-doc
  "Return the value (* f (expt (float 2 f) ex)), but with no unnecessary loss
  of precision or overflow."
  (number-dispatch ((f float))
    ((single-float)
     (scale-single-float f ex))
    ((double-float)
     (scale-double-float f ex))
    #!+long-float
    ((long-float)
     (scale-long-float f ex))))

;;;; converting to/from floats

(defun float (number &optional (other () otherp))
  #!+sb-doc
  "Converts any REAL to a float. If OTHER is not provided, it returns a
  SINGLE-FLOAT if NUMBER is not already a FLOAT. If OTHER is provided, the
  result is the same float format as OTHER."
  (if otherp
      (number-dispatch ((number real) (other float))
	(((foreach rational single-float double-float #!+long-float long-float)
	  (foreach single-float double-float #!+long-float long-float))
	 (coerce number '(dispatch-type other))))
      (if (floatp number)
	  number
	  (coerce number 'single-float))))

(macrolet ((frob (name type)
	     `(defun ,name (x)
		(number-dispatch ((x real))
		  (((foreach single-float double-float #!+long-float long-float
			     fixnum))
		   (coerce x ',type))
		  ((bignum)
		   (bignum-to-float x ',type))
		  ((ratio)
		   (float-ratio x ',type))))))
  (frob %single-float single-float)
  (frob %double-float double-float)
  #!+long-float
  (frob %long-float long-float))

;;; Convert a ratio to a float. We avoid any rounding error by doing an
;;; integer division. Accuracy is important to preserve read/print
;;; consistency, since this is ultimately how the reader reads a float. We
;;; scale the numerator by a power of two until the division results in the
;;; desired number of fraction bits, then do round-to-nearest.
(defun float-ratio (x format)
  (let* ((signed-num (numerator x))
	 (plusp (plusp signed-num))
	 (num (if plusp signed-num (- signed-num)))
	 (den (denominator x))
	 (digits (float-format-digits format))
	 (scale 0))
    (declare (fixnum digits scale))
    ;; Strip any trailing zeros from the denominator and move it into the scale
    ;; factor (to minimize the size of the operands.)
    (let ((den-twos (1- (integer-length (logxor den (1- den))))))
      (declare (fixnum den-twos))
      (decf scale den-twos)
      (setq den (ash den (- den-twos))))
    ;; Guess how much we need to scale by from the magnitudes of the numerator
    ;; and denominator. We want one extra bit for a guard bit.
    (let* ((num-len (integer-length num))
	   (den-len (integer-length den))
	   (delta (- den-len num-len))
	   (shift (1+ (the fixnum (+ delta digits))))
	   (shifted-num (ash num shift)))
      (declare (fixnum delta shift))
      (decf scale delta)
      (labels ((float-and-scale (bits)
		 (let* ((bits (ash bits -1))
			(len (integer-length bits)))
		   (cond ((> len digits)
			  (aver (= len (the fixnum (1+ digits))))
			  (scale-float (floatit (ash bits -1)) (1+ scale)))
			 (t
			  (scale-float (floatit bits) scale)))))
	       (floatit (bits)
		 (let ((sign (if plusp 0 1)))
		   (case format
		     (single-float
		      (single-from-bits sign sb!vm:single-float-bias bits))
		     (double-float
		      (double-from-bits sign sb!vm:double-float-bias bits))
		     #!+long-float
		     (long-float
		      (long-from-bits sign sb!vm:long-float-bias bits))))))
	(loop
	  (multiple-value-bind (fraction-and-guard rem)
	      (truncate shifted-num den)
	    (let ((extra (- (integer-length fraction-and-guard) digits)))
	      (declare (fixnum extra))
	      (cond ((/= extra 1)
		     (aver (> extra 1)))
		    ((oddp fraction-and-guard)
		     (return
		      (if (zerop rem)
			  (float-and-scale
			   (if (zerop (logand fraction-and-guard 2))
			       fraction-and-guard
			       (1+ fraction-and-guard)))
			  (float-and-scale (1+ fraction-and-guard)))))
		    (t
		     (return (float-and-scale fraction-and-guard)))))
	    (setq shifted-num (ash shifted-num -1))
	    (incf scale)))))))

#|
These might be useful if we ever have a machine without float/integer
conversion hardware. For now, we'll use special ops that
uninterruptibly frob the rounding modes & do ieee round-to-integer.

;;; The compiler compiles a call to this when we are doing %UNARY-TRUNCATE
;;; and the result is known to be a fixnum. We can avoid some generic
;;; arithmetic in this case.
(defun %unary-truncate-single-float/fixnum (x)
  (declare (single-float x) (values fixnum))
  (locally (declare (optimize (speed 3) (safety 0)))
    (let* ((bits (single-float-bits x))
	   (exp (ldb sb!vm:single-float-exponent-byte bits))
	   (frac (logior (ldb sb!vm:single-float-significand-byte bits)
			 sb!vm:single-float-hidden-bit))
	   (shift (- exp sb!vm:single-float-digits sb!vm:single-float-bias)))
      (when (> exp sb!vm:single-float-normal-exponent-max)
	(error 'floating-point-invalid-operation :operator 'truncate
	       :operands (list x)))
      (if (<= shift (- sb!vm:single-float-digits))
	  0
	  (let ((res (ash frac shift)))
	    (declare (type (unsigned-byte 31) res))
	    (if (minusp bits)
		(- res)
		res))))))

;;; Double-float version of this operation (see above single op).
(defun %unary-truncate-double-float/fixnum (x)
  (declare (double-float x) (values fixnum))
  (locally (declare (optimize (speed 3) (safety 0)))
    (let* ((hi-bits (double-float-high-bits x))
	   (exp (ldb sb!vm:double-float-exponent-byte hi-bits))
	   (frac (logior (ldb sb!vm:double-float-significand-byte hi-bits)
			 sb!vm:double-float-hidden-bit))
	   (shift (- exp (- sb!vm:double-float-digits sb!vm:n-word-bits)
		     sb!vm:double-float-bias)))
      (when (> exp sb!vm:double-float-normal-exponent-max)
	(error 'floating-point-invalid-operation :operator 'truncate
	       :operands (list x)))
      (if (<= shift (- sb!vm:n-word-bits sb!vm:double-float-digits))
	  0
	  (let* ((res-hi (ash frac shift))
		 (res (if (plusp shift)
			  (logior res-hi
				  (the fixnum
				       (ash (double-float-low-bits x)
					    (- shift sb!vm:n-word-bits))))
			  res-hi)))
	    (declare (type (unsigned-byte 31) res-hi res))
	    (if (minusp hi-bits)
		(- res)
		res))))))
|#

;;; This function is called when we are doing a truncate without any funky
;;; divisor, i.e. converting a float or ratio to an integer. Note that we do
;;; *not* return the second value of truncate, so it must be computed by the
;;; caller if needed.
;;;
;;; In the float case, we pick off small arguments so that compiler can use
;;; special-case operations. We use an exclusive test, since (due to round-off
;;; error), (float most-positive-fixnum) may be greater than
;;; most-positive-fixnum.
(defun %unary-truncate (number)
  (number-dispatch ((number real))
    ((integer) number)
    ((ratio) (values (truncate (numerator number) (denominator number))))
    (((foreach single-float double-float #!+long-float long-float))
     (if (< (float most-negative-fixnum number)
	    number
	    (float most-positive-fixnum number))
	 (truly-the fixnum (%unary-truncate number))
	 (multiple-value-bind (bits exp) (integer-decode-float number)
	   (let ((res (ash bits exp)))
	     (if (minusp number)
		 (- res)
		 res)))))))

;;; Similar to %UNARY-TRUNCATE, but rounds to the nearest integer. If we
;;; can't use the round primitive, then we do our own round-to-nearest on the
;;; result of i-d-f. [Note that this rounding will really only happen with
;;; double floats, since the whole single-float fraction will fit in a fixnum,
;;; so all single-floats larger than most-positive-fixnum can be precisely
;;; represented by an integer.]
(defun %unary-round (number)
  (number-dispatch ((number real))
    ((integer) number)
    ((ratio) (values (round (numerator number) (denominator number))))
    (((foreach single-float double-float #!+long-float long-float))
     (if (< (float most-negative-fixnum number)
	    number
	    (float most-positive-fixnum number))
	 (truly-the fixnum (%unary-round number))
	 (multiple-value-bind (bits exp) (integer-decode-float number)
	   (let* ((shifted (ash bits exp))
		  (rounded (if (and (minusp exp)
				    (oddp shifted)
				    (eql (logand bits
						 (lognot (ash -1 (- exp))))
					 (ash 1 (- -1 exp))))
			       (1+ shifted)
			       shifted)))
	     (if (minusp number)
		 (- rounded)
		 rounded)))))))

(defun rational (x)
  #!+sb-doc
  "RATIONAL produces a rational number for any real numeric argument. This is
  more efficient than RATIONALIZE, but it assumes that floating-point is
  completely accurate, giving a result that isn't as pretty."
  (number-dispatch ((x real))
    (((foreach single-float double-float #!+long-float long-float))
     (multiple-value-bind (bits exp) (integer-decode-float x)
       (if (eql bits 0)
	   0
	   (let* ((int (if (minusp x) (- bits) bits))
		  (digits (float-digits x))
		  (ex (+ exp digits)))
	     (if (minusp ex)
		 (integer-/-integer int (ash 1 (+ digits (- ex))))
		 (integer-/-integer (ash int ex) (ash 1 digits)))))))
    ((rational) x)))

(defun rationalize (x)
  #!+sb-doc
  "Converts any REAL to a RATIONAL. Floats are converted to a simple rational
  representation exploiting the assumption that floats are only accurate to
  their precision. RATIONALIZE (and also RATIONAL) preserve the invariant:
      (= x (float (rationalize x) x))"
  (number-dispatch ((x real))
    (((foreach single-float double-float #!+long-float long-float))
     ;; Thanks to Kim Fateman, who stole this function rationalize-float from
     ;; macsyma's rational. Macsyma'a rationalize was written by the legendary
     ;; Gosper (rwg). Guy Steele said about Gosper, "He has been called the
     ;; only living 17th century mathematician and is also the best pdp-10
     ;; hacker I know." So, if you can understand or debug this code you win
     ;; big.
     (cond ((minusp x) (- (rationalize (- x))))
	   ((zerop x) 0)
	   (t
	    (let ((eps (etypecase x
			   (single-float single-float-epsilon)
			   (double-float double-float-epsilon)
			   #!+long-float
			   (long-float long-float-epsilon)))
		  (y ())
		  (a ()))
	      (do ((xx x (setq y (/ (float 1.0 x) (- xx (float a x)))))
		   (num (setq a (truncate x))
			(+ (* (setq a (truncate y)) num) onum))
		   (den 1 (+ (* a den) oden))
		   (onum 1 num)
		   (oden 0 den))
		  ((and (not (zerop den))
			(not (> (abs (/ (- x (/ (float num x)
						(float den x)))
					x))
				eps)))
		   (integer-/-integer num den))
		(declare ((dispatch-type x) xx)))))))
    ((rational) x)))

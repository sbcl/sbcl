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

(in-package "SB-KERNEL")

;;;; utilities
;;; Don't need to define it in the host in both passes
(eval-when (#-sb-xc :compile-toplevel :load-toplevel :execute)
;;; These functions let us create floats from bits with the
;;; significand uniformly represented as an integer. This is less
;;; efficient for double floats, but is more convenient when making
;;; special values, etc.
(declaim (inline single-from-bits double-from-bits))

(defun single-from-bits (sign exp sig)
  (declare (type bit sign) (type (unsigned-byte 24) sig)
           (type (unsigned-byte 8) exp))
  (make-single-float
   (dpb exp sb-vm:single-float-exponent-byte
        (dpb sig sb-vm:single-float-significand-byte
             (if (zerop sign) 0 -1)))))
(defun double-from-bits (sign exp sig)
  (declare (type bit sign) (type (unsigned-byte 53) sig)
           (type (unsigned-byte 11) exp))
  #-64-bit
  (make-double-float (dpb exp sb-vm:double-float-hi-exponent-byte
                          (dpb (ash sig -32)
                               sb-vm:double-float-hi-significand-byte
                               (if (zerop sign) 0 -1)))
                     (ldb (byte 32 0) sig))
  #+64-bit
  (%make-double-float
   (dpb exp sb-vm:double-float-exponent-byte
        (dpb sig sb-vm:double-float-significand-byte
             (if (zerop sign) 0 -1)))))
#+(and long-float x86)
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
#-long-float
(defconstant least-positive-long-float (double-from-bits 0 0 1))
#+(and long-float x86)
(defconstant least-positive-long-float (long-from-bits 0 0 1))
(defconstant least-negative-double-float (double-from-bits 1 0 1))
#-long-float
(defconstant least-negative-long-float (double-from-bits 1 0 1))
#+(and long-float x86)
(defconstant least-negative-long-float (long-from-bits 1 0 1))

(defconstant least-positive-normalized-single-float
  (single-from-bits 0 sb-vm:single-float-normal-exponent-min 0))
(defconstant least-positive-normalized-short-float
  least-positive-normalized-single-float)
(defconstant least-negative-normalized-single-float
  (single-from-bits 1 sb-vm:single-float-normal-exponent-min 0))
(defconstant least-negative-normalized-short-float
  least-negative-normalized-single-float)
(defconstant least-positive-normalized-double-float
  (double-from-bits 0 sb-vm:double-float-normal-exponent-min 0))
#-long-float
(defconstant least-positive-normalized-long-float
  least-positive-normalized-double-float)
#+(and long-float x86)
(defconstant least-positive-normalized-long-float
  (long-from-bits 0 sb-vm:long-float-normal-exponent-min
                  (ash sb-vm:long-float-hidden-bit 32)))
(defconstant least-negative-normalized-double-float
  (double-from-bits 1 sb-vm:double-float-normal-exponent-min 0))
#-long-float
(defconstant least-negative-normalized-long-float
  least-negative-normalized-double-float)
#+(and long-float x86)
(defconstant least-negative-normalized-long-float
  (long-from-bits 1 sb-vm:long-float-normal-exponent-min
                  (ash sb-vm:long-float-hidden-bit 32)))

(defconstant most-positive-single-float
  (single-from-bits 0 sb-vm:single-float-normal-exponent-max
                    (ldb sb-vm:single-float-significand-byte -1)))
(defconstant most-positive-short-float most-positive-single-float)
(defconstant most-negative-single-float
  (single-from-bits 1 sb-vm:single-float-normal-exponent-max
                    (ldb sb-vm:single-float-significand-byte -1)))
(defconstant most-negative-short-float most-negative-single-float)
(defconstant most-positive-double-float
  (double-from-bits 0 sb-vm:double-float-normal-exponent-max
                    (ldb (byte sb-vm:double-float-digits 0) -1)))
#-long-float
(defconstant most-positive-long-float most-positive-double-float)
#+(and long-float x86)
(defconstant most-positive-long-float
  (long-from-bits 0 sb-vm:long-float-normal-exponent-max
                  (ldb (byte sb-vm:long-float-digits 0) -1)))
(defconstant most-negative-double-float
  (double-from-bits 1 sb-vm:double-float-normal-exponent-max
                    (ldb (byte sb-vm:double-float-digits 0) -1)))
#-long-float
(defconstant most-negative-long-float most-negative-double-float)
#+(and long-float x86)
(defconstant most-negative-long-float
  (long-from-bits 1 sb-vm:long-float-normal-exponent-max
                  (ldb (byte sb-vm:long-float-digits 0) -1)))

(defconstant single-float-positive-infinity
  (single-from-bits 0 (1+ sb-vm:single-float-normal-exponent-max) 0))
(defconstant short-float-positive-infinity single-float-positive-infinity)
(defconstant single-float-negative-infinity
  (single-from-bits 1 (1+ sb-vm:single-float-normal-exponent-max) 0))
(defconstant short-float-negative-infinity single-float-negative-infinity)
(defconstant double-float-positive-infinity
  (double-from-bits 0 (1+ sb-vm:double-float-normal-exponent-max) 0))
#+(not long-float)
(defconstant long-float-positive-infinity double-float-positive-infinity)
#+(and long-float x86)
(defconstant long-float-positive-infinity
  (long-from-bits 0 (1+ sb-vm:long-float-normal-exponent-max)
                  (ash sb-vm:long-float-hidden-bit 32)))
(defconstant double-float-negative-infinity
  (double-from-bits 1 (1+ sb-vm:double-float-normal-exponent-max) 0))
#+(not long-float)
(defconstant long-float-negative-infinity double-float-negative-infinity)
#+(and long-float x86)
(defconstant long-float-negative-infinity
  (long-from-bits 1 (1+ sb-vm:long-float-normal-exponent-max)
                  (ash sb-vm:long-float-hidden-bit 32)))

(defconstant single-float-epsilon
  (single-from-bits 0 (- sb-vm:single-float-bias
                         (1- sb-vm:single-float-digits)) 1))
(defconstant short-float-epsilon single-float-epsilon)
(defconstant single-float-negative-epsilon
  (single-from-bits 0 (- sb-vm:single-float-bias sb-vm:single-float-digits) 1))
(defconstant short-float-negative-epsilon single-float-negative-epsilon)
(defconstant double-float-epsilon
  (double-from-bits 0 (- sb-vm:double-float-bias
                         (1- sb-vm:double-float-digits)) 1))
#-long-float
(defconstant long-float-epsilon double-float-epsilon)
#+(and long-float x86)
(defconstant long-float-epsilon
  (long-from-bits 0 (- sb-vm:long-float-bias (1- sb-vm:long-float-digits))
                  (+ 1 (ash sb-vm:long-float-hidden-bit 32))))
(defconstant double-float-negative-epsilon
  (double-from-bits 0 (- sb-vm:double-float-bias sb-vm:double-float-digits) 1))
#-long-float
(defconstant long-float-negative-epsilon double-float-negative-epsilon)
#+(and long-float x86)
(defconstant long-float-negative-epsilon
  (long-from-bits 0 (- sb-vm:long-float-bias sb-vm:long-float-digits)
                  (+ 1 (ash sb-vm:long-float-hidden-bit 32))))

;;; Limits for floats that can be truncated into a fixnum
;;; with no loss of precision.
;;; (We don't have constants for "most-fooative-fixnum as a mumble-float")
(defconstant most-positive-fixnum-single-float
  (single-from-bits 0 (+ sb-vm:n-fixnum-bits sb-vm:single-float-bias -1)
                               (ldb (byte (1- sb-vm:single-float-digits) 0) -1)))

(defconstant most-negative-fixnum-single-float
  (single-from-bits 1 (+ sb-vm:n-fixnum-bits sb-vm:single-float-bias) 0))

(defconstant most-positive-fixnum-double-float
  (double-from-bits 0 (+ sb-vm:n-fixnum-bits sb-vm:double-float-bias -1)
                               (ldb (byte (1- sb-vm:double-float-digits) 0) -1)))

(defconstant most-negative-fixnum-double-float
  (double-from-bits 1 (+ sb-vm:n-fixnum-bits sb-vm:double-float-bias) 0))


;;;; float predicates and environment query

(declaim (maybe-inline float-denormalized-p float-infinity-p float-nan-p
                       float-trapping-nan-p))

(defmacro sfloat-bits-subnormalp (bits)
  `(zerop (ldb sb-vm:single-float-exponent-byte ,bits)))
#-64-bit
(defmacro dfloat-high-bits-subnormalp (bits)
  `(zerop (ldb sb-vm:double-float-hi-exponent-byte ,bits)))
#+64-bit
(defmacro dfloat-bits-subnormalp (bits)
  `(zerop (ldb sb-vm:double-float-exponent-byte ,bits)))

(defun float-denormalized-p (x)
  "Return true if the float X is denormalized."
  (declare (explicit-check))
  (number-dispatch ((x float))
    ((single-float)
     #+64-bit
     (let ((bits (single-float-bits x)))
       (and (ldb-test (byte 31 0) bits) ; is nonzero (disregard the sign bit)
            (sfloat-bits-subnormalp bits)))
     #-64-bit
     (and (zerop (ldb sb-vm:single-float-exponent-byte (single-float-bits x)))
          (not (zerop x))))
    ((double-float)
     #+64-bit
     (let ((bits (double-float-bits x)))
       ;; is nonzero after shifting out the sign bit
       (and (not (zerop (logand (ash bits 1) most-positive-word)))
            (dfloat-bits-subnormalp bits)))
     #-64-bit
     (and (zerop (ldb sb-vm:double-float-hi-exponent-byte
                      (double-float-high-bits x)))
          (not (zerop x))))
    #+(and long-float x86)
    ((long-float)
     (and (zerop (ldb sb-vm:long-float-exponent-byte (long-float-exp-bits x)))
          (not (zerop x))))))

(defmacro float-inf-or-nan-test (var single double #+(and long-float x86) long)
  `(number-dispatch ((,var float))
     ((single-float)
      (let ((bits (single-float-bits ,var)))
        (and (> (ldb sb-vm:single-float-exponent-byte bits)
                sb-vm:single-float-normal-exponent-max)
             ,single)))
     ((double-float)
      #+64-bit
      (let ((bits (double-float-bits ,var)))
        (and (> (ldb sb-vm:double-float-exponent-byte bits)
                sb-vm:double-float-normal-exponent-max)
             ,double))
      #-64-bit
      (let ((hi (double-float-high-bits ,var))
            (lo (double-float-low-bits ,var)))
        (declare (ignorable lo))
        (and (> (ldb sb-vm:double-float-hi-exponent-byte hi)
                sb-vm:double-float-normal-exponent-max)
             ,double)))
     #+(and long-float x86)
     ((long-float)
      (let ((exp (long-float-exp-bits ,var))
            (hi (long-float-high-bits ,var))
            (lo (long-float-low-bits ,var)))
        (declare (ignorable lo))
        (and (> (ldb sb-vm:long-float-exponent-byte exp)
                sb-vm:long-float-normal-exponent-max)
             ,long)))))

;; Infinities and NANs have the maximum exponent
(defun float-infinity-or-nan-p (x)
  (float-inf-or-nan-test x t t #+(and long-float x86) t))

;; Infinity has 0 for the significand
(defun float-infinity-p (x)
  "Return true if the float X is an infinity (+ or -)."
  (float-inf-or-nan-test
   x
   (zerop (ldb sb-vm:single-float-significand-byte bits))

   #+64-bit (zerop (ldb sb-vm:double-float-significand-byte bits))
   #-64-bit (zerop (logior (ldb sb-vm:double-float-hi-significand-byte hi) lo))

   #+(and long-float x86)
   (and (zerop (ldb sb-vm:long-float-significand-byte hi))
        (zerop lo))))

;; NaNs have nonzero for the significand
(defun float-nan-p (x)
  "Return true if the float X is a NaN (Not a Number)."
  (float-inf-or-nan-test
   x
   (not (zerop (ldb sb-vm:single-float-significand-byte bits)))

   #+64-bit (not (zerop (ldb sb-vm:double-float-significand-byte bits)))
   #-64-bit (not (zerop (logior (ldb sb-vm:double-float-hi-significand-byte hi) lo)))

   #+(and long-float x86)
   (or (not (zerop (ldb sb-vm:long-float-significand-byte hi)))
       (not (zerop lo)))))

(defmacro with-float-inf-or-nan-test (float infinity nan normal)
  `(block nil
     ,(if (equal infinity nan)
          `(float-inf-or-nan-test
            ,float
            (return ,nan)
            (return ,nan))
          `(float-inf-or-nan-test
            ,float
            (if (zerop (ldb sb-vm:single-float-significand-byte bits))
                (return ,infinity)
                (return ,nan))
            (if #+64-bit (zerop (ldb sb-vm:double-float-significand-byte bits))
                #-64-bit (zerop (logior (ldb sb-vm:double-float-hi-significand-byte hi) lo))
                (return ,infinity)
                (return ,nan))))
     ,normal))

(defun float-trapping-nan-p (x)
  "Return true if the float X is a trapping NaN (Not a Number)."
  ;; MIPS has trapping NaNs (SNaNs) with the trapping-nan-bit SET.
  ;; All the others have trapping NaNs (SNaNs) with the
  ;; trapping-nan-bit CLEAR.  Note that the given implementation
  ;; considers infinities to be FLOAT-TRAPPING-NAN-P on most
  ;; architectures.
  (float-inf-or-nan-test
   x
   ;; SINGLE-FLOAT
   #+mips (logbitp 22 bits)
   #-mips (not (logbitp 22 bits))

   ;; DOUBLE-FLOAT
   #+mips (logbitp 19 hi)
   #+(and (not mips) 64-bit) (not (logbitp 51 bits))
   #+(and (not mips) (not 64-bit)) (not (logbitp 19 hi))

   ;; LONG-FLOAT (this code is dead anyway)
   #+(and long-float x86)
   (zerop (logand (ldb sb-vm:long-float-significand-byte hi)
                  (ash 1 30)))))

;;; If denormalized, use a subfunction from INTEGER-DECODE-FLOAT to find the
;;; actual exponent (and hence how denormalized it is), otherwise we just
;;; return the number of digits or 0.
(declaim (maybe-inline float-precision))
(defun float-precision (f)
  "Return a non-negative number of significant digits in its float argument.
  Will be less than FLOAT-DIGITS if denormalized or zero."
  (declare (explicit-check))
  (integer-length
   (number-dispatch ((f float))
     ((single-float)
      (let ((bits (single-float-bits f)))
        (if (sfloat-bits-subnormalp bits)
            (ldb sb-vm:single-float-significand-byte bits)
            (return-from float-precision sb-vm:single-float-digits))))
     ((double-float)
      #+64-bit
      (let ((bits (double-float-bits f)))
        (if (dfloat-bits-subnormalp bits)
            (ldb sb-vm:double-float-significand-byte bits)
            (return-from float-precision sb-vm:double-float-digits)))
      #-64-bit
      (let ((high (double-float-high-bits f)))
        (if (not (dfloat-high-bits-subnormalp high))
            (return-from float-precision sb-vm:double-float-digits)
            (let ((n (integer-length (ldb sb-vm:double-float-hi-significand-byte high))))
              (if (/= 0 n)
                  (return-from float-precision (+ n 32))
                  (double-float-low-bits f)))))))))

(defun float-sign (float1 &optional (float2 (float 1 float1)))
  "Return a floating-point number that has the same sign as
   FLOAT1 and, if FLOAT2 is given, has the same absolute value
   as FLOAT2."
  (declare (type float float1 float2) (explicit-check))
  (sb-xc:*
   (if (etypecase float1
         (single-float (minusp (single-float-bits float1)))
         ;; If 64-bits words, use all the bits. No need to right-shift them.
         (double-float (minusp #+64-bit (double-float-bits float1)
                               #-64-bit (double-float-high-bits float1)))
         #+long-float
         (long-float (minusp (long-float-exp-bits float1))))
       (float -1 float1)
       (float 1 float1))
   (abs float2)))

;;; When all we want is the sign bit, there is a simpler way to extract it
;;; than via either integer-decode-float or float-sign. Just shift the msb
;;; over to the lsb position. FLOAT-SIGN produces some pretty horrific code
;;; if the specific subtype of float is unnown:
;;;  (minusp (float-sign x)) becomes (< (float-sign x) (float 0 x))
;;; which ends up calling not only FLOAT-SIGN, but also FLOAT merely to cast
;;; the integer 0 into a float of whatever type X is.
#-sb-xc-host
(defun float-sign-bit (x)      ; return 1 or 0, literally the sign bit
  (declare (explicit-check))
  (number-dispatch ((x float))
    ((single-float) (float-sign-bit x))
    ((double-float) (float-sign-bit x))))
#-sb-xc-host
(defun float-sign-bit-set-p (x)
  (declare (explicit-check))
  (number-dispatch ((x float))
    ((single-float) (float-sign-bit-set-p x))
    ((double-float) (float-sign-bit-set-p x))))

(declaim (inline float-digits float-radix))

(defun float-digits (f)
  "Return a non-negative number of radix-b digits used in the
   representation of its argument."
  (declare (explicit-check))
  (number-dispatch ((f float))
    ((single-float) sb-vm:single-float-digits)
    ((double-float) sb-vm:double-float-digits)
    #+long-float
    ((long-float) sb-vm:long-float-digits)))

(defun float-radix (x)
  "Return (as an integer) the radix b of its floating-point argument."
  (declare (ignore x) (type float x))
  2)


;;;; INTEGER-DECODE-FLOAT and DECODE-FLOAT

(defconstant-eqx float-decoding-error "Can't decode NaN or infinity: ~S."
  #'string=)

(declaim (maybe-inline integer-decode-single-float
                       integer-decode-double-float))

;;; binary point is to the left of the 23 represented mantissa bits,
;;; and normal exponent min is -126, so -149 is the effective exponent
;;; of a subnormal in common-lisp terms.
(defconstant subnormal-sfloat-exponent -149)
;; binary point is to the left of the 52 represented mantissa bits,
;; and normal exponent min is -1022, so -1074 is the effective exponent.
(defconstant subnormal-dfloat-exponent -1074)

;;; Handle the single-float case of INTEGER-DECODE-FLOAT. If an infinity or
;;; NaN, error.
(defun integer-decode-single-float (x)
  (declare (type single-float x))
  (let* ((bits (single-float-bits x))
         (frac (ldb sb-vm:single-float-significand-byte bits))
         (sign (if (minusp bits) -1 1))
         (exp (ldb sb-vm:single-float-exponent-byte bits)))
    (cond ((= exp 0)
           (values frac (if (= frac 0) 0 subnormal-sfloat-exponent) sign))
          ((> exp sb-vm:single-float-normal-exponent-max)
           (error float-decoding-error x))
          (t
           (values (logior sb-vm:single-float-hidden-bit frac)
                   (- exp sb-vm:single-float-bias sb-vm:single-float-digits)
                   sign)))))

;;; like INTEGER-DECODE-SINGLE-FLOAT, only doubly so
(defun integer-decode-double-float (x)
  (declare (type double-float x))
  #-64-bit ; treat high and low bits separately until the end
  (let* ((hi (double-float-high-bits x))
         (sign (if (minusp hi) -1 1))
         (lo (double-float-low-bits x))
         (mantissa (logior (ash (ldb sb-vm:double-float-hi-significand-byte hi) 32) lo))
         (exp (ldb sb-vm:double-float-hi-exponent-byte hi)))
    (cond ((zerop (logior (ldb (byte 31 0) hi) lo))
           (values 0 0 sign))
          ((< exp sb-vm:double-float-normal-exponent-min)
           (values mantissa subnormal-dfloat-exponent sign))
          ((> exp sb-vm:double-float-normal-exponent-max)
           (error float-decoding-error x))
          (t
           (values (logior sb-vm:double-float-hidden-bit mantissa)
                   (- exp sb-vm:double-float-bias sb-vm:double-float-digits)
                   sign))))
  #+64-bit ; don't split the high and low bits
  (let* ((bits (double-float-bits x))
         (frac (ldb sb-vm:double-float-significand-byte bits))
         (sign (if (minusp bits) -1 1))
         (exp (ldb sb-vm:double-float-exponent-byte bits)))
    (cond ((= exp 0)
           (values frac (if (= frac 0) 0 subnormal-dfloat-exponent) sign))
          ((> exp sb-vm:double-float-normal-exponent-max)
           (error float-decoding-error x))
          (t
           (values (logior sb-vm:double-float-hidden-bit frac)
                   (- exp sb-vm:double-float-bias sb-vm:double-float-digits)
                   sign)))))

;;; Dispatch to the correct type-specific i-d-f function.
(defun integer-decode-float (x)
  "Return three values:
   1) an integer representation of the significand.
   2) the exponent for the power of 2 that the significand must be multiplied
      by to get the actual value. This differs from the DECODE-FLOAT exponent
      by FLOAT-DIGITS, since the significand has been scaled to have all its
      digits before the radix point.
   3) -1 or 1 (i.e. the sign of the argument.)"
  (declare (explicit-check))
  (number-dispatch ((x float))
    ((single-float)
     (integer-decode-single-float x))
    ((double-float)
     (integer-decode-double-float x))))

;;; Handle the single-float case of DECODE-FLOAT. If an infinity or NaN,
;;; error. For subnormals, we left-align the significant bits into a field
;;; that is FLOAT-DIGITS wide, and decrease the exponent.
(defun decode-single-float (x)
  (declare (type single-float x))
  (let* ((bits (single-float-bits x))
         (biased-exp (ldb sb-vm:single-float-exponent-byte bits)))
    (if (> biased-exp sb-vm:single-float-normal-exponent-max)
        (error float-decoding-error x)
        (let ((frac (ldb sb-vm:single-float-significand-byte bits)))
          (multiple-value-bind (new-exp new-frac lisp-exponent)
              (cond ((/= biased-exp 0) ; normal
                     ;; SINGLE-FLOAT-BIAS as the stored exponent yields
                     ;; an effective exponent of -1.
                     (values sb-vm:single-float-bias frac
                             (- biased-exp sb-vm:single-float-bias)))
                    ((= frac 0) (values 0 0 0))
                    (t ; subnormal. Normalize it and unset the implied 1 bit
                     (let ((prec (integer-length frac)))
                       (values sb-vm:single-float-bias
                               (ldb (byte (1- sb-vm:single-float-digits) 0)
                                    (ash frac (- sb-vm:single-float-digits prec)))
                               (+ subnormal-sfloat-exponent prec)))))
            (values (make-single-float (dpb new-exp sb-vm:single-float-exponent-byte new-frac))
                    lisp-exponent
                    (float-sign x)))))))

;;; The double-float logic mostly follows the skeleton of the above code,
;;; but there is a consed bignum or two on 32-bit architectures.
;;; Consing for the sake of code clarity is worth it as far as I'm concerned.
(defun decode-double-float (x)
  (declare (type double-float x))
  (let* #+64-bit ((bits (double-float-bits x))
                  (biased-exp (ldb sb-vm:double-float-exponent-byte bits)))
        #-64-bit ((high (double-float-high-bits x))
                  (biased-exp (ldb sb-vm:double-float-hi-exponent-byte high)))
    (if (> biased-exp sb-vm:double-float-normal-exponent-max)
        (error float-decoding-error x)
        (let ((frac #+64-bit (ldb sb-vm:double-float-significand-byte bits)
                    #-64-bit (logior (ash (ldb sb-vm:double-float-hi-significand-byte high) 32)
                                     (double-float-low-bits x))))
          (multiple-value-bind (new-exp new-frac lisp-exponent)
              (cond ((/= biased-exp 0)  ; normal
                     ;; DOUBLE-FLOAT-BIAS as the stored exponent yields
                     ;; an effective exponent of -1.
                     (values sb-vm:double-float-bias frac
                             (- biased-exp sb-vm:double-float-bias)))
                    ((= frac 0) (values 0 0 0))
                    (t ; subnormal. Normalize it and unset the implied 1 bit
                     (let ((prec (integer-length frac)))
                       (values sb-vm:double-float-bias
                               (ldb (byte (1- sb-vm:double-float-digits) 0)
                                    (ash frac (- sb-vm:double-float-digits prec)))
                               (+ subnormal-dfloat-exponent prec)))))
            (values #-64-bit
                    (make-double-float (dpb new-exp sb-vm:double-float-hi-exponent-byte
                                            (ldb (byte 32 32) new-frac))
                                       (ldb (byte 32 0) new-frac))
                    #+64-bit
                    (%make-double-float (dpb new-exp sb-vm:double-float-exponent-byte new-frac))
                    lisp-exponent
                    (float-sign x)))))))

;;; Dispatch to the appropriate type-specific function.
(defun decode-float (f)
  "Return three values:
   1) a floating-point number representing the significand. This is always
      between 0.5 (inclusive) and 1.0 (exclusive).
   2) an integer representing the exponent.
   3) -1.0 or 1.0 (i.e. the sign of the argument.)"
  (declare (explicit-check))
  (number-dispatch ((f float))
    ((single-float)
     (decode-single-float f))
    ((double-float)
     (decode-double-float f))))


;;;; SCALE-FLOAT

(declaim (maybe-inline scale-single-float scale-double-float))

;;; Scale a single or double float, calling the correct over/underflow
;;; functions.
(defun scale-single-float (x exp)
  (declare (single-float x) (integer exp))
  (etypecase exp
    (fixnum
     (let* ((bits (single-float-bits x))
            (old-exp (ldb sb-vm:single-float-exponent-byte bits))
            (new-exp (+ old-exp exp)))
       (cond
         ((zerop x) x)
         ((or (< old-exp sb-vm:single-float-normal-exponent-min)
              (< new-exp sb-vm:single-float-normal-exponent-min))
          (scale-single-float-maybe-underflow x exp))
         ((or (> old-exp sb-vm:single-float-normal-exponent-max)
              (> new-exp sb-vm:single-float-normal-exponent-max))
          (scale-single-float-maybe-overflow x exp))
         (t
          (make-single-float (dpb new-exp
                                  sb-vm:single-float-exponent-byte
                                  bits))))))
    (unsigned-byte (scale-single-float-maybe-overflow x exp))
    ((integer * 0) (scale-single-float-maybe-underflow x exp))))

(defun scale-double-float (x exp)
  (etypecase exp
    (fixnum
     #+64-bit
     (let* ((bits (double-float-bits x))
            (old-exp (ldb sb-vm:double-float-exponent-byte bits))
            (new-exp (+ old-exp exp)))
       (cond
         ((zerop x) x)
         ((or (< old-exp sb-vm:double-float-normal-exponent-min)
              (< new-exp sb-vm:double-float-normal-exponent-min))
          (scale-double-float-maybe-underflow x exp))
         ((or (> old-exp sb-vm:double-float-normal-exponent-max)
              (> new-exp sb-vm:double-float-normal-exponent-max))
          (scale-double-float-maybe-overflow x exp))
         (t
          (%make-double-float (dpb new-exp sb-vm:double-float-exponent-byte bits)))))
     #-64-bit
     (let* ((hi (double-float-high-bits x))
            (lo (double-float-low-bits x))
            (old-exp (ldb sb-vm:double-float-hi-exponent-byte hi))
            (new-exp (+ old-exp exp)))
       (cond
         ((zerop x) x)
         ((or (< old-exp sb-vm:double-float-normal-exponent-min)
              (< new-exp sb-vm:double-float-normal-exponent-min))
          (scale-double-float-maybe-underflow x exp))
         ((or (> old-exp sb-vm:double-float-normal-exponent-max)
              (> new-exp sb-vm:double-float-normal-exponent-max))
          (scale-double-float-maybe-overflow x exp))
         (t
          (make-double-float (dpb new-exp sb-vm:double-float-hi-exponent-byte hi)
                             lo)))))
    (unsigned-byte (scale-double-float-maybe-overflow x exp))
    ((integer * 0) (scale-double-float-maybe-underflow x exp))))

;;; Dispatch to the correct type-specific scale-float function.
(defun scale-float (f ex)
  "Return the value (* f (expt (float 2 f) ex)), but with no unnecessary loss
  of precision or overflow."
  (declare (explicit-check f))
  (number-dispatch ((f float))
    ((single-float)
     (scale-single-float f ex))
    ((double-float)
     (scale-double-float f ex))))

;;;; converting to/from floats

(defun float (number &optional (other () otherp))
  "Converts any REAL to a float. If OTHER is not provided, it returns a
  SINGLE-FLOAT if NUMBER is not already a FLOAT. If OTHER is provided, the
  result is the same float format as OTHER."
  (declare (explicit-check))
  (if otherp
      (number-dispatch ((number real) (other float))
        (((foreach rational single-float double-float #+long-float long-float)
          (foreach single-float double-float #+long-float long-float))
         (coerce number '(dispatch-type other))))
      (if (floatp number)
          number
          (coerce number 'single-float))))

(macrolet ((frob (name type)
             `(defun ,name (x)
                (declare (explicit-check x))
                (number-dispatch ((x real))
                  (((foreach single-float double-float #+long-float long-float
                     sb-vm:signed-word
                     #-sb-xc-host
                     ,@(and (sb-c::template-translates-arg-p '%double-float 0 'word)
                            '(word))))
                   (coerce x ',type))
                  ((ratio)
                   (,(symbolicate type '-ratio) x))
                  ((bignum)
                   (,(symbolicate 'bignum-to- type) x))))))
  (frob %single-float single-float)
  (frob %double-float double-float)
  #+long-float
  (frob %long-float long-float))

;;; Convert a ratio to a float. We avoid any rounding error by doing an
;;; integer division. Accuracy is important to preserve print-read
;;; consistency, since this is ultimately how the reader reads a float. We
;;; scale the numerator by a power of two until the division results in the
;;; desired number of fraction bits, then do round-to-nearest.
(macrolet ((def (format)
             `(defun ,(symbolicate format '-ratio) (x)
                (dispatch-ratio (x signed-num den)
                  (let* ((plusp (plusp signed-num))
                         (num (if plusp signed-num (- signed-num)))
                         (digits ,(package-symbolicate :sb-vm format '-digits))
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
                                   ,(case format
                                      (single-float
                                       `(single-from-bits sign sb-vm:single-float-bias bits))
                                      (double-float
                                       `(double-from-bits sign sb-vm:double-float-bias bits))
                                      #+long-float
                                      (long-float
                                       `(long-from-bits sign sb-vm:long-float-bias bits))))))
                        (declare (inline floatit))
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
                           (incf scale))))))))))
  (def double-float)
  (def single-float))

;;; Specialized versions for floats.
(macrolet ((def (type name)
             `(defun ,name (number)
                (if (sb-xc:<= ,(symbol-value (symbolicate 'most-negative-fixnum- type))
                              number
                              ,(symbol-value (symbolicate 'most-positive-fixnum- type)))
                    (truly-the fixnum (,name number))
                    (error "bad float arg")))))
  (def single-float %unary-truncate/single-float)
  (def double-float %unary-truncate/double-float)
  #+long-float
  (def double-float %unary-truncate/long-float))

(defun %unary-ceiling (m)
  (values (ceiling m)))
(defun %unary-floor (m)
  (values (floor m)))

#-round-float
(defun %unary-fround (number)
  (number-dispatch ((number real))
    ((integer) (float number))
    ((ratio) (float (round (numerator number) (denominator number))))
    (((foreach single-float double-float)) (%unary-fround number))))

(defun rational (x)
  "RATIONAL produces a rational number for any real numeric argument. This is
  more efficient than RATIONALIZE, but it assumes that floating-point is
  completely accurate, giving a result that isn't as pretty."
  (declare (explicit-check))
  (number-dispatch ((x real))
    (((foreach single-float double-float #+long-float long-float))
     (multiple-value-bind (bits exp sign) (integer-decode-float x)
       (if (eql bits 0)
           0
           (let ((int (if (minusp sign) (- bits) bits)))
             (cond ((minusp exp)
                    ;; Instead of division (which also involves GCD)
                    ;; find the first set bit of the numerator and shift accordingly,
                    ;; as the denominator is a power of two.
                    (let* ((pexp (- exp))
                           (set (first-bit-set bits)))
                      (if (> pexp set)
                          (%make-ratio (ash int (- set))
                                       (let ((shift (- pexp set)))
                                         (if (< shift sb-vm:n-fixnum-bits)
                                             (ash 1 shift)
                                             (bignum-ashift-left-fixnum 1 shift))))
                          (ash int exp))))
                   (t
                    (ash int exp)))))))
    ((rational) x)))

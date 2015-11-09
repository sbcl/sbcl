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
  (declare (ignorable doc))
  `(defun ,name (x)
     #!+sb-doc ,doc
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
  ;; HPPA (and apparently MIPS) have trapping NaNs (SNaNs) with the
  ;; trapping-nan-bit SET.  PPC, SPARC, Alpha, and x86 (and presumably
  ;; x86-64, ARM, and ARM64) have trapping NaNs (SNaNs) with the
  ;; trapping-nan-bit CLEAR.  Note that the given implementation
  ;; considers infinities to be FLOAT-TRAPPING-NAN-P on most
  ;; architectures.
  #!-(or mips hppa)
  (zerop (logand (ldb sb!vm:single-float-significand-byte bits)
                 sb!vm:single-float-trapping-nan-bit))
  #!+(or mips hppa)
  (not (zerop (logand (ldb sb!vm:single-float-significand-byte bits)
                      sb!vm:single-float-trapping-nan-bit)))
  #!-(or mips hppa)
  (zerop (logand (ldb sb!vm:double-float-significand-byte hi)
                 sb!vm:double-float-trapping-nan-bit))
  #!+(or mips hppa)
  (not (zerop (logand (ldb sb!vm:double-float-significand-byte hi)
                      sb!vm:double-float-trapping-nan-bit)))
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
  (declare (explicit-check))
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
  (declare (float float1 float2) (explicit-check))
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
  (declare (explicit-check))
  (number-dispatch ((f float))
    ((single-float) sb!vm:single-float-digits)
    ((double-float) sb!vm:double-float-digits)
    #!+long-float
    ((long-float) sb!vm:long-float-digits)))

(defun float-radix (x)
  #!+sb-doc
  "Return (as an integer) the radix b of its floating-point argument."
  (declare (ignore x) (type float x))
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
  (declare (explicit-check))
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
  (declare (explicit-check))
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
         (single-float
          ;; SINGLE-FLOAT-POSITIVE-INFINITY
          (single-from-bits 0 (1+ sb!vm:single-float-normal-exponent-max) 0))
         (double-float
          ;; DOUBLE-FLOAT-POSITIVE-INFINITY
          (double-from-bits 0 (1+ sb!vm:double-float-normal-exponent-max) 0)))))))

;;; Scale a single or double float, calling the correct over/underflow
;;; functions.
(defun scale-single-float (x exp)
  (declare (single-float x) (integer exp))
  (etypecase exp
    (fixnum
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
    (unsigned-byte (scale-float-maybe-overflow x exp))
    ((integer * 0) (scale-float-maybe-underflow x exp))))
(defun scale-double-float (x exp)
  (declare (double-float x) (integer exp))
  (etypecase exp
    (fixnum
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
    (unsigned-byte (scale-float-maybe-overflow x exp))
    ((integer * 0) (scale-float-maybe-underflow x exp))))

#!+(and x86 long-float)
(defun scale-long-float (x exp)
  (declare (long-float x) (integer exp))
  (scale-float x exp))

;;; Dispatch to the correct type-specific scale-float function.
(defun scale-float (f ex)
  #!+sb-doc
  "Return the value (* f (expt (float 2 f) ex)), but with no unnecessary loss
  of precision or overflow."
  (declare (explicit-check))
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
  (declare (explicit-check))
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
;;; integer division. Accuracy is important to preserve print-read
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

;;; These might be useful if we ever have a machine without float/integer
;;; conversion hardware. For now, we'll use special ops that
;;; uninterruptibly frob the rounding modes & do ieee round-to-integer.
#+nil
(progn
  ;; The compiler compiles a call to this when we are doing %UNARY-TRUNCATE
  ;; and the result is known to be a fixnum. We can avoid some generic
  ;; arithmetic in this case.
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
  ;; Double-float version of this operation (see above single op).
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
                  res)))))))

;;; This function is called when we are doing a truncate without any funky
;;; divisor, i.e. converting a float or ratio to an integer. Note that we do
;;; *not* return the second value of truncate, so it must be computed by the
;;; caller if needed.
;;;
;;; In the float case, we pick off small arguments so that compiler
;;; can use special-case operations. We use an exclusive test, since
;;; (due to round-off error), (float most-positive-fixnum) is likely
;;; to be equal to (1+ most-positive-fixnum).  An exclusive test is
;;; good enough, because most-positive-fixnum will be one less than a
;;; power of two, and that power of two will be exactly representable
;;; as a float (at least until we get 128-bit fixnums).
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

;;; Specialized versions for floats.
(macrolet ((def (type name)
             `(defun ,name (number)
                (if (< ,(coerce sb!xc:most-negative-fixnum type)
                       number
                       ,(coerce sb!xc:most-positive-fixnum type))
                    (truly-the fixnum (,name number))
                    ;; General -- slow -- case.
                    (multiple-value-bind (bits exp) (integer-decode-float number)
                      (let ((res (ash bits exp)))
                        (if (minusp number)
                            (- res)
                            res)))))))
  (def single-float %unary-truncate/single-float)
  (def double-float %unary-truncate/double-float)
  #!+long-float
  (def double-float %unary-truncate/long-float))

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
                  (rounded (if (minusp exp)
                               (let ((fractional-bits (logand bits (lognot (ash -1 (- exp)))))
                                     (0.5bits (ash 1 (- -1 exp))))
                                 (cond
                                   ((> fractional-bits 0.5bits) (1+ shifted))
                                   ((< fractional-bits 0.5bits) shifted)
                                   (t (if (oddp shifted) (1+ shifted) shifted))))
                               shifted)))
             (if (minusp number)
                 (- rounded)
                 rounded)))))))

(defun %unary-ftruncate (number)
  (number-dispatch ((number real))
    ((integer) (float number))
    ((ratio) (float (truncate (numerator number) (denominator number))))
    (((foreach single-float double-float #!+long-float long-float))
     (%unary-ftruncate number))))

(defun rational (x)
  #!+sb-doc
  "RATIONAL produces a rational number for any real numeric argument. This is
  more efficient than RATIONALIZE, but it assumes that floating-point is
  completely accurate, giving a result that isn't as pretty."
  (declare (explicit-check))
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

;;; This algorithm for RATIONALIZE, due to Bruno Haible, is included
;;; with permission.
;;;
;;; Algorithm (recursively presented):
;;;   If x is a rational number, return x.
;;;   If x = 0.0, return 0.
;;;   If x < 0.0, return (- (rationalize (- x))).
;;;   If x > 0.0:
;;;     Call (integer-decode-float x). It returns a m,e,s=1 (mantissa,
;;;     exponent, sign).
;;;     If m = 0 or e >= 0: return x = m*2^e.
;;;     Search a rational number between a = (m-1/2)*2^e and b = (m+1/2)*2^e
;;;     with smallest possible numerator and denominator.
;;;     Note 1: If m is a power of 2, we ought to take a = (m-1/4)*2^e.
;;;       But in this case the result will be x itself anyway, regardless of
;;;       the choice of a. Therefore we can simply ignore this case.
;;;     Note 2: At first, we need to consider the closed interval [a,b].
;;;       but since a and b have the denominator 2^(|e|+1) whereas x itself
;;;       has a denominator <= 2^|e|, we can restrict the seach to the open
;;;       interval (a,b).
;;;     So, for given a and b (0 < a < b) we are searching a rational number
;;;     y with a <= y <= b.
;;;     Recursive algorithm fraction_between(a,b):
;;;       c := (ceiling a)
;;;       if c < b
;;;         then return c       ; because a <= c < b, c integer
;;;         else
;;;           ; a is not integer (otherwise we would have had c = a < b)
;;;           k := c-1          ; k = floor(a), k < a < b <= k+1
;;;           return y = k + 1/fraction_between(1/(b-k), 1/(a-k))
;;;                             ; note 1 <= 1/(b-k) < 1/(a-k)
;;;
;;; You can see that we are actually computing a continued fraction expansion.
;;;
;;; Algorithm (iterative):
;;;   If x is rational, return x.
;;;   Call (integer-decode-float x). It returns a m,e,s (mantissa,
;;;     exponent, sign).
;;;   If m = 0 or e >= 0, return m*2^e*s. (This includes the case x = 0.0.)
;;;   Create rational numbers a := (2*m-1)*2^(e-1) and b := (2*m+1)*2^(e-1)
;;;   (positive and already in lowest terms because the denominator is a
;;;   power of two and the numerator is odd).
;;;   Start a continued fraction expansion
;;;     p[-1] := 0, p[0] := 1, q[-1] := 1, q[0] := 0, i := 0.
;;;   Loop
;;;     c := (ceiling a)
;;;     if c >= b
;;;       then k := c-1, partial_quotient(k), (a,b) := (1/(b-k),1/(a-k)),
;;;            goto Loop
;;;   finally partial_quotient(c).
;;;   Here partial_quotient(c) denotes the iteration
;;;     i := i+1, p[i] := c*p[i-1]+p[i-2], q[i] := c*q[i-1]+q[i-2].
;;;   At the end, return s * (p[i]/q[i]).
;;;   This rational number is already in lowest terms because
;;;   p[i]*q[i-1]-p[i-1]*q[i] = (-1)^i.
;;;
;;; See also
;;;   Hardy, Wright: An introduction to number theory
;;; and/or
;;;   <http://modular.fas.harvard.edu/edu/Fall2001/124/lectures/lecture17/lecture17/>
;;;   <http://modular.fas.harvard.edu/edu/Fall2001/124/lectures/lecture17/lecture18/>

(defun rationalize (x)
  #!+sb-doc
  "Converts any REAL to a RATIONAL.  Floats are converted to a simple rational
  representation exploiting the assumption that floats are only accurate to
  their precision.  RATIONALIZE (and also RATIONAL) preserve the invariant:
      (= x (float (rationalize x) x))"
  (declare (explicit-check))
  (number-dispatch ((x real))
    (((foreach single-float double-float #!+long-float long-float))
     ;; This is a fairly straigtforward implementation of the
     ;; iterative algorithm above.
     (multiple-value-bind (frac expo sign)
         (integer-decode-float x)
       (cond ((or (zerop frac) (>= expo 0))
              (if (minusp sign)
                  (- (ash frac expo))
                  (ash frac expo)))
             (t
              ;; expo < 0 and (2*m-1) and (2*m+1) are coprime to 2^(1-e),
              ;; so build the fraction up immediately, without having to do
              ;; a gcd.
              (let ((a (build-ratio (- (* 2 frac) 1) (ash 1 (- 1 expo))))
                    (b (build-ratio (+ (* 2 frac) 1) (ash 1 (- 1 expo))))
                    (p0 0)
                    (q0 1)
                    (p1 1)
                    (q1 0))
                (do ((c (ceiling a) (ceiling a)))
                    ((< c b)
                     (let ((top (+ (* c p1) p0))
                           (bot (+ (* c q1) q0)))
                       (build-ratio (if (minusp sign)
                                        (- top)
                                        top)
                                    bot)))
                  (let* ((k (- c 1))
                         (p2 (+ (* k p1) p0))
                         (q2 (+ (* k q1) q0)))
                    (psetf a (/ (- b k))
                           b (/ (- a k)))
                    (setf p0 p1
                          q0 q1
                          p1 p2
                          q1 q2))))))))
    ((rational) x)))

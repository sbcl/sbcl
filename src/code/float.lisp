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

;;;; SCALE-FLOAT

(declaim (maybe-inline scale-single-float scale-double-float))

;;; Handle float scaling where the X is denormalized or the result is
;;; denormalized or underflows to 0.
(macrolet ((def (type)
             `(defun ,(symbolicate 'scale- type '-maybe-underflow) (x exp)
                (declare (inline ,(symbolicate 'integer-decode- type)))
                (cond ((float-infinity-p x)
                       x)
                      ((float-nan-p x)
                       (when (and (float-trapping-nan-p x)
                                  (sb-vm:current-float-trap :invalid))
                         (error 'floating-point-invalid-operation :operation 'scale-float
                                                                  :operands (list x exp)))
                       x)
                      (t
                       (multiple-value-bind (sig old-exp sign) (,(symbolicate 'integer-decode- type) x)
                         (let* ((digits (float-digits x))
                                (new-exp (+ exp old-exp digits
                                            ,(case type
                                               (single-float 'sb-vm:single-float-bias)
                                               (double-float 'sb-vm:double-float-bias))))
                                ;; convert decoded values {-1,+1} into {1,0} respectively
                                (sign (if (minusp sign) 1 0)))
                           (cond
                             ((< new-exp
                                 ,(case type
                                    (single-float 'sb-vm:single-float-normal-exponent-min)
                                    (double-float 'sb-vm:double-float-normal-exponent-min)))
                              (when (sb-vm:current-float-trap :inexact)
                                (error 'floating-point-inexact :operation 'scale-float
                                                               :operands (list x exp)))
                              (when (sb-vm:current-float-trap :underflow)
                                (error 'floating-point-underflow :operation 'scale-float
                                                                 :operands (list x exp)))
                              (let ((shift (1- new-exp)))
                                (if (< shift (- (1- digits)))
                                    (float-sign x ,(case type
                                                     (single-float $0f0)
                                                     (double-float $0d0)))
                                    ,(case type
                                       (single-float '(single-from-bits sign 0 (ash sig shift)))
                                       (double-float '(double-from-bits sign 0 (ash sig shift)))))))
                             (t
                              ,(case type
                                 (single-float '(single-from-bits sign new-exp sig))
                                 (double-float '(double-from-bits sign new-exp sig))))))))))))
  (def single-float)
  (def double-float))

;;; Called when scaling a float overflows, or the original float was a
;;; NaN or infinity. If overflow errors are trapped, then error,
;;; otherwise return the appropriate infinity. If a NaN, signal or not
;;; as appropriate.
(macrolet ((def (type)
             `(defun ,(symbolicate 'scale- type '-maybe-overflow) (x exp)
                (declare (inline float-infinity-p float-nan-p))
                (cond
                  ((float-infinity-p x)
                   ;; Infinity is infinity, no matter how small...
                   x)
                  ((float-nan-p x)
                   (when (and (float-trapping-nan-p x)
                              (sb-vm:current-float-trap :invalid))
                     (error 'floating-point-invalid-operation :operation 'scale-float
                                                              :operands (list x exp)))
                   x)
                  (t
                   (when (sb-vm:current-float-trap :overflow)
                     (error 'floating-point-overflow :operation 'scale-float
                                                     :operands (list x exp)))
                   (when (sb-vm:current-float-trap :inexact)
                     (error 'floating-point-inexact :operation 'scale-float
                                                    :operands (list x exp)))
                   (* (float-sign x)
                      ,(ecase type
                         (single-float
                          ;; SINGLE-FLOAT-POSITIVE-INFINITY
                          `(single-from-bits 0 (1+ sb-vm:single-float-normal-exponent-max) 0))
                         (double-float
                          ;; DOUBLE-FLOAT-POSITIVE-INFINITY
                          `(double-from-bits 0 (1+ sb-vm:double-float-normal-exponent-max) 0)))))))))
  (def single-float)
  (def double-float))

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
                (let* ((signed-num (numerator x))
                       (plusp (plusp signed-num))
                       (num (if plusp signed-num (- signed-num)))
                       (den (denominator x))
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
                         (incf scale)))))))))
  (def double-float)
  (def single-float))

;;; This function is called when we are doing a truncate without any funky
;;; divisor, i.e. converting a float or ratio to an integer. Note that we do
;;; *not* return the second value of truncate, so it must be computed by the
;;; caller if needed.
;;;
;;; In the float case, we pick off small arguments so that compiler
;;; can use special-case operations.
(defun %unary-truncate (number)
  (declare (explicit-check number))
  (macrolet ((fits-fixnum (type)
               `(<= ,(symbol-value (symbolicate 'most-negative-fixnum- type))
                    number
                    ,(symbol-value (symbolicate 'most-positive-fixnum- type))))
             (shift (type integer count)
               `(,(case type
                    #-64-bit
                    (double-float 'ash)
                    (t 'bignum-ashift-left-fixnum))
                 ,integer ,count)))
    (number-dispatch ((number real))
      ((integer) number)
      ((ratio) (values (truncate (numerator number) (denominator number))))
      (((foreach single-float double-float #+long-float long-float))
       (if (fits-fixnum (dispatch-type number))
           (truly-the fixnum (%unary-truncate number))
           (multiple-value-bind (bits exp sign) (integer-decode-float number)
             (shift (dispatch-type number)
                    (if (minusp sign)
                        (- bits)
                        bits)
                    exp)))))))

;;; Produce both values, unlike %unary-truncate
(defun unary-truncate (number)
  (declare (explicit-check number))
  (macrolet ((fits-fixnum (type)
               `(<= ,(symbol-value (symbolicate 'most-negative-fixnum- type))
                    number
                    ,(symbol-value (symbolicate 'most-positive-fixnum- type))))
             (shift (type)
               (case type
                 #-64-bit
                 (double-float
                  ;; Shifting negatives right is different
                  `(let ((truncated (ash bits exp)))
                     (if (minusp sign)
                         (- truncated)
                         truncated)))
                 (t
                  `(bignum-ashift-left-fixnum (if (minusp sign)
                                                  (- bits)
                                                  bits)
                                              exp)))))
    (number-dispatch ((number real))
      ((integer) (values number 0))
      ((ratio)
       (let ((truncated (truncate (numerator number) (denominator number))))
         (values truncated
                 (- number truncated))))
      (((foreach single-float double-float #+long-float long-float))
       (if (fits-fixnum (dispatch-type number))
           (let* ((truncated (truly-the fixnum (%unary-truncate number))))
             (values truncated
                     (- number
                        (coerce truncated '(dispatch-type number)))))
           (multiple-value-bind (bits exp sign) (integer-decode-float number)
             (let ((truncated (shift (dispatch-type number))))
               (values
                truncated
                #+64-bit
                (coerce 0 '(dispatch-type number))
                #-64-bit
                (if (eq '(dispatch-type number) 'single-float)
                    (coerce 0 '(dispatch-type number))
                    (- number (coerce truncated '(dispatch-type number))))))))))))

(macrolet ((def (type)
             (let ((decode (symbolicate 'integer-decode- type)))
               `(defun ,(symbolicate 'unary-truncate- type '-to-bignum) (number)
                  (declare (inline ,decode))
                  (multiple-value-bind (bits exp sign) (,decode number)
                    (let ((truncated ,(case type
                                        #-64-bit
                                        (double-float
                                         ;; Shifting negatives right is different
                                         `(let ((truncated (ash bits exp)))
                                            (if (minusp sign)
                                                (- truncated)
                                                truncated)))
                                        (t
                                         `(bignum-ashift-left-fixnum (if (minusp sign)
                                                                         (- bits)
                                                                         bits)
                                                                     exp)))))
                      (values
                       truncated
                       ,(case type
                          ((single-float #+64-bit double-float)
                           `(coerce 0 ',type))
                          (t
                           `(- number (coerce truncated ',type)))))))))))
  (def double-float)
  (def single-float))

(macrolet ((def (type)
             (let ((decode (symbolicate 'integer-decode- type)))
              `(defun ,(symbolicate '%unary-truncate- type '-to-bignum) (number)
                 (declare (inline ,decode))
                 (multiple-value-bind (bits exp sign) (,decode number)
                   ,(case type
                      #-64-bit
                      (double-float
                       ;; Shifting negatives right is different
                       `(let ((truncated (ash bits exp)))
                          (if (minusp sign)
                              (- truncated)
                              truncated)))
                      (t
                       `(bignum-ashift-left-fixnum (if (minusp sign)
                                                       (- bits)
                                                       bits)
                                                   exp))))))))
  (def double-float)
  (def single-float))

;;; Needs to be synchronized with sxhash-bignum
(macrolet ((def (type)
             (let ((decode (symbolicate 'integer-decode- type)))
               `(defun ,(symbolicate 'sxhash-bignum- type) (number)
                  (declare (inline ,decode))
                  (let ((result 316495330)
                        (digit-size sb-bignum::digit-size))
                    (declare (type fixnum result))
                    (multiple-value-bind (bits exp sign) (,decode number)
                      (let ((bits (if (minusp sign)
                                      (- bits)
                                      bits)))
                        (multiple-value-bind (digits remaining) (truncate exp digit-size)
                          (dotimes (i digits)
                            do (mixf result 0))
                          ;; Taken from bignum-ashift-left-fixnum.
                          (let* ((right-half (ldb (byte digit-size 0)
                                                  (ash bits remaining)))
                                 (sign-bit-p
                                   (logbitp (1- digit-size) right-half))
                                 (left-half (ash bits
                                                 (- remaining digit-size)))
                                 (left-half-p (if sign-bit-p
                                                  (/= left-half -1)
                                                  (/= left-half 0))))
                            (mixf result
                                  (logand most-positive-fixnum
                                          (logxor right-half
                                                  (ash right-half -7))))
                            (when left-half-p
                              (let ((left-half (ldb (byte digit-size 0) left-half)))
                                (mixf result
                                      (logand most-positive-fixnum
                                              (logxor left-half
                                                      (ash left-half -7))))))))))
                    result)))))
  (def double-float)
  (def single-float))

;;; Specialized versions for floats.
(macrolet ((def (type name)
             `(defun ,name (number)
                (if (<= ,(symbol-value (symbolicate 'most-negative-fixnum- type))
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

;;; Similar to %UNARY-TRUNCATE, but rounds to the nearest integer. If we
;;; can't use the round primitive, then we do our own round-to-nearest on the
;;; result of i-d-f. [Note that this rounding will really only happen
;;; with double floats on 32-bit architectures, where there are
;;; fractional floats past most-x-fixnum]
(defun %unary-round (number)
  (declare (explicit-check))
  (macrolet ((fits-fixnum (type)
               `(<= ,(symbol-value (symbolicate 'most-negative-fixnum- type))
                    number
                    ,(symbol-value (symbolicate 'most-positive-fixnum- type)))))
    (number-dispatch ((number real))
      ((integer) number)
      ((ratio) (values (round (numerator number) (denominator number))))
      (((foreach single-float double-float #+long-float long-float))
       (if (fits-fixnum (dispatch-type number))
           (truly-the fixnum (%unary-round number))
           #+64-bit
           (multiple-value-bind (bits exp sign) (integer-decode-float number)
             (bignum-ashift-left-fixnum
              (if (minusp sign)
                  (- bits)
                  bits)
              exp))
           #-64-bit
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
                   rounded))))))))

#-round-float
(macrolet ((def (name op)
             `(defun ,name (number)
                (number-dispatch ((number real))
                                 ((integer) (float number))
                                 ((ratio) (float (,op (numerator number) (denominator number))))
                                 (((foreach single-float double-float #+long-float long-float))
                                  (,name number))))))
  (def %unary-ftruncate truncate)
  (def %unary-fround round))

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
  "Converts any REAL to a RATIONAL.  Floats are converted to a simple rational
  representation exploiting the assumption that floats are only accurate to
  their precision.  RATIONALIZE (and also RATIONAL) preserve the invariant:
      (= x (float (rationalize x) x))"
  (declare (explicit-check))
  (number-dispatch ((x real))
    (((foreach single-float double-float #+long-float long-float))
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

;;; Unlike most interpreter stubs the definitions of which can be deferred
;;; until warm build, these two are essential to sanity-checking
;;; the floating-point operation cache at the very start of warm build.
(defun make-single-float (x) (make-single-float x))
(defun make-double-float (hi lo) (make-double-float hi lo))

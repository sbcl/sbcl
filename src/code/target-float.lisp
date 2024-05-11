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
                                                     (single-float 0f0)
                                                     (double-float 0d0)))
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

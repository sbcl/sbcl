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

(in-package "SB-KERNEL")

;;;; miscellaneous constants, utility functions, and macros

(defconstant pi #.pi)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun handle-reals (function var)
    `((((foreach fixnum single-float bignum ratio))
       (coerce (,function (coerce ,var 'double-float)) 'single-float))
      ((double-float)
       (,function ,var))))

  (defun handle-complex (form)
    `((((foreach (complex double-float) (complex single-float) (complex rational)))
       ,form))))

;;; Make these INLINE, since the call to C is at least as compact as a
;;; Lisp call, and saves number consing to boot.
(defmacro def-math-rtn (name num-args &optional wrapper)
  (let ((function (symbolicate "%" (string-upcase name)))
        (args (loop for i below num-args
                    collect (intern (format nil "ARG~D" i)))))
    `(progn
       (declaim (inline ,function))
       (defun ,function ,args
         (declare (sb-c:flushable sb-c:%alien-funcall))
         (truly-the ;; avoid checking the result
          ,(type-specifier (fun-type-returns (info :function :type function)))
          (alien-funcall
           (extern-alien ,(format nil "~:[~;sb_~]~a" wrapper name)
                         (function double-float
                                   ,@(loop repeat num-args
                                           collect 'double-float)))
           ,@args))))))


#+x86 ;; for constant folding
(macrolet ((def (name ll)
             `(defun ,name ,ll (,name ,@ll))))
  (def %atan2 (x y))
  (def %atan (x))
  (def %tan (x))
  (def %tan-quick (x))
  (def %cos (x))
  (def %cos-quick (x))
  (def %sin (x))
  (def %sin-quick (x))
  (def %sqrt (x))
  (def %log (x))
  (def %log1p (x))
  (def %log2 (x))
  (def %exp (x)))

#+(or x86-64 arm-vfp arm64 riscv) ;; for constant folding
(macrolet ((def (name ll)
             `(defun ,name ,ll (,name ,@ll))))
  (def %sqrt (x)))

;;;; stubs for the Unix math library
;;;;
;;;; Many of these are unnecessary on the X86 because they're built
;;;; into the FPU.

;;; trigonometric
#-x86 (def-math-rtn "sin" 1)
#-x86 (def-math-rtn "cos" 1)
#-x86 (def-math-rtn "tan" 1)
#-x86 (def-math-rtn "atan" 1)
#-x86 (def-math-rtn "atan2" 2)

;;; See src/runtime/wrap.c for the definitions of the "sb_"-prefixed things.
(def-math-rtn "acos" 1 #+win32 t)
(def-math-rtn "asin" 1 #+win32 t)
(def-math-rtn "cosh" 1 #+win32 t)
(def-math-rtn "sinh" 1 #+win32 t)
(def-math-rtn "tanh" 1 #+win32 t)
(def-math-rtn "asinh" 1 #+win32 t)
(def-math-rtn "acosh" 1 #+win32 t)
(def-math-rtn "atanh" 1 #+win32 t)

;;; exponential and logarithmic
(def-math-rtn "hypot" 2 #+win32 t)
#-x86 (def-math-rtn "exp" 1)
#-x86 (def-math-rtn "log" 1)
#-x86 (def-math-rtn "log10" 1)
(def-math-rtn "pow" 2)
#-(or x86 x86-64 arm-vfp arm64 riscv) (def-math-rtn "sqrt" 1)
#-x86 (def-math-rtn "log1p" 1)
#-x86 (def-math-rtn "log2" 1)


;;;; power functions

(defun exp (number)
  "Return e raised to the power NUMBER."
  (declare (explicit-check))
  (number-dispatch ((number number))
    (handle-reals %exp number)
    (handle-complex
     (* (exp (realpart number))
        (cis (imagpart number))))))

;;; INTEXP -- Handle the rational base, integer power case.
;;; This function precisely calculates base raised to an integral
;;; power. It separates the cases by the sign of power, for efficiency
;;; reasons, as powers can be calculated more efficiently if power is
;;; a positive integer. Values of power are calculated as positive
;;; integers, and inverted if negative.
(defun intexp (base power)
  (declare (explicit-check))
  (cond ((eql base 1)
         base)
        ((eql base -1)
         (if (evenp power)
             1
             base))
        ((eql base 0)
         (cond ((= power 0) 1)
               ((> power 0) 0)
               (t (error 'division-by-zero
                         :operands (list 0 power)
                         :operation 'expt))))
        ((ratiop base)
         (let ((den (denominator base))
               (num (numerator base)))
           (if (minusp power)
               (let ((negated (- power)))
                 (cond ((eql num 1)
                        (intexp den negated))
                       ((eql num -1)
                        (intexp (- den) negated))
                       (t
                        (build-ratio (intexp den negated)
                                     (intexp num negated)))))
               (build-ratio (intexp num power)
                            (intexp den power)))))
        ((minusp power)
         (/ (intexp base (- power))))
        ((eql base 2)
         (ash 1 power))
        (t
         (do* ((base base)
               (power power)
               (nextn (ash power -1) (ash power -1))
               (total (if (oddp power) base 1)
                      (if (oddp power) (* base total) total)))
              ((zerop nextn) total)
           (setq base (* base base))
           (setq power nextn)))))

;;; If an integer power of a rational, use INTEXP above. Otherwise, do
;;; floating point stuff. If both args are real, we try %POW right
;;; off, assuming it will return 0 if the result may be complex. If
;;; so, we call COMPLEX-POW which directly computes the complex
;;; result. We also separate the complex-real and real-complex cases
;;; from the general complex case.
(defun expt (base power)
  "Return BASE raised to the POWER."
  (declare (explicit-check))
  (if (zerop power)
    (if (and (zerop base) (floatp power))
        (error 'arguments-out-of-domain-error
               :operands (list base power)
               :operation 'expt
               :references '((:ansi-cl :function expt)))
        (let ((result (1+ (* base power))))
          (if (and (floatp result) (float-nan-p result))
              (float 1 result)
              result)))
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
                 (cond ((>= ihi #x43400000)     ; exponent >= 53
                        (setq isint 2))
                       ((>= ihi #x3ff00000)
                        (let ((k (- (ash ihi -20) #x3ff)))      ; exponent
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
                 (let* ((x-hi (double-float-high-bits x))
                        (x-lo (double-float-low-bits x))
                        (x-ihi (logand x-hi #x7fffffff))
                        (y-hi (double-float-high-bits y))
                        (y-lo (double-float-low-bits y))
                        (y-ihi (logand y-hi #x7fffffff)))
                   (declare (type (signed-byte 32) x-hi y-hi)
                            (type (unsigned-byte 31) x-ihi y-ihi)
                            (type (unsigned-byte 32) x-lo y-lo))
                   ;; y==zero: x**0 = 1
                   (when (zerop (logior y-ihi y-lo))
                     (return-from real-expt (coerce 1d0 rtype)))
                   ;; +-NaN return x+y
                   ;; FIXME: Hardcoded qNaN/sNaN values are not portable.
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
                                      (/ 1 abs-x)       ; z = (1/|x|)
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
                           (coerce (%pow x y) rtype)
                           ;; x<0
                           (let ((pow (%pow abs-x y)))
                             (declare (double-float pow))
                             (case yisint
                               (1 ; odd
                                (coerce (* -1d0 pow) rtype))
                               (2 ; even
                                (coerce pow rtype))
                               (t ; non-integer
                                (let ((y*pi (* y pi)))
                                  (declare (double-float y*pi))
                                  (complex
                                   (coerce (* pow (%cos y*pi))
                                           rtype)
                                   (coerce (* pow (%sin y*pi))
                                           rtype))))))))))))
             (complex-expt (base power)
               (if (and (zerop base) (plusp (realpart power)))
                   (* base power)
                   (exp (* power (log base))))))
      (declare (inline real-expt complex-expt))
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
        ;; Handle (expt <complex> <rational>), except the case dealt with
        ;; in the first clause above, (expt <(complex rational)> <integer>).
        (((foreach (complex rational))
          ratio)
         (* (expt (abs base) power)
            (cis (* power (phase base)))))
        (((foreach (complex single-float) (complex double-float))
          (foreach fixnum (or bignum ratio)))
         (* (expt (abs base) power)
            (cis (* power (phase base)))))
        ;; The next three clauses handle (expt <real> <complex>).
        (((foreach fixnum (or bignum ratio) single-float)
          (foreach (complex single-float) (complex rational)))
         (complex-expt base power))
        (((foreach fixnum (or bignum ratio) single-float)
          (complex double-float))
         (complex-expt (coerce base 'double-float) power))
        ((double-float complex)
         (complex-expt base power))
        ;; The next three clauses handle (expt <complex> <float>) and
        ;; (expt <complex> <complex>).
        (((foreach (complex single-float) (complex rational))
          (foreach (complex single-float) (complex rational) single-float))
         (complex-expt base power))
        (((foreach (complex single-float) (complex rational))
          (foreach (complex double-float) double-float))
         (complex-expt (coerce base '(complex double-float)) power))
        (((complex double-float)
          (foreach complex double-float single-float))
         (complex-expt base power))))))

(declaim (start-block log))
(defun log2/nonnegative-integer (x)
  (declare (type (integer 0) x))
  ;; CMUCL comment:
  ;;
  ;;   Write x = 2^n*f where 1/2 <= f < 1.  Then log2(x) = n +
  ;;   log2(f).  So we grab the top few bits of x and scale that
  ;;   appropriately, take the log of it and add it to n.
  ;;
  ;; Motivated by an attempt to get LOG to work better on bignums.
  (cond ((typep x 'sb-vm:signed-word)
         (%log2 (coerce x 'double-float)))
        (t
         (let ((n (integer-length x)))
           (cond ((< n sb-vm:double-float-digits)
                  (%log2 (coerce x 'double-float)))
                 (t
                  (let ((f (ldb (byte sb-vm:double-float-digits
                                      (- n sb-vm:double-float-digits))
                                x)))
                    (+ n (%log2 (scale-float (coerce f 'double-float)
                                             (- sb-vm:double-float-digits)))))))))))

(defun log2/double-float (x)
  (declare (type double-float x))
  (if (float-sign-bit-set-p x)
      (complex (%log2 (- x)) (sb-xc:/ pi (log 2d0)))
      (%log2 x)))

(defun log2/nonnegative-ratio (x)
  (declare (type (and (rational 0) (not integer)) x))
  (truly-the double-float
             (- (truly-the double-float (log2/nonnegative-integer (numerator x)))
                (truly-the double-float (log2/nonnegative-integer (denominator x))))))

(defun log2/nonnegative-rational (x)
  (declare (type (rational 0) x))
  (if (typep x 'integer)
      (log2/nonnegative-integer x)
      (log2/nonnegative-ratio x)))

(defun log2/rational (x)
  (declare (type rational x))
  (if (minusp x)
      (complex (log2/nonnegative-rational (- x)) (sb-xc:/ pi (log 2d0)))
      (log2/nonnegative-rational x)))

(defun log (number &optional (base nil base-p))
  "Return the logarithm of NUMBER in the base BASE, which defaults to e."
  (declare (explicit-check))
  (if base-p
      (cond
        ((zerop base)
         (if (or (typep number '(or double-float (complex double-float)))
                 (typep base '(or double-float (complex double-float))))
             0.0d0
             0.0f0))
        ((eql number 0)
         (error 'division-by-zero :operands (list number base) :operation 'log))
        ((and (typep number '(rational 0))
              (typep base '(rational 0)))
         (coerce (/ (truly-the double-float (log2/nonnegative-rational number))
                    (truly-the double-float (log2/nonnegative-rational base)))
                 'single-float))
        (t
         (number-dispatch ((number number) (base number))
           ;; (log <real> <real>)
           ((rational rational)
            ;; we excluded real results from rational arguments above
            (coerce (/ (log2/rational number) (log2/rational base))
                    '(complex single-float)))
           ((rational (foreach single-float double-float))
            (let ((result (/ (log2/rational number) (log2/double-float (coerce base 'double-float)))))
              (if (floatp result)
                  (coerce result '(dispatch-type base))
                  (coerce result '(complex (dispatch-type base))))))
           (((foreach single-float double-float) rational)
            (let ((result (/ (log2/double-float (coerce number 'double-float)) (log2/rational base))))
              (if (floatp result)
                  (coerce result '(dispatch-type number))
                  (coerce result '(complex (dispatch-type number))))))
           ((single-float single-float)
            (/ (log number) (log base)))
           ((single-float double-float)
            (/ (log (coerce number 'double-float)) (log base)))
           ((double-float single-float)
            (/ (log number) (log (coerce base 'double-float))))
           ((double-float double-float)
            (/ (log number) (log base)))
           ;; complex single-float result
           (((foreach rational single-float) (foreach (complex rational) (complex single-float)))
            (/ (log number) (log base)))
           (((foreach (complex rational) (complex single-float))
             (foreach rational single-float (complex rational) (complex single-float)))
            (/ (log number) (log base)))
           ;; complex double-float result, from contagion
           (((foreach double-float (complex double-float))
             (foreach (complex rational) (complex single-float)))
            (/ (log number) (log (coerce base '(complex double-float)))))
           (((foreach (complex rational) (complex single-float))
             (foreach double-float (complex double-float)))
            (/ (log (coerce number '(complex double-float))) (log base)))
           (((foreach rational single-float) (complex double-float))
            (/ (log (coerce number 'double-float)) (log base)))
           (((complex double-float) (foreach rational single-float))
            (/ (log number) (log (coerce base 'double-float))))
           ;; complex double-float result, no contagion
           ((double-float (complex double-float))
            (/ (log number) (log base)))
           (((complex double-float) (foreach double-float (complex double-float)))
            (/ (log number) (log base))))))
      (let ((log2e 1.4426950408889634d0))
        (when (eql number 0)
          (error 'division-by-zero :operands (list number) :operation 'log))
        (number-dispatch ((number number))
          (((foreach fixnum bignum))
           (if (minusp number)
               (complex (log (- number)) (coerce pi 'single-float))
               (coerce (/ (truly-the double-float (log2/nonnegative-integer number))
                          log2e)
                       'single-float)))
          ((ratio)
           (if (minusp number)
               (complex (log (- number)) (coerce pi 'single-float))
               (let ((numerator (numerator number))
                     (denominator (denominator number)))
                 (if (and (<= -1 (- (integer-length numerator) (integer-length denominator)) 1)
                          (<= 3/4 number 5/4))
                     (coerce (%log1p (coerce (- number 1) 'double-float))
                             'single-float)
                     (coerce (/ (- (truly-the double-float (log2/nonnegative-integer numerator))
                                   (truly-the double-float (log2/nonnegative-integer denominator)))
                                log2e)
                             'single-float)))))
          (((foreach single-float double-float))
           ;; Is (log -0) -infinity (libm.a) or -infinity + i*pi (Kahan)?
           ;; Since this doesn't seem to be an implementation issue
           ;; I (pw) take the Kahan result.
           (if (float-sign-bit-set-p number)
               (complex (log (- number)) (coerce pi '(dispatch-type number)))
               (coerce (%log (coerce number 'double-float))
                       '(dispatch-type number))))
          ((complex)
           (complex-log number))))))
(declaim (end-block))

(defun sqrt (number)
  "Return the square root of NUMBER."
  (declare (explicit-check))
  (number-dispatch ((number number))
    (((foreach fixnum bignum ratio))
     (if (minusp number)
         (complex 0f0
                  (coerce (%sqrt (- (coerce number 'double-float))) 'single-float))
         (coerce (%sqrt (coerce number 'double-float)) 'single-float)))
    (((foreach single-float double-float))
     (if (minusp number)
         (complex (coerce 0.0 '(dispatch-type number))
                  (coerce (%sqrt (- (coerce number 'double-float)))
                          '(dispatch-type number)))
         (coerce (%sqrt (coerce number 'double-float))
                 '(dispatch-type number))))
    ((complex)
     (complex-sqrt number))))

;;;; trigonometic and related functions

(defun abs (number)
  "Return the absolute value of the number."
  (declare (explicit-check))
  (number-dispatch ((number number))
    (((foreach single-float double-float fixnum rational))
     (abs number))
    (handle-complex
     (let ((rx (realpart number))
           (ix (imagpart number)))
       (etypecase rx
         (rational
          (sqrt (+ (* rx rx) (* ix ix))))
         (single-float
          (coerce (%hypot (coerce rx 'double-float)
                          (coerce (truly-the single-float ix) 'double-float))
                  'single-float))
         (double-float
          (%hypot rx (truly-the double-float ix))))))))

(defun phase (number)
  "Return the angle part of the polar representation of a complex number.
  For complex numbers, this is (atan (imagpart number) (realpart number)).
  For non-complex positive numbers, this is 0. For non-complex negative
  numbers this is PI."
  (declare (explicit-check))
  (number-dispatch ((number number))
    ((rational)
     (if (minusp number)
         (coerce pi 'single-float)
         0.0f0))
    ((single-float)
     (if (minusp (float-sign number))
         (coerce pi 'single-float)
         0.0f0))
    ((double-float)
     (if (minusp (float-sign number))
         (coerce pi 'double-float)
         0.0d0))
    (handle-complex
     (atan (imagpart number) (realpart number)))))

(defun sin (number)
  "Return the sine of NUMBER."
  (declare (explicit-check))
  (number-dispatch ((number number))
    (handle-reals %sin number)
    (handle-complex
     (let ((x (realpart number))
           (y (imagpart number)))
       (complex (* (sin x) (cosh y))
                (* (cos x) (sinh y)))))))

(defun cos (number)
  "Return the cosine of NUMBER."
  (declare (explicit-check))
  (number-dispatch ((number number))
    (handle-reals %cos number)
    (handle-complex
     (let ((x (realpart number))
           (y (imagpart number)))
       (complex (* (cos x) (cosh y))
                (- (* (sin x) (sinh y))))))))

(defun tan (number)
  "Return the tangent of NUMBER."
  (declare (explicit-check))
  (number-dispatch ((number number))
    (handle-reals %tan number)
    (handle-complex
     ;; tan z = -i * tanh(i*z)
     (let* ((result (complex-tanh (complex (- (imagpart number))
                                           (realpart number)))))
       (complex (imagpart result)
                (- (realpart result)))))))

(defun cis (theta)
  "Return cos(Theta) + i sin(Theta), i.e. exp(i Theta)."
  (declare (explicit-check ))
  (number-dispatch ((theta real))
    (((foreach single-float double-float rational))
     (complex (cos theta) (sin theta)))))

(defun asin (number)
  "Return the arc sine of NUMBER."
  (declare (explicit-check))
  (number-dispatch ((number number))
    ((rational)
     (if (or (> number 1) (< number -1))
         (complex-asin number)
         (coerce (%asin (coerce number 'double-float)) 'single-float)))
    (((foreach single-float double-float))
     (if (or (> number (coerce 1 '(dispatch-type number)))
             (< number (coerce -1 '(dispatch-type number))))
         (complex-asin (complex number))
         (coerce (%asin (coerce number 'double-float))
                 '(dispatch-type number))))
    ((complex)
     (complex-asin number))))

(defun acos (number)
  "Return the arc cosine of NUMBER."
  (declare (explicit-check))
  (number-dispatch ((number number))
    ((rational)
     (if (or (> number 1) (< number -1))
         (complex-acos number)
         (coerce (%acos (coerce number 'double-float)) 'single-float)))
    (((foreach single-float double-float))
     (if (or (> number (coerce 1 '(dispatch-type number)))
             (< number (coerce -1 '(dispatch-type number))))
         (complex-acos (complex number))
         (coerce (%acos (coerce number 'double-float))
                 '(dispatch-type number))))
    ((complex)
     (complex-acos number))))

(defun atan (y &optional (x nil xp))
  "Return the arc tangent of Y if X is omitted or Y/X if X is supplied."
  (declare (explicit-check))
  (if xp
      (flet ((atan2 (y x)
               (declare (type double-float y x)
                        (values double-float))
               (if (zerop x)
                   (if (zerop y)
                       (if (not (float-sign-bit-set-p x))
                           y
                           (float-sign y pi))
                       (float-sign y (sb-xc:/ pi 2)))
                   (%atan2 y x))))
        (number-dispatch ((y real) (x real))
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

;;; It seems that every target system has a C version of sinh, cosh,
;;; and tanh. Let's use these for reals because the original
;;; implementations based on the definitions lose big in round-off
;;; error. These bad definitions also mean that sin and cos for
;;; complex numbers can also lose big.

(defun sinh (number)
  "Return the hyperbolic sine of NUMBER."
  (declare (explicit-check))
  (number-dispatch ((number number))
    (handle-reals %sinh number)
    (handle-complex
     (let ((x (realpart number))
           (y (imagpart number)))
       (complex (* (sinh x) (cos y))
                (* (cosh x) (sin y)))))))

(defun cosh (number)
  "Return the hyperbolic cosine of NUMBER."
  (declare (explicit-check))
  (number-dispatch ((number number))
    (handle-reals %cosh number)
    (handle-complex
     (let ((x (realpart number))
           (y (imagpart number)))
       (complex (* (cosh x) (cos y))
                (* (sinh x) (sin y)))))))

(defun tanh (number)
  "Return the hyperbolic tangent of NUMBER."
  (declare (explicit-check))
  (number-dispatch ((number number))
    (handle-reals %tanh number)
    ((complex)
     (complex-tanh number))))

(defun asinh (number)
  "Return the hyperbolic arc sine of NUMBER."
  (declare (explicit-check))
  (number-dispatch ((number number))
    (handle-reals %asinh number)
    ((complex)
     (complex-asinh number))))

(defun acosh (number)
  "Return the hyperbolic arc cosine of NUMBER."
  (declare (explicit-check))
  (number-dispatch ((number number))
    ((rational)
     ;; acosh is complex if number < 1
     (if (< number 1)
         (complex-acosh number)
         (coerce (%acosh (coerce number 'double-float)) 'single-float)))
    (((foreach single-float double-float))
     (if (< number (coerce 1 '(dispatch-type number)))
         (complex-acosh (complex number))
         (coerce (%acosh (coerce number 'double-float))
                 '(dispatch-type number))))
    ((complex)
     (complex-acosh number))))

(defun atanh (number)
  "Return the hyperbolic arc tangent of NUMBER."
  (declare (explicit-check))
  (number-dispatch ((number number))
    ((rational)
     ;; atanh is complex if |number| > 1
     (if (or (> number 1) (< number -1))
         (complex-atanh number)
         (coerce (%atanh (coerce number 'double-float)) 'single-float)))
    (((foreach single-float double-float))
     (if (or (> number (coerce 1 '(dispatch-type number)))
             (< number (coerce -1 '(dispatch-type number))))
         (complex-atanh (complex number))
         (coerce (%atanh (coerce number 'double-float))
                 '(dispatch-type number))))
    ((complex)
     (complex-atanh number))))


;;;; not-OLD-SPECFUN stuff
;;;;
;;;; (This was conditional on #-OLD-SPECFUN in the CMU CL sources,
;;;; but OLD-SPECFUN was mentioned nowhere else, so it seems to be
;;;; the standard special function system.)
;;;;
;;;; This is a set of routines that implement many elementary
;;;; transcendental functions as specified by ANSI Common Lisp.  The
;;;; implementation is based on Kahan's paper.
;;;;
;;;; I believe I have accurately implemented the routines and are
;;;; correct, but you may want to check for your self.
;;;;
;;;; These functions are written for CMU Lisp and take advantage of
;;;; some of the features available there.  It may be possible,
;;;; however, to port this to other Lisps.
;;;;
;;;; Some functions are significantly more accurate than the original
;;;; definitions in CMU Lisp.  In fact, some functions in CMU Lisp
;;;; give the wrong answer like (acos #c(-2.0 0.0)), where the true
;;;; answer is pi + i*log(2-sqrt(3)).
;;;;
;;;; All of the implemented functions will take any number for an
;;;; input, but the result will always be a either a complex
;;;; single-float or a complex double-float.
;;;;
;;;; general functions:
;;;;   complex-sqrt
;;;;   complex-log
;;;;   complex-atanh
;;;;   complex-tanh
;;;;   complex-acos
;;;;   complex-acosh
;;;;   complex-asin
;;;;   complex-asinh
;;;;   complex-atan
;;;;
;;;; utility functions:
;;;;   logb
;;;;
;;;; internal functions:
;;;;    square coerce-to-complex-type cssqs
;;;;
;;;; references:
;;;;   Kahan, W. "Branch Cuts for Complex Elementary Functions, or Much
;;;;   Ado About Nothing's Sign Bit" in Iserles and Powell (eds.) "The
;;;;   State of the Art in Numerical Analysis", pp. 165-211, Clarendon
;;;;   Press, 1987
;;;;
;;;; The original CMU CL code requested:
;;;;   Please send any bug reports, comments, or improvements to
;;;;   Raymond Toy at <email address deleted during 2002 spam avalanche>.

;;; FIXME: As of 2004-05, when PFD noted that IMAGPART and COMPLEX
;;; differ in their interpretations of the real line, IMAGPART was
;;; patch, which without a certain amount of effort would have altered
;;; all the branch cut treatment.  Clients of these COMPLEX- routines
;;; were patched to use explicit COMPLEX, rather than implicitly
;;; passing in real numbers for treatment with IMAGPART, and these
;;; COMPLEX- functions altered to require arguments of type COMPLEX;
;;; however, someone needs to go back to Kahan for the definitive
;;; answer for treatment of negative real floating point numbers and
;;; branch cuts.  If adjustment is needed, it is probably the removal
;;; of explicit calls to COMPLEX in the clients of irrational
;;; functions.  -- a slightly bitter CSR, 2004-05-16

(declaim (inline square))
(defun square (x)
  (declare (double-float x))
  (* x x))

;;; original CMU CL comment, apparently re. LOGB and
;;; perhaps CSSQS:
;;;   If you have these functions in libm, perhaps they should be used
;;;   instead of these Lisp versions. These versions are probably good
;;;   enough, especially since they are portable.

;;; This is like LOGB, but X is not infinity and non-zero and not a
;;; NaN, so we can always return an integer.
(declaim (inline logb-finite))
(defun logb-finite (x)
  (declare (type double-float x))
  (multiple-value-bind (signif exponent sign)
      (decode-float x)
    (declare (ignore signif sign))
    ;; DECODE-FLOAT is almost right, except that the exponent is off
    ;; by one.
    (1- exponent)))

;;; Compute an integer N such that 1 <= |2^N * x| < 2.
;;; For the special cases, the following values are used:
;;;    x             logb
;;;   NaN            NaN
;;;   +/- infinity   +infinity
;;;   0              -infinity
(defun logb (x)
  (declare (type double-float x))
  (cond ((float-nan-p x)
         x)
        ((float-infinity-p x) double-float-positive-infinity)
        ((zerop x)
         ;; The answer is negative infinity, but we are supposed to
          ;; signal divide-by-zero, so do the actual division
         (/ -1.0d0 x))
        (t
          (logb-finite x))))

;;; This function is used to create a complex number of the
;;; appropriate type:
;;;   Create complex number with real part X and imaginary part Y
;;;   such that has the same type as Z.  If Z has type (complex
;;;   rational), the X and Y are coerced to single-float.
#+long-float (eval-when (:compile-toplevel :load-toplevel :execute)
                (error "needs work for long float support"))
(declaim (inline coerce-to-complex-type))
(defun coerce-to-complex-type (x y z)
  (declare (double-float x y)
           (number z))
  (if (typep (realpart z) 'double-float)
      (complex x y)
      ;; Convert anything that's not already a DOUBLE-FLOAT (because
      ;; the initial argument was a (COMPLEX DOUBLE-FLOAT) and we
      ;; haven't done anything to lose precision) to a SINGLE-FLOAT.
      (complex (float x 1f0)
               (float y 1f0))))

;;; Compute |(x+i*y)/2^k|^2 scaled to avoid over/underflow. The
;;; result is r + i*k, where k is an integer.
#+long-float (eval-when (:compile-toplevel :load-toplevel :execute)
                (error "needs work for long float support"))
(defun cssqs (z)
  (declare (muffle-conditions compiler-note))
  (let ((x (float (realpart z) 1d0))
        (y (float (imagpart z) 1d0)))
    ;; Would this be better handled using an exception handler to
    ;; catch the overflow or underflow signal?  For now, we turn all
    ;; traps off and look at the accrued exceptions to see if any
    ;; signal would have been raised.
    (with-float-traps-masked (:underflow :overflow)
      (let ((rho (+ (square x) (square y))))
       (declare (optimize (speed 3) (space 0)))
      (cond ((and (float-infinity-or-nan-p rho)
                  (or (float-infinity-p x) (float-infinity-p y)))
             (values double-float-positive-infinity 0))
            ((let ((threshold
                    ;; (/ least-positive-double-float double-float-epsilon)
                     #-long-float
                     (make-double-float #x1fffff #xfffffffe)
                     #+long-float
                     (error "(/ least-positive-long-float long-float-epsilon)"))
                   (traps (ldb sb-vm:float-sticky-bits
                               (sb-vm:floating-point-modes))))
                ;; Overflow raised or (underflow raised and rho <
                ;; lambda/eps)
               (or (not (zerop (logand sb-vm:float-overflow-trap-bit traps)))
                   (and (not (zerop (logand sb-vm:float-underflow-trap-bit
                                            traps)))
                        (< rho threshold))))
              ;; If we're here, neither x nor y are infinity and at
              ;; least one is non-zero.. Thus logb returns a nice
              ;; integer.
              (let ((k (- (logb-finite (max (abs x) (abs y))))))
                (values (+ (square (scale-float x k))
                           (square (scale-float y k)))
                        (- k))))
             (t
              (values rho 0)))))))

;;; principal square root of Z
;;;
;;; Z may be RATIONAL or COMPLEX; the result is always a COMPLEX.
(defun complex-sqrt (z)
  ;; KLUDGE: Here and below, we can't just declare Z to be of type
  ;; COMPLEX, because one-arg COMPLEX on rationals returns a rational.
  ;; Since there isn't a rational negative zero, this is OK from the
  ;; point of view of getting the right answer in the face of branch
  ;; cuts, but declarations of the form (OR RATIONAL COMPLEX) are
  ;; still ugly.  -- CSR, 2004-05-16
  (declare (type (or complex rational) z))
  (multiple-value-bind (rho k)
      (cssqs z)
    (declare (type (or (member 0d0) (double-float 0d0)) rho)
             (type fixnum k))
    (let ((x (float (realpart z) 1.0d0))
          (y (float (imagpart z) 1.0d0))
          (eta 0d0)
          (nu 0d0))
      (declare (double-float x y eta nu)
               ;; get maybe-inline functions inlined.
               (optimize (space 0)))
      (if (not (float-nan-p x))
          (setf rho (+ (scale-float (abs x) (- k)) (sqrt rho))))

      (cond ((oddp k)
             (setf k (ash k -1)))
            (t
             (setf k (1- (ash k -1)))
             (setf rho (+ rho rho))))

      (setf rho (scale-float (sqrt rho) k))

      (setf eta rho)
      (setf nu y)

      (when (/= rho 0d0)
        (when (not (float-infinity-p nu))
          (setf nu (/ (/ nu rho) 2d0)))
        (when (< x 0d0)
          (setf eta (abs nu))
          (setf nu (float-sign y rho))))
      (coerce-to-complex-type eta nu z))))

;;; log of Z = log |Z| + i * arg Z
;;;
;;; Z may be any number, but the result is always a complex.
(defun complex-log (z)
  (declare (muffle-conditions compiler-note))
  (declare (type (or rational complex) z))
  ;; The constants t0, t1, t2 should be evaluated to machine
  ;; precision.  In addition, Kahan says the accuracy of log1p
  ;; influences the choices of these constants but doesn't say how to
  ;; choose them.  We'll just assume his choices matches our
  ;; implementation of log1p.
  (let ((t0 #-long-float (make-double-float #x3fe6a09e #x667f3bcd)
            #+long-float (error "(/ (sqrt 2l0) 2)"))
        (t1 1.2d0)
        (t2 3d0)
        (ln2 #-long-float (make-double-float #x3fe62e42 #xfefa39ef)
             #+long-float (error "(log 2l0)"))
        (x (float (realpart z) 1.0d0))
        (y (float (imagpart z) 1.0d0)))
    (multiple-value-bind (rho k)
        (cssqs z)
      (declare (optimize (speed 3)))
      (let ((beta (max (abs x) (abs y)))
            (theta (min (abs x) (abs y))))
        (coerce-to-complex-type (if (and (zerop k)
                                         (< t0 beta)
                                         (or (<= beta t1)
                                             (< rho t2)))
                                    (/ (%log1p (+ (* (- beta 1.0d0)
                                                     (+ beta 1.0d0))
                                                  (* theta theta)))
                                       2d0)
                                    (+ (/ (log rho) 2d0)
                                       (* k ln2)))
                                (atan y x)
                                z)))))

;;; KLUDGE: Let us note the following "strange" behavior. atanh 1.0d0
;;; is +infinity, but the following code returns approx 176 + i*pi/4.
;;; The reason for the imaginary part is caused by the fact that arg
;;; i*y is never 0 since we have positive and negative zeroes. -- rtoy
;;; Compute atanh z = (log(1+z) - log(1-z))/2.
(defun complex-atanh (z)
  (declare (muffle-conditions compiler-note))
  (declare (type (or rational complex) z))
  (let* (;; constants
         (theta (sb-xc:/ (sb-xc:sqrt most-positive-double-float) 4.0d0))
         (rho (sb-xc:/ 4.0d0 (sb-xc:sqrt most-positive-double-float)))
         (half-pi (sb-xc:/ pi 2.0d0))
         (rp (float (realpart z) 1.0d0))
         (beta (float-sign rp 1.0d0))
         (x (* beta rp))
         (y (* beta (- (float (imagpart z) 1.0d0))))
         (eta 0.0d0)
         (nu 0.0d0))
    ;; Shouldn't need this declare.
    (declare (double-float x y))
    (locally
       (declare (optimize (speed 3)))
    (cond ((or (> x theta)
               (> (abs y) theta))
           ;; To avoid overflow...
           (setf nu (float-sign y half-pi))
           ;; ETA is real part of 1/(x + iy).  This is x/(x^2+y^2),
           ;; which can cause overflow.  Arrange this computation so
           ;; that it won't overflow.
           (setf eta (let* ((x-bigger (> x (abs y)))
                            (r (if x-bigger (/ y x) (/ x y)))
                            (d (+ 1.0d0 (* r r))))
                       (if x-bigger
                           (/ (/ x) d)
                           (/ (/ r y) d)))))
          ((= x 1.0d0)
           ;; Should this be changed so that if y is zero, eta is set
           ;; to +infinity instead of approx 176?  In any case
           ;; tanh(176) is 1.0d0 within working precision.
           (let ((t1 (+ 4d0 (square y)))
                 (t2 (+ (abs y) rho)))
             (setf eta (log (/ (sqrt (sqrt t1))
                               (sqrt t2))))
             (setf nu (* 0.5d0
                         (float-sign y
                                     (+ half-pi (atan (* 0.5d0 t2))))))))
          (t
           (let ((t1 (+ (abs y) rho)))
              ;; Normal case using log1p(x) = log(1 + x)
             (setf eta (* 0.25d0
                          (%log1p (/ (* 4.0d0 x)
                                     (+ (square (- 1.0d0 x))
                                        (square t1))))))
             (setf nu (* 0.5d0
                         (atan (* 2.0d0 y)
                               (- (* (- 1.0d0 x)
                                     (+ 1.0d0 x))
                                  (square t1))))))))
    (coerce-to-complex-type (* beta eta)
                            (- (* beta nu))
                             z))))

#|
(format t "~x~%" (double-float-bits (/ (+ (log 2l0) (log most-positive-long-float)) 4l0)))
=> 406633CE8FB9F87D

and:
#include <math.h>
#include <string.h>
#include <stdio.h>
void main() {
    double most_pos_dbl = 1.7976931348623157e308;
    double thing = (log(most_pos_dbl) + log(2.0e0)) / 4.0e0;
    unsigned long word;
    memcpy(&word, &thing, 8);
    printf("%lX = %20.15lf\n", word, thing);
}
prints: 406633CE8FB9F87D =  177.618965018485966
|#

;;; Compute tanh z = sinh z / cosh z.
(defun complex-tanh (z)
  (declare (muffle-conditions compiler-note))
  (declare (type (or rational complex) z))
  (let ((x (float (realpart z) 1.0d0))
        (y (float (imagpart z) 1.0d0)))
    (locally
      ;; space 0 to get maybe-inline functions inlined
      (declare (optimize (speed 3) (space 0)))
    (cond ((> (abs x)
              #-long-float (make-double-float #x406633ce #x8fb9f87d)
              #+long-float (error "(/ (+ (log 2l0) (log most-positive-long-float)) 4l0)"))
           (coerce-to-complex-type (float-sign x)
                                   (float-sign y) z))
          (t
           (let* ((tv (%tan y))
                  (beta (+ 1.0d0 (* tv tv)))
                  (s (sinh x))
                  (rho (sqrt (+ 1.0d0 (* s s)))))
             (if (float-infinity-p tv)
                 (coerce-to-complex-type (/ rho s)
                                         (/ tv)
                                         z)
                 (let ((den (+ 1.0d0 (* beta s s))))
                   (coerce-to-complex-type (/ (* beta rho s)
                                              den)
                                           (/ tv den)
                                            z)))))))))

;;; Compute acos z = pi/2 - asin z.
;;;
;;; Z may be any NUMBER, but the result is always a COMPLEX.
(defun complex-acos (z)
  ;; Kahan says we should only compute the parts needed.  Thus, the
  ;; REALPART's below should only compute the real part, not the whole
  ;; complex expression.  Doing this can be important because we may get
  ;; spurious signals that occur in the part that we are not using.
  ;;
  ;; However, we take a pragmatic approach and just use the whole
  ;; expression.
  ;;
  ;; NOTE: The formula given by Kahan is somewhat ambiguous in whether
  ;; it's the conjugate of the square root or the square root of the
  ;; conjugate.  This needs to be checked.
  ;;
  ;; I checked.  It doesn't matter because (conjugate (sqrt z)) is the
  ;; same as (sqrt (conjugate z)) for all z.  This follows because
  ;;
  ;; (conjugate (sqrt z)) = exp(0.5*log |z|)*exp(-0.5*j*arg z).
  ;;
  ;; (sqrt (conjugate z)) = exp(0.5*log|z|)*exp(0.5*j*arg conj z)
  ;;
  ;; and these two expressions are equal if and only if arg conj z =
  ;; -arg z, which is clearly true for all z.
  (declare (type (or rational complex) z))
  (let ((sqrt-1+z (complex-sqrt (+ 1 z)))
        (sqrt-1-z (complex-sqrt (- 1 z))))
    (with-float-traps-masked (:divide-by-zero)
      (complex (* 2 (atan (/ (realpart sqrt-1-z)
                             (realpart sqrt-1+z))))
               (asinh (imagpart (* (conjugate sqrt-1+z)
                                   sqrt-1-z)))))))

;;; Compute acosh z = 2 * log(sqrt((z+1)/2) + sqrt((z-1)/2))
;;;
;;; Z may be any NUMBER, but the result is always a COMPLEX.
(defun complex-acosh (z)
  (declare (type (or rational complex) z))
  (let ((sqrt-z-1 (complex-sqrt (- z 1)))
        (sqrt-z+1 (complex-sqrt (+ z 1))))
    (with-float-traps-masked (:divide-by-zero)
      (complex (asinh (realpart (* (conjugate sqrt-z-1)
                                   sqrt-z+1)))
               (* 2 (atan (/ (imagpart sqrt-z-1)
                             (realpart sqrt-z+1))))))))

;;; Compute asin z = asinh(i*z)/i.
;;;
;;; Z may be any NUMBER, but the result is always a COMPLEX.
(defun complex-asin (z)
  (declare (type (or rational complex) z))
  (let ((sqrt-1-z (complex-sqrt (- 1 z)))
        (sqrt-1+z (complex-sqrt (+ 1 z))))
    (with-float-traps-masked (:divide-by-zero)
      (complex (atan (/ (realpart z)
                        (realpart (* sqrt-1-z sqrt-1+z))))
               (asinh (imagpart (* (conjugate sqrt-1-z)
                                   sqrt-1+z)))))))

;;; Compute asinh z = log(z + sqrt(1 + z*z)).
;;;
;;; Z may be any number, but the result is always a complex.
(defun complex-asinh (z)
  (declare (type (or rational complex) z))
  ;; asinh z = -i * asin (i*z)
  (let* ((iz (complex (- (imagpart z)) (realpart z)))
         (result (complex-asin iz)))
    (complex (imagpart result)
             (- (realpart result)))))

;;; Compute atan z = atanh (i*z) / i.
;;;
;;; Z may be any number, but the result is always a complex.
(defun complex-atan (z)
  (declare (type (or rational complex) z))
  ;; atan z = -i * atanh (i*z)
  (let* ((iz (complex (- (imagpart z)) (realpart z)))
         (result (complex-atanh iz)))
    (complex (imagpart result)
             (- (realpart result)))))

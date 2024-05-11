;;;; This file contains the definitions of most number functions.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-KERNEL")

;;;; canonicalization utilities

;;; If IMAGPART is 0, return REALPART, otherwise make a complex. This is
;;; used when we know that REALPART and IMAGPART are the same type, but
;;; rational canonicalization might still need to be done.
(declaim (inline canonical-complex))
(defun canonical-complex (realpart imagpart)
  (if (eql imagpart 0)
      realpart
      (cond #+long-float
            ((and (typep realpart 'long-float)
                  (typep imagpart 'long-float))
             (truly-the (complex long-float) (complex realpart imagpart)))
            ((and (typep realpart 'double-float)
                  (typep imagpart 'double-float))
             (truly-the (complex double-float) (complex realpart imagpart)))
            ((and (typep realpart 'single-float)
                  (typep imagpart 'single-float))
             (truly-the (complex single-float) (complex realpart imagpart)))
            (t
             (%make-complex realpart imagpart)))))

;;; Given a numerator and denominator with the GCD already divided
;;; out, make a canonical rational. We make the denominator positive,
;;; and check whether it is 1.
(declaim (inline build-ratio))
(defun build-ratio (num den)
  (multiple-value-bind (num den)
      (if (minusp den)
          (values (- num) (- den))
          (values num den))
    (cond
      ((eql den 0)
       (error 'division-by-zero
              :operands (list num den)
              :operation '/))
      ((eql den 1) num)
      (t (%make-ratio num den)))))

;;; Truncate X and Y, but bum the case where Y is 1.
(declaim (inline maybe-truncate))
(defun maybe-truncate (x y)
  (if (eql y 1)
      x
      (truncate x y)))

;;;; COMPLEXes

(defun complex (realpart &optional (imagpart 0))
  "Return a complex number with the specified real and imaginary components."
  (declare (explicit-check))
  (flet ((%%make-complex (realpart imagpart)
           (cond #+long-float
                 ((and (typep realpart 'long-float)
                       (typep imagpart 'long-float))
                  (truly-the (complex long-float)
                             (complex realpart imagpart)))
                 ((and (typep realpart 'double-float)
                       (typep imagpart 'double-float))
                  (truly-the (complex double-float)
                             (complex realpart imagpart)))
                 ((and (typep realpart 'single-float)
                       (typep imagpart 'single-float))
                  (truly-the (complex single-float)
                             (complex realpart imagpart)))
                 (t
                  (%make-complex realpart imagpart)))))
  (number-dispatch ((realpart real) (imagpart real))
    ((rational rational)
     (canonical-complex realpart imagpart))
    (float-contagion %%make-complex realpart imagpart (rational)))))

(defun realpart (number)
  "Extract the real part of a number."
  (etypecase number
    #+long-float
    ((complex long-float)
     (truly-the long-float (realpart number)))
    ((complex double-float)
     (truly-the double-float (realpart number)))
    ((complex single-float)
     (truly-the single-float (realpart number)))
    ((complex rational)
     (%realpart number))
    (number
     number)))

(defun imagpart (number)
  "Extract the imaginary part of a number."
  (etypecase number
    #+long-float
    ((complex long-float)
     (truly-the long-float (imagpart number)))
    ((complex double-float)
     (truly-the double-float (imagpart number)))
    ((complex single-float)
     (truly-the single-float (imagpart number)))
    ((complex rational)
     (%imagpart number))
    (float
     (* 0 number))
    (number
     0)))

(defun conjugate (number)
  "Return the complex conjugate of NUMBER. For non-complex numbers, this is
  an identity."
  (declare (type number number) (explicit-check))
  (if (complexp number)
      (complex (realpart number) (- (imagpart number)))
      number))

(defun signum (number)
  "If NUMBER is zero, return NUMBER, else return (/ NUMBER (ABS NUMBER))."
  (declare (explicit-check))
  (if (zerop number)
      number
      (number-dispatch ((number number))
        (((foreach fixnum rational single-float double-float))
         (if (plusp number)
             (coerce 1 '(dispatch-type number))
             (coerce -1 '(dispatch-type number))))
        ((complex)
         (/ number (abs number))))))

;;;; ratios

(defun numerator (number)
  "Return the numerator of NUMBER, which must be rational."
  (numerator number))

(defun denominator (number)
  "Return the denominator of NUMBER, which must be rational."
  (denominator number))

;;;; arithmetic operations
;;;;
;;;; IMPORTANT NOTE: Accessing &REST arguments with NTH is actually extremely
;;;; efficient in SBCL, as is taking their LENGTH -- so this code is very
;;;; clever instead of being charmingly naive. Please check that "obvious"
;;;; improvements don't actually ruin performance.
;;;;
;;;; (Granted that the difference between very clever and charmingly naivve
;;;; can sometimes be sliced exceedingly thing...)

(macrolet ((define-arith (op init doc)
             `(defun ,op (&rest numbers)
                (declare (explicit-check))
                ,doc
                (if numbers
                    (let ((result (the number (fast-&rest-nth 0 numbers))))
                      (do-rest-arg ((n) numbers 1 result)
                        (setq result (,op result n))))
                    ,init))))
  (define-arith + 0
    "Return the sum of its arguments. With no args, returns 0.")
  (define-arith * 1
    "Return the product of its arguments. With no args, returns 1."))

(defun - (number &rest more-numbers)
  "Subtract the second and all subsequent arguments from the first;
  or with one argument, negate the first argument."
  (declare (explicit-check))
  (if more-numbers
      (let ((result number))
        (do-rest-arg ((n) more-numbers 0 result)
          (setf result (- result n))))
      (- number)))

(defun / (number &rest more-numbers)
  "Divide the first argument by each of the following arguments, in turn.
  With one argument, return reciprocal."
  (declare (explicit-check))
  (if more-numbers
      (let ((result number))
        (do-rest-arg ((n) more-numbers 0 result)
          (setf result (/ result n))))
      (/ number)))

(defun 1+ (number)
  "Return NUMBER + 1."
  (declare (explicit-check))
  (1+ number))

(defun 1- (number)
  "Return NUMBER - 1."
  (declare (explicit-check))
  (1- number))

(defmacro two-arg-+/- (name op big-op)
  `(defun ,name (x y)
     (declare (explicit-check))
     (number-dispatch ((x number) (y number))
       (bignum-cross-fixnum ,op ,big-op)
       (float-contagion ,op x y)

       ((complex complex)
        (canonical-complex (,op (realpart x) (realpart y))
                           (,op (imagpart x) (imagpart y))))
       (((foreach bignum fixnum ratio single-float double-float
                  #+long-float long-float) complex)
        (complex (,op x (realpart y)) (,op 0 (imagpart y))))
       ((complex (or rational float))
        (complex (,op (realpart x) y) (,op (imagpart x) 0)))

       (((foreach fixnum bignum) ratio)
        (let* ((dy (denominator y))
               (n (,op (* x dy) (numerator y))))
          (%make-ratio n dy)))
       ((ratio integer)
        (let* ((dx (denominator x))
               (n (,op (numerator x) (* y dx))))
          (%make-ratio n dx)))
       ((ratio ratio)
        (let* ((nx (numerator x))
               (dx (denominator x))
               (ny (numerator y))
               (dy (denominator y))
               (g1 (gcd dx dy)))
          (if (eql g1 1)
              (%make-ratio (,op (* nx dy) (* dx ny)) (* dx dy))
              (let* ((t2 (truncate dx g1))
                     (t1 (,op (* nx (truncate dy g1)) (* t2 ny)))
                     (g2 (gcd t1 g1)))
                (cond ((eql t1 0) 0)
                      ((eql g2 1)
                       (%make-ratio t1 (* t2 dy)))
                      (t (let* ((nn (truncate t1 g2))
                                (t3 (truncate dy g2))
                                (nd (if (eql t2 1) t3 (* t2 t3))))
                           (if (eql nd 1) nn (%make-ratio nn nd))))))))))))

(two-arg-+/- two-arg-+ + add-bignums)
(two-arg-+/- two-arg-- - subtract-bignum)

(defun two-arg-* (x y)
  (declare (explicit-check))
  (flet ((integer*ratio (x y)
           (if (eql x 0) 0
               (let* ((ny (numerator y))
                      (dy (denominator y))
                      (gcd (gcd x dy)))
                 (if (eql gcd 1)
                     (%make-ratio (* x ny) dy)
                     (let ((nn (* (truncate x gcd) ny))
                           (nd (truncate dy gcd)))
                       (if (eql nd 1)
                           nn
                           (%make-ratio nn nd)))))))
         (complex*real (x y)
           (canonical-complex (* (realpart x) y) (* (imagpart x) y))))
    (number-dispatch ((x number) (y number))
      (float-contagion * x y)
      ((fixnum fixnum) (* x y))
      ((bignum fixnum) (multiply-bignum-and-fixnum x y))
      ((fixnum bignum) (multiply-bignum-and-fixnum y x))
      ((bignum bignum) (multiply-bignums x y))

      ((complex complex)
       (let* ((rx (realpart x))
              (ix (imagpart x))
              (ry (realpart y))
              (iy (imagpart y)))
         (canonical-complex (- (* rx ry) (* ix iy)) (+ (* rx iy) (* ix ry)))))
      (((foreach bignum fixnum ratio single-float double-float
                 #+long-float long-float)
        complex)
       (complex*real y x))
      ((complex (or rational float))
       (complex*real x y))

      (((foreach bignum fixnum) ratio) (integer*ratio x y))
      ((ratio integer) (integer*ratio y x))
      ((ratio ratio)
       (let* ((nx (numerator x))
              (dx (denominator x))
              (ny (numerator y))
              (dy (denominator y))
              (g1 (gcd nx dy))
              (g2 (gcd dx ny)))
         (build-ratio (* (maybe-truncate nx g1)
                         (maybe-truncate ny g2))
                      (* (maybe-truncate dx g2)
                         (maybe-truncate dy g1))))))))

(defun %negate (n)
  (declare (explicit-check))
  (number-dispatch ((n number))
    (((foreach fixnum single-float double-float #+long-float long-float))
     (%negate n))
    ((bignum)
     (negate-bignum n))
    ((ratio)
     (%make-ratio (- (numerator n)) (denominator n)))
    ((complex)
     (complex (- (realpart n)) (- (imagpart n))))))

(defun %multiply-high (x y)
  (declare (type word x y))
  (%multiply-high x y))


;;;; TRUNCATE and friends

(defun truncate (number &optional (divisor 1))
  "Return number (or number/divisor) as an integer, rounded toward 0.
  The second returned value is the remainder."
  (declare (explicit-check))
  (macrolet ((truncate-float (rtype)
               `(let* ((float-div (coerce divisor ',rtype))
                       (divided (/ number float-div))
                       (res (unary-truncate divided)))
                  (values res
                          (- number
                             (* #-round-float
                                (coerce res ',rtype)
                                #+round-float
                                (- (,(ecase rtype
                                       (double-float 'round-double)
                                       (single-float 'round-single))
                                     divided :truncate)
                                   ,(ecase rtype
                                      (double-float -0.0d0)
                                      (single-float -0.0f0)))
                                float-div)))))
             (single-digit-bignum-p (x)
               #+(or x86-64 x86 ppc64)
               `(or (typep ,x 'word)
                    (typep ,x 'sb-vm:signed-word))
               ;; Other backends don't have native double-word/word division,
               ;; and their bigfloor implementation doesn't handle
               ;; full-width divisors.
               #-(or x86-64 x86 ppc64)
               `(or (typep ,x '(unsigned-byte ,(1- sb-vm:n-word-bits)))
                    (typep ,x '(integer ,(- 1 (expt 2 (- sb-vm:n-word-bits 1)))
                                ,(1- (expt 2 (- sb-vm:n-word-bits 1))))))))
    (number-dispatch ((number real) (divisor real))
      ((fixnum fixnum) (truncate number divisor))
      (((foreach fixnum bignum) ratio)
       (if (= (numerator divisor) 1)
           (values (* number (denominator divisor)) 0)
           (multiple-value-bind (quot rem)
               (truncate (* number (denominator divisor))
                         (numerator divisor))
             (values quot (/ rem (denominator divisor))))))
      ((fixnum bignum)
       (if (single-digit-bignum-p divisor)
           (bignum-truncate-single-digit (make-small-bignum number) divisor)
           (bignum-truncate (make-small-bignum number) divisor)))
      ((ratio (or float rational))
       (let ((q (truncate (numerator number)
                          (* (denominator number) divisor))))
         (values q (- number (* q divisor)))))
      ((bignum fixnum)
       (bignum-truncate-single-digit number divisor))
      ((bignum bignum)
       (if (single-digit-bignum-p divisor)
           (bignum-truncate-single-digit number divisor)
           (bignum-truncate number divisor)))
      (((foreach single-float double-float #+long-float long-float)
        (or rational single-float))
       (if (eql divisor 1)
           (let ((res (unary-truncate number)))
             (values res (- number (coerce res '(dispatch-type number)))))
           (truncate-float (dispatch-type number))))
      #+long-float
      ((long-float (or single-float double-float long-float))
       (truncate-float long-float))
      #+long-float
      (((foreach double-float single-float) long-float)
       (truncate-float long-float))
      ((double-float (or single-float double-float))
       (truncate-float double-float))
      ((single-float double-float)
       (truncate-float double-float))
      (((foreach fixnum bignum ratio)
        (foreach single-float double-float #+long-float long-float))
       (truncate-float (dispatch-type divisor))))))

(defun floor (number &optional (divisor 1))
  "Return the greatest integer not greater than number, or number/divisor.
  The second returned value is (mod number divisor)."
  (declare (explicit-check))
  (macrolet ((truncate-float (rtype)
               `(floor (coerce number ',rtype) (coerce divisor ',rtype)))
             (single-digit-bignum-p (x)
               #+(or x86-64 x86 ppc64)
               `(or (typep ,x 'word)
                    (typep ,x 'sb-vm:signed-word))
               ;; Other backends don't have native double-word/word division,
               ;; and their bigfloor implementation doesn't handle
               ;; full-width divisors.
               #-(or x86-64 x86 ppc64)
               `(or (typep ,x '(unsigned-byte ,(1- sb-vm:n-word-bits)))
                    (typep ,x '(integer ,(- 1 (expt 2 (- sb-vm:n-word-bits 1)))
                                ,(1- (expt 2 (- sb-vm:n-word-bits 1)))))))
             (fixup (form)
               `(multiple-value-bind (tru rem) ,form
                  (if (if (minusp divisor)
                          (> rem 0)
                          (< rem 0))
                      (values (1- tru) (+ rem divisor))
                      (values tru rem)))))
    (number-dispatch ((number real) (divisor real))
      ((fixnum fixnum) (floor number divisor))
      (((foreach fixnum bignum) ratio)
       (multiple-value-bind (quot rem)
           (truncate (* number (denominator divisor))
                     (numerator divisor))
         (fixup (values quot (/ rem (denominator divisor))))))
      ((fixnum bignum)
       (fixup
        (if (single-digit-bignum-p divisor)
            (bignum-truncate-single-digit (make-small-bignum number) divisor)
            (bignum-truncate (make-small-bignum number) divisor))))
      ((ratio (or float rational))
       (let ((q (truncate (numerator number)
                          (* (denominator number) divisor))))
         (fixup (values q (- number (* q divisor))))))
      ((bignum fixnum)
       (fixup (bignum-truncate-single-digit number divisor)))
      ((bignum bignum)
       (fixup (if (single-digit-bignum-p divisor)
                  (bignum-truncate-single-digit number divisor)
                  (bignum-truncate number divisor))))
      (((foreach single-float double-float #+long-float long-float)
        (or rational single-float))
       (truncate-float (dispatch-type number)))
      #+long-float
      ((long-float (or single-float double-float long-float))
       (truncate-float long-float))
      #+long-float
      (((foreach double-float single-float) long-float)
       (truncate-float long-float))
      ((double-float (or single-float double-float))
       (truncate-float double-float))
      ((single-float double-float)
       (truncate-float double-float))
      (((foreach fixnum bignum ratio)
        (foreach single-float double-float #+long-float long-float))
       (truncate-float (dispatch-type divisor))))))

(defun ceiling (number &optional (divisor 1))
  "Return number (or number/divisor) as an integer, rounded toward 0.
  The second returned value is the remainder."
  (declare (explicit-check))
  (macrolet ((truncate-float (rtype)
               `(ceiling (coerce number ',rtype) (coerce divisor ',rtype)))
             (single-digit-bignum-p (x)
               #+(or x86-64 x86 ppc64)
               `(or (typep ,x 'word)
                    (typep ,x 'sb-vm:signed-word))
               ;; Other backends don't have native double-word/word division,
               ;; and their bigfloor implementation doesn't handle
               ;; full-width divisors.
               #-(or x86-64 x86 ppc64)
               `(or (typep ,x '(unsigned-byte ,(1- sb-vm:n-word-bits)))
                    (typep ,x '(integer ,(- 1 (expt 2 (- sb-vm:n-word-bits 1)))
                                ,(1- (expt 2 (- sb-vm:n-word-bits 1)))))))
             (fixup (form)
               `(multiple-value-bind (tru rem) ,form
                  (if (if (minusp divisor)
                          (< rem 0)
                          (> rem 0))
                      (values (+ tru 1) (- rem divisor))
                      (values tru rem)))))
    (number-dispatch ((number real) (divisor real))
      ((fixnum fixnum) (ceiling number divisor))
      (((foreach fixnum bignum) ratio)
       (multiple-value-bind (quot rem)
           (truncate (* number (denominator divisor))
                     (numerator divisor))
         (fixup (values quot (/ rem (denominator divisor))))))
      ((fixnum bignum)
       (fixup
        (if (single-digit-bignum-p divisor)
            (bignum-truncate-single-digit (make-small-bignum number) divisor)
            (bignum-truncate (make-small-bignum number) divisor))))
      ((ratio (or float rational))
       (let ((q (truncate (numerator number)
                          (* (denominator number) divisor))))
         (fixup (values q (- number (* q divisor))))))
      ((bignum fixnum)
       (fixup (bignum-truncate-single-digit number divisor)))
      ((bignum bignum)
       (fixup
        (if (single-digit-bignum-p divisor)
            (bignum-truncate-single-digit number divisor)
            (bignum-truncate number divisor))))
      (((foreach single-float double-float #+long-float long-float)
        (or rational single-float))
       (truncate-float (dispatch-type number)))
      #+long-float
      ((long-float (or single-float double-float long-float))
       (truncate-float long-float))
      #+long-float
      (((foreach double-float single-float) long-float)
       (truncate-float long-float))
      ((double-float (or single-float double-float))
       (truncate-float double-float))
      ((single-float double-float)
       (truncate-float double-float))
      (((foreach fixnum bignum ratio)
        (foreach single-float double-float #+long-float long-float))
        (truncate-float (dispatch-type divisor))))))

(defun rem (number divisor)
  "Return second result of TRUNCATE."
  (declare (explicit-check))
  (rem number divisor))

(defun mod (number divisor)
  "Return second result of FLOOR."
  (declare (explicit-check))
  (mod number divisor))

(defun round (number &optional (divisor 1))
  "Rounds number (or number/divisor) to nearest integer.
  The second returned value is the remainder."
  (declare (explicit-check))
  (if (eql divisor 1)
      (round number)
      (multiple-value-bind (tru rem) (truncate number divisor)
        (if (zerop rem)
            (values tru rem)
            (let ((thresh (/ (abs divisor) 2)))
              (cond ((or (> rem thresh)
                         (and (= rem thresh) (oddp tru)))
                     (if (minusp divisor)
                         (values (- tru 1) (+ rem divisor))
                         (values (+ tru 1) (- rem divisor))))
                    ((let ((-thresh (- thresh)))
                       (or (< rem -thresh)
                           (and (= rem -thresh) (oddp tru))))
                     (if (minusp divisor)
                         (values (+ tru 1) (- rem divisor))
                         (values (- tru 1) (+ rem divisor))))
                    (t (values tru rem))))))))

(defmacro !define-float-rounding-function (name op doc)
  `(defun ,name (number &optional (divisor 1))
    ,doc
    (multiple-value-bind (res rem) (,op number divisor)
      (values (float res (if (floatp rem) rem 1.0)) rem))))

;;; Declare these guys inline to let them get optimized a little.
;;; ROUND and FROUND are not declared inline since they seem too
;;; obscure and too big to inline-expand by default. Also, this gives
;;; the compiler a chance to pick off the unary float case.
(declaim (inline fceiling ffloor ftruncate))

#-round-float
(progn
  (defun ftruncate (number &optional (divisor 1))
    "Same as TRUNCATE, but returns first value as a float."
    (declare (explicit-check))
    (macrolet ((ftruncate-float (rtype)
                 `(let* ((float-div (coerce divisor ',rtype))
                         (res (%unary-ftruncate (/ number float-div))))
                    (values res
                            (- number
                               (* (coerce res ',rtype) float-div))))))
      (number-dispatch ((number real) (divisor real))
        (((foreach fixnum bignum ratio) (or fixnum bignum ratio))
         (multiple-value-bind (q r)
             (truncate number divisor)
           (if (and (zerop q) (or (and (minusp number) (not (minusp divisor)))
                                  (and (not (minusp number)) (minusp divisor))))
               (values -0f0 r)
               (values (float q) r))))
        (((foreach single-float double-float #+long-float long-float)
          (or rational single-float))
         (if (eql divisor 1)
             (let ((res (%unary-ftruncate number)))
               (values res (- number (coerce res '(dispatch-type number)))))
             (ftruncate-float (dispatch-type number))))
        #+long-float
        ((long-float (or single-float double-float long-float))
         (ftruncate-float long-float))
        #+long-float
        (((foreach double-float single-float) long-float)
         (ftruncate-float long-float))
        ((double-float (or single-float double-float))
         (ftruncate-float double-float))
        ((single-float double-float)
         (ftruncate-float double-float))
        (((foreach fixnum bignum ratio)
          (foreach single-float double-float #+long-float long-float))
         (ftruncate-float (dispatch-type divisor))))))

  (defun ffloor (number &optional (divisor 1))
    "Same as FLOOR, but returns first value as a float."
    (declare (explicit-check))
    (multiple-value-bind (tru rem) (ftruncate number divisor)
      (if (and (not (zerop rem))
               (if (minusp divisor)
                   (plusp number)
                   (minusp number)))
          (values (1- tru) (+ rem divisor))
          (values tru rem))))

  (defun fceiling (number &optional (divisor 1))
    "Same as CEILING, but returns first value as a float."
    (declare (explicit-check))
    (multiple-value-bind (tru rem) (ftruncate number divisor)
      (if (and (not (zerop rem))
               (if (minusp divisor)
                   (minusp number)
                   (plusp number)))
          (values (+ tru 1) (- rem divisor))
          (values tru rem))))

  (defun fround (number &optional (divisor 1))
    "Same as ROUND, but returns first value as a float."
    (declare (explicit-check))
    (macrolet ((fround-float (rtype)
                 `(let* ((float-div (coerce divisor ',rtype))
                         (res (%unary-fround (/ number float-div))))
                    (values res
                            (- number
                               (* (coerce res ',rtype) float-div))))))
      (number-dispatch ((number real) (divisor real))
        (((foreach fixnum bignum ratio) (or fixnum bignum ratio))
         (multiple-value-bind (q r)
             (round number divisor)
           (if (and (zerop q) (or (and (minusp number) (not (minusp divisor)))
                                  (and (not (minusp number)) (minusp divisor))))
               (values -0f0 r)
               (values (float q) r))))
        (((foreach single-float double-float #+long-float long-float)
          (or rational single-float))
         (if (eql divisor 1)
             (let ((res (%unary-fround number)))
               (values res (- number (coerce res '(dispatch-type number)))))
             (fround-float (dispatch-type number))))
        #+long-float
        ((long-float (or single-float double-float long-float))
         (fround-float long-float))
        #+long-float
        (((foreach double-float single-float) long-float)
         (fround-float long-float))
        ((double-float (or single-float double-float))
         (fround-float double-float))
        ((single-float double-float)
         (fround-float double-float))
        (((foreach fixnum bignum ratio)
          (foreach single-float double-float #+long-float long-float))
         (fround-float (dispatch-type divisor)))))))

#+round-float
(macrolet ((def (name mode docstring)
             `(defun ,name (number &optional (divisor 1))
                ,docstring
                (declare (explicit-check))
                (macrolet ((ftruncate-float (rtype)
                             `(let* ((float-div (coerce divisor ',rtype))
                                     (res (,(case rtype
                                              (double-float 'sb-kernel:round-double)
                                              (single-float 'sb-kernel:round-single))
                                           (/ number float-div)
                                           ,,mode)))
                                (values res
                                        (- number
                                           (* (coerce res ',rtype) float-div)))))
                           (unary-ftruncate-float (rtype)
                             `(let* ((res (,(case rtype
                                              (double-float 'sb-kernel:round-double)
                                              (single-float 'sb-kernel:round-single))
                                           number
                                           ,,mode)))
                                (values res (- number res)))))
                  (number-dispatch ((number real) (divisor real))
                    (((foreach fixnum bignum ratio) (or fixnum bignum ratio))
                     (multiple-value-bind (q r)
                         (,(find-symbol (string mode) :cl) number divisor)
                       (if (and (zerop q) (or (and (minusp number) (not (minusp divisor)))
                                              (and (not (minusp number)) (minusp divisor))))
                           (values -0f0 r)
                           (values (float q) r))))
                    (((foreach single-float double-float)
                      (or rational single-float))
                     (if (eql divisor 1)
                         (unary-ftruncate-float (dispatch-type number))
                         (ftruncate-float (dispatch-type number))))
                    ((double-float (or single-float double-float))
                     (ftruncate-float double-float))
                    ((single-float double-float)
                     (ftruncate-float double-float))
                    (((foreach fixnum bignum ratio)
                      (foreach single-float double-float))
                     (ftruncate-float (dispatch-type divisor))))))))
  (def ftruncate :truncate
       "Same as TRUNCATE, but returns first value as a float.")

  (def ffloor :floor
       "Same as FLOOR, but returns first value as a float.")

  (def fceiling :ceiling
       "Same as CEILING, but returns first value as a float.")
  (def fround :round
    "Same as ROUND, but returns first value as a float.")

  (macrolet ((def (name)
               `(defun ,name (x mode)
                  (ecase mode
                    ,@(loop for m in '(:round :floor :ceiling :truncate)
                            collect `(,m (,name x ,m)))))))


    (def round-single)
    (def round-double)))

;;;; comparisons

(defun = (number &rest more-numbers)
  "Return T if all of its arguments are numerically equal, NIL otherwise."
  (declare (number number) (explicit-check))
  (do-rest-arg ((n i) more-numbers 0 t)
    (unless (= number n)
      (return (do-rest-arg ((n) more-numbers (1+ i))
                (the number n)))))) ; for effect

(defun /= (number &rest more-numbers)
  "Return T if no two of its arguments are numerically equal, NIL otherwise."
  (declare (number number) (explicit-check))
  (if more-numbers
      (do ((n number (nth i more-numbers))
            (i 0 (1+ i)))
          ((>= i (length more-numbers))
           t)
        (do-rest-arg ((n2) more-numbers i)
          (when (= n n2)
            (return-from /= nil))))
      t))

(macrolet ((def (op doc)
             `(defun ,op (number &rest more-numbers)
                ,doc
                (declare (explicit-check))
                (let ((n1 number))
                  (declare (real n1))
                  (do-rest-arg ((n2 i) more-numbers 0 t)
                    (if (,op n1 n2)
                        (setf n1 n2)
                        (return (do-rest-arg ((n) more-numbers (1+ i))
                                  (the real n))))))))) ; for effect
  (def <  "Return T if its arguments are in strictly increasing order, NIL otherwise.")
  (def >  "Return T if its arguments are in strictly decreasing order, NIL otherwise.")
  (def <= "Return T if arguments are in strictly non-decreasing order, NIL otherwise.")
  (def >= "Return T if arguments are in strictly non-increasing order, NIL otherwise."))

(defun max (number &rest more-numbers)
  "Return the greatest of its arguments; among EQUALP greatest, return
the first."
  (declare (explicit-check))
  (let ((n number))
    (declare (real n))
    (do-rest-arg ((arg) more-numbers 0 n)
      (when (> arg n)
        (setf n arg)))))

(defun min (number &rest more-numbers)
  "Return the least of its arguments; among EQUALP least, return
the first."
  (declare (explicit-check))
  (let ((n number))
    (declare (real n))
    (do-rest-arg ((arg) more-numbers 0 n)
      (when (< arg n)
        (setf n arg)))))

#+64-bit
(defmacro float-bignum-= (float bignum type)
  (macrolet ((sym (name)
               `(package-symbolicate #.(find-package "SB-VM") type '- ',name)))
    `(let* ((float ,float)
            (bignum ,bignum)
            (bignum-length (%bignum-length bignum))
            (bits (,(sym bits) float))
            (exp (ldb ,(sym exponent-byte) bits)))
       (cond ((< exp (+ sb-vm:n-fixnum-bits ,(sym bias)))
              nil)
             (t
              (let* ((exp (- exp ,(sym bias) ,(sym digits)))
                     (sig (logior ,(sym hidden-bit)
                                  (ldb ,(sym significand-byte) bits)))
                     (int (if (minusp bits)
                              (- sig)
                              sig))
                     (int-length (integer-length int)))
                (and (= (truly-the bignum-length (sb-bignum::bignum-buffer-integer-length bignum bignum-length))
                        (+ int-length exp))
                     (sb-bignum::bignum-lower-bits-zero-p bignum exp bignum-length)
                     (= int
                        (truly-the fixnum
                                   (sb-bignum::last-bignum-part=>fixnum (- (1- sb-bignum::digit-size) int-length) exp bignum))))))))))

#+64-bit
(defmacro float-bignum-< (float bignum type)
  (macrolet ((sym (name)
               `(package-symbolicate #.(find-package "SB-VM") type '- ',name)))
    `(let* ((float ,float)
            (bignum ,bignum)
            (bignum-length (%bignum-length bignum))
            (bits (,(sym bits) float))
            (exp (ldb ,(sym exponent-byte) bits)))
       (cond ((< exp (+ sb-vm:n-fixnum-bits ,(sym bias)))
              (sb-bignum::%bignum-0-or-plusp bignum bignum-length))
             (t
              (let ((sig (logior ,(sym hidden-bit)
                                  (ldb ,(sym significand-byte) bits)))
                    (exp (- exp ,(sym bias) ,(sym digits))))
                (let* ((int (if (minusp bits)
                                (- sig)
                                sig))
                       (int-length (integer-length int))
                       (length-diff (- (truly-the bignum-length (sb-bignum::bignum-buffer-integer-length bignum bignum-length))
                                       (+ int-length exp))))
                  (cond
                    ((plusp length-diff) (sb-bignum::%bignum-0-or-plusp bignum bignum-length))
                    ((minusp length-diff) (minusp float))
                    (t
                     (let ((diff (- (truly-the fixnum
                                               (sb-bignum::last-bignum-part=>fixnum (- (1- sb-bignum::digit-size) int-length) exp bignum))
                                    int)))
                       (cond ((plusp diff) t)
                             ((minusp diff) nil)
                             (t
                              (not (sb-bignum::bignum-lower-bits-zero-p bignum exp bignum-length))))))))))))))

#+64-bit
(defmacro float-bignum-> (float bignum type)
  (macrolet ((sym (name)
               `(package-symbolicate #.(find-package "SB-VM") type '- ',name)))
    `(let* ((float ,float)
            (bignum ,bignum)
            (bignum-length (%bignum-length bignum))
            (bits (,(sym bits) float))
            (exp (ldb ,(sym exponent-byte) bits)))
       (cond ((< exp (+ sb-vm:n-fixnum-bits ,(sym bias)))
              (not (sb-bignum::%bignum-0-or-plusp bignum bignum-length)))
             (t
              (let ((sig (logior ,(sym hidden-bit)
                                 (ldb ,(sym significand-byte) bits)))
                    (exp (- exp ,(sym bias) ,(sym digits))))
                (let* ((int
                         (if (minusp bits)
                             (- sig)
                             sig))
                       (int-length (integer-length int))
                       (length-diff (- (truly-the bignum-length (sb-bignum::bignum-buffer-integer-length bignum bignum-length))
                                       (+ int-length exp))))
                  (cond ((plusp length-diff)
                         (not (sb-bignum::%bignum-0-or-plusp bignum bignum-length)))
                        ((minusp length-diff)
                         (not (minusp float)))
                        (t
                         (< (truly-the fixnum (sb-bignum::last-bignum-part=>fixnum (- (1- sb-bignum::digit-size) int-length) exp bignum))
                            int))))))))))

(defmacro make-fixnum-float-comparer (operation integer float float-type)
  (multiple-value-bind (min max)
      (ecase float-type
        (single-float
         (values most-negative-fixnum-single-float most-positive-fixnum-single-float))
        (double-float
         (values most-negative-fixnum-double-float most-positive-fixnum-double-float)))
    ` (cond ((> ,float ,max)
             ,(ecase operation
                ((= > >=) nil)
                ((< <=) t)))
            ((< ,float ,min)
             ,(ecase operation
                ((= < <=) nil)
                ((> >=) t)))
            (t
             (sb-c::if-vop-existsp (:translate %unary-floor)
               ,(ecase operation
                  (=
                   `(let ((quot (%unary-truncate ,float)))
                      (and (= quot ,integer)
                           (= (float quot ,float) ,float))))
                  (>
                   `(,operation ,integer (truly-the fixnum (%unary-floor ,float))))
                  (<
                   `(,operation ,integer (truly-the fixnum (%unary-ceiling ,float))))
                  (>=
                   `(let ((quot (truly-the fixnum (%unary-floor ,float))))
                      (or (> ,integer quot)
                          (and (= ,integer quot)
                               (= (float quot ,float) ,float)))))
                  (<=
                   `(let ((quot (truly-the fixnum (%unary-ceiling ,float))))
                      (or (< ,integer quot)
                          (and (= ,integer quot)
                               (= (float quot ,float) ,float))))))
               (let ((quot (%unary-truncate ,float)))
                 ,(ecase operation
                    (=
                     `(and (= quot ,integer)
                           (= (float quot ,float) ,float)))
                    (>
                     `(cond ((> ,integer quot))
                            ((< ,integer quot)
                             nil)
                            ((<= ,integer 0)
                             (> (float quot ,float) ,float))))
                    (<
                     `(cond ((< ,integer quot))
                            ((> ,integer quot)
                             nil)
                            ((>= ,integer 0)
                             (< (float quot ,float) ,float))))
                    (>=
                     `(cond ((> ,integer quot))
                            ((< ,integer quot)
                             nil)
                            (t
                             (or (< ,float 0)
                                 (= (float quot ,float) ,float)))))
                    (<=
                     `(cond ((< ,integer quot))
                            ((> ,integer quot)
                             nil)
                            (t
                             (or (>= ,float 0)
                                 (= (float quot ,float) ,float))))))))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
;;; The INFINITE-X-FINITE-Y and INFINITE-Y-FINITE-X args tell us how
;;; to handle the case when X or Y is a floating-point infinity and
;;; the other arg is a rational. (Section 12.1.4.1 of the ANSI spec
;;; says that comparisons are done by converting the float to a
;;; rational when comparing with a rational, but infinities can't be
;;; converted to a rational, so we show some initiative and do it this
;;; way instead.)
  (defun basic-compare (op &key infinite-x-finite-y infinite-y-finite-x)
    `(((fixnum fixnum) (,op x y))
      ((single-float single-float) (,op x y))
      (((foreach single-float double-float) double-float)
       (,op (coerce x 'double-float) y))
      #+long-float
      (((foreach single-float double-float long-float) long-float)
       (,op (coerce x 'long-float) y))
      #+long-float
      ((long-float (foreach single-float double-float))
       (,op x (coerce y 'long-float)))
      ((fixnum (foreach single-float double-float))
       (with-float-inf-or-nan-test y
         ,infinite-y-finite-x
         nil
         (make-fixnum-float-comparer ,op x y (dispatch-type y))))
      (((foreach single-float double-float) fixnum)
       (if (eql y 0)
           (,op x (coerce 0 '(dispatch-type x)))
           (with-float-inf-or-nan-test x
             ,infinite-x-finite-y
             nil
             (make-fixnum-float-comparer ,(case op
                                            (> '<)
                                            (< '>)
                                            (>= '<=)
                                            (<= '>=)
                                            (= '=))
                                         y x (dispatch-type x)))))
      ((double-float single-float)
       (,op x (coerce y 'double-float)))
      (((foreach single-float double-float #+long-float long-float) ratio)
       (with-float-inf-or-nan-test x
         ,infinite-x-finite-y
         nil
         ;; Avoid converting the float into a rational, since it
         ;; will be taken apart later anyway.
         (multiple-value-bind (bits exp) (integer-decode-float x)
           (if (eql bits 0)
               (,op 0 y)
               (let ((int (if (minusp x) (- bits) bits)))
                 (if (minusp exp)
                     (,op (* int (denominator y))
                          (ash (numerator y) (- exp)))
                     (,op (ash int exp) y)))))))
      ((ratio (foreach single-float double-float))
       (with-float-inf-or-nan-test y
         ,infinite-y-finite-x
         nil
         (multiple-value-bind (bits exp) (integer-decode-float y)
           (if (eql bits 0)
               (,op x 0)
               (let ((int (if (minusp y) (- bits) bits)))
                 (if (minusp exp)
                     (,op (ash (numerator x) (- exp))
                          (* int (denominator x)))
                     (,op x (ash int exp))))))))
      (((foreach single-float double-float) bignum)
       (with-float-inf-or-nan-test x
         ,infinite-x-finite-y
         nil
         #+64-bit
         ,(case op
            (> '(float-bignum-> x y (dispatch-type x)))
            (< '(float-bignum-< x y (dispatch-type x)))
            (>= '(not (float-bignum-< x y (dispatch-type x))))
            (<= '(not (float-bignum-> x y (dispatch-type x))))
            (= '(float-bignum-= x y (dispatch-type x))))
         #-64-bit
         (,op (rational x) y)))
      ((bignum (foreach single-float double-float))
       (with-float-inf-or-nan-test y
         ,infinite-y-finite-x
         nil
         #+64-bit
         ,(case op
            (> '(float-bignum-< y x (dispatch-type y)))
            (< '(float-bignum-> y x (dispatch-type y)))
            (>= '(not (float-bignum-> y x (dispatch-type y))))
            (<= '(not (float-bignum-< y x (dispatch-type y))))
            (= '(float-bignum-= y x (dispatch-type y))))
         #-64-bit
         (,op x (rational y))))))
  )                                     ; EVAL-WHEN

(defmacro dispatch-ratio ((ratio numerator denominator) &body body)
  `(let ((,numerator (numerator ,ratio))
         (,denominator (denominator ,ratio)))
     (if (and (fixnump ,numerator)
              (fixnump ,denominator))
         (progn ,@body)
         (progn ,@body))))

(defmacro dispatch-two-ratios ((ratio1 numerator1 denominator1)
                               (ratio2 numerator2 denominator2)
                               body
                               &optional (fixnum-body body))
  `(let ((,numerator1 (numerator ,ratio1))
         (,denominator1 (denominator ,ratio1))
         (,numerator2 (numerator ,ratio2))
         (,denominator2 (denominator ,ratio2)))
     (if (and (fixnump ,numerator1)
              (fixnump ,numerator2)
              (fixnump ,denominator1)
              (fixnump ,denominator2))
         ,fixnum-body
         ,body)))


(macrolet ((def-two-arg-</> (name op ratio-arg1 ratio-arg2 &rest cases)
             `(defun ,name (x y)
                (declare (explicit-check))
                (number-dispatch ((x real) (y real))
                  (basic-compare
                   ,op
                   :infinite-x-finite-y
                   (,op x (coerce 0 '(dispatch-type x)))
                   :infinite-y-finite-x
                   (,op (coerce 0 '(dispatch-type y)) y))
                  (((foreach fixnum bignum) ratio)
                   (dispatch-ratio (y numerator denominator)
                       (,(case op
                           (<= '<)
                           (>= '>)
                           (t op))
                        x (,ratio-arg2 numerator
                                       denominator))))
                  ((ratio (foreach fixnum bignum))
                   (dispatch-ratio (x numerator denominator)
                     (,(case op
                         (<= '<)
                         (>= '>)
                         (t op))
                      (,ratio-arg1 numerator
                                   denominator)
                      y)))
                  ((ratio ratio)
                   (dispatch-two-ratios
                       (x numerator-x denominator-x)
                       (y numerator-y denominator-y)
                       (or
                        ,@(case op
                            ((<= >=)
                             `((and (eql numerator-x numerator-y)
                                    (eql denominator-x denominator-y)))))
                        (,(case op
                            (<= '<)
                            (>= '>)
                            (t op))
                         (* numerator-x denominator-y)
                         (* numerator-y denominator-x)))
                       ,@(or
                          (sb-c::when-vop-existsp (:named sb-vm::signed-multiply-low-high)
                            `((multiple-value-bind (low1 high1) (%primitive sb-vm::signed-multiply-low-high numerator-x denominator-y)
                                (multiple-value-bind (low2 high2) (%primitive sb-vm::signed-multiply-low-high numerator-y denominator-x)
                                  (cond ((= high1 high2)
                                         (,op low1
                                              low2))
                                        ((,(case op
                                             (<= '<)
                                             (>= '>)
                                             (t op))
                                          high1 high2)
                                         t))))))
                          (sb-c::when-vop-existsp (:translate %signed-multiply-high)
                            `((let ((high1 (%signed-multiply-high numerator-x denominator-y))
                                    (high2 (%signed-multiply-high numerator-y denominator-x)))
                                (cond ((= high1 high2)
                                       (,op (logand most-positive-word (* numerator-x denominator-y))
                                            (logand most-positive-word (* numerator-y denominator-x))))
                                      ((,(case op
                                           (<= '<)
                                           (>= '>)
                                           (t op))
                                        high1 high2)
                                       t))))))))
                  ,@cases))))
  (def-two-arg-</> two-arg-< < floor ceiling
    ((fixnum bignum)
     (bignum-plus-p y))
    ((bignum fixnum)
     (not (bignum-plus-p x)))
    ((bignum bignum)
     (minusp (bignum-compare x y))))
  (def-two-arg-</> two-arg-> > ceiling floor
    ((fixnum bignum)
     (not (bignum-plus-p y)))
    ((bignum fixnum)
     (bignum-plus-p x))
    ((bignum bignum)
     (plusp (bignum-compare x y))))
  (def-two-arg-</> two-arg-<= <= floor ceiling
    ((fixnum bignum)
     (bignum-plus-p y))
    ((bignum fixnum)
     (not (bignum-plus-p x)))
    ((bignum bignum)
     (<= (bignum-compare x y) 0)))
  (def-two-arg-</> two-arg->= >= ceiling floor
    ((fixnum bignum)
     (not (bignum-plus-p y)))
    ((bignum fixnum)
     (bignum-plus-p x))
    ((bignum bignum)
     (>= (bignum-compare x y) 0))))

(defun two-arg-= (x y)
  (declare (inline float-infinity-p)
           (explicit-check))
  (number-dispatch ((x number) (y number))
    (basic-compare =
                   ;; An infinite value is never equal to a finite value.
                   :infinite-x-finite-y nil
                   :infinite-y-finite-x nil)
    ((fixnum (or bignum ratio)) nil)

    ((bignum (or fixnum ratio)) nil)
    ((bignum bignum)
     (zerop (bignum-compare x y)))

    ((ratio integer) nil)
    ((ratio ratio)
     (and (eql (numerator x) (numerator y))
          (eql (denominator x) (denominator y))))

    ((complex complex)
     (and (= (realpart x) (realpart y))
          (= (imagpart x) (imagpart y))))
    (((foreach fixnum bignum ratio single-float double-float
               #+long-float long-float) complex)
     (and (= x (realpart y))
          (zerop (imagpart y))))
    ((complex (or float rational))
     (and (= (realpart x) y)
          (zerop (imagpart x))))))

(macrolet ((make-float-fixnum-range-comparer (operation low high float float-type)
             (multiple-value-bind (min max)
                 (ecase float-type
                   (single-float
                    (values most-negative-fixnum-single-float most-positive-fixnum-single-float))
                   (double-float
                    (values most-negative-fixnum-double-float most-positive-fixnum-double-float)))
               `(cond
                  ((or (> ,float ,max)
                       (< ,float ,min)
                       (float-inf-or-nan-test ,float nil nil))
                   nil)
                  (t
                   (let ((quot (%unary-truncate ,float)))
                     ,(ecase operation
                        (<
                         `(cond ((or (> quot ,high)
                                     (< quot ,low)
                                     (= low high))
                                 nil)
                                ((= quot high)
                                 (and (minusp ,float)
                                      (/= ,float (float quot))))
                                ((= quot low)
                                 (and (plusp ,float)
                                      (/= ,float (float quot))))
                                (t)))
                        (<=
                         `(cond ((or (> quot ,high)
                                     (< quot ,low))
                                 nil)
                                ((= quot high)
                                 (or (and (minusp ,float)
                                          (/= low high))
                                     (= ,float (float quot))))
                                ((= quot low)
                                 (or (plusp ,float)
                                     (= ,float (float quot))))
                                (t)))
                        (<<=
                         `(cond ((or (> quot ,high)
                                     (< quot ,low)
                                     (= low high))
                                 nil)
                                ((= quot high)
                                 (or (minusp ,float)
                                     (= ,float (float quot))))
                                ((= quot low)
                                 (and (plusp ,float)
                                      (/= ,float (float quot))))
                                (t)))
                        (<=<
                         `(cond ((or (> quot ,high)
                                     (< quot ,low)
                                     (= low high))
                                 nil)
                                ((= quot high)
                                 (and (minusp ,float)
                                      (/= ,float (float quot))))
                                ((= quot low)
                                 (or (plusp ,float)
                                     (= ,float (float quot))))
                                (t)))))))))
           (def-range (name op &optional (low-op op) (high-op op))
             `(progn
                (defun ,name (low x high)
                  (declare (explicit-check)
                           (fixnum low high))
                  (number-dispatch ((x real))
                    ((fixnum) (and (,low-op low x)
                                   (,high-op x high)))
                    (((foreach single-float double-float))
                     (make-float-fixnum-range-comparer ,op low high x (dispatch-type x)))
                    ((bignum) nil)
                    ((ratio)
                     (let* ((numerator (numerator x))
                            (minusp (minusp numerator))
                            (x (truncate numerator (denominator x))))
                       (cond ((or (not (fixnump x))
                                  (< x low)
                                  (> x high))
                              nil)
                             ((= low high)
                              nil)
                             ((= x high)
                              minusp)
                             ((= x low)
                              (not minusp))
                             (t)))))))))
  (def-range range< <)
  (def-range range<= <=)
  (def-range range<<= <<= < <=)
  (def-range range<=< <=< <= <))

(sb-c::when-vop-existsp (:translate check-range<=)
  (defun check-range<= (low x high)
    (when (typep x 'fixnum)
      (range<= low x high))))


;;;; logicals

(macrolet ((def (op init doc)
             `(defun ,op (&rest integers)
                ,doc
                (declare (explicit-check))
                (if integers
                    (do ((result (fast-&rest-nth 0 integers)
                                 (,op result (fast-&rest-nth i integers)))
                         (i 1 (1+ i)))
                        ((>= i (length integers))
                         result)
                      (declare (integer result)))
                    ,init))))
  (def logior 0 "Return the bit-wise or of its arguments. Args must be integers.")
  (def logxor 0 "Return the bit-wise exclusive or of its arguments. Args must be integers.")
  (def logand -1 "Return the bit-wise and of its arguments. Args must be integers.")
  (def logeqv -1 "Return the bit-wise equivalence of its arguments. Args must be integers."))

(defun lognot (number)
  "Return the bit-wise logical not of integer."
  (declare (explicit-check))
  (etypecase number
    (fixnum (lognot (truly-the fixnum number)))
    (bignum (bignum-logical-not number))))

(macrolet ((def (name op big-op &optional doc)
             `(defun ,name (integer1 integer2)
                ,@(when doc (list doc))
                (declare (explicit-check))
                (let ((x integer1)
                      (y integer2))
                  (number-dispatch ((x integer) (y integer))
                    (bignum-cross-fixnum ,op ,big-op))))))
  (def two-arg-and logand bignum-logical-and)
  (def two-arg-ior logior bignum-logical-ior)
  (def two-arg-xor logxor bignum-logical-xor)
  ;; BIGNUM-LOGICAL-{AND,IOR,XOR} need not return a bignum, so must
  ;; call the generic LOGNOT...
  (def two-arg-eqv logeqv (lambda (x y) (lognot (bignum-logical-xor x y))))
  (def lognand lognand
       (lambda (x y) (lognot (bignum-logical-and x y)))
       "Complement the logical AND of INTEGER1 and INTEGER2.")
  (def lognor lognor
       (lambda (x y) (lognot (bignum-logical-ior x y)))
       "Complement the logical OR of INTEGER1 and INTEGER2.")
  ;; ... but BIGNUM-LOGICAL-NOT on a bignum will always return a bignum
  (def logandc1 logandc1
       (lambda (x y) (bignum-logical-and (bignum-logical-not x) y))
       "Bitwise AND (LOGNOT INTEGER1) with INTEGER2.")
  (def logandc2 logandc2
       (lambda (x y) (bignum-logical-and x (bignum-logical-not y)))
       "Bitwise AND INTEGER1 with (LOGNOT INTEGER2).")
  (def logorc1 logorc1
       (lambda (x y) (bignum-logical-ior (bignum-logical-not x) y))
       "Bitwise OR (LOGNOT INTEGER1) with INTEGER2.")
  (def logorc2 logorc2
       (lambda (x y) (bignum-logical-ior x (bignum-logical-not y)))
       "Bitwise OR INTEGER1 with (LOGNOT INTEGER2)."))

(defun logcount (integer)
  "Count the number of 1 bits if INTEGER is non-negative,
and the number of 0 bits if INTEGER is negative."
  (declare (explicit-check))
  (etypecase integer
    (fixnum
     (logcount #-(or x86-64 arm64)
               (truly-the (integer 0
                                   #.(max most-positive-fixnum
                                          (lognot most-negative-fixnum)))
                          (if (minusp (truly-the fixnum integer))
                              (lognot (truly-the fixnum integer))
                              integer))
               ;; The VOP handles that case better
               #+(or x86-64 arm64) integer))
    (bignum
     (bignum-logcount integer))))

(defun logtest (integer1 integer2)
  "Predicate which returns T if logand of integer1 and integer2 is not zero."
  (logtest integer1 integer2))

(defun logbitp (index integer)
  "Predicate returns T if bit index of integer is a 1."
  (declare (explicit-check))
  (number-dispatch ((index unsigned-byte) (integer integer))
    ((fixnum fixnum) (if (< index sb-vm:n-positive-fixnum-bits)
                         (not (zerop (logand integer (ash 1 index))))
                         (minusp integer)))
    ((fixnum bignum) (bignum-logbitp index integer))
    ((bignum (foreach fixnum bignum)) (minusp integer))))

(defun ash (integer count)
  "Shifts integer left by count places preserving sign. - count shifts right."
  (declare (explicit-check))
  (etypecase integer
    (fixnum
     (cond ((fixnump count)
            (if (zerop integer)
                0
                (let ((length (integer-length (truly-the fixnum integer)))
                      (count (truly-the fixnum count)))
                  (declare (fixnum length count))
                  (cond ((and (plusp count)
                              (>= (+ length count)
                                  sb-vm:n-word-bits))
                         (bignum-ashift-left-fixnum integer count))
                        (t
                         (truly-the sb-vm:signed-word
                                    (ash (truly-the fixnum integer) count)))))))
           ((minusp (the integer count))
            (if (minusp integer) -1 0))
           (t
            (bignum-ashift-left (make-small-bignum integer) count))))
    (bignum
     (if (plusp (the integer count))
         (bignum-ashift-left integer count)
         (bignum-ashift-right integer (- count))))))

(defun integer-length (integer)
  "Return the number of non-sign bits in the twos-complement representation
  of INTEGER."
  (declare (explicit-check))
  (etypecase integer
    (fixnum
     (integer-length (truly-the fixnum integer)))
    (bignum
     (bignum-integer-length integer))))

;;;; BYTE, bytespecs, and related operations

(defun byte (size position)
  "Return a byte specifier which may be used by other byte functions
  (e.g. LDB)."
  (byte size position))

(defun byte-size (bytespec)
  "Return the size part of the byte specifier bytespec."
  (byte-size bytespec))

(defun byte-position (bytespec)
  "Return the position part of the byte specifier bytespec."
  (byte-position bytespec))

(defun ldb (bytespec integer)
  "Extract the specified byte from integer, and right justify result."
  (ldb bytespec integer))

(defun ldb-test (bytespec integer)
  "Return T if any of the specified bits in integer are 1's."
  (ldb-test bytespec integer))

(defun mask-field (bytespec integer)
  "Extract the specified byte from integer,  but do not right justify result."
  (mask-field bytespec integer))

(defun dpb (newbyte bytespec integer)
  "Return new integer with newbyte in specified position, newbyte is right justified."
  (dpb newbyte bytespec integer))

(defun deposit-field (newbyte bytespec integer)
  "Return new integer with newbyte in specified position, newbyte is not right justified."
  (deposit-field newbyte bytespec integer))

(defun %ldb (size posn integer)
  ;; The naive algorithm is horrible in the general case.
  ;; Consider (LDB (BYTE 1 2) (SOME-GIANT-BIGNUM)) which has to shift the
  ;; input rightward 2 bits, consing a new bignum just to read 1 bit.
  (cond ((<= 0 size sb-vm:n-positive-fixnum-bits)
         (if (fixnump integer)
             (logand (ash integer (- posn))
                     (1- (ash 1 size)))
             (sb-bignum::ldb-bignum=>fixnum size posn integer)))
        (t
         (logand (ash integer (- posn))
                 (1- (ash 1 size))))))

(defun %mask-field (size posn integer)
  (declare (type bit-index size posn) (explicit-check))
  (logand integer (ash (1- (ash 1 size)) posn)))

(defun %dpb (newbyte size posn integer)
  (declare (type bit-index size posn) (explicit-check))
  (let ((mask (1- (ash 1 size))))
    (logior (logand integer (lognot (ash mask posn)))
            (ash (logand newbyte mask) posn))))

(defun %deposit-field (newbyte size posn integer)
  (declare (type bit-index size posn) (explicit-check))
  (let ((mask (ash (ldb (byte size 0) -1) posn)))
    (logior (logand newbyte mask)
            (logand integer (lognot mask)))))

(defun sb-c::mask-signed-field (size integer)
  "Extract SIZE lower bits from INTEGER, considering them as a
2-complement SIZE-bits representation of a signed integer."
  (macrolet ((msf (size integer)
               `(if (logbitp (1- ,size) ,integer)
                    (dpb ,integer (byte (1- ,size) 0) -1)
                    (ldb (byte (1- ,size) 0) ,integer))))
    (typecase size
      ((eql 0) 0)
      ((integer 1 #.sb-vm:n-fixnum-bits)
       (number-dispatch ((integer integer))
         ((fixnum) (msf size integer))
         ((bignum) (let ((fix (sb-c::mask-signed-field #.sb-vm:n-fixnum-bits (%bignum-ref integer 0))))
                     (if (= size #.sb-vm:n-fixnum-bits)
                         fix
                         (msf size fix))))))
      ((integer (#.sb-vm:n-fixnum-bits) #.sb-vm:n-word-bits)
       (number-dispatch ((integer integer))
         ((fixnum) integer)
         ((bignum) (let ((word (sb-c::mask-signed-field #.sb-vm:n-word-bits (%bignum-ref integer 0))))
                     (if (= size #.sb-vm:n-word-bits)
                         word
                         (msf size word))))))
      ((unsigned-byte) (msf size integer)))))

;;;; BOOLE

(defun boole (op integer1 integer2)
  "Bit-wise boolean function on two integers. Function chosen by OP:
        0       BOOLE-CLR
        1       BOOLE-SET
        2       BOOLE-1
        3       BOOLE-2
        4       BOOLE-C1
        5       BOOLE-C2
        6       BOOLE-AND
        7       BOOLE-IOR
        8       BOOLE-XOR
        9       BOOLE-EQV
        10      BOOLE-NAND
        11      BOOLE-NOR
        12      BOOLE-ANDC1
        13      BOOLE-ANDC2
        14      BOOLE-ORC1
        15      BOOLE-ORC2"
  (case op
    (0 (boole 0 integer1 integer2))
    (1 (boole 1 integer1 integer2))
    (2 (boole 2 integer1 integer2))
    (3 (boole 3 integer1 integer2))
    (4 (boole 4 integer1 integer2))
    (5 (boole 5 integer1 integer2))
    (6 (boole 6 integer1 integer2))
    (7 (boole 7 integer1 integer2))
    (8 (boole 8 integer1 integer2))
    (9 (boole 9 integer1 integer2))
    (10 (boole 10 integer1 integer2))
    (11 (boole 11 integer1 integer2))
    (12 (boole 12 integer1 integer2))
    (13 (boole 13 integer1 integer2))
    (14 (boole 14 integer1 integer2))
    (15 (boole 15 integer1 integer2))
    (t (error 'type-error :datum op :expected-type '(mod 16)))))

;;;; GCD and LCM

(defun gcd (&rest integers)
  "Return the greatest common divisor of the arguments, which must be
  integers. GCD with no arguments is defined to be 0."
  (declare (explicit-check))
  (case (length integers)
    (0 0)
    (1 (abs (the integer (fast-&rest-nth 0 integers))))
    (otherwise
     (do ((result (fast-&rest-nth 0 integers)
                  ;; Call TWO-ARG-GCD directly, because self calls are
                  ;; recognized before REWRITE-FULL-CALL can act,
                  ;; because GCD has no templates to prevent self
                  ;; call optimization.
                  (two-arg-gcd result (the integer (fast-&rest-nth i integers))))
          (i 1 (1+ i)))
         ((>= i (length integers))
          result)
       (declare (integer result))))))

(defun lcm (&rest integers)
  "Return the least common multiple of one or more integers. LCM of no
  arguments is defined to be 1."
  (declare (explicit-check))
  (case (length integers)
    (0 1)
    (1 (abs (the integer (fast-&rest-nth 0 integers))))
    (otherwise
     (do ((result (fast-&rest-nth 0 integers)
                  (two-arg-lcm result (the integer (fast-&rest-nth i integers))))
          (i 1 (1+ i)))
         ((>= i (length integers))
          result)
       (declare (integer result))))))

(defun two-arg-lcm (n m)
  (declare (integer n m))
  (if (or (zerop n) (zerop m))
      0
      ;; KLUDGE: I'm going to assume that it was written this way
      ;; originally for a reason.  However, this is a somewhat
      ;; complicated way of writing the algorithm in the CLHS page for
      ;; LCM, and I don't know why.  To be investigated.  -- CSR,
      ;; 2003-09-11
      ;;
      ;;    It seems to me that this is written this way to avoid
      ;;    unnecessary bignumification of intermediate results.
      ;;        -- TCR, 2008-03-05
      (let ((m (abs m))
            (n (abs n)))
        (multiple-value-bind (max min)
            (if (> m n)
                (values m n)
                (values n m))
          (* (truncate max (gcd n m)) min)))))

(declaim (maybe-inline fixnum-gcd))
(defun fixnum-gcd (u v)
  (declare (optimize (safety 0)))
  (locally
      (declare (optimize (speed 3) (safety 0)))
    (do ((k 0 (1+ k))
         (u (abs u) (ash u -1))
         (v (abs v) (ash v -1)))
        ((oddp (logior u v))
         (do ((temp (if (oddp u) (- v) (ash u -1))
                    (ash temp -1)))
             (nil)
           (declare (fixnum temp))
           (when (oddp temp)
             (if (plusp temp)
                 (setq u temp)
                 (setq v (- temp)))
             (setq temp (- u v))
             (when (zerop temp)
               (return (the (integer 0 #.(1+ most-positive-fixnum)) (ash u k)))))))
      (declare (type (mod #.sb-vm:n-word-bits) k)
               (type sb-vm:signed-word u v)))))

;;; Do the GCD of two integer arguments. With fixnum arguments, we use the
;;; binary GCD algorithm from Knuth's seminumerical algorithms (slightly
;;; structurified), otherwise we call BIGNUM-GCD. We pick off the special case
;;; of 0 before the dispatch so that the bignum code doesn't have to worry
;;; about "small bignum" zeros.
(defun two-arg-gcd (u v)
  (declare (muffle-conditions compiler-note)
           (inline fixnum-gcd))
  (cond ((eql u 0) (abs v))
        ((eql v 0) (abs u))
        (t
         (number-dispatch ((u integer) (v integer))
           ((fixnum fixnum)
            (fixnum-gcd u v))
           ((bignum bignum)
            (bignum-gcd u v))
           ((bignum fixnum)
            (bignum-gcd u (make-small-bignum v)))
           ((fixnum bignum)
            (bignum-gcd (make-small-bignum u) v))))))

;;; Divide two integers, producing a canonical rational. If a fixnum,
;;; we see whether they divide evenly before trying the GCD. In the
;;; bignum case, we don't bother, since bignum division is expensive,
;;; and the test is not very likely to succeed.
(defun integer-/-integer (x y)
  (declare (explicit-check))
  (if (and (typep x 'fixnum) (typep y 'fixnum))
      (multiple-value-bind (quo rem) (truncate x y)
        (if (zerop rem)
            quo
            (let* ((gcd (gcd (truly-the (not (eql 0)) x) y)))
              (multiple-value-bind (num den)
                  (if (eql gcd 1)
                      (values x y)
                      (values (truncate x gcd) (truncate y gcd)))
                (build-ratio num (truly-the (not (integer 0 1)) den))))))
      (let ((gcd (gcd x y)))
        (multiple-value-bind (num den)
            (if (eql gcd 1)
                (values x y)
                (values (truncate x gcd) (truncate y gcd)))
          (build-ratio num den)))))

(defun two-arg-/ (x y)
  (declare (explicit-check))
  (number-dispatch ((x number) (y number))
    (float-contagion / x y (ratio integer))

    ((complex complex)
     (let* ((rx (realpart x))
            (ix (imagpart x))
            (ry (realpart y))
            (iy (imagpart y)))
       (if (> (abs ry) (abs iy))
           (let* ((r (/ iy ry))
                  (dn (+ ry (* r iy))))
             (complex (/ (+ rx (* ix r)) dn)
                      (/ (- ix (* rx r)) dn)))
           (let* ((r (/ ry iy))
                  (dn (+ iy (* r ry))))
             (complex (/ (+ (* rx r) ix) dn)
                      (/ (- (* ix r) rx) dn))))))
    (((foreach integer ratio single-float double-float) complex)
     (let* ((ry (realpart y))
            (iy (imagpart y)))
       (if (> (abs ry) (abs iy))
           (let* ((r (/ iy ry))
                  (dn (+ ry (* r iy))))
             (complex (/ x dn)
                      (/ (- (* x r)) dn)))
           (let* ((r (/ ry iy))
                  (dn (+ iy (* r ry))))
             (complex (/ (* x r) dn)
                      (/ (- x) dn))))))
    ((complex (or rational float))
     (canonical-complex (/ (realpart x) y)
                        (/ (imagpart x) y)))

    ((ratio ratio)
     (let* ((nx (numerator x))
            (dx (denominator x))
            (ny (numerator y))
            (dy (denominator y))
            (g1 (gcd nx ny))
            (g2 (gcd dx dy)))
       (build-ratio (* (maybe-truncate nx g1) (maybe-truncate dy g2))
                    (* (maybe-truncate dx g2) (maybe-truncate ny g1)))))

    ((integer integer)
     (integer-/-integer x y))

    ((integer ratio)
     (if (zerop x)
         0
         (let* ((ny (numerator y))
                (dy (denominator y))
                (gcd (gcd x ny)))
           (build-ratio (* (maybe-truncate x gcd) dy)
                        (maybe-truncate ny gcd)))))

    ((ratio integer)
     (let* ((nx (numerator x))
            (gcd (gcd nx y)))
       (build-ratio (maybe-truncate nx gcd)
                    (* (maybe-truncate y gcd) (denominator x)))))))

;;; from Robert Smith; changed not to cons unnecessarily, and tuned for
;;; faster operation on fixnum inputs by compiling the central recursive
;;; algorithm twice, once using generic and once fixnum arithmetic, and
;;; dispatching on function entry into the applicable part. For maximum
;;; speed, the fixnum part recurs into itself, thereby avoiding further
;;; type dispatching. This pattern is not supported by NUMBER-DISPATCH
;;; thus some special-purpose macrology is needed.
(defun isqrt (n)
  "Return the greatest integer less than or equal to the square root of N."
  (declare (type unsigned-byte n) (explicit-check))
  (macrolet
      ((isqrt-recursion (arg recurse fixnum-p)
         ;; Expands into code for the recursive step of the ISQRT
         ;; calculation. ARG is the input variable and RECURSE the name
         ;; of the function to recur into. If FIXNUM-P is true, some
         ;; type declarations are added that, together with ARG being
         ;; declared as a fixnum outside of here, make the resulting code
         ;; compile into fixnum-specialized code without any calls to
         ;; generic arithmetic. Else, the code works for bignums, too.
         ;; The input must be at least 16 to ensure that RECURSE is called
         ;; with a strictly smaller number and that the result is correct
         ;; (provided that RECURSE correctly implements ISQRT, itself).
         `(macrolet ((if-fixnum-p-truly-the (type expr)
                       ,@(if fixnum-p
                             '(`(truly-the ,type ,expr))
                             '((declare (ignore type))
                               expr))))
            (let* ((fourth-size (ash (1- (integer-length ,arg)) -2))
                   (significant-half (ash ,arg (- (ash fourth-size 1))))
                   (significant-half-isqrt
                     (if-fixnum-p-truly-the
                      (integer 1 #.(isqrt most-positive-fixnum))
                      (,recurse significant-half)))
                   (zeroth-iteration (ash significant-half-isqrt
                                          fourth-size)))
              (multiple-value-bind (quot rem)
                  (floor ,arg zeroth-iteration)
                (let ((first-iteration (ash (+ zeroth-iteration quot) -1)))
                  (cond ((oddp quot)
                         first-iteration)
                        ((> (if-fixnum-p-truly-the
                             fixnum
                             (expt (- first-iteration zeroth-iteration) 2))
                            rem)
                         (1- first-iteration))
                        (t
                         first-iteration))))))))
    (typecase n
      (fixnum (labels ((fixnum-isqrt (n)
                         (declare (type fixnum n))
                         (cond ((> n 24)
                                (isqrt-recursion n fixnum-isqrt t))
                               ((> n 15) 4)
                               ((> n  8) 3)
                               ((> n  3) 2)
                               ((> n  0) 1)
                               ((= n  0) 0))))
                (fixnum-isqrt n)))
      (bignum (isqrt-recursion n isqrt nil)))))

;;;; miscellaneous number predicates

(macrolet ((def (name doc)
             `(defun ,name (number) ,doc
                (declare (explicit-check))
                (,name number))))
  (def zerop "Is this number zero?")
  (def plusp "Is this real number strictly positive?")
  (def minusp "Is this real number strictly negative?")
  (def oddp "Is this integer odd?")
  (def evenp "Is this integer even?"))

;;;; modular functions
#.
(collect ((forms))
  (flet ((unsigned-definition (name lambda-list width)
           (let ((pattern (1- (ash 1 width))))
             `(defun ,name ,(copy-list lambda-list)
               (flet ((prepare-argument (x)
                        (declare (integer x))
                        (etypecase x
                          ((unsigned-byte ,width) x)
                          (fixnum (logand x ,pattern))
                          (bignum (logand x ,pattern)))))
                 (,name ,@(loop for arg in lambda-list
                                collect `(prepare-argument ,arg)))))))
         (signed-definition (name lambda-list width)
           `(defun ,name ,(copy-list lambda-list)
              (flet ((prepare-argument (x)
                       (declare (integer x))
                       (etypecase x
                         ((signed-byte ,width) x)
                         (fixnum (sb-c::mask-signed-field ,width x))
                         (bignum (sb-c::mask-signed-field ,width x)))))
                (,name ,@(loop for arg in lambda-list
                               collect `(prepare-argument ,arg)))))))
    (flet ((do-mfuns (class)
             (loop for infos being each hash-value of (sb-c::modular-class-funs class)
                   ;; FIXME: We need to process only "toplevel" functions
                   when (listp infos)
                   do (loop for info in infos
                            for name = (sb-c::modular-fun-info-name info)
                            and width = (sb-c::modular-fun-info-width info)
                            and signedp = (sb-c::modular-fun-info-signedp info)
                            and lambda-list = (sb-c::modular-fun-info-lambda-list info)
                            if signedp
                            do (forms (signed-definition name lambda-list width))
                            else
                            do (forms (unsigned-definition name lambda-list width))))))
      (do-mfuns sb-c::*untagged-unsigned-modular-class*)
      (do-mfuns sb-c::*untagged-signed-modular-class*)
      (do-mfuns sb-c::*tagged-modular-class*)))
  `(progn ,@(sort (forms) #'string< :key #'cadr)))

#-(or 64-bit 64-bit-registers)
(defun sb-vm::ash-left-mod32 (integer amount)
  (etypecase integer
    ((unsigned-byte 32) (ldb (byte 32 0) (ash integer amount)))
    (fixnum (ldb (byte 32 0) (ash (logand integer #xffffffff) amount)))
    (bignum (ldb (byte 32 0) (ash (logand integer #xffffffff) amount)))))
#+(or 64-bit 64-bit-registers)
(defun sb-vm::ash-left-mod64 (integer amount)
  (etypecase integer
    ((unsigned-byte 64) (ldb (byte 64 0) (ash integer amount)))
    (fixnum (ldb (byte 64 0) (ash (logand integer #xffffffffffffffff) amount)))
    (bignum (ldb (byte 64 0)
                 (ash (logand integer #xffffffffffffffff) amount)))))

#+(or x86 x86-64 arm arm64)
(defun sb-vm::ash-left-modfx (integer amount)
  (let ((fixnum-width (- sb-vm:n-word-bits sb-vm:n-fixnum-tag-bits)))
    (etypecase integer
      (fixnum (sb-c::mask-signed-field fixnum-width (ash integer amount)))
      (integer (sb-c::mask-signed-field fixnum-width (ash (sb-c::mask-signed-field fixnum-width integer) amount))))))

(sb-c::when-vop-existsp (:translate sb-vm::ash-modfx)
  (defun sb-vm::ash-mod64 (integer amount)
    (ldb (byte 64 0) (ash integer amount)))

  (defun sb-vm::ash-modfx (integer amount)
    (if (minusp integer)
        (sb-c::mask-signed-field sb-vm:n-fixnum-bits (ash integer amount))
        (logand most-positive-fixnum (ash integer amount)))))

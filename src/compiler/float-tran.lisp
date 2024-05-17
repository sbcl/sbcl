;;;; This file contains floating-point-specific transforms, and may be
;;;; somewhat implementation-dependent in its assumptions of what the
;;;; formats are.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-C")

;;;; coercions

(deftransform float ((n f) (t single-float) *)
  '(%single-float n))

(deftransform float ((n f) (t double-float) *)
  '(%double-float n))

(deftransform float ((n) *)
  '(if (floatp n)
       n
       (%single-float n)))

(deftransform %single-float ((n) (single-float) * :important nil)
  'n)

(deftransform %double-float ((n) (double-float) * :important nil)
  'n)

(deftransform %single-float ((n) (ratio) * :important nil)
  '(sb-kernel::single-float-ratio n))

(deftransform %double-float ((n) (ratio) * :important nil)
  '(sb-kernel::double-float-ratio n))

(macrolet ((def (type from-type)
             `(deftransform ,(symbolicate "%" type) ((n) ((or ,type ,from-type)) * :important nil)
                (when (or (csubtypep (lvar-type n) (specifier-type ',type))
                          (csubtypep (lvar-type n) (specifier-type ',from-type)))
                  (give-up-ir1-transform))
                `(if (,',(symbolicate type "-P") n)
                     (truly-the ,',type n)
                     (,',(symbolicate "%" type) (truly-the ,',from-type n))))))
  (def single-float double-float)
  (def single-float sb-vm:signed-word)
  (def single-float word)
  (def double-float single-float)
  (def double-float sb-vm:signed-word)
  (def double-float word))

;;; RANDOM
(macrolet ((frob (fun type)
             `(deftransform random ((num &optional state)
                                    (,type &optional t) *)
                "Use inline float operations."
                '(,fun num (or state *random-state*)))))
  (frob %random-single-float single-float)
  (frob %random-double-float double-float))

;;; Return an expression to generate an integer of N-BITS many random
;;; bits, using the minimal number of random chunks possible.
(defun generate-random-expr-for-power-of-2 (n-bits state)
  (declare (type (integer 1 #.sb-vm:n-word-bits) n-bits))
  (multiple-value-bind (n-chunk-bits chunk-expr)
      (cond ((<= n-bits n-random-chunk-bits)
             (values n-random-chunk-bits `(random-chunk ,state)))
            ((<= n-bits (* 2 n-random-chunk-bits))
             (values (* 2 n-random-chunk-bits) `(big-random-chunk ,state)))
            (t
             (error "Unexpectedly small N-RANDOM-CHUNK-BITS")))
    (if (< n-bits n-chunk-bits)
        `(logand ,(1- (ash 1 n-bits)) ,chunk-expr)
        chunk-expr)))

;;; This transform for compile-time constant word-sized integers
;;; generates an accept-reject loop to achieve equidistribution of the
;;; returned values. Several optimizations are done: If NUM is a power
;;; of two no loop is needed. If the random chunk size is half the word
;;; size only one chunk is used where sufficient. For values of NUM
;;; where it is possible and results in faster code, the rejection
;;; probability is reduced by accepting all values below the largest
;;; multiple of the limit that fits into one or two chunks and and doing
;;; a division to get the random value into the desired range.
(deftransform random ((num &optional state)
                      ((constant-arg (integer 1 #.(expt 2 sb-vm:n-word-bits)))
                       &optional t)
                      *
                      :policy (and (> speed compilation-speed)
                                   (> speed space)))
  "optimize to inlined RANDOM-CHUNK operations"
  (let ((num (lvar-value num)))
    (if (= num 1)
        0
        (flet ((chunk-n-bits-and-expr (n-bits)
                 (cond ((<= n-bits n-random-chunk-bits)
                        (values n-random-chunk-bits
                                '(random-chunk (or state *random-state*))))
                       ((<= n-bits (* 2 n-random-chunk-bits))
                        (values (* 2 n-random-chunk-bits)
                                '(big-random-chunk (or state *random-state*))))
                       (t
                        (error "Unexpectedly small N-RANDOM-CHUNK-BITS")))))
          (if (zerop (logand num (1- num)))
              ;; NUM is a power of 2.
              (let ((n-bits (integer-length (1- num))))
                (multiple-value-bind (n-chunk-bits chunk-expr)
                    (chunk-n-bits-and-expr n-bits)
                  (if (< n-bits n-chunk-bits)
                      `(logand ,(1- (ash 1 n-bits)) ,chunk-expr)
                      chunk-expr)))
              ;; Generate an accept-reject loop.
              (let ((n-bits (integer-length num)))
                (multiple-value-bind (n-chunk-bits chunk-expr)
                    (chunk-n-bits-and-expr n-bits)
                  (if (or (> (* num 3) (expt 2 n-chunk-bits))
                          (logbitp (- n-bits 2) num))
                      ;; Division can't help as the quotient is below 3,
                      ;; or is too costly as the rejection probability
                      ;; without it is already small (namely at most 1/4
                      ;; with the given test, which is experimentally a
                      ;; reasonable threshold and cheap to test for).
                      `(loop
                         (let ((bits ,(generate-random-expr-for-power-of-2
                                       n-bits '(or state *random-state*))))
                           (when (< bits num)
                             (return bits))))
                      (let ((d (truncate (expt 2 n-chunk-bits) num)))
                        `(loop
                           (let ((bits ,chunk-expr))
                             (when (< bits ,(* num d))
                               (return (values (truncate bits ,d)))))))))))))))


;;;; float accessors

;;; NaNs can not be constructed from constant bits mainly due to compiler problems
;;; in so doing. See https://bugs.launchpad.net/sbcl/+bug/486812
(deftransform make-single-float ((bits) ((constant-arg t)))
  "Conditional constant folding"
  (let ((float (make-single-float (lvar-value bits))))
    (if (float-nan-p float) (give-up-ir1-transform) float)))

(deftransform make-double-float ((hi lo) ((constant-arg t) (constant-arg t)))
  "Conditional constant folding"
  (let ((float (make-double-float (lvar-value hi) (lvar-value lo))))
    (if (float-nan-p float) (give-up-ir1-transform) float)))

;;; I'd like to transition all the 64-bit backends to use the single-arg
;;; %MAKE-DOUBLE-FLOAT constructor instead of the 2-arg MAKE-DOUBLE-FLOAT.
;;; So we need a transform to fold constant calls for either.
#+64-bit
(deftransform %make-double-float ((bits) ((constant-arg t)))
  "Conditional constant folding"
  (let ((float (%make-double-float (lvar-value bits))))
    (if (float-nan-p float) (give-up-ir1-transform) float)))

;;; On the face of it, these transforms are ridiculous because if we're going
;;; to express (MINUSP X) as (MINUSP (foo-FLOAT-BITS X)), then why not _always_
;;; transform MINUSP of a float into an integer comparison instead of a
;;; floating-point comparison, and then express this as (if (minusp float) ...)
;;; rather than (if (minusp (bits float)) ...) ?
;;; I suspect that the difference is that FLOAT-SIGN must remain silent
;;; when given a signaling NaN.
(deftransform float-sign ((float &optional float2)
                          (single-float &optional single-float) *)
  (if (vop-existsp :translate single-float-copysign)
      (if float2
          `(single-float-copysign float float2)
          `(single-float-sign float))
      (if float2
          (let ((temp (gensym)))
            `(let ((,temp (abs float2)))
               (if (minusp (single-float-bits float)) (- ,temp) ,temp)))
          '(if (minusp (single-float-bits float)) -1f0 1f0))))

(deftransform float-sign ((float &optional float2)
                          (double-float &optional double-float) *)
  ;; If words are 64 bits, then it's actually simpler to  extract _all_ bits
  ;; instead of only the upper bits.
  (let ((bits #+64-bit '(double-float-bits float)
              #-64-bit '(double-float-high-bits float)))
    (if float2
        (let ((temp (gensym)))
          `(let ((,temp (abs float2)))
            (if (minusp ,bits) (- ,temp) ,temp)))
        `(if (minusp ,bits) -1d0 1d0))))

(deftransform float-sign-bit ((x) (single-float) *)
  `(logand (ash (single-float-bits x) -31) 1))
(deftransform float-sign-bit ((x) (double-float) *)
  #-64-bit `(logand (ash (double-float-high-bits x) -31) 1)
  #+64-bit `(ash (logand (double-float-bits x) most-positive-word) -63))

(deftransform float-sign-bit-set-p ((x) (single-float) *)
  `(logbitp 31 (single-float-bits x)))
(deftransform float-sign-bit-set-p ((x) (double-float) *)
  #-64-bit `(logbitp 31 (double-float-high-bits x))
  #+64-bit `(logbitp 63 (double-float-bits x)))

;;; This doesn't deal with complex at the moment.
(deftransform signum ((x) (number))
  (let* ((ctype (lvar-type x))
         (result-type
          (dolist (x '(single-float double-float rational))
            (when (csubtypep ctype (specifier-type x))
              (return x)))))
    ;; SB-XC:COERCE doesn't like RATIONAL for some reason.
    (when (eq result-type 'rational) (setq result-type 'integer))
    (if result-type
        `(cond ((zerop x) x)
               ((plusp x) ,(sb-xc:coerce 1 result-type))
               (t ,(sb-xc:coerce -1 result-type)))
        (give-up-ir1-transform))))

;;;; DECODE-FLOAT, INTEGER-DECODE-FLOAT, and SCALE-FLOAT

(defknown decode-single-float (single-float)
  (values single-float single-float-exponent (member -1f0 1f0))
  (movable foldable flushable))

(defknown decode-double-float (double-float)
  (values double-float double-float-exponent (member -1d0 1d0))
  (movable foldable flushable))

(defknown integer-decode-single-float (single-float)
  (values single-float-significand single-float-int-exponent (member -1 1))
  (movable foldable flushable))

(defknown integer-decode-double-float (double-float)
  (values double-float-significand double-float-int-exponent (member -1 1))
  (movable foldable flushable))

(defknown scale-single-float (single-float integer) single-float
  (movable foldable flushable fixed-args unboxed-return))
(defknown scale-double-float (double-float integer) double-float
    (movable foldable flushable fixed-args unboxed-return))

(defknown sb-kernel::scale-single-float-maybe-overflow
    (single-float integer) single-float
    (movable foldable flushable fixed-args unboxed-return))
(defknown sb-kernel::scale-single-float-maybe-underflow
    (single-float integer) single-float
  (movable foldable flushable fixed-args unboxed-return))
(defknown sb-kernel::scale-double-float-maybe-overflow
    (double-float integer) double-float
    (movable foldable flushable fixed-args unboxed-return))
(defknown sb-kernel::scale-double-float-maybe-underflow
    (double-float integer) double-float
    (movable foldable flushable fixed-args unboxed-return))

(deftransform decode-float ((x) (single-float) *)
  '(decode-single-float x))

(deftransform decode-float ((x) (double-float) *)
  '(decode-double-float x))

(deftransform integer-decode-float ((x) (single-float) *)
  '(integer-decode-single-float x))

(deftransform integer-decode-float ((x) (double-float) *)
  '(integer-decode-double-float x))

(deftransform scale-float ((f ex) (single-float t) *)
  (cond #+(and x86 ()) ;; this producess different results based on whether it's inlined or not
        ((csubtypep (lvar-type ex)
                    (specifier-type '(signed-byte 32)))
         '(coerce (%scalbn (coerce f 'double-float) ex) 'single-float))
        (t
         '(scale-single-float f ex))))

(deftransform scale-float ((f ex) (double-float t) *)
  (cond #+(and x86 ())
        ((csubtypep (lvar-type ex)
                    (specifier-type '(signed-byte 32)))
         '(%scalbn f ex))
        (t
         '(scale-double-float f ex))))

;;; Given a number X, create a form suitable as a bound for an
;;; interval. Make the bound open if OPEN-P is T. NIL remains NIL.
;;; FIXME: as this is a constructor, shouldn't it be named MAKE-BOUND?
(declaim (inline set-bound))
(defun set-bound (x open-p)
  (if (and x open-p) (list x) x))

;;; optimizers for SCALE-FLOAT. If the float has bounds, new bounds
;;; are computed for the result, if possible.
(defun scale-float-derive-type-aux (f ex same-arg)
  (declare (ignore same-arg))
  (flet ((scale-bound (x n)
           ;; We need to be a bit careful here and catch any overflows
           ;; that might occur. We can ignore underflows which become
           ;; zeros.
           (set-bound
            (handler-case
                (scale-float (type-bound-number x) n)
              (floating-point-overflow ()
                nil))
            (consp x))))
    (when (and (numeric-type-p f) (numeric-type-p ex))
      (let ((f-lo (numeric-type-low f))
            (f-hi (numeric-type-high f))
            (ex-lo (numeric-type-low ex))
            (ex-hi (numeric-type-high ex))
            (new-lo nil)
            (new-hi nil))
        (when f-hi
          (if (sb-xc:< (float-sign (type-bound-number f-hi)) 0.0)
              (when ex-lo
                (setf new-hi (scale-bound f-hi ex-lo)))
              (when ex-hi
                (setf new-hi (scale-bound f-hi ex-hi)))))
        (when f-lo
          (if (sb-xc:< (float-sign (type-bound-number f-lo)) 0.0)
              (when ex-hi
                (setf new-lo (scale-bound f-lo ex-hi)))
              (when ex-lo
                (setf new-lo (scale-bound f-lo ex-lo)))))
        (make-numeric-type :class (numeric-type-class f)
                           :format (numeric-type-format f)
                           :complexp :real
                           :low new-lo
                           :high new-hi)))))
(defoptimizer (scale-single-float derive-type) ((f ex))
  (two-arg-derive-type f ex #'scale-float-derive-type-aux
                       #'scale-single-float))
(defoptimizer (scale-double-float derive-type) ((f ex))
  (two-arg-derive-type f ex #'scale-float-derive-type-aux
                       #'scale-double-float))

;;; DEFOPTIMIZERs for %SINGLE-FLOAT and %DOUBLE-FLOAT. This makes the
;;; FLOAT function return the correct ranges if the input has some
;;; defined range. Quite useful if we want to convert some type of
;;; bounded integer into a float.
(macrolet
    ((frob (fun type most-negative most-positive)
       (let ((aux-name (symbolicate fun "-DERIVE-TYPE-AUX")))
         `(progn
            (defun ,aux-name (num)
              ;; When converting a number to a float, the limits are
              ;; the same.
              (let* ((lo (bound-func (lambda (x)
                                       (if (sb-xc:< x ,most-negative)
                                           ,most-negative
                                           (coerce x ',type)))
                                     (numeric-type-low num)
                                     nil))
                     (hi (bound-func (lambda (x)
                                       (if (sb-xc:< ,most-positive x )
                                           ,most-positive
                                           (coerce x ',type)))
                                     (numeric-type-high num)
                                     nil)))
                (specifier-type `(,',type ,(or lo '*) ,(or hi '*)))))

            (defoptimizer (,fun derive-type) ((num))
              (handler-case
                  (one-arg-derive-type num #',aux-name #',fun)
                (type-error ()
                  nil)))))))
  (frob %single-float single-float
        most-negative-single-float most-positive-single-float)
  (frob %double-float double-float
        most-negative-double-float most-positive-double-float))

(defoptimizer (float derive-type) ((number prototype))
  (let ((type (lvar-type prototype)))
    (unless (or (csubtypep type (specifier-type 'double-float))
                (csubtypep type (specifier-type 'single-float)))
      (handler-case
          (type-union
           (one-arg-derive-type number #'%single-float-derive-type-aux #'%single-float)
           (one-arg-derive-type number #'%double-float-derive-type-aux #'%double-float))
        (type-error ()
          nil)))))

(macrolet ((def (type &rest args)
             `(deftransform * ((x y) (,type (constant-arg (member ,@args))) *
                               ;; Beware the SNaN!
                               :policy (zerop float-accuracy))
                "optimize multiplication by one"
                (let ((y (lvar-value y)))
                  (if (minusp y)
                      '(%negate x)
                      'x)))))
  (def single-float 1.0 -1.0)
  (def double-float 1.0d0 -1.0d0))

;;; Return the reciprocal of X if it can be represented exactly, NIL otherwise.
(defun maybe-exact-reciprocal (x)
  (unless (zerop x)
    (handler-case
        (multiple-value-bind (significand exponent sign)
            (integer-decode-float x)
          ;; only powers of 2 can be inverted exactly
          (unless (zerop (logand significand (1- significand)))
            (return-from maybe-exact-reciprocal nil))
          (let ((expected   (/ sign significand (expt 2 exponent)))
                (reciprocal (/ x)))
            (multiple-value-bind (significand exponent sign)
                (integer-decode-float reciprocal)
              ;; Denorms can't be inverted safely.
              (and (eql expected (* sign significand (expt 2 exponent)))
                   reciprocal))))
      (error () (return-from maybe-exact-reciprocal nil)))))

;;; Replace constant division by multiplication with exact reciprocal,
;;; if one exists.
(macrolet ((def (type)
             `(deftransform / ((x y) (,type (constant-arg ,type)) *
                               :node node)
                "convert to multiplication by reciprocal"
                (let ((n (lvar-value y)))
                  (if (policy node (zerop float-accuracy))
                      `(* x ,(/ n))
                      (let ((r (maybe-exact-reciprocal n)))
                        (if r
                            `(* x ,r)
                            (give-up-ir1-transform
                             "~S does not have an exact reciprocal"
                             n))))))))
  (def single-float)
  (def double-float))

;;; Optimize addition and subtraction of zero
(macrolet ((def (op type &rest args)
             `(deftransform ,op ((x y) (,type (constant-arg (member ,@args))) *
                                 ;; Beware the SNaN!
                                 :policy (zerop float-accuracy))
                'x)))
  ;; No signed zeros, thanks.
  (def + single-float 0 0.0)
  (def - single-float 0 0.0)
  (def + double-float 0 0.0 0.0d0)
  (def - double-float 0 0.0 0.0d0))

;;; On most platforms (+ x x) is faster than (* x 2)
(macrolet ((def (type &rest args)
             `(deftransform * ((x y) (,type (constant-arg (member ,@args))))
                '(+ x x))))
  (def single-float 2 2.0)
  (def double-float 2 2.0 2.0d0))

;;; Prevent ZEROP, PLUSP, and MINUSP from losing horribly. We can't in
;;; general float rational args to comparison, since Common Lisp
;;; semantics says we are supposed to compare as rationals, but we can
;;; do it for any rational that has a precise representation as a
;;; float (such as 0).
(macrolet ((frob (op &optional complex)
             `(deftransform ,op ((x y) (:or ((,(if complex
                                                   '(complex single-float)
                                                   'single-float)
                                              rational) *)
                                            ((,(if complex
                                                   '(complex double-float)
                                                   'double-float)
                                              rational) *)))
                "open-code FLOAT to RATIONAL comparison"
                (unless (constant-lvar-p y)
                  (give-up-ir1-transform
                   "The RATIONAL value isn't known at compile time."))
                (let ((val (lvar-value y)))
                  (multiple-value-bind (low high type)
                      (if (csubtypep (lvar-type x) (specifier-type 'double-float))
                          (values most-negative-double-float most-positive-double-float
                                  'double-float)
                          (values most-negative-single-float most-positive-single-float
                                  'single-float))
                    (unless (and (sb-xc:<= low val high)
                                 (eql (rational (coerce val type)) val))
                      (give-up-ir1-transform
                       "~S doesn't have a precise float representation."
                       val))))
                `(,',op x (float y ,',(if complex
                                          '(realpart x)
                                          'x))))))
  (frob <)
  (frob >)
  (frob <=)
  (frob >=)
  (frob =)
  (frob = t))


;;;; irrational transforms

(macrolet ((def (name prim rtype)
             `(progn
               (deftransform ,name ((x) (single-float) ,rtype :node node)
                 (delay-ir1-transform node :ir1-phases)
                 `(%single-float (,',prim (%double-float x))))
               (deftransform ,name ((x) (double-float) ,rtype :node node)
                 (delay-ir1-transform node :ir1-phases)
                 `(,',prim x)))))
  (def exp %exp *)
  (def log %log float)
  (def sqrt %sqrt float)
  (def asin %asin float)
  (def acos %acos float)
  (def atan %atan *)
  (def sinh %sinh *)
  (def cosh %cosh *)
  (def tanh %tanh *)
  (def asinh %asinh *)
  (def acosh %acosh float)
  (def atanh %atanh float))

;;; The argument range is limited on the x86 FP trig. functions. A
;;; post-test can detect a failure (and load a suitable result), but
;;; this test is avoided if possible.
(macrolet ((def (name prim prim-quick)
             (declare (ignorable prim-quick))
             `(progn
                (deftransform ,name ((x) (single-float) *)
                  #+x86 (cond ((csubtypep (lvar-type x)
                                          (specifier-type
                                           `(single-float (,(sb-xc:- (expt 2f0 63)))
                                                          (,(expt 2f0 63)))))
                                `(coerce (,',prim-quick (coerce x 'double-float))
                                  'single-float))
                               (t
                                (compiler-notify
                                 "unable to avoid inline argument range check~@
                                  because the argument range (~S) was not within 2^63"
                                 (type-specifier (lvar-type x)))
                                `(coerce (,',prim (coerce x 'double-float)) 'single-float)))
                  #-x86 `(coerce (,',prim (coerce x 'double-float)) 'single-float))
               (deftransform ,name ((x) (double-float) *)
                 #+x86 (cond ((csubtypep (lvar-type x)
                                         (specifier-type
                                          `(double-float (,(sb-xc:- (expt 2d0 63)))
                                                         (,(expt 2d0 63)))))
                               `(,',prim-quick x))
                              (t
                               (compiler-notify
                                "unable to avoid inline argument range check~@
                                 because the argument range (~S) was not within 2^63"
                                (type-specifier (lvar-type x)))
                               `(,',prim x)))
                 #-x86 `(,',prim x)))))
  (def sin %sin %sin-quick)
  (def cos %cos %cos-quick)
  (def tan %tan %tan-quick))

(deftransform atan ((x y) (single-float single-float) *)
  `(coerce (%atan2 (coerce x 'double-float) (coerce y 'double-float))
    'single-float))
(deftransform atan ((x y) (double-float double-float) *)
  `(%atan2 x y))

(deftransform expt ((x y) (single-float single-float) single-float)
  `(coerce (%pow (coerce x 'double-float) (coerce y 'double-float))
           'single-float))
(deftransform expt ((x y) (double-float double-float) double-float)
  `(%pow x y))
(deftransform expt ((x y) (single-float integer) single-float)
  `(coerce (%pow (coerce x 'double-float) (coerce y 'double-float))
    'single-float))
(deftransform expt ((x y) (double-float integer) double-float)
  `(%pow x (coerce y 'double-float)))

;;; ANSI says log with base zero returns zero.
(deftransform log ((x y) (float float) float)
  '(if (zerop y) y (/ (log x) (log y))))

;;; Handle some simple transformations.

(deftransform abs ((x) ((complex double-float)) double-float)
  '(%hypot (realpart x) (imagpart x)))

(deftransform abs ((x) ((complex single-float)) single-float)
  '(coerce (%hypot (coerce (realpart x) 'double-float)
                   (coerce (imagpart x) 'double-float))
          'single-float))

(deftransform phase ((x) ((complex double-float)) double-float)
  '(%atan2 (imagpart x) (realpart x)))

(deftransform phase ((x) ((complex single-float)) single-float)
  '(coerce (%atan2 (coerce (imagpart x) 'double-float)
                   (coerce (realpart x) 'double-float))
          'single-float))

(deftransform phase ((x) ((float)) float)
  '(if (minusp (float-sign x))
       (float pi x)
       (float 0 x)))

;;; The number is of type REAL.
(defun numeric-type-real-p (type)
  (and (numeric-type-p type)
       (eq (numeric-type-complexp type) :real)))

;;;; optimizers for elementary functions
;;;;
;;;; These optimizers compute the output range of the elementary
;;;; function, based on the domain of the input.

;;; Generate a specifier for a complex type specialized to the same
;;; type as the argument.
(defun complex-float-type (arg)
  (declare (type numeric-type arg))
  (let* ((format (case (numeric-type-class arg)
                   ((integer rational) 'single-float)
                   (t (numeric-type-format arg))))
         (float-type (or format 'float)))
    (specifier-type `(complex ,float-type))))

;;; Compute a specifier like '(OR FLOAT (COMPLEX FLOAT)), except float
;;; should be the right kind of float. Allow bounds for the float
;;; part too.
(defun float-or-complex-float-type (arg &optional lo hi)
  (cond
    ((numeric-type-p arg)
     (let* ((format (case (numeric-type-class arg)
                      ((integer rational) 'single-float)
                      (t (numeric-type-format arg))))
            (float-type (or format 'float))
            (lo (coerce-numeric-bound lo float-type))
            (hi (coerce-numeric-bound hi float-type)))
       (specifier-type `(or (,float-type ,(or lo '*) ,(or hi '*))
                            (complex ,float-type)))))
    ((union-type-p arg)
     (apply #'type-union
            (loop for type in (union-type-types arg)
                  collect (float-or-complex-float-type type lo hi))))
    (t (specifier-type 'number))))

;;; Test whether the numeric-type ARG is within the domain specified by
;;; DOMAIN-LOW and DOMAIN-HIGH, consider negative and positive zero to
;;; be distinct.
(defun domain-subtypep (arg domain-low domain-high)
  (declare (type numeric-type arg)
           (type (or real null) domain-low domain-high))
  (let* ((arg-lo (numeric-type-low arg))
         (arg-lo-val (type-bound-number arg-lo))
         (arg-hi (numeric-type-high arg))
         (arg-hi-val (type-bound-number arg-hi)))
    ;; Check that the ARG bounds are correctly canonicalized.
    (when (and arg-lo (floatp arg-lo-val) (zerop arg-lo-val) (consp arg-lo)
               (minusp (float-sign arg-lo-val)))
      (setf arg-lo
            (typecase arg-lo-val
              (single-float 0f0)
              (double-float 0d0)
              #+long-float
              (long-float 0l0))
            arg-lo-val arg-lo))
    (when (and arg-hi (zerop arg-hi-val) (floatp arg-hi-val) (consp arg-hi)
               (plusp (float-sign arg-hi-val)))
      (setf arg-hi
            (typecase arg-lo-val
              (single-float -0f0)
              (double-float -0d0)
              #+long-float
              (long-float   -0L0))
            arg-hi-val arg-hi))
    (flet ((fp-neg-zero-p (f)           ; Is F -0.0?
             (and (floatp f) (zerop f) (float-sign-bit-set-p f)))
           (fp-pos-zero-p (f)           ; Is F +0.0?
             (and (floatp f) (zerop f) (not (float-sign-bit-set-p f)))))
      (and (or (null domain-low)
               (and arg-lo (sb-xc:>= arg-lo-val domain-low)
                    (not (and (fp-pos-zero-p domain-low)
                              (fp-neg-zero-p arg-lo)))))
           (or (null domain-high)
               (and arg-hi (sb-xc:<= arg-hi-val domain-high)
                    (not (and (fp-neg-zero-p domain-high)
                              (fp-pos-zero-p arg-hi)))))))))


;;; Handle monotonic functions of a single variable whose domain is
;;; possibly part of the real line. ARG is the variable, FUN is the
;;; function, and DOMAIN is a specifier that gives the (real) domain
;;; of the function. If ARG is a subset of the DOMAIN, we compute the
;;; bounds directly. Otherwise, we compute the bounds for the
;;; intersection between ARG and DOMAIN, and then append a complex
;;; result, which occurs for the parts of ARG not in the DOMAIN.
;;;
;;; Negative and positive zero are considered distinct within
;;; DOMAIN-LOW and DOMAIN-HIGH.
;;;
;;; DEFAULT-LOW and DEFAULT-HIGH are the lower and upper bounds if we
;;; can't compute the bounds using FUN.
(defun elfun-derive-type-simple (arg fun domain-low domain-high
                                     default-low default-high
                                     &optional (increasingp t))
  (declare (type (or null real) domain-low domain-high))
  (etypecase arg
    (numeric-type
     (cond ((eq (numeric-type-complexp arg) :complex)
            (complex-float-type arg))
           ((numeric-type-real-p arg)
            ;; The argument is real, so let's find the intersection
            ;; between the argument and the domain of the function.
            ;; We compute the bounds on the intersection, and for
            ;; everything else, we return a complex number of the
            ;; appropriate type.
            (multiple-value-bind (intersection difference)
                (interval-intersection/difference (numeric-type->interval arg)
                                                  (make-interval
                                                   :low domain-low
                                                   :high domain-high))
              (cond
                (intersection
                 ;; Process the intersection.
                 (let* ((low (interval-low intersection))
                        (high (interval-high intersection))
                        (res-lo (or (bound-func fun (if increasingp low high) nil)
                                    default-low))
                        (res-hi (or (bound-func fun (if increasingp high low) nil)
                                    default-high))
                        (format (case (numeric-type-class arg)
                                  ((integer rational) 'single-float)
                                  (t (numeric-type-format arg))))
                        (bound-type (or format 'float))
                        (result-type
                         (make-numeric-type
                          :class 'float
                          :format format
                          :low (coerce-numeric-bound res-lo bound-type)
                          :high (coerce-numeric-bound res-hi bound-type))))
                   ;; If the ARG is a subset of the domain, we don't
                   ;; have to worry about the difference, because that
                   ;; can't occur.
                   (if (or (null difference)
                           ;; Check whether the arg is within the domain.
                           (domain-subtypep arg domain-low domain-high))
                       result-type
                       (list result-type
                             (specifier-type `(complex ,bound-type))))))
                (t
                 ;; No intersection so the result must be purely complex.
                 (complex-float-type arg)))))
           (t
            (float-or-complex-float-type arg default-low default-high))))))

(macrolet
    ((frob (name domain-low domain-high def-low-bnd def-high-bnd
                 &key (increasingp t))
       (let ((num (gensym)))
         `(defoptimizer (,name derive-type) ((,num))
           (one-arg-derive-type
            ,num
            (lambda (arg)
              (elfun-derive-type-simple arg #',name
                                        ,domain-low ,domain-high
                                        ,def-low-bnd ,def-high-bnd
                                        ,increasingp))
            #',name)))))
  ;; These functions are easy because they are defined for the whole
  ;; real line.
  (frob exp nil nil 0 nil)
  (frob sinh nil nil nil nil)
  (frob tanh nil nil -1 1)
  (frob asinh nil nil nil nil)

  ;; These functions are only defined for part of the real line. The
  ;; condition selects the desired part of the line.
  (frob asin -1d0 1d0 (sb-xc:- (sb-xc:/ pi 2)) (sb-xc:/ pi 2))
  ;; Acos is monotonic decreasing, so we need to swap the function
  ;; values at the lower and upper bounds of the input domain.
  (frob acos -1d0 1d0 0 pi :increasingp nil)
  (frob acosh 1d0 nil nil nil)
  (frob atanh -1d0 1d0 -1 1)
  ;; Kahan says that (sqrt -0.0) is -0.0, so use a specifier that
  ;; includes -0.0.
  (frob sqrt -0.0d0 nil 0 nil))

;;; Compute bounds for (expt x y). This should be easy since (expt x
;;; y) = (exp (* y (log x))). However, computations done this way
;;; have too much roundoff. Thus we have to do it the hard way.
(defun safe-expt (x y)
  (when (and (numberp x) (numberp y))
    (handler-case
        (when (sb-xc:< (abs y) 10000)
          (expt x y))
      ;; Currently we can hide unanticipated errors (such as failure to use SB-XC: math
      ;; when cross-compiling) as well as the anticipated potential problem of overflow.
      ;; So don't handle anything when cross-compiling.
      ;; FIXME: I think this should not handle ERROR, but just FLOATING-POINT-OVERFLOW.
      (#+sb-xc-host nil
       #-sb-xc-host error ()
        nil))))

;;; Handle the case when x >= 1.
(defun interval-expt-> (x y)
  (case (interval-range-info y 0d0)
    (+
     ;; Y is positive and log X >= 0. The range of exp(y * log(x)) is
     ;; obviously non-negative. We just have to be careful for
     ;; infinite bounds (given by nil).
     (let ((lo (safe-expt (type-bound-number (interval-low x))
                          (type-bound-number (interval-low y))))
           (hi (safe-expt (type-bound-number (interval-high x))
                          (type-bound-number (interval-high y)))))
       (list (make-interval :low (or lo 1) :high hi))))
    (-
     ;; Y is negative and log x >= 0. The range of exp(y * log(x)) is
     ;; obviously [0, 1]. However, underflow (nil) means 0 is the
     ;; result.
     (let ((lo (safe-expt (type-bound-number (interval-high x))
                          (type-bound-number (interval-low y))))
           (hi (safe-expt (type-bound-number (interval-low x))
                          (type-bound-number (interval-high y)))))
       (list (make-interval :low (or lo 0) :high (or hi 1)))))
    (t
     ;; Split the interval in half.
     (destructuring-bind (y- y+)
         (interval-split 0 y t)
       (list (interval-expt-> x y-)
             (interval-expt-> x y+))))))

;;; Handle the case when 0 <= x <= 1
(defun interval-expt-< (x y)
  (case (interval-range-info x 0d0)
    (+
     ;; The case of 0 <= x <= 1 is easy
     (case (interval-range-info y)
       (+
        ;; Y is positive and log X <= 0. The range of exp(y * log(x)) is
        ;; obviously [0, 1]. We just have to be careful for infinite bounds
        ;; (given by nil).
        (let ((lo (safe-expt (type-bound-number (interval-low x))
                             (type-bound-number (interval-high y))))
              (hi (safe-expt (type-bound-number (interval-high x))
                             (type-bound-number (interval-low y)))))
          (list (make-interval :low (or lo 0) :high (or hi 1)))))
       (-
        ;; Y is negative and log x <= 0. The range of exp(y * log(x)) is
        ;; obviously [1, inf].
        (let ((hi (safe-expt (type-bound-number (interval-low x))
                             (type-bound-number (interval-low y))))
              (lo (safe-expt (type-bound-number (interval-high x))
                             (type-bound-number (interval-high y)))))
          (list (make-interval :low (or lo 1) :high hi))))
       (t
        ;; Split the interval in half
        (destructuring-bind (y- y+)
            (interval-split 0 y t)
          (list (interval-expt-< x y-)
                (interval-expt-< x y+))))))
    (-
     ;; The case where x <= 0. Y MUST be an INTEGER for this to work!
     ;; The calling function must insure this!
     (loop for interval in (flatten-list (interval-expt (interval-neg x) y))
           for low = (interval-low interval)
           for high = (interval-high interval)
           collect interval
           when (or high low)
           collect (interval-neg interval)))
    (t
     (destructuring-bind (neg pos)
         (interval-split 0 x t t)
       (list (interval-expt-< neg y)
             (interval-expt-< pos y))))))

;;; Compute bounds for (expt x y).
(defun interval-expt (x y)
  (case (interval-range-info x 1)
    (+
     ;; X >= 1
         (interval-expt-> x y))
    (-
     ;; X <= 1
     (interval-expt-< x y))
    (t
     (destructuring-bind (left right)
         (interval-split 1 x t t)
       (list (interval-expt left y)
             (interval-expt right y))))))

(defun fixup-interval-expt (bnd x-int y-int x-type y-type)
  (declare (ignore x-int))
  ;; Figure out what the return type should be, given the argument
  ;; types and bounds and the result type and bounds.
  (cond ((csubtypep x-type (specifier-type 'integer))
         ;; an integer to some power
         (case (numeric-type-class y-type)
           (integer
            ;; Positive integer to an integer power is either an
            ;; integer or a rational.
            (let ((lo (or (interval-low bnd) '*))
                  (hi (or (interval-high bnd) '*))
                  (y-lo (interval-low y-int))
                  (y-hi (interval-high y-int)))
              (cond ((and (eq lo '*)
                          (eql y-lo y-hi)
                          (typep y-lo 'unsigned-byte)
                          (evenp y-lo))
                     (specifier-type `(integer 0 ,hi)))
                    ((and (interval-low y-int)
                          (>= (type-bound-number y-lo) 0))

                     (specifier-type `(integer ,lo ,hi)))
                    (t
                     (specifier-type `(rational ,lo ,hi))))))
           (rational
            ;; Positive integer to rational power is either a rational
            ;; or a single-float.
            (let* ((lo (interval-low bnd))
                   (hi (interval-high bnd))
                   (int-lo (if lo
                               (floor (type-bound-number lo))
                               '*))
                   (int-hi (if hi
                               (ceiling (type-bound-number hi))
                               '*))
                   (f-lo (or (bound-func #'float lo nil)
                             '*))
                   (f-hi (or (bound-func #'float hi nil)
                             '*)))
              (specifier-type `(or (rational ,int-lo ,int-hi)
                                (single-float ,f-lo, f-hi)))))
           (float
            ;; A positive integer to a float power is a float.
            (let ((format (numeric-type-format y-type)))
              (aver format)
              (modified-numeric-type
               y-type
               :low (coerce-numeric-bound (interval-low bnd) format)
               :high (coerce-numeric-bound (interval-high bnd) format))))
           (t
            ;; A positive integer to a number is a number (for now).
            (specifier-type 'number))))
        ((csubtypep x-type (specifier-type 'rational))
         ;; a rational to some power
         (case (numeric-type-class y-type)
           (integer
            ;; A positive rational to an integer power is always a rational.
            (specifier-type `(rational ,(or (interval-low bnd) '*)
                                       ,(or (interval-high bnd) '*))))
           (rational
            ;; A positive rational to rational power is either a rational
            ;; or a single-float.
            (let* ((lo (interval-low bnd))
                   (hi (interval-high bnd))
                   (int-lo (if lo
                               (floor (type-bound-number lo))
                               '*))
                   (int-hi (if hi
                               (ceiling (type-bound-number hi))
                               '*))
                   (f-lo (or (bound-func #'float lo nil)
                             '*))
                   (f-hi (or (bound-func #'float hi nil)
                             '*)))
              (specifier-type `(or (rational ,int-lo ,int-hi)
                                (single-float ,f-lo, f-hi)))))
           (float
            ;; A positive rational to a float power is a float.
            (let ((format (numeric-type-format y-type)))
              (aver format)
              (modified-numeric-type
               y-type
               :low (coerce-numeric-bound (interval-low bnd) format)
               :high (coerce-numeric-bound (interval-high bnd) format))))
           (t
            ;; A positive rational to a number is a number (for now).
            (specifier-type 'number))))
        ((csubtypep x-type (specifier-type 'float))
         ;; a float to some power
         (case (numeric-type-class y-type)
           ((or integer rational)
            ;; A positive float to an integer or rational power is
            ;; always a float.
            (let ((format (numeric-type-format x-type)))
              (aver format)
              (make-numeric-type
               :class 'float
               :format format
               :low (coerce-numeric-bound (interval-low bnd) format)
               :high (coerce-numeric-bound (interval-high bnd) format))))
           (float
            ;; A positive float to a float power is a float of the
            ;; higher type.
            (let ((format (float-format-max (numeric-type-format x-type)
                                            (numeric-type-format y-type))))
              (aver format)
              (make-numeric-type
               :class 'float
               :format format
               :low (coerce-numeric-bound (interval-low bnd) format)
               :high (coerce-numeric-bound (interval-high bnd) format))))
           (t
            ;; A positive float to a number is a number (for now)
            (specifier-type 'number))))
        (t
         ;; A number to some power is a number.
         (specifier-type 'number))))

(defun merged-interval-expt (x y)
  (let* ((x-int (numeric-type->interval x))
         (y-int (numeric-type->interval y)))
    (mapcar (lambda (type)
              (fixup-interval-expt type x-int y-int x y))
            (flatten-list (interval-expt x-int y-int)))))

(defun integer-float-p (float)
  (and (floatp float)
       (multiple-value-bind (significand exponent) (integer-decode-float float)
         (or (plusp exponent)
             (<= (- exponent) (sb-kernel::first-bit-set significand))))))

(defun expt-derive-type-aux (x y same-arg)
  (declare (ignore same-arg))
  (cond ((or (not (numeric-type-real-p x))
             (not (numeric-type-real-p y)))
         ;; Use numeric contagion if either is not real.
         (numeric-contagion x y))
        ((or (csubtypep y (specifier-type 'integer))
             (integer-float-p (nth-value 1 (type-singleton-p y))))
         ;; A real raised to an integer power is well-defined.
         (merged-interval-expt x y))
        ;; A real raised to a non-integral power can be a float or a
        ;; complex number.
        ((csubtypep x (specifier-type '(real 0)))
         ;; But a positive real to any power is well-defined.
         (merged-interval-expt x y))
        ((and (csubtypep x (specifier-type 'rational))
              (csubtypep y (specifier-type 'rational)))
         ;; A rational to the power of a rational could be a rational
         ;; or a possibly-complex single float
         (specifier-type '(or rational single-float (complex single-float))))
        (t
         ;; a real to some power. The result could be a real or a
         ;; complex.
         (float-or-complex-float-type (numeric-contagion x y)))))

(defoptimizer (expt derive-type) ((x y))
  (two-arg-derive-type x y #'expt-derive-type-aux #'expt))

;;; Note we must assume that a type including 0.0 may also include
;;; -0.0 and thus the result may be complex -infinity + i*pi.
(defun log-derive-type-aux-1 (x)
  (elfun-derive-type-simple x #'log 0d0 nil
                            ;; (log 0) is an error
                            ;; and there's nothing between 0 and 1 for integers.
                            (and (integer-type-p x)
                                 0f0)
                            nil))

(defun log-derive-type-aux-2 (x y same-arg)
  (let ((log-x (log-derive-type-aux-1 x))
        (log-y (log-derive-type-aux-1 y))
        (accumulated-list nil))
    ;; LOG-X or LOG-Y might be union types. We need to run through
    ;; the union types ourselves because /-DERIVE-TYPE-AUX doesn't.
    (dolist (x-type (prepare-arg-for-derive-type log-x))
      (dolist (y-type (prepare-arg-for-derive-type log-y))
        (push (/-derive-type-aux x-type y-type same-arg) accumulated-list)))
    (apply #'type-union (flatten-list accumulated-list))))

(defoptimizer (log derive-type) ((x &optional y))
  (if y
      (two-arg-derive-type x y #'log-derive-type-aux-2 #'log)
      (one-arg-derive-type x #'log-derive-type-aux-1 #'log)))

(defun atan-derive-type-aux-1 (y)
  (elfun-derive-type-simple y #'atan nil nil (sb-xc:- (sb-xc:/ pi 2)) (sb-xc:/ pi 2)))

(defun atan-derive-type-aux-2 (y x same-arg)
  (declare (ignore same-arg))
  ;; The hard case with two args. We just return the max bounds.
  (let ((result-type (numeric-contagion y x)))
    (cond ((and (numeric-type-real-p x)
                (numeric-type-real-p y))
           (let* (;; FIXME: This expression for FORMAT seems to
                  ;; appear multiple times, and should be factored out.
                  (format (case (numeric-type-class result-type)
                            ((integer rational) 'single-float)
                            (t (numeric-type-format result-type))))
                  (bound-format (or format 'float)))
             (make-numeric-type :class 'float
                                :format format
                                :complexp :real
                                :low (coerce (sb-xc:- pi) bound-format)
                                :high (coerce pi bound-format))))
          (t
           ;; The result is a float or a complex number
           (float-or-complex-float-type result-type)))))

(defoptimizer (atan derive-type) ((y &optional x))
  (if x
      (two-arg-derive-type y x #'atan-derive-type-aux-2 #'atan)
      (one-arg-derive-type y #'atan-derive-type-aux-1 #'atan)))

(defun cosh-derive-type-aux (x)
  ;; We note that cosh x = cosh |x| for all real x.
  (elfun-derive-type-simple
   (if (numeric-type-real-p x)
       (abs-derive-type-aux x)
       x)
   #'cosh nil nil 0 nil))

(defoptimizer (cosh derive-type) ((num))
  (one-arg-derive-type num #'cosh-derive-type-aux #'cosh))

(defun phase-derive-type-aux (arg)
  (let* ((format (case (numeric-type-class arg)
                   ((integer rational) 'single-float)
                   (t (numeric-type-format arg))))
         (bound-type (or format 'float)))
    (cond ((numeric-type-real-p arg)
           (case (interval-range-info> (numeric-type->interval arg) 0.0)
             (+
              ;; The number is positive, so the phase is 0.
              (make-numeric-type :class 'float
                                 :format format
                                 :complexp :real
                                 :low (coerce 0 bound-type)
                                 :high (coerce 0 bound-type)))
             (-
              ;; The number is always negative, so the phase is pi.
              (make-numeric-type :class 'float
                                 :format format
                                 :complexp :real
                                 :low (coerce pi bound-type)
                                 :high (coerce pi bound-type)))
             (t
              ;; We can't tell. The result is 0 or pi. Use a union
              ;; type for this.
              (list
               (make-numeric-type :class 'float
                                  :format format
                                  :complexp :real
                                  :low (coerce 0 bound-type)
                                  :high (coerce 0 bound-type))
               (make-numeric-type :class 'float
                                  :format format
                                  :complexp :real
                                  :low (coerce pi bound-type)
                                  :high (coerce pi bound-type))))))
          (t
           ;; We have a complex number. The answer is the range -pi
           ;; to pi. (-pi is included because we have -0.)
           (make-numeric-type :class 'float
                              :format format
                              :complexp :real
                              :low (coerce (sb-xc:- pi) bound-type)
                              :high (coerce pi bound-type))))))

(defoptimizer (phase derive-type) ((num))
  (one-arg-derive-type num #'phase-derive-type-aux #'phase))

(deftransform realpart ((x) ((complex rational)) * :important nil)
  '(%realpart x))
(deftransform imagpart ((x) ((complex rational)) * :important nil)
  '(%imagpart x))

(deftransform realpart ((x) (real) * :important nil)
  'x)
(deftransform imagpart ((x) ((and single-float (not (eql -0f0)))) * :important nil)
  0f0)
(deftransform imagpart ((x) ((and double-float (not (eql -0d0)))) * :important nil)
  0d0)

;;; Make REALPART and IMAGPART return the appropriate types. This
;;; should help a lot in optimized code.
(defun realpart-derive-type-aux (type)
  (let ((class (numeric-type-class type))
        (format (numeric-type-format type)))
    (cond ((numeric-type-real-p type)
           ;; The realpart of a real has the same type and range as
           ;; the input.
           (make-numeric-type :class class
                              :format format
                              :complexp :real
                              :low (numeric-type-low type)
                              :high (numeric-type-high type)))
          (t
           ;; We have a complex number. The result has the same type
           ;; as the real part, except that it's real, not complex,
           ;; obviously.
           (make-numeric-type :class class
                              :format format
                              :complexp :real
                              :low (numeric-type-low type)
                              :high (numeric-type-high type))))))

(defoptimizer (realpart derive-type) ((num))
  (one-arg-derive-type num #'realpart-derive-type-aux #'realpart))

(defun imagpart-derive-type-aux (type)
  (let ((class (numeric-type-class type))
        (format (numeric-type-format type)))
    (cond ((numeric-type-real-p type)
           ;; The imagpart of a real has the same type as the input,
           ;; except that it's zero.
           (let ((bound-format (or format class 'real)))
             (make-numeric-type :class class
                                :format format
                                :complexp :real
                                :low (coerce 0 bound-format)
                                :high (coerce 0 bound-format))))
          (t
           ;; We have a complex number. The result has the same type as
           ;; the imaginary part, except that it's real, not complex,
           ;; obviously.
           (make-numeric-type :class class
                              :format format
                              :complexp :real
                              :low (numeric-type-low type)
                              :high (numeric-type-high type))))))

(defoptimizer (imagpart derive-type) ((num))
  (one-arg-derive-type num #'imagpart-derive-type-aux #'imagpart))

(defun complex-derive-type-aux-1 (re-type)
  (if (numeric-type-p re-type)
      (make-numeric-type :class (numeric-type-class re-type)
                         :format (numeric-type-format re-type)
                         :complexp (if (csubtypep re-type
                                                  (specifier-type 'rational))
                                       :real
                                       :complex)
                         :low (numeric-type-low re-type)
                         :high (numeric-type-high re-type))
      (specifier-type 'complex)))

(defun complex-derive-type-aux-2 (re-type im-type same-arg)
  (declare (ignore same-arg))
  (if (and (numeric-type-p re-type)
           (numeric-type-p im-type))
      ;; Need to check to make sure numeric-contagion returns the
      ;; right type for what we want here.

      ;; Also, what about rational canonicalization, like (complex 5 0)
      ;; is 5?  So, if the result must be complex, we make it so.
      ;; If the result might be complex, which happens only if the
      ;; arguments are rational, we make it a union type of (or
      ;; rational (complex rational)).
      (let* ((element-type (numeric-contagion re-type im-type))
             (maybe-rat-result-p (types-equal-or-intersect
                                  element-type (specifier-type 'rational)))
             (definitely-rat-result-p (csubtypep element-type (specifier-type 'rational)))
             (real-result-p (and definitely-rat-result-p
                                 (csubtypep im-type (specifier-type '(eql 0))))))
        (cond
          (real-result-p re-type)
          (maybe-rat-result-p
           (type-union element-type
                       (specifier-type
                        `(complex ,(numeric-type-class element-type)))))
          (t
           (make-numeric-type :class (numeric-type-class element-type)
                              :format (numeric-type-format element-type)
                              :complexp (if definitely-rat-result-p
                                            :real
                                            :complex)))))
      (specifier-type 'complex)))

(defoptimizer (complex derive-type) ((re &optional im))
  (if im
      (two-arg-derive-type re im #'complex-derive-type-aux-2 #'complex)
      (one-arg-derive-type re #'complex-derive-type-aux-1 #'complex)))

;;; Define some transforms for complex operations in lieu of complex operation
;;; VOPs for most backends. If vops exist, they must support the following
;;; on complex-single-float and complex-double-float:
;;;   * real-complex, complex-real and complex-complex addition and subtraction
;;;   * complex-real and real-complex multiplication
;;;   * complex-real division
;;;   * sb-vm::swap-complex, which swaps the real and imaginary parts.
;;;   * conjugate
;;;   * complex-real, real-complex and complex-complex CL:=
;;;     (complex-complex EQL would usually be a good idea).
(macrolet ((frob (type contagion)
             `(progn
                (deftransform complex ((r) (,type))
                  '(complex r ,(coerce 0 type)))
                (deftransform complex ((r i) (,type ,contagion))
                  (when (csubtypep (lvar-type i) (specifier-type ',type))
                    (give-up-ir1-transform))
                  '(complex r (truly-the ,type (coerce i ',type))))
                (deftransform complex ((r i) (,contagion ,type))
                  (when (csubtypep (lvar-type r) (specifier-type ',type))
                    (give-up-ir1-transform))
                  '(complex (truly-the ,type (coerce r ',type)) i))

                ;; Arbitrarily use %NEGATE/COMPLEX-DOUBLE-FLOAT as an indicator
                ;; of whether all the operations below are translated by vops.
                ;; We could be more fine-grained, but it seems reasonable that
                ;; they be implemented on an all-or-none basis.
                (unless (vop-existsp :named sb-vm::%negate/complex-double-float)
                ;; negation
                (deftransform %negate ((z) ((complex ,type)) * :important nil)
                  '(complex (%negate (realpart z)) (%negate (imagpart z))))
                ;; complex addition and subtraction
                (deftransform + ((w z) ((complex ,type) (complex ,type)) * :important nil)
                  '(complex (+ (realpart w) (realpart z))
                            (+ (imagpart w) (imagpart z))))
                (deftransform - ((w z) ((complex ,type) (complex ,type)) * :important nil)
                  '(complex (- (realpart w) (realpart z))
                            (- (imagpart w) (imagpart z))))
                ;; Add and subtract a complex and a real.
                (deftransform + ((w z) ((complex ,type) real) * :important nil)
                  `(complex (+ (realpart w) z)
                            (+ (imagpart w) ,(coerce 0 ',type))))
                (deftransform + ((z w) (real (complex ,type)) * :important nil)
                  `(complex (+ (realpart w) z)
                            (+ (imagpart w) ,(coerce 0 ',type))))
                ;; Add and subtract a real and a complex number.
                (deftransform - ((w z) ((complex ,type) real) * :important nil)
                  `(complex (- (realpart w) z)
                            (- (imagpart w) ,(coerce 0 ',type))))
                (deftransform - ((z w) (real (complex ,type)) * :important nil)
                  `(complex (- z (realpart w))
                            (- ,(coerce 0 ',type) (imagpart w))))
                ;; Multiply a complex by a real or vice versa.
                (deftransform * ((w z) ((complex ,type) real) * :important nil)
                  '(complex (* (realpart w) z) (* (imagpart w) z)))
                (deftransform * ((z w) (real (complex ,type)) * :important nil)
                  '(complex (* (realpart w) z) (* (imagpart w) z)))
                ;; conjugate of complex number
                (deftransform conjugate ((z) ((complex ,type)) * :important nil)
                  '(complex (realpart z) (- (imagpart z))))
                ;; comparison
                (deftransform = ((w z) ((complex ,type) (complex ,type)) * :important nil)
                  '(and (= (realpart w) (realpart z))
                    (= (imagpart w) (imagpart z))))
                (deftransform = ((w z) ((complex ,type) real) * :important nil)
                  '(and (= (realpart w) z) (zerop (imagpart w))))
                (deftransform = ((w z) (real (complex ,type)) * :important nil)
                  '(and (= (realpart z) w) (zerop (imagpart z))))
                ;; Multiply two complex numbers.
                (deftransform * ((x y) ((complex ,type) (complex ,type)) * :important nil)
                  '(let* ((rx (realpart x))
                          (ix (imagpart x))
                          (ry (realpart y))
                          (iy (imagpart y)))
                    (complex (- (* rx ry) (* ix iy))
                             (+ (* rx iy) (* ix ry)))))
                ;; Divide a complex by a real.
                (deftransform / ((w z) ((complex ,type) real) * :important nil)
                  '(complex (/ (realpart w) z) (/ (imagpart w) z)))
                )

                ;; Divide two complex numbers.
                (deftransform / ((x y) ((complex ,type) (complex ,type)) * :important nil)
                  (if (vop-existsp :translate sb-vm::swap-complex)
                      '(let* ((cs (conjugate (sb-vm::swap-complex x)))
                              (ry (realpart y))
                              (iy (imagpart y)))
                         (if (> (abs ry) (abs iy))
                             (let* ((r (/ iy ry))
                                    (dn (+ ry (* r iy))))
                               (/ (+ x (* cs r)) dn))
                             (let* ((r (/ ry iy))
                                    (dn (+ iy (* r ry))))
                               (/ (+ (* x r) cs) dn))))
                      '(let* ((rx (realpart x))
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
                                       (/ (- (* ix r) rx) dn)))))))
                ;; Divide a real by a complex.
                (deftransform / ((x y) (real (complex ,type)) * :important nil)
                  (if (vop-existsp :translate sb-vm::swap-complex)
                      '(let* ((ry (realpart y))
                              (iy (imagpart y)))
                        (if (> (abs ry) (abs iy))
                            (let* ((r (/ iy ry))
                                   (dn (+ ry (* r iy))))
                              (/ (complex x (- (* x r))) dn))
                            (let* ((r (/ ry iy))
                                   (dn (+ iy (* r ry))))
                              (/ (complex (* x r) (- x)) dn))))
                      '(let* ((ry (realpart y))
                              (iy (imagpart y)))
                        (if (> (abs ry) (abs iy))
                            (let* ((r (/ iy ry))
                                   (dn (+ ry (* r iy))))
                              (complex (/ x dn)
                                       (/ (- (* x r)) dn)))
                            (let* ((r (/ ry iy))
                                   (dn (+ iy (* r ry))))
                              (complex (/ (* x r) dn)
                                       (/ (- x) dn)))))))
                ;; CIS
                (deftransform cis ((z) ((,type)) *)
                  '(complex (cos z) (sin z)))
                )))
  (frob single-float (or rational single-float))
  (frob double-float (or rational single-float double-float)))


;;;; float contagion
(deftransform single-float-real-contagion ((x y) * * :node node :defun-only t)
  (if (csubtypep (lvar-type y) (specifier-type 'single-float))
      (give-up-ir1-transform)
      `(,(lvar-fun-name (basic-combination-fun node)) x (%single-float y))))

(deftransform real-single-float-contagion ((x y) * * :node node :defun-only t)
  (if (csubtypep (lvar-type x) (specifier-type 'single-float))
      (give-up-ir1-transform)
      `(,(lvar-fun-name (basic-combination-fun node)) (%single-float x) y)))

(deftransform double-float-real-contagion ((x y) * * :node node :defun-only t)
  (if (csubtypep (lvar-type y) (specifier-type 'double-float))
      (give-up-ir1-transform)
      `(,(lvar-fun-name (basic-combination-fun node)) x (%double-float y))))

(deftransform real-double-float-contagion ((x y) * * :node node :defun-only t)
  (if (csubtypep (lvar-type x) (specifier-type 'double-float))
      (give-up-ir1-transform)
      `(,(lvar-fun-name (basic-combination-fun node)) (%double-float x) y)))

(deftransform double-float-real-contagion-cmp ((x y) * * :node node :defun-only t)
  (cond ((csubtypep (lvar-type y) (specifier-type 'double-float))
         (give-up-ir1-transform))
        ;; Turn (= single-float 1d0) into (= single-float 1f0)
        ((and (constant-lvar-p x)
              (csubtypep (lvar-type y) (specifier-type 'single-float))
              (let ((x (lvar-value x)))
                (when (and (safe-single-coercion-p x)
                           (= x (coerce x 'single-float)))
                  `(,(lvar-fun-name (basic-combination-fun node)) ,(coerce x 'single-float) y)))))
        (t
         `(,(lvar-fun-name (basic-combination-fun node)) x (%double-float y)))))

(deftransform real-double-float-contagion-cmp ((x y) * * :node node :defun-only t)
  (cond ((csubtypep (lvar-type x) (specifier-type 'double-float))
         (give-up-ir1-transform))
        ((and (constant-lvar-p y)
              (csubtypep (lvar-type x) (specifier-type 'single-float))
              (let ((y (lvar-value y)))
                (when (and (safe-single-coercion-p y)
                           (= y (coerce y 'single-float)))
                  `(,(lvar-fun-name (basic-combination-fun node)) x ,(coerce y 'single-float))))))
        (t
         `(,(lvar-fun-name (basic-combination-fun node)) (%double-float x) y))))

(flet ((def (op)
         (%deftransform op nil '(function (single-float real) single-float)
                        #'single-float-real-contagion nil)
         (%deftransform op nil '(function (real single-float) single-float)
                        #'real-single-float-contagion nil)
         (%deftransform op nil '(function (double-float real))
                        #'double-float-real-contagion nil)
         (%deftransform op nil '(function (real double-float))
                        #'real-double-float-contagion nil)

         (%deftransform op nil '(function ((complex single-float) real) (complex single-float))
                        #'single-float-real-contagion nil)
         (%deftransform op nil '(function (real (complex single-float)) (complex single-float))
                        #'real-single-float-contagion nil)
         (%deftransform op nil '(function ((complex double-float) real) (complex double-float))
                        #'double-float-real-contagion nil)
         (%deftransform op nil '(function (real (complex double-float)) (complex double-float))
                        #'real-double-float-contagion nil)))
  (dolist (op '(+ * / -))
    (def op)))

(flet ((def (op)
         (%deftransform op nil `(function (single-float (integer ,most-negative-exactly-single-float-integer
                                                                 ,most-positive-exactly-single-float-integer)))
                        #'single-float-real-contagion nil)
         (%deftransform op nil `(function ((integer ,most-negative-exactly-single-float-integer
                                                    ,most-positive-exactly-single-float-integer)
                                           single-float))
                        #'real-single-float-contagion nil)

         (%deftransform op nil `(function (double-float
                                           (or single-float
                                               (integer ,most-negative-exactly-double-float-integer
                                                        ,most-positive-exactly-double-float-integer))))
                        #'double-float-real-contagion-cmp nil)
         (%deftransform op nil `(function ((or single-float
                                               (integer ,most-negative-exactly-double-float-integer
                                                        ,most-positive-exactly-double-float-integer))
                                           double-float))
                        #'real-double-float-contagion-cmp nil)))
  (dolist (op '(= < > <= >=))
    (def op)))

(%deftransform '= nil '(function ((complex double-float) single-float))
               #'double-float-real-contagion nil)
(%deftransform '= nil '(function (single-float (complex double-float)))
               #'real-double-float-contagion nil)

(deftransform complex ((realpart &optional imagpart) (rational &optional (or null (integer 0 0))) * :important nil)
  'realpart)

;;; Here are simple optimizers for SIN, COS, and TAN. They do not
;;; produce a minimal range for the result; the result is the widest
;;; possible answer. This gets around the problem of doing range
;;; reduction correctly but still provides useful results when the
;;; inputs are union types.
(defun trig-derive-type-aux (arg domain fun
                             &optional def-lo def-hi (increasingp t))
  (etypecase arg
    (numeric-type
     (flet ((floatify-format ()
              (case (numeric-type-class arg)
                ((integer rational) 'single-float)
                (t (numeric-type-format arg)))))
       (cond ((eq (numeric-type-complexp arg) :complex)
              (make-numeric-type :class 'float
                                 :format (floatify-format)
                                 :complexp :complex))
             ((numeric-type-real-p arg)
              (let* ((format (floatify-format))
                     (bound-type (or format 'float)))
                ;; If the argument is a subset of the "principal" domain
                ;; of the function, we can compute the bounds because
                ;; the function is monotonic. We can't do this in
                ;; general for these periodic functions because we can't
                ;; (and don't want to) do the argument reduction in
                ;; exactly the same way as the functions themselves do
                ;; it.
                (if (csubtypep arg domain)
                    (let ((res-lo (bound-func fun (numeric-type-low arg) nil))
                          (res-hi (bound-func fun (numeric-type-high arg) nil)))
                      (unless increasingp
                        (rotatef res-lo res-hi))
                      (make-numeric-type
                       :class 'float
                       :format format
                       :low (coerce-numeric-bound res-lo bound-type)
                       :high (coerce-numeric-bound res-hi bound-type)))
                    (make-numeric-type
                     :class 'float
                     :format format
                     :low (and def-lo (coerce def-lo bound-type))
                     :high (and def-hi (coerce def-hi bound-type))))))
             (t
              (float-or-complex-float-type arg def-lo def-hi)))))))

(defoptimizer (sin derive-type) ((num))
  (one-arg-derive-type
   num
   (lambda (arg)
     ;; Derive the bounds if the arg is in [-pi/2, pi/2].
     (trig-derive-type-aux
      arg
      (specifier-type `(float ,(sb-xc:- (sb-xc:/ pi 2)) ,(sb-xc:/ pi 2)))
      #'sin
      -1 1))
   #'sin))

(defoptimizer (cos derive-type) ((num))
  (one-arg-derive-type
   num
   (lambda (arg)
     ;; Derive the bounds if the arg is in [0, pi].
     (trig-derive-type-aux arg
                           (specifier-type `(float 0d0 ,pi))
                           #'cos
                           -1 1
                           nil))
   #'cos))

(defoptimizer (tan derive-type) ((num))
  (one-arg-derive-type
   num
   (lambda (arg)
     ;; Derive the bounds if the arg is in [-pi/2, pi/2].
     (trig-derive-type-aux arg
                           (specifier-type `(float ,(sb-xc:- (sb-xc:/ pi 2))
                                                   ,(sb-xc:/ pi 2)))
                           #'tan
                           nil nil))
   #'tan))

(defoptimizer (conjugate derive-type) ((num))
  (one-arg-derive-type num
    (lambda (arg)
      (flet ((most-negative-bound (l h)
               (and l h
                    (if (< (type-bound-number l) (- (type-bound-number h)))
                        l
                        (set-bound (- (type-bound-number h)) (consp h)))))
             (most-positive-bound (l h)
               (and l h
                    (if (> (type-bound-number h) (- (type-bound-number l)))
                        h
                        (set-bound (- (type-bound-number l)) (consp l))))))
        (if (numeric-type-real-p arg)
            (lvar-type num)
            (let ((low (numeric-type-low arg))
                  (high (numeric-type-high arg)))
              (let ((new-low (most-negative-bound low high))
                    (new-high (most-positive-bound low high)))
                (modified-numeric-type arg :low new-low :high new-high))))))
    #'conjugate))

(defoptimizer (cis derive-type) ((num))
  (one-arg-derive-type num
    (lambda (arg)
      (specifier-type
       `(complex ,(or (numeric-type-format arg) 'float))))
    #'cis))


;;;; TRUNCATE, FLOOR, CEILING, and ROUND
(deftransform truncate ((x &optional by)
                        (t &optional (constant-arg (member 1))))
  '(unary-truncate x))

(deftransform round ((x &optional by)
                     (t &optional (constant-arg (member 1))))
  '(let ((res (%unary-round x)))
    (values res (locally
                    (declare (flushable %single-float
                                        %double-float))
                  (- x res)))))

(deftransform %unary-truncate ((x) (single-float))
  `(values (unary-truncate x)))
(deftransform %unary-truncate ((x) (double-float))
  `(values (unary-truncate x)))

(defun value-within-numeric-type (type)
  (labels ((try (x)
             (when (ctypep x type)
               (return-from value-within-numeric-type x)))
           (next-float (float)
             (multiple-value-bind (frac exp sign)
                 (integer-decode-float float)
               (* (scale-float (float (1+ frac) float) exp)
                  sign)))
           (prev-float (float)
             (multiple-value-bind (frac exp sign)
                 (integer-decode-float float)
               (* (scale-float (float (1- frac) float) exp)
                  sign)))
           (next (x)
             (typecase x
               (integer
                (1+ x))
               (float
                (next-float x))
               (t
                0)))
           (prev (x)
             (typecase x
               (integer
                (1- x))
               (float
                (prev-float x))
               (t
                0)))
           (ratio-between (low high)
             (+ low (/ (- high low) 2)))
           (numeric (x)
             (when (numeric-type-p x)
               (let ((lo (numeric-type-low x))
                     (hi (numeric-type-high x)))
                 (when (numberp lo)
                   (try lo))
                 (when (numberp hi)
                   (try hi))
                 (when (consp lo)
                   (try (next (car lo))))
                 (when (consp hi)
                   (try (prev (car hi))))
                 (when (and (typep lo '(cons rational))
                            (typep hi '(cons rational)))
                   (try (ratio-between (car lo) (car hi))))
                 (when (csubtypep x (specifier-type 'rational))
                   (try 0))
                 (when (csubtypep x (specifier-type 'double-float))
                   (try 0d0))
                 (when (csubtypep x (specifier-type 'single-float))
                   (try 0f0))))))
    (typecase type
      (numeric-type (numeric type))
      (union-type (mapc #'numeric (union-type-types type))))
    (error "Couldn't come up with a value for ~s" type)))

#-(or sb-xc-host 64-bit)
(progn
  (declaim (inline %make-double-float))
  (defun %make-double-float (bits)
    (make-double-float (ash bits -32) (ldb (byte 32 0) bits))))

;;; Transform inclusive integer bounds so that they work on floats
;;; before truncating to zero.
(macrolet
    ((def (type)
       `(defun ,(symbolicate type '-integer-bounds) (low high)
          (macrolet ((const (name)
                       (package-symbolicate :sb-vm ',type '- name)))
            (labels ((fractions-p (number)
                       (< (integer-length (abs number))
                          (const digits)))
                     (c (number round)
                       (if (zerop number)
                           (sb-kernel:make-single-float 0)
                           (let* ((negative (minusp number))
                                  (number (abs number))
                                  (length (integer-length number))
                                  (shift (- length (const digits)))
                                  (shifted (truly-the fixnum
                                                      (ash number
                                                           (- shift))))
                                  ;; Cut off the hidden bit
                                  (signif (ldb (const significand-byte) shifted))
                                  (exp (+ (const bias) length))
                                  (bits (ash exp
                                             (byte-position (const exponent-byte)))))
                             (incf signif round)
                             ;; If rounding up overflows this will increase the exponent too
                             (let ((bits (+ bits signif)))
                               (when negative
                                 (setf bits (logior (ash -1 ,(case type
                                                               (double-float 63)
                                                               (single-float 31)))
                                                    bits)))
                               (,(case type
                                   (single-float 'make-single-float)
                                   (double-float '%make-double-float)) bits))))))
              (values (if (<= low 0)
                          (if (fractions-p low)
                              (c (1- low) -1)
                              (c low 0))
                          (c low 0))
                      (if (< high 0)
                          (c high 0)
                          (if (fractions-p high)
                              (c (1+ high) -1)
                              (c high 0)))))))))
  (def single-float)
  (def double-float))

(deftransform unary-truncate ((x) * * :result result :node node)
  (unless (lvar-single-value-p result)
    (give-up-ir1-transform))
  (let ((rem-type (second (values-type-required (node-derived-type node)))))
    `(values (%unary-truncate x)
             ,(value-within-numeric-type rem-type))))

(macrolet ((def (type)
             `(deftransform unary-truncate ((number) (,type) * :node node)
                (let ((cast (cast-or-check-bound-type node)))
                  (if (and cast
                           (csubtypep cast (specifier-type 'sb-vm:signed-word)))
                      (let ((int (type-approximate-interval cast)))
                        (when int
                          (multiple-value-bind (low high) (,(symbolicate type '-integer-bounds)
                                                           (interval-low int)
                                                           (interval-high int))
                            `(if (typep number
                                        '(,',type ,low ,high))
                                 (let ((truncated (truly-the ,(type-specifier cast) (,',(symbolicate '%unary-truncate/ type) number))))
                                   (declare (flushable ,',(symbolicate "%" type)))
                                   (values truncated
                                           (- number
                                              (coerce truncated ',',type))))
                                 ,(internal-type-error-call 'number (type-specifier cast) 'truncate-to-integer)))))
                      '(if (typep number
                            '(,type
                              ,(symbol-value (package-symbolicate :sb-kernel 'most-negative-fixnum- type))
                              ,(symbol-value (package-symbolicate :sb-kernel 'most-positive-fixnum- type))))
                        (let ((truncated (truly-the fixnum (,(symbolicate '%unary-truncate/ type) number))))
                          (declare (flushable ,(symbolicate "%" type)))
                          (values truncated
                                  (- number
                                     (coerce truncated ',type))))
                        (,(symbolicate 'unary-truncate- type '-to-bignum) number)))))))
  (def single-float)
  (def double-float))

;;; Convert (TRUNCATE x y) to the obvious implementation.
;;;
;;; ...plus hair: Insert explicit coercions to appropriate float types: Python
;;; is reluctant it generate explicit integer->float coercions due to
;;; precision issues (see SAFE-SINGLE-COERCION-P &co), but this is not an
;;; issue here as there is no DERIVE-TYPE optimizer on specialized versions of
;;; %UNARY-TRUNCATE, so the derived type of TRUNCATE remains the best we can
;;; do here -- which is fine. Also take care not to add unnecassary division
;;; or multiplication by 1, since we are not able to always eliminate them,
;;; depending on FLOAT-ACCURACY. Finally, leave out the secondary value when
;;; we know it is unused: COERCE is not flushable.
(macrolet ((def (type other-float-arg-types)
             (let* ((unary (symbolicate "%UNARY-TRUNCATE/" type))
                    (unary-to-bignum (symbolicate '%unary-truncate- type '-to-bignum))
                    (coerce (symbolicate "%" type))
                    (unary `(lambda (number)
                              (if (typep number
                                         '(,type
                                           ,(symbol-value (package-symbolicate :sb-kernel 'most-negative-fixnum- type))
                                           ,(symbol-value (package-symbolicate :sb-kernel 'most-positive-fixnum- type))))
                                  (truly-the fixnum (,unary number))
                                  (,unary-to-bignum number)))))
               `(deftransform truncate ((x &optional y)
                                        (,type
                                         &optional (or ,type ,@other-float-arg-types integer))
                                        * :result result)
                  (let* ((result-type (and result
                                           (lvar-derived-type result)))
                         (compute-all (and (or (eq result-type *wild-type*)
                                               (values-type-p result-type))
                                           (not (type-single-value-p result-type)))))
                    (if (or (not y)
                            (and (constant-lvar-p y) (sb-xc:= 1 (lvar-value y))))
                        (if compute-all
                            `(unary-truncate x)
                            `(let ((res (,',unary x)))
                               ;; Dummy secondary value!
                               (values res x)))
                        (if compute-all
                            `(let* ((f (,',coerce y))
                                    (div (/ x f))
                                    (res (,',unary div)))
                               (values res
                                       (- x (* f
                                               #+round-float
                                               (- (,',(ecase type
                                                        (double-float 'round-double)
                                                        (single-float 'round-single))
                                                      div :truncate)
                                                  ,,(ecase type
                                                      (double-float -0.0d0)
                                                      (single-float -0.0f0)))
                                               #-round-float
                                               (locally
                                                   (declare (flushable ,',coerce))
                                                 (,',coerce res))))))
                            `(let* ((f (,',coerce y))
                                    (res (,',unary (/ x f))))
                               ;; Dummy secondary value!
                               (values res x)))))))))
  (def single-float ())
  (def double-float (single-float)))

;;; truncate on bignum floats will always have a remainder of zero
;;; on 64-bit, so ceiling and floor are the same as truncate.
#+64-bit
(macrolet ((def (name type other-float-arg-types
                 fixup)
             (let* ((unary (symbolicate "%UNARY-TRUNCATE/" type))
                    (unary-to-bignum (symbolicate 'unary-truncate- type '-to-bignum))
                    (coerce (symbolicate "%" type)))
               `(deftransform ,name ((number &optional divisor)
                                     (,type
                                      &optional (or ,type ,@other-float-arg-types integer))
                                     *)
                  (block nil
                    (let ((one-p (or (not divisor)
                                     (and (constant-lvar-p divisor) (sb-xc:= (lvar-value divisor) 1)))))
                      #+round-float
                      (when-vop-existsp (:translate %unary-ceiling)
                        (when one-p
                          (return
                            `(if (typep number
                                        '(,',type
                                          ,',(symbol-value (package-symbolicate :sb-kernel 'most-negative-fixnum- type))
                                          ,',(symbol-value (package-symbolicate :sb-kernel 'most-positive-fixnum- type))))
                                 (values (truly-the fixnum (,',(symbolicate '%unary- name) number))
                                         (- number
                                            (,',(ecase type
                                                  (double-float 'round-double)
                                                  (single-float 'round-single))
                                             number ,,(keywordicate name))))
                                 (,',unary-to-bignum number)))))
                      `(let* ,(if one-p
                                  `((f-divisor 1)
                                    (div number))
                                  `((f-divisor (,',coerce divisor))
                                    (div (/ number f-divisor))))
                         (if (typep div
                                    '(,',type
                                      ,',(symbol-value (package-symbolicate :sb-kernel 'most-negative-fixnum- type))
                                      ,',(symbol-value (package-symbolicate :sb-kernel 'most-positive-fixnum- type))))
                             (let* ((tru (truly-the fixnum (,',unary div)))
                                    (rem (- number (* ,@(unless one-p
                                                          '(f-divisor))
                                                      #+round-float
                                                      (- (,',(ecase type
                                                               (double-float 'round-double)
                                                               (single-float 'round-single))
                                                             div :truncate)
                                                         ,,(ecase type
                                                             (double-float -0.0d0)
                                                             (single-float -0.0f0)))
                                                      #-round-float
                                                      (locally
                                                          (declare (flushable ,',coerce))
                                                        (,',coerce tru))))))
                               ,',fixup)
                             (,',unary-to-bignum div)))))))))
  (def floor single-float ()
    #1=(if (and (not (zerop rem))
                (if (minusp f-divisor)
                    (plusp number)
                    (minusp number)))
           (values
            ;; the above conditions wouldn't hold when tru is m-n-f
            (truly-the fixnum (1- tru))
            (+ rem f-divisor))
           (values tru rem)))
  (def floor double-float (single-float)
    #1#)
  (def ceiling single-float ()
    #2=(if (and (not (zerop rem))
                (if (minusp f-divisor)
                    (minusp number)
                    (plusp number)))
           (values (+ tru 1) (- rem f-divisor))
           (values tru rem)))
  (def ceiling double-float (single-float)
    #2#))

#-64-bit
(macrolet ((def (number-type divisor-type)
             `(progn
                (deftransform floor ((number divisor) (,number-type ,divisor-type) * :node node)
                  `(let ((divisor (coerce divisor ',',number-type)))
                     (multiple-value-bind (tru rem) (truncate number divisor)
                       (if (and (not (zerop rem))
                                (if (minusp divisor)
                                    (plusp number)
                                    (minusp number)))
                           (values (1- tru) (+ rem divisor))
                           (values tru rem)))))

                (deftransform ceiling ((number divisor) (,number-type ,divisor-type) * :node node)
                  `(let ((divisor (coerce divisor ',',number-type)))
                     (multiple-value-bind (tru rem) (truncate number divisor)
                       (if (and (not (zerop rem))
                                (if (minusp divisor)
                                    (minusp number)
                                    (plusp number)))
                           (values (+ tru 1) (- rem divisor))
                           (values tru rem))))))))
  (def double-float (or float integer))
  (def single-float (or single-float integer)))

#-round-float
(progn
  (defknown (%unary-ftruncate %unary-fround) (real) float (movable foldable flushable))
  #-64-bit
  (defknown (%unary-ftruncate/double %unary-fround/double) (double-float) double-float
    (movable foldable flushable))

  (deftransform %unary-ftruncate ((x) (single-float))
    `(cond ((or (typep x '(single-float (-1f0) (0f0)))
                (eql x -0f0))
            -0f0)
           ((typep x '(single-float ,(float (- (expt 2 sb-vm:single-float-digits)) 1f0)
                       ,(float (1- (expt 2 sb-vm:single-float-digits)) 1f0)))
            (float (truncate x) 1f0))
           (t
            x)))

  (deftransform %unary-fround ((x) (single-float))
    `(cond ((or (typep x '(single-float -0.5f0 (0f0)))
                (eql x -0f0))
            -0f0)
           ((typep x '(single-float ,(float (- (expt 2 sb-vm:single-float-digits)) 1f0)
                       ,(float (1- (expt 2 sb-vm:single-float-digits)) 1f0)))
            (float (round x) 1f0))
           (t
            x)))

  #+64-bit
  (progn
    (deftransform %unary-ftruncate ((x) (double-float))
      `(cond ((or (typep x '(double-float (-1d0) (0d0)))
                  (eql x -0d0))
              -0d0)
             ((typep x '(double-float ,(float (- (expt 2 sb-vm:double-float-digits)) 1d0)
                         ,(float (1- (expt 2 sb-vm:double-float-digits)) 1d0)))
              (float (truncate x) 1d0))
             (t
              x)))

    (deftransform %unary-fround ((x) (double-float))
      `(cond ((or (typep x '(double-float -0.5d0 (0d0)))
                  (eql x -0d0))
              -0d0)
             ((typep x '(double-float ,(float (- (expt 2 sb-vm:double-float-digits)) 1d0)
                         ,(float (1- (expt 2 sb-vm:double-float-digits)) 1d0)))
              (float (round x) 1d0))
             (t
              x))))

  #-64-bit
  (progn
    #-sb-xc-host
    (progn
      (defun %unary-ftruncate/double (x)
        (declare (muffle-conditions compiler-note))
        (declare (type double-float x))
        (declare (optimize speed (safety 0)))
        (let* ((high (double-float-high-bits x))
               (low (double-float-low-bits x))
               (exp (ldb sb-vm:double-float-hi-exponent-byte high))
               (biased (the double-float-exponent
                            (- exp sb-vm:double-float-bias))))
          (declare (type (signed-byte 32) high)
                   (type (unsigned-byte 32) low))
          (cond
            ((= exp sb-vm:double-float-normal-exponent-max) x)
            ((<= biased 0) (* x 0d0))
            ((>= biased (float-digits x)) x)
            (t
             (let ((frac-bits (- (float-digits x) biased)))
               (cond ((< frac-bits 32)
                      (setf low (logandc2 low (- (ash 1 frac-bits) 1))))
                     (t
                      (setf low 0)
                      (setf high (logandc2 high (- (ash 1 (- frac-bits 32)) 1)))))
               (make-double-float high low))))))
      (defun %unary-fround/double (x)
        (declare (muffle-conditions compiler-note))
        (declare (type double-float x))
        (declare (optimize speed (safety 0)))
        (let* ((high (double-float-high-bits x))
               (low (double-float-low-bits x))
               (exp (ldb sb-vm:double-float-hi-exponent-byte high))
               (biased (the double-float-exponent
                            (- exp sb-vm:double-float-bias))))
          (declare (type (signed-byte 32) high)
                   (type (unsigned-byte 32) low))
          (cond
            ((= exp sb-vm:double-float-normal-exponent-max) x)
            ((<= biased -1) (* x 0d0)) ; [0,0.5)
            ((and (= biased 0) (= low 0) (= (ldb sb-vm:double-float-hi-significand-byte high) 0)) ; [0.5,0.5]
             (* x 0d0))
            ((= biased 0) (float-sign x 1d0)) ; (0.5,1.0)
            ((= biased 1) ; [1.0,2.0)
             (cond
               ((>= (ldb sb-vm:double-float-hi-significand-byte high) (ash 1 19))
                (float-sign x 2d0))
               (t (float-sign x 1d0))))
            ((>= biased (float-digits x)) x)
            (t
             ;; it's probably possible to do something very contorted
             ;; to avoid consing intermediate bignums, by performing
             ;; arithmetic on the fractional part, the low integer
             ;; part, the high integer part, and the exponent of the
             ;; double float.  But in the interest of getting
             ;; something correct to start with, delegate to ROUND.
             (float (round x) 1d0))))))
    (deftransform %unary-ftruncate ((x) (double-float))
      `(%unary-ftruncate/double x))
    (deftransform %unary-fround ((x) (double-float))
      `(%unary-fround/double x))))

#+round-float
(deftransform fround ((number &optional divisor) (double-float &optional t))
  (if (or (not divisor)
          (and (constant-lvar-p divisor)
               (= (lvar-value divisor) 1)))
      `(let ((res (round-double number :round)))
         (values res (- number res)))
      `(let* ((divisor (%double-float divisor))
              (res (round-double (/ number (%double-float divisor)) :round)))
         (values res (- number (* res divisor))))))

#+round-float
(deftransform fround ((number &optional divisor) (single-float &optional (or null single-float rational)))
  (if (or (not divisor)
          (and (constant-lvar-p divisor)
               (= (lvar-value divisor) 1)))
      `(let ((res (round-single number :round)))
         (values res (- number res)))
      `(let* ((divisor (%single-float divisor))
              (res (round-single (/ number divisor) :round)))
         (values res (- number (* res divisor))))))

;;;; TESTS

;;; Dumping of double-float literals in genesis got some bits messed up,
;;; but only if the double-float was the value of a slot in a ctype instance.
;;; It was broken for either endianness, but miraculously didn't crash
;;; for little-endian builds even though it could have.
;;; (The dumped constants were legal normalalized float bit patterns, albeit wrong)
;;; For 32-bit big-endian machines, the bit patterns were those of subnormals.
;;; So thank goodness for that - it allowed detection of the problem.
(defun test-ctype-involving-double-float ()
  (specifier-type '(double-float #.pi)))
(assert (sb-xc:= (numeric-type-low (test-ctype-involving-double-float)) pi))

;;; Dummy functions to test that complex number are dumped correctly in genesis.
(defun try-folding-complex-single ()
  (let ((re (make-single-float #x4E000000))
        (im (make-single-float #x-21800000)))
    (values (complex re im)
            (locally (declare (notinline complex)) (complex re im)))))

(defun try-folding-complex-double ()
  (let ((re (make-double-float #X3FE62E42 #xFEFA39EF))
        (im (make-double-float #X43CFFFFF #XFFFFFFFF)))
    (values (complex re im)
            (locally (declare (notinline complex)) (complex re im)))))

(dolist (test '(try-folding-complex-single try-folding-complex-double))
  (multiple-value-bind (a b) (funcall test)
    (assert (eql a b)))
  #-sb-xc-host
  (let ((code (fun-code-header (symbol-function test))))
    (aver (loop for index from sb-vm:code-constants-offset
                below (code-header-words code)
                thereis (typep (code-header-ref code index) 'complex))))
  (fmakunbound test))

(defun more-folding ()
  (values (complex single-float-positive-infinity single-float-positive-infinity)
          (complex single-float-negative-infinity single-float-positive-infinity)
          (complex single-float-negative-infinity single-float-negative-infinity)
          (complex single-float-positive-infinity single-float-negative-infinity)))

(multiple-value-bind (a b c d) (funcall 'more-folding)
  (assert (sb-ext:float-infinity-p (realpart a)))
  (assert (sb-ext:float-infinity-p (imagpart a)))
  (assert (sb-ext:float-infinity-p (realpart b)))
  (assert (sb-ext:float-infinity-p (imagpart b)))
  (assert (sb-ext:float-infinity-p (realpart c)))
  (assert (sb-ext:float-infinity-p (imagpart c)))
  (assert (sb-ext:float-infinity-p (realpart d)))
  (assert (sb-ext:float-infinity-p (imagpart d)))
  #-sb-xc-host
  (let ((code (fun-code-header (symbol-function 'more-folding))))
    (aver (loop for index from sb-vm:code-constants-offset
                below (code-header-words code)
                thereis (typep (code-header-ref code index) 'complex))))
  (fmakunbound 'more-folding))

;;; Inline (= float 1) by doing two comparisons.
(macrolet ((def (op)
             `(deftransform ,op ((x y) (:or ((float
                                              (integer #.most-negative-exactly-single-float-integer
                                                       #.most-positive-exactly-single-float-integer)) *)
                                            ((float (constant-arg float)) *))
                                 * :node node :important nil
                                   :policy (> speed 1))
                (unless (and (types-equal-or-intersect (lvar-type x) (specifier-type 'double-float))
                             (types-equal-or-intersect (lvar-type x) (specifier-type 'single-float)))
                  (give-up-ir1-transform))
                (delay-ir1-transform node :ir1-phases)
                (if (csubtypep (lvar-type y) (specifier-type 'float))
                    (let ((y (lvar-value y)))
                      (if (and (safe-single-coercion-p y)
                               (sb-xc:= y (coerce y 'single-float))
                               (sb-xc:= y (coerce y 'double-float)))
                          `(if (single-float-p x)
                               (,',op (truly-the single-float x) ,(coerce y 'single-float))
                               (,',op (truly-the double-float x) ,(coerce y 'double-float)))
                          (give-up-ir1-transform)))
                    `(if (single-float-p x)
                         (,',op (truly-the single-float x) (%single-float y))
                         (,',op (truly-the double-float x) (%double-float y)))))))
  (def =)
  (def <)
  (def >)
  (def <=)
  (def >=))

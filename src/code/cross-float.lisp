;;;; portable implementations or stubs for nonportable floating point
;;;; things, useful for building Python as a cross-compiler when
;;;; running under an ordinary ANSI Common Lisp implementation

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

(defun float-sign-bit (float)
  (declare (type float float))
  (logand (ash (flonum-bits float)
               (- (1- (etypecase float
                        (single-float 32)
                        (double-float 64)))))
          1))
(defun float-sign-bit-set-p (float)
  (declare (type float float))
  (= (float-sign-bit float) 1))
(declaim (inline flonum-minus-zero-p))
(defun flonum-minus-zero-p (flonum)
  (or (eq flonum -0.0f0) (eq flonum -0.0d0)))

(defun pick-result-format (&rest args)
  (flet ((target-num-fmt (num)
           (cond ((rationalp num) 'rational)
                 ((floatp num) (type-of num))
                 (t (error "What? ~S" num)))))
    (let* ((result-fmt 'rational)
           (result-contagion 0))
      (dolist (arg args result-fmt)
        (let* ((arg-fmt (target-num-fmt arg))
               ;; This is inadequate for complex numbers,
               ;; but we don't need them.
               (arg-contagion
                 (position arg-fmt
                           '(rational short-float single-float double-float long-float))))
          (when (cl:> arg-contagion result-contagion)
            (setq result-fmt arg-fmt result-contagion arg-contagion)))))))

(defun pick-float-result-format (&rest args)
  (let ((format (apply #'pick-result-format args)))
    (if (eq format 'rational)
        'single-float
        format)))

(defun rationalize (x)
  (if (rationalp x)
      x
      (let ((rational (rational x)))
        (if (integerp rational)
            rational
            (error "Won't do (RATIONALIZE ~S) due to possible precision loss" x)))))

(defun xfloat-coerce (object type)
  (declare (number object))
  (when (member type '(integer rational real))
    ;; This branch won't accept (coerce x 'real) if X is one of our
    ;; target-floats. We don't need that apparently.
    (assert (if (eq type 'integer) (integerp object) (rationalp object)))
    (return-from xfloat-coerce object))
  (unless (member type '(float short-float single-float double-float long-float))
    (error "Can't COERCE ~S ~S" object type))
  (when (and (floatp object)
             (or (eq type 'float) (eq (type-of object) type)))
    (return-from xfloat-coerce object))
  (with-memoized-math-op (coerce (list object type))
    (cond ((not (realp object))
           (error "Can't COERCE ~S ~S" object type))
          ((and (flonum-minus-zero-p object)
                (member type '(double-float single-float)))
           (ecase type (single-float -0.0f0) (double-float -0.0d0)))
          ((and (floatp object)
                (float-infinity-p object))
           (ecase type
             (single-float
              (if (float-sign-bit-set-p object)
                  single-float-negative-infinity
                  single-float-positive-infinity))
             (double-float
              (if (float-sign-bit-set-p object)
                  double-float-negative-infinity
                  double-float-positive-infinity))))
          (t
           (let ((actual-type (if (member type '(double-float long-float))
                                  'double-float
                                  'single-float))
                 (source-value (rational object)))
             (flonum-from-rational source-value actual-type))))))

(defun xfloat-abs (x)
  (if (float-sign-bit-set-p x)
      (sb-xc:- x)
      x))

;;; Signum should return -0 of the correct type for -0 input.
;;; We don't need it currently.
(defun xfloat-signum (x)
  (if (zerop x)
      x
      (float-sign x)))

(macrolet ((define (name float-fun)
             (declare (ignore name))
             `(defun ,float-fun (number divisor)
                (declare (ignore number divisor))
                (error "Unimplemented"))))
  (define mod xfloat-mod)
  (define rem xfloat-rem))

(macrolet ((define (name float-fun)
             (let ((clname (intern (string name) "CL")))
               `(defun ,float-fun (number divisor)
                  (let ((type (pick-float-result-format number divisor)))
                    (with-memoized-math-op (,name (list number divisor))
                      (if (flonum-minus-zero-p number)
                          (values 0
                                  (coerce (if (< divisor 0) 0 -0.0) type))
                          (multiple-value-bind (q r)
                              (,clname (rational number) (rational divisor))
                            (values q (flonum-from-rational r type))))))))))
  (define floor xfloat-floor)
  (define ceiling xfloat-ceiling)
  (define truncate xfloat-truncate)
  (define round xfloat-round))

(defun sgn (thing)
  ;; return 1 or -1 if the sign bit of THING (as if converted to
  ;; FLONUM) is unset or set respectively.
  (typecase thing
    ((eql 0) 1)
    (rational (signum thing))
    (float (if (float-sign-bit-set-p thing) -1 1))))

(macrolet ((define (name clname)
             `(defun ,name (number &optional (divisor 1 divisorp))
                (with-memoized-math-op (,name (if divisorp (list number divisor) number))
                  (multiple-value-bind (q r)
                      (,clname (rational number) (rational divisor))
                    (let* ((type (pick-float-result-format number divisor))
                           (format (pick-result-format number divisor))
                           (remainder (if (eql format 'rational)
                                          r
                                          (flonum-from-rational r format))))
                      (if (cl:= q 0)
                          (values (coerce (if (cl:= (sgn number) (sgn divisor)) 0 -0.0)
                                          type)
                                  remainder)
                          (values (flonum-from-rational q type) remainder))))))))
  (define fceiling cl:ceiling)
  (define ffloor cl:floor)
  (define fround cl:round)
  (define ftruncate cl:truncate))

(defun xfloat-expt (base power)
  (if (not (integerp power))
      (error "Unimplemented: EXPT with non-integer power")
      (with-memoized-math-op (expt (list base power))
        (if (zerop power)
            (coerce 1 (type-of base))
            (flonum-from-rational
             (cl:expt (rational base) power)
             (pick-result-format base power))))))

;;; Four possible return values.  NIL if the numbers (rationals or
;;; flonums) are incomparable (either is a NaN).  Otherwise: -1, 0, 1
;;; if A is less than, equal to or greater than B.
(defun numeric-compare (a b)
  (cond
    ((or (and (floatp a) (float-nan-p a))
         (and (floatp b) (float-nan-p b)))
     nil)
    ((and (and (floatp a) (float-infinity-p a))
          (and (floatp b) (float-infinity-p b)))
     (let ((sa (float-sign-bit a))
           (sb (float-sign-bit b)))
       (if (cl:= sa sb)
           0
           ;; sign bit is 1 if flonum is negative
           (if (cl:< sa sb) 1 -1))))
    ((and (floatp a) (float-infinity-p a))
     (if (cl:= (float-sign-bit a) 1) -1 1))
    ((and (floatp b) (float-infinity-p b))
     (if (cl:= (float-sign-bit b) 1) 1 -1))
    ((eql a b) 0)
    (t (let ((ra (rational a))
             (rb (rational b)))
         (if (cl:= ra rb)
             0
             (if (cl:< ra rb) -1 1))))))

(defmacro define-flonum-comparator (name form)
  (let ((clname (intern (string name) "CL")))
    `(defun ,name (&rest args)
       (if (every #'rationalp args)
           (apply #',clname args)
           (with-memoized-math-op (,clname args)
             (loop for (a b) on args
                   while b
                   always (let ((c (numeric-compare a b)))
                            ,form)))))))

(define-flonum-comparator sb-xc:< (eql c -1))
(define-flonum-comparator sb-xc:<= (or (eql c -1) (eql c 0)))
(define-flonum-comparator sb-xc:= (eql c 0))
(define-flonum-comparator sb-xc:>= (or (eql c 0) (eql c 1)))
(define-flonum-comparator sb-xc:> (eql c 1))

;;; what should (/= NaN NaN) return?  I'm not convinced that we have a
;;; consistent story.  On the other hand it looks like we never call
;;; /= in cross-compilation, let alone /= on NaNs.
(defun sb-xc:/= (&rest args)
  (if (every #'rationalp args)
      (apply #'cl:/= args)
      (with-memoized-math-op (/= args)
        (loop for (a . rest) on args
              always (loop for b in rest
                           for c = (numeric-compare a b)
                           always (cl:/= c 0))))))

(defmacro define-flonum-extremizer (name comparator)
  (let ((clname (intern (string name) "CL")))
    `(defun ,name (arg &rest rest &aux (args (cons arg rest)))
       (if (every #'rationalp args)
           (apply #',clname args)
           (with-memoized-math-op (,clname args)
             (let ((ret arg))
               (dolist (a rest ret)
                 (when (,comparator a ret)
                   (setf ret a)))))))))

(define-flonum-extremizer sb-xc:max sb-xc:>)
(define-flonum-extremizer sb-xc:min sb-xc:<)

(defun wrap-two-arg-fun (fun value)
  (lambda (&optional (x nil xp) y)
    (if xp (funcall fun x y) value)))

(defun sb-xc:+ (&rest args)
  (flet ((two-arg-+ (x y)
           (let ((format (pick-result-format x y)))
             (cond
               ((eql format 'rational) (cl:+ x y))
               ((and (flonum-minus-zero-p x)
                     (flonum-minus-zero-p y))
                (coerce -0.0 format))
               (t (flonum-from-rational (cl:+ (rational x) (rational y)) format))))))
    (if (every #'rationalp args)
        (apply #'cl:+ args)
        (with-memoized-math-op (+ args)
          (reduce (wrap-two-arg-fun #'two-arg-+ 0) args)))))

(defun sb-xc:- (arg &rest rest &aux (args (cons arg rest)))
  (flet ((one-arg-- (x)
           (etypecase x
             (rational (cl:- x))
             (single-float (make-single-float (logxor (ash -1 31) (single-float-bits x))))
             (double-float (%make-double-float (logxor (ash -1 63) (double-float-bits x))))))
         (two-arg-- (x y)
           (let ((format (pick-result-format x y)))
             (cond
               ((eql format 'rational) (cl:- x y))
               ((and (flonum-minus-zero-p x)
                     (and (zerop y) (not (flonum-minus-zero-p y))))
                (coerce -0.0 format))
               (t (flonum-from-rational (cl:- (rational x) (rational y)) format))))))
    (if (every #'rationalp args)
        (apply #'cl:- args)
        (with-memoized-math-op (- args)
          (if (null rest)
              (one-arg-- arg)
              (reduce #'two-arg-- args))))))

(defun sb-xc:* (&rest args)
  (flet ((two-arg-* (x y)
           (let ((format (pick-result-format x y)))
             (cond
               ((eql format 'rational) (cl:* x y))
               ((or (and (floatp x) (float-infinity-p x))
                    (and (floatp y) (float-infinity-p y)))
                (when (or (zerop x) (zerop y))
                  (error "Can't multiply infinity with 0."))
                (coerce (if (cl:= (sgn x) (sgn y))
                            single-float-positive-infinity
                            single-float-negative-infinity)
                        format))
               ((or (flonum-minus-zero-p x)
                    (flonum-minus-zero-p y))
                (coerce (if (cl:= (sgn x) (sgn y)) 0 -0.0) format))
               (t (flonum-from-rational (cl:* (rational x) (rational y)) format))))))
    (if (every #'rationalp args)
        (apply #'cl:* args)
        (with-memoized-math-op (* args)
          (reduce (wrap-two-arg-fun #'two-arg-* 1) args)))))

(defun sb-xc:/ (arg &rest rest &aux (args (cons arg rest)))
  (flet ((one-arg-/ (x)
           (cond
             ((rationalp x) (cl:/ x))
             ((zerop x)
              (float-sign x single-float-positive-infinity))
             (t (flonum-from-rational (cl:/ (rational x)) (type-of x)))))
         (two-arg-/ (x y)
           (let ((format (pick-result-format x y)))
             (cond
               ((eql format 'rational) (cl:/ x y))
               ((and (zerop x) (zerop y))
                (error "can't represent NaN for (/ 0 0)"))
               ((zerop y)
                (error "can't represent Inf for (/ x 0)"))
               ((zerop x)
                (coerce (if (cl:= (sgn x) (sgn y)) 0 -0.0) format))
               (t (flonum-from-rational (cl:/ (rational x) (rational y)) format))))))
    (if (every #'rationalp args)
        (apply #'cl:/ args)
        (with-memoized-math-op (/ args)
          (if (null rest)
              (one-arg-/ arg)
              (reduce #'two-arg-/ args))))))

(defun %sqrt (rational)
  (flet ((%%sqrt (rational initial)
           (let ((current initial))
             ;; why 7? our initial "guess" has at least ~1 bit of
             ;; precision (for e.g. RATIONAL = 2), and each iteration
             ;; gives 2n+1 bits, so 7 gives ~127 bits, which should be
             ;; enough for everybody.
             (dotimes (i 7 current)
               (setf current (cl:/ (cl:+ current (cl:/ rational current)) 2))))))
    (%%sqrt rational (cl:/ (isqrt (numerator rational)) (isqrt (denominator rational))))))

(defun sb-xc:sqrt (arg)
  (let ((format (if (rationalp arg) 'single-float (type-of arg))))
    (with-memoized-math-op (sqrt arg)
      (flonum-from-rational (%sqrt (rational arg)) format))))

;;; There seems to be no portable way to mask float traps, so right
;;; now we ignore them and hardcode special cases.
(defmacro sb-vm::with-float-traps-masked (traps &body body)
  (declare (ignore traps))
  #+nil
  (format *error-output*
          "~&(can't portably mask float traps, proceeding anyway)~%")
  `(progn ,@body))

(defun realpart (x) (if (realp x) x (complexnum-real x)))
(defun imagpart (x)
  (cond ((rationalp x) 0)
        ((single-float-p x) 0f0)
        ((double-float-p x) 0d0)
        (t (complexnum-imag x))))

(defun sb-vm::sign-extend (x size)
  (if (logbitp (1- size) x) (cl:dpb x (cl:byte size 0) -1) x))

;;; PI is needed in order to build the cross-compiler mainly so that vm-fndb
;;; can define bounds on irrational functions.
(defconstant pi 3.14159265358979323846264338327950288419716939937511L0)

(macrolet ((def (name lambda-list)
             `(defun ,(intern (string name) "SB-XC") ,lambda-list
                (declare (ignorable ,@lambda-list))
                (error "Unimplemented."))))
  (def acos (number))
  (def acosh (number))
  (def asin (number))
  (def asinh (number))
  (def atanh (number))
  (def cis (number))
  (def conjugate (number))
  (def cos (number))
  (def exp (number))
  (def phase (number))
  (def sin (number))
  (def sinh (number))
  (def tan (number))
  (def tanh (number)))

(defun atan (number1 &optional (number2 nil number2p))
  (if number2p
      (with-memoized-math-op (atan (list number1 number2))
        (error "Unimplemented."))
      (with-memoized-math-op (atan number1)
        (if (eql number1 1.4916681462400417d-154)
            number1
            (error "Unimplemented.")))))

(defun cosh (number)
  (with-memoized-math-op (cosh number)
    (case number
      ((0 0f0) 1f0)
      (0d0 1d0)
      (t (error "Unimplemented.")))))

(defun log (number &optional (base nil base-p))
  (validate-args number base)
  (when (eql number 0)
    (error 'division-by-zero :operation 'log :operands `(,number ,@(when base-p base))))
  (with-memoized-math-op (log (if base-p (list number base) number))
    (let ((format (pick-float-result-format number (if base-p base 0))))
      (if (zerop number)
          (coerce single-float-negative-infinity format)
          (case base
            ((nil)
             (let ((table '((1 . 0f0)
                            (10 . 2.3025851f0)
                            (#x1fffffff . 20.101269f0)
                            (#x20000000 . 20.101269f0)
                            (#x20000001 . 20.101269f0)
                            (#xfffffffffffffff . 41.58883f0)
                            (#x1000000000000000 . 41.58883f0)
                            (#x1000000000000001 . 41.58883f0)
                            (#x3fffffffffffffff . 42.975124f0)
                            (#x4000000000000000 . 42.975124f0)
                            (#x4000000000000001 . 42.975124f0)
                            (0.9999999999999999d0 . -1.1102230246251565d-16)
                            (1d0 . 0d0)
                            (2d0 . 0.6931471805599453d0)
                            (2.718281828459045d0 . 1d0)
                            (5.36870911d8 . 20.10126823437577d0)
                            (5.36870912d8 . 20.101268236238415d0)
                            (2.147483647d9 . 21.487562596892644d0)
                            (2.147483648d9 . 21.487562597358306d0)
                            (1.152921504606847d18 . 41.58883083359672d0)
                            (4.611686018427388d18 . 42.97512519471661d0)
                            (9.223372036854776d18 . 43.66827237527655d0))))
               (or (cdr (assoc number table))
                   (error "missing entry for (LOG ~A)" number))))
            ((10 10f0 10d0)
             (let ((table '((2d0 . 0.3010299956639812d0))))
               (or (cdr (assoc number table))
                   (error "missing entry for (LOG ~A 10)" number))))
            (t (error "missing entries for (LOG ~A ~A)" number base)))))))

;;; Canonicalize and write out the memoization table.
(defun dump-math-memoization-table (table stream)
  (format stream ";;; This file is machine-generated. DO NOT EDIT~2%")
  (format stream "~%(~%")
  (labels ((spelling-of (expr)
             ;; MUST not write package prefixes !
             ;; e.g. avoid writing a line like (COERCE (-33619991 SB-XC:DOUBLE-FLOAT) ...)
             (if (stringp expr)
                 (write-to-string expr :pretty nil :escape t)
                 (let ((hex (write-to-string expr :pretty nil :base 16 :radix t :escape nil))
                       (dec (write-to-string expr :pretty nil :base 10 :escape nil)))
                   (if (<= (length hex) (length dec))
                       hex
                       dec)))))
    ;; Record each <fun,args> combination to STREAM
    ;; Though all symbols we print, such as SINGLE-FLOAT, are accessible
    ;; in any of the SB- packages, ABCL would buggily output package prefixes
    ;; if ~S is used here.
    (let ((*print-pretty* nil))
      (maphash (lambda (key result)
                 (destructuring-bind (fun . args) key
                   (format stream "(~A ~A~{ ~A~})~%"
                           fun
                           ;; Why do ABS and RATIONAL write the unary arg as an atom
                           ;; but SQRT writes it as a singleton list?
                           (if (listp args)
                               (mapcar #'spelling-of args)
                               (spelling-of args))
                           ;; Can't use ENSURE-LIST. We need NIL -> (NIL)
                           (if (consp result)
                               result
                               (list result)))))
               table)))
  (format stream ")~%"))

(defun show-interned-numbers (stream)
  (flet ((to-native (x)
            (declare (ignorable x))
            #+host-quirks-sbcl
            (flet ((realize (r)
                     (if (rationalp r)
                         r
                         (etypecase r
                          (single-float
                           (host-sb-kernel:make-single-float (flonum-bits r)))
                          (double-float
                           (host-sb-kernel:make-double-float
                            (double-float-high-bits r)
                            (double-float-low-bits r)))))))
              (if (complexp x)
                  (cl:complex (realize (complexnum-real x))
                              (realize (complexnum-imag x)))
                  (realize x)))))
    (let (values)
      (format stream "~2&; Interned flonums:~%")
      (dolist (table (list *interned-single-floats*
                           *interned-double-floats*
                           *interned-complex-numbers*))
        (maphash (lambda (k v)
                   (let ((actual (to-native v)))
                     (format stream "; ~S -> ~S~@[ = ~D~]~%" k v actual)
                     (when actual
                       (when (member actual values)
                         ;; Duplicates means that the host's EQL
                         ;; would not answer correctly for certain inputs.
                         (error "Duplicate float in interned flonum table"))
                       (push actual values))))
                 table)))))

;;; Perform some simple checks
(assert (not (eq -0.0f0 -0.0d0)))
(assert (not (eq single-float-negative-infinity 0f0)))
(dolist (format '(single-float double-float))
  (assert (zerop (coerce 0 format)))
  (assert (zerop (coerce -0.0 format)))
  (assert (float-infinity-p (coerce single-float-positive-infinity format)))
  (assert (float-infinity-or-nan-p (coerce single-float-positive-infinity format)))
  (assert (not (float-nan-p (coerce single-float-positive-infinity format))))
  (assert (float-infinity-p (coerce single-float-negative-infinity format)))
  (assert (float-infinity-or-nan-p (coerce single-float-negative-infinity format)))
  (assert (not (float-nan-p (coerce single-float-negative-infinity format))))
  (assert (eq (coerce -0.0 format) (coerce -0.0 format)))
  (assert (eq (coerce single-float-positive-infinity format)
              (coerce single-float-positive-infinity format)))
  (assert (eq (coerce single-float-negative-infinity format)
              (coerce single-float-negative-infinity format)))
  (assert (eq (sb-xc:+ (coerce -0.0 format) (coerce 0 format)) (coerce 0 format)))
  (assert (eq (sb-xc:+ (coerce 0 format) (coerce -0.0 format)) (coerce 0 format)))
  (assert (eq (sb-xc:+ (coerce -0.0 format) (coerce -0.0 format)) (coerce -0.0 format)))
  (assert (eq (sb-xc:- (coerce 0 format)) (coerce -0.0 format)))
  (assert (eq (sb-xc:- (coerce -0.0 format)) (coerce 0 format)))
  (assert (eq (coerce single-float-positive-infinity format)
              (sb-xc:- (coerce single-float-negative-infinity format))))
  (assert (eq (coerce single-float-negative-infinity format)
              (sb-xc:- (coerce single-float-positive-infinity format))))
  (assert (eq (sb-xc:- (coerce 0 format) (coerce 0 format)) (coerce 0 format)))
  (assert (eq (sb-xc:- (coerce 0 format) (coerce -0.0 format)) (coerce 0 format)))
  (assert (eq (sb-xc:- (coerce -0.0 format) (coerce 0 format)) (coerce -0.0 format)))
  (assert (eq (sb-xc:- (coerce -0.0 format) (coerce -0.0 format)) (coerce 0 format)))
  (assert (eq (sb-xc:* (coerce 0 format) (coerce 0 format)) (coerce 0 format)))
  (assert (eq (sb-xc:* (coerce -0.0 format) (coerce 0 format)) (coerce -0.0 format)))
  (assert (eq (sb-xc:* (coerce 0 format) (coerce -0.0 format)) (coerce -0.0 format)))
  (assert (eq (sb-xc:* (coerce -0.0 format) (coerce -0.0 format)) (coerce 0 format)))
  (assert (eq (sb-xc:/ (coerce -0.0 format) (coerce -1 format)) (coerce 0 format)))
  (assert (eq (sb-xc:/ (coerce -0.0 format) (coerce 1 format)) (coerce -0.0 format)))
  (assert (eq (sb-xc:/ (coerce 0 format) (coerce -1 format)) (coerce -0.0 format)))
  (assert (eq (sb-xc:/ (coerce 0 format) (coerce 1 format)) (coerce 0 format)))
  (assert (eq (sb-xc:fceiling -1/2) -0.0f0))
  (assert (eq (sb-xc:fceiling (coerce -1/2 format)) (coerce -0.0 format)))
  (assert (eq (sb-xc:ffloor -1/2) (coerce -1 'single-float)))
  (assert (eq (sb-xc:ffloor (coerce -1/2 format)) (coerce -1 format)))
  (assert (eq (sb-xc:ftruncate -1/2) -0.0f0))
  (assert (eq (sb-xc:ftruncate (coerce -1/2 format)) (coerce -0.0 format)))
  (assert (eq (sb-xc:fround -1/2) -0.0f0))
  (assert (eq (sb-xc:fround (coerce -1/2 format)) (coerce -0.0 format)))
  (assert (equal (multiple-value-list (sb-xc:integer-decode-float 1.0f0))
                 '(8388608 -23 1)))
  (assert (equal (multiple-value-list (sb-xc:integer-decode-float 1.0d0))
                 '(4503599627370496 -52 1)))
  (let ((*break-on-signals* nil))
  (flet ((assert-not-number (x)
           (handler-case (rational x)
             (:no-error (x) (error "Expected an error, got ~S" x))
             (simple-error (x) (declare (ignore x))))))
    (let ((nan (make-single-float #b01111111101000000000000000000000)))
      ;;                             [ exp  ]
      (assert-not-number nan)
      (assert (float-nan-p nan))
      (assert (float-infinity-or-nan-p nan))
      (assert (not (float-infinity-p nan))))
    (assert-not-number single-float-negative-infinity)
    (assert-not-number single-float-positive-infinity)
    (assert-not-number double-float-negative-infinity)
    (assert-not-number double-float-positive-infinity))))

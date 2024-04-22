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

(eval-when (:compile-toplevel :load-toplevel :execute)

(defun flonum-from-rational (rational format)
  (ecase format
    (single-float (make-single-float (%single-bits-from-rational rational)))
    (double-float (%make-double-float (%double-bits-from-rational rational)))))

(defun make-float-infinity (sign format)
  ;; TOTAL-BITS is the size in bits. PRECISION includes the hidden
  ;; bit.
  (multiple-value-bind (total-bits precision)
      (ecase format
        (single-float (values 32 24))
        (double-float (values 64 53)))
    ;; The sign consumes 1 bit, but the significand has 1 hidden bit,
    ;; so it nets out the same in terms of how many remain for the
    ;; exponent.
    (let* ((exponent-bits (- total-bits precision))
           (bits
             (logior (ash sign (1- total-bits))
                     (ash (1- (ash 1 exponent-bits)) ; all 1s
                          ;; shift left by the number of represented
                          ;; significand bits. i.e. exclude the hidden bit.
                          (1- precision)))))
      (ecase format
        (single-float (make-single-float bits))
        (double-float (%make-double-float bits))))))

(defvar *floating-point-number-buffer* (make-array 100 :element-type 'character))

(defun parse-xfloat-math-file (stream table)
  ;; Ensure that we're reading the correct variant of the file
  ;; in case there is more than one set of floating-point formats.
  (assert (eq (read stream) :default))
  (let ((pkg (make-package "SB-FLOAT-GENIE" :use '("CL")))
        (line 0))
    (import 'make-single-float pkg)
    (import 'make-double-float pkg)
    (unwind-protect
         (dolist (expr (let ((*package* pkg)) (read stream)))
           (incf line)
           (destructuring-bind (fun args . values) expr
             (let* ((key (cons fun args))
                    (existsp (gethash key table)))
               (when existsp
                 (error "Line ~D of float cache: ~S is repeated" line key))
               (setf (gethash key table) values))))
      (delete-package pkg))))

(defun get-float-ops-cache (&aux (cache sb-cold::*math-ops-memoization*))
  (when (atom cache)
    (return-from get-float-ops-cache cache))
  (let ((table (car cache))
        (pathname))
    (when (= (hash-table-count table) 0)
      (with-open-file (stream (setq pathname (sb-cold::math-journal-pathname :input))
                              :if-does-not-exist nil)
        (when stream
          (parse-xfloat-math-file stream table)
          (setf (cdr cache) (hash-table-count table))
          (when cl:*compile-verbose*
            (format t "~&; Math journal: prefilled ~D entries from ~S~%"
                    (cdr cache) pathname)))))
    table))

(defun record-math-op (fun args &rest values)
  (let* ((cache sb-cold::*math-ops-memoization*)
         (table (if (atom cache) cache (car cache))))
    (setf (gethash (cons fun args) table) values))
  (values-list values))

;;; Disallow non-canonical symbols in the float math cache,
;;; or it gets very confusing as to whether the cache is dirty.
(defun canonical-math-op-args (expr)
  ;; Try to avoid consing a new list unless we have to.
  (labels ((check (expr)
             (cond ((consp expr) (and (check (car expr)) (check (cdr expr))))
                   ((symbolp expr) (eq (cl:symbol-package expr) *cl-package*))
                   (t)))
           (recons (expr)
             (cond ((consp expr) (cons (recons (car expr)) (recons (cdr expr))))
                   ((symbolp expr) (intern (string expr) *cl-package*))
                   (t expr))))
    (if (check expr) expr (recons expr))))

(defmacro with-memoized-math-op ((name key-expr) &body calculation)
  (assert (symbolp name))
  ;; In theory I could make this so that only a cache miss has to call SANIFY-MATH-OP-ARGS
  ;; so that in the frequently-occuring cases we do not have to make an extra pass over
  ;; the expression to determine whether was is canonical. But since only COERCE can have
  ;; a problem of non-canononical symbols, it's easiest to just always canonicalize
  ;; for COERCE, and nothing else.
  `(let ((fun ',(intern (string name) "CL"))
         (args ,(if (string= name 'coerce)
                    `(canonical-math-op-args ,key-expr)
                    key-expr)))
     (multiple-value-bind (answer foundp)
         (dx-let ((cache-key (cons fun args)))
           (gethash cache-key (get-float-ops-cache)))
       (if foundp
           (values-list answer)
           (multiple-value-call #'record-math-op fun args (progn ,@calculation))))))

(defun sb-cold::read-target-float (stream char)
  (let ((buffer *floating-point-number-buffer*)
        (index -1)
        string)
    (loop (setq char (read-char stream))
          (cond ((or (digit-char-p char)
                     (member char '(#\+ #\- #\. #\D #\E #\F #\L #\S) :test #'char-equal))
                 (setf (aref buffer (incf index)) char))
                (t
                 (unread-char char stream)
                 (return))))
    (when *read-suppress*
      (return-from sb-cold::read-target-float nil))
    (setf string (subseq buffer 0 (1+ index)))
    (multiple-value-bind (flonum nchars)
        (with-memoized-math-op (read-from-string (list *read-default-float-format* string))
          (let* ((marker-pos
                   (position-if (lambda (x)
                                  (member x '(#\E #\S #\F #\D #\L) :test #'char-equal))
                                string))
                 (exp-marker (if (and marker-pos
                                      (char-not-equal (char string marker-pos) #\E))
                                 (char-upcase (char string marker-pos))
                                 (ecase cl:*read-default-float-format*
                                   ((cl:single-float cl:short-float) #\F)
                                   ((cl:double-float cl:long-float)  #\D))))
                 (significand (if marker-pos (subseq string 0 marker-pos) string))
                 (dot-pos (position #\. significand))
                 (integer (if (eql dot-pos 0) 0 (parse-integer significand :start 0 :end dot-pos)))
                 (fraction (if (and dot-pos (cl:> (length significand) (1+ dot-pos)))
                               (cl:/ (parse-integer significand :start (1+ dot-pos))
                                     (cl:expt 10 (cl:- (length significand) (1+ dot-pos))))
                               0))
                 (exponent (if marker-pos
                               (parse-integer string :start (1+ marker-pos))
                               0))
                 (rational (cl:* (if (char= (char string 0) #\-)
                                     (cl:- integer fraction)
                                     (cl:+ integer fraction))
                                 (cl:expt 10 exponent)))
                 (format (ecase exp-marker
                           ((#\F #\S) 'single-float)
                           ((#\D #\L) 'double-float))))
            ;; Since we are working with rationals, we must special-case
            ;; negative zero (which does not have a natural rational
            ;; representation: explicitly look for -0 string.
            (if (or (string= significand "-0.0")
                    (string= significand "-.0")
                    (and (or (string= significand "-0") (string= significand "-0."))
                         (or marker-pos (error "~S has integer syntax" string))))
                (ecase format
                  (single-float (values #.(make-single-float (ash -1 31)) (length string)))
                  (double-float (values #.(%make-double-float (ash -1 63)) (length string))))
                (let ((result (flonum-from-rational rational format)))
                  (values result (length string))))))
      (declare (ignore nchars))
      flonum)))
) ; EVAL-WHEN

(defconstant single-float-negative-infinity (make-float-infinity -1 'single-float))
(defconstant single-float-positive-infinity (make-float-infinity 0 'single-float))
(defconstant double-float-negative-infinity (make-float-infinity -1 'double-float))
(defconstant double-float-positive-infinity (make-float-infinity 0 'double-float))

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
  (or (eq flonum $-0.0f0) (eq flonum $-0.0d0)))

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
  (flet ((target-num-fmt (num)
           (cond ((rationalp num) 'single-float)
                 ((floatp num) (type-of num))
                 (t (error "What? ~S" num)))))
    (let* ((result-fmt 'single-float)
           (result-contagion 0))
      (dolist (arg args result-fmt)
        (let* ((arg-fmt (target-num-fmt arg))
               ;; This is inadequate for complex numbers,
               ;; but we don't need them.
               (arg-contagion
                (position arg-fmt
                          '(short-float single-float double-float long-float))))
          (when (cl:> arg-contagion result-contagion)
            (setq result-fmt arg-fmt result-contagion arg-contagion)))))))

(defmacro validate-args (&rest args)
  `(when (or ,@(mapcar (lambda (arg) `(typep ,arg '(or cl:float cl:complex))) args))
     (error "Unexpectedly got host float/complex args")))

(defun rational (x)
  (cond
    ((rationalp x) x)
    ((float-infinity-or-nan-p x)
     (error "Can't convert Inf or NaN to rational."))
    (t (with-memoized-math-op (rational x)
         (cl:* (flonum-sign x) (flonum-mantissa x) (cl:expt 2 (flonum-exponent x)))))))

(defun rationalize (x)
  (if (rationalp x)
      x
      (with-memoized-math-op (rationalize x)
        (let ((rational (rational x)))
          (if (integerp rational)
              rational
              (error "Won't do (RATIONALIZE ~S) due to possible precision loss" x))))))

(defun coerce (object type)
  ;; OBJECT is validated prior to testing the quick bail out case, because supposing
  ;; that we accidentally got a host complex number or float, and we accidentally got
  ;; CL:FLOAT or something else in CL as the type, NUMBER would return NIL because host
  ;; floats do NOT satisfy "our" NUMBERP. But we want this to fail, not succeed.
  (validate-args object)
  (when (or (arrayp object) (listp object))
    (when (or (member type '(vector simple-vector simple-string simple-base-string list))
              (equal type '(simple-array character (*))))
      (return-from coerce (cl:coerce object type))) ; string or unspecialized array
    (let ((et (ecase (car type)
                (simple-array (destructuring-bind (et &optional dims) (cdr type)
                                (assert (or (eql dims 1) (equal dims '(*))))
                                et))
                (vector (destructuring-bind (et) (cdr type) et)))))
      (return-from coerce
        (sb-xc:make-array (length object) :element-type et
                                          :initial-contents object))))
  (unless (numberp object)
    (return-from coerce (cl:coerce object type)))
  (when (member type '(integer rational real))
    ;; This branch won't accept (coerce x 'real) if X is one of our
    ;; target-floats. We don't need that apparently.
    (assert (if (eq type 'integer) (integerp object) (rationalp object)))
    (return-from coerce object))
  (unless (member type '(float short-float single-float double-float long-float))
    (error "Can't COERCE ~S ~S" object type))
  (when (and (floatp object)
             (or (eq type 'float) (eq (type-of object) type)))
    (return-from coerce object))
  (with-memoized-math-op (coerce (list object type))
    (if (realp object)
        (cond ((and (flonum-minus-zero-p object)
                    (member type '(double-float single-float)))
               (ecase type (single-float $-0.0f0) (double-float $-0.0d0)))
              ((float-infinity-p object)
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
                 (flonum-from-rational source-value actual-type))))
        (error "Can't COERCE ~S ~S" object type))))

(macrolet ((define (name)
             `(progn
                (declaim (inline ,name))
                (defun ,name (x)
                  (if (rationalp x)
                      (,(intern (string name) "CL") x)
                      (,(symbolicate "XFLOAT-" name) x))))))
  (define abs)
  (define signum))

(defun xfloat-abs (x)
  (with-memoized-math-op (abs x)
    (if (float-sign-bit-set-p x)
        (sb-xc:- x)
        x)))

;;; Signum should return -0 of the correct type for -0 input.
;;; We don't need it currently.
(defun xfloat-signum (x)
  (if (zerop x)
      x
      (coerce (if (float-sign-bit-set-p x)
                  -1
                  1)
              (type-of x))))

(macrolet ((define (name float-fun)
             `(progn
                (declaim (inline ,name))
                (defun ,name (number divisor)
                  (if (and (rationalp number) (rationalp divisor))
                      (,(intern (string name) "CL") number divisor)
                      (,float-fun number divisor)))
                (defun ,float-fun (number divisor)
                  (declare (ignore number divisor))
                  (error "Unimplemented")))))
  (define mod xfloat-mod)
  (define rem xfloat-rem))

(defun float (number &optional (prototype nil prototypep))
  (validate-args number prototype)
  (with-memoized-math-op (float (cons number (if prototypep (list prototype))))
    (let* ((format
             (if (not prototypep) 'single-float (type-of prototype))))
      (flonum-from-rational (rational number) format))))

;;; Produce a float with the magnitude of FLOAT2 and sign of FLOAT1.
(defun float-sign (float1 &optional (float2 (float 1 float1) float2p))
  (validate-args float1 float2)
  (with-memoized-math-op (float-sign (cons float1 (if float2p (list float2))))
    (typecase float2
      (single-float
       (let* ((sign (float-sign-bit float1))
              (exponent (%single-exponent-bits float2))
              (mantissa (%single-mantissa-bits float2))
              (bits (%single-bits-from sign exponent mantissa)))
         (coerce (make-single-float bits) (type-of float1))))
      (double-float
       (let* ((sign (float-sign-bit float1))
              (exponent (%double-exponent-bits float2))
              (mantissa (%double-mantissa-bits float2))
              (bits (%double-bits-from sign exponent mantissa)))
         (%make-double-float bits))))))

(macrolet ((define (name float-fun)
             (let ((clname (intern (string name) "CL")))
               `(progn
                  (declaim (inline ,name))
                  (defun ,name (number &optional (divisor 1))
                    (if (and (rationalp number) (rationalp divisor))
                        (,clname number divisor)
                        (,float-fun number divisor)))
                  (defun ,float-fun (number divisor)
                    (let ((type (if (or (typep number 'double-float)
                                        (typep divisor 'double-float))
                                    'double-float
                                    'single-float)))
                      (with-memoized-math-op (,name (list number divisor))
                        (if (flonum-minus-zero-p number)
                            (values 0
                                    (coerce (if (< divisor 0) 0 $-0.0) type))
                            (multiple-value-bind (q r)
                                (,clname (rational number) (rational divisor))
                              (values q (flonum-from-rational r type)))))))))))
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
             `(progn
                (defun ,name (number &optional (divisor 1 divisorp))
                  (let ((type (if (or (typep number 'double-float)
                                      (typep divisor 'double-float))
                                  'double-float
                                  'single-float))
                        (format (pick-result-format number divisor)))
                    (with-memoized-math-op (,name (list* number (and divisorp (list divisor))))
                      (multiple-value-bind (q r)
                          (,clname (rational number) (rational divisor))
                        (let ((remainder (if (eql format 'rational) r (flonum-from-rational r format))))
                          (if (cl:= q 0)
                              (values (coerce (if (cl:= (sgn number) (sgn divisor)) 0 $-0.0)
                                              type)
                                      remainder)
                              (values (flonum-from-rational q type) remainder))))))))))
  (define fceiling cl:ceiling)
  (define ffloor cl:floor)
  (define fround cl:round)
  (define ftruncate cl:truncate))

(defun expt (base power)
  (cond
    ((not (integerp power))
     (error "Unimplemented: EXPT with non-integer power"))
    ((rationalp base) (cl:expt base power))
    (t
     (with-memoized-math-op (expt (list base power))
       (if (zerop power)
           (coerce 1 (type-of base))
           (flonum-from-rational
            (cl:expt (rational base) power)
            (pick-result-format base power)))))))

(defun %unary-truncate (number)
  (typecase number
    (integer number)
    (ratio (values (truncate (numerator number) (denominator number))))
    ((or single-float double-float #+long-float long-float)
     (error "Unimplemented."))))

(defun %unary-ftruncate (number)
  (typecase number
    (integer number)
    (ratio (values (ftruncate (numerator number) (denominator number))))
    ((or single-float double-float #+long-float long-float)
     (error "Unimplemented."))))

(defun %unary-round (number)
  (typecase number
    (integer number)
    (ratio (values (round (numerator number) (denominator number))))
    ((or single-float double-float #+long-float long-float)
     (error "Unimplemented."))))

(defun %unary-fround (number)
  (typecase number
    (integer number)
    (ratio (values (fround (numerator number) (denominator number))))
    ((or single-float double-float #+long-float long-float)
     (error "Unimplemented."))))

(defun scale-float (f ex)
  (validate-args f)
  (with-memoized-math-op (scale-float (list f ex))
    (flonum-from-rational (cl:* (rational f) (expt 2 ex)) (type-of f))))

(defun scale-single-float (f ex)
  (validate-args f)
  (scale-float f ex))

(defun scale-double-float (f ex)
  (validate-args f)
  (scale-float f ex))

(defun float-infinity-p (flonum)
  (typecase flonum
    (single-float
     (and (cl:= (%single-exponent-bits flonum) #xff) (cl:= (%single-mantissa-bits flonum) 0)))
    (double-float
     (and (cl:= (%double-exponent-bits flonum) #x7ff) (cl:= (%double-mantissa-bits flonum) 0)))))

(defun float-nan-p (flonum)
  (typecase flonum
    (single-float
     (and (cl:= (%single-exponent-bits flonum) #xff) (cl:/= (%single-mantissa-bits flonum) 0)))
    (double-float
     (and (cl:= (%double-exponent-bits flonum) #x7ff) (cl:/= (%double-mantissa-bits flonum) 0)))))

(defun float-infinity-or-nan-p (flonum)
  (typecase flonum
    (single-float (cl:= (%single-exponent-bits flonum) #xff))
    (double-float (cl:= (%double-exponent-bits flonum) #x7ff))))

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
                (coerce $-0.0 format))
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
                (coerce $-0.0 format))
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
               ((or (flonum-minus-zero-p x)
                    (flonum-minus-zero-p y))
                (coerce (if (cl:= (sgn x) (sgn y)) 0 $-0.0) format))
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
              (if (cl:= (float-sign-bit x) 1)
                  (coerce single-float-negative-infinity (type-of x))
                  (coerce single-float-positive-infinity (type-of x))))
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
                (coerce (if (cl:= (sgn x) (sgn y)) 0 $-0.0) format))
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
    (with-memoized-math-op (sqrt (list arg))
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
        ((single-float-p x) $0f0)
        ((double-float-p x) $0d0)
        (t (complexnum-imag x))))

(defun sb-vm::sign-extend (x size)
  (if (logbitp (1- size) x) (cl:dpb x (cl:byte size 0) -1) x))

;;; PI is needed in order to build the cross-compiler mainly so that vm-fndb
;;; can define bounds on irrational functions.
(defconstant pi $3.14159265358979323846264338327950288419716939937511L0)

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
      (with-memoized-math-op (atan (list number1))
        (if (eql number1 $1.4916681462400417d-154)
            number1
            (error "Unimplemented.")))))

(defun cosh (number)
  (with-memoized-math-op (cosh (list number))
    (case number
      ((0 $0f0) $1f0)
      ($0d0 $1d0)
      (t (error "Unimplemented.")))))

(defun log (number &optional (base nil base-p))
  (validate-args number base)
  (with-memoized-math-op (log (cons number (if base-p (list base))))
    (let ((format (pick-float-result-format number (if base-p base 0))))
      (if (zerop number)
          (coerce single-float-negative-infinity format)
          (case base
            ((nil)
             (let ((table '((1 . $0f0)
                            (10 . $2.3025851f0)
                            (#x1fffffff . $20.101269f0)
                            (#x20000000 . $20.101269f0)
                            (#x20000001 . $20.101269f0)
                            (#xfffffffffffffff . $41.58883f0)
                            (#x1000000000000000 . $41.58883f0)
                            (#x1000000000000001 . $41.58883f0)
                            (#x3fffffffffffffff . $42.975124f0)
                            (#x4000000000000000 . $42.975124f0)
                            (#x4000000000000001 . $42.975124f0)
                            ($0.9999999999999999d0 . $-1.1102230246251565d-16)
                            ($1d0 . $0d0)
                            ($2d0 . $0.6931471805599453d0)
                            ($2.718281828459045d0 . $1d0)
                            ($5.36870911d8 . $20.10126823437577d0)
                            ($5.36870912d8 . $20.101268236238415d0)
                            ($2.147483647d9 . $21.487562596892644d0)
                            ($2.147483648d9 . $21.487562597358306d0)
                            ($1.152921504606847d18 . $41.58883083359672d0)
                            ($4.611686018427388d18 . $42.97512519471661d0)
                            ($9.223372036854776d18 . $43.66827237527655d0))))
               (or (cdr (assoc number table))
                   (error "missing entry for (LOG ~A)" number))))
            ((10 $10f0 $10d0)
             (let ((table '(($2d0 . $0.3010299956639812d0))))
               (or (cdr (assoc number table))
                   (error "missing entry for (LOG ~A 10)" number))))
            (t (error "missing entries for (LOG ~A ~A)" number base)))))))

;;; The full logic of MAYBE-EXACT-RECIPROCAL is defined in 'float-tran'
;;; but we don't want to use that in the cross-compiler (yet, if ever)
;;; because it needs INTEGER-DECODE-FLOAT which we haven't emulated
;;; using our flonum abstraction. So just mock out the answers.
(defun sb-c::maybe-exact-reciprocal (x)
  (cond ((eql x $2.0d0) $.5d0)
        ((eql x $10.0d0) nil)
        ((or (eql x #.(make-flonum #x3FE62E42FEFA39EF 'double-float))
             (eql x #.(make-flonum #x3FF71547652B82FE 'double-float)))
         nil)
        ((eql x #.(make-flonum #x49742400 'single-float))
         nil)
        (t (error "MAYBE-EXACT-RECIPROCAL: didn't expect ~S" x))))

;;; Canonicalize and write out the memoization table. Order it by function,
;;; then number of arguments, then each argument's value. Rational precedes
;;; single-float which precedes double-float, then order by bits.
(defun dump-math-memoization-table (table stream)
  (format stream ";;; This file is machine-generated. DO NOT EDIT~2%")
  (format stream ":DEFAULT~%(~%")
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
(assert (not (eq $-0.0f0 $-0.0d0)))
(assert (not (eq single-float-negative-infinity $0f0)))
(dolist (format '(single-float double-float))
  (assert (zerop (coerce 0 format)))
  (assert (zerop (coerce $-0.0 format)))
  (assert (float-infinity-p (coerce single-float-positive-infinity format)))
  (assert (float-infinity-or-nan-p (coerce single-float-positive-infinity format)))
  (assert (not (float-nan-p (coerce single-float-positive-infinity format))))
  (assert (float-infinity-p (coerce single-float-negative-infinity format)))
  (assert (float-infinity-or-nan-p (coerce single-float-negative-infinity format)))
  (assert (not (float-nan-p (coerce single-float-negative-infinity format))))
  (assert (eq (coerce $-0.0 format) (coerce $-0.0 format)))
  (assert (eq (coerce single-float-positive-infinity format)
              (coerce single-float-positive-infinity format)))
  (assert (eq (coerce single-float-negative-infinity format)
              (coerce single-float-negative-infinity format)))
  (assert (eq (sb-xc:+ (coerce $-0.0 format) (coerce 0 format)) (coerce 0 format)))
  (assert (eq (sb-xc:+ (coerce 0 format) (coerce $-0.0 format)) (coerce 0 format)))
  (assert (eq (sb-xc:+ (coerce $-0.0 format) (coerce $-0.0 format)) (coerce $-0.0 format)))
  (assert (eq (sb-xc:- (coerce 0 format)) (coerce $-0.0 format)))
  (assert (eq (sb-xc:- (coerce $-0.0 format)) (coerce 0 format)))
  (assert (eq (coerce single-float-positive-infinity format)
              (sb-xc:- (coerce single-float-negative-infinity format))))
  (assert (eq (coerce single-float-negative-infinity format)
              (sb-xc:- (coerce single-float-positive-infinity format))))
  (assert (eq (sb-xc:- (coerce 0 format) (coerce 0 format)) (coerce 0 format)))
  (assert (eq (sb-xc:- (coerce 0 format) (coerce $-0.0 format)) (coerce 0 format)))
  (assert (eq (sb-xc:- (coerce $-0.0 format) (coerce 0 format)) (coerce $-0.0 format)))
  (assert (eq (sb-xc:- (coerce $-0.0 format) (coerce $-0.0 format)) (coerce 0 format)))
  (assert (eq (sb-xc:* (coerce 0 format) (coerce 0 format)) (coerce 0 format)))
  (assert (eq (sb-xc:* (coerce $-0.0 format) (coerce 0 format)) (coerce $-0.0 format)))
  (assert (eq (sb-xc:* (coerce 0 format) (coerce $-0.0 format)) (coerce $-0.0 format)))
  (assert (eq (sb-xc:* (coerce $-0.0 format) (coerce $-0.0 format)) (coerce 0 format)))
  (assert (eq (sb-xc:/ (coerce $-0.0 format) (coerce -1 format)) (coerce 0 format)))
  (assert (eq (sb-xc:/ (coerce $-0.0 format) (coerce 1 format)) (coerce $-0.0 format)))
  (assert (eq (sb-xc:/ (coerce 0 format) (coerce -1 format)) (coerce $-0.0 format)))
  (assert (eq (sb-xc:/ (coerce 0 format) (coerce 1 format)) (coerce 0 format)))
  (assert (eq (sb-xc:fceiling -1/2) $-0.0f0))
  (assert (eq (sb-xc:fceiling (coerce -1/2 format)) (coerce $-0.0 format)))
  (assert (eq (sb-xc:ffloor -1/2) (coerce -1 'single-float)))
  (assert (eq (sb-xc:ffloor (coerce -1/2 format)) (coerce -1 format)))
  (assert (eq (sb-xc:ftruncate -1/2) $-0.0f0))
  (assert (eq (sb-xc:ftruncate (coerce -1/2 format)) (coerce $-0.0 format)))
  (assert (eq (sb-xc:fround -1/2) $-0.0f0))
  (assert (eq (sb-xc:fround (coerce -1/2 format)) (coerce $-0.0 format)))
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

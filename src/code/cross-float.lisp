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

(defun single-float-bits (target-flonum)
  (flonum-%bits (the single-float target-flonum)))

(defun double-float-bits (target-flonum)
  (flonum-%bits (the double-float target-flonum)))

(defun double-float-low-bits (x)
  (logand (double-float-bits x) #xffffffff))

(defun double-float-high-bits (x)
  (ash (double-float-bits x) -32))

(flet ((output-part (x stream)
         (typecase x
           (single-float
            (format stream "(MAKE-SINGLE-FLOAT #x~x)" (flonum-%bits x)))
           (double-float
            (format stream "(MAKE-DOUBLE-FLOAT #x~x #x~x)"
                    (double-float-high-bits x)
                    (double-float-low-bits x)))
           (rational
            (prin1 x stream)))))
  (defmethod print-object ((self float) stream)
    (write-string "#." stream)
    (output-part self stream))

  (defmethod print-object ((self complexnum) stream)
    (write-string "#.(COMPLEX " stream)
    (output-part (complexnum-real self) stream)
    (write-char #\Space stream)
    (output-part (complexnum-imag self) stream)
    (write-char #\) stream)))

(defun compute-mantissa (significand exponent nbits)
  (flet ((show-bits ()
           #+nil
           (format t "~&shift: ~b ~d ~s~%"
                   significand
                   exponent
                   (if (minusp exponent)
                       (/ (cl:coerce significand 'cl:long-float) (ash 1 (- exponent)))
                       (* (cl:coerce significand 'cl:long-float) (ash 1 exponent))))))
    (show-bits)
    (let ((shift-out 0))
      (loop while (> (integer-length significand) nbits)
            do (setq shift-out (logand significand 1)
                     significand (ash significand -1)
                     exponent (1+ exponent))
               (show-bits))
      (when (= shift-out 1)
        ;; Rounding is complicated. Thankfully I can cross-check the answer.
        (incf significand)
        (when (> (integer-length significand) nbits) ; Shift once moe
          (setq significand (ash significand -1)
                exponent (1+ exponent))))
      ;; - unset the implied 1 bit of the mantissa.
      ;; - move the binary point. CL's notion of the point is to the right
      ;;   of the LSB, but IEEE format has it to the left of the MSB.
      (values (cl:ldb (cl:byte (1- nbits) 0) significand)
              (+ exponent (1- nbits))))))

#+host-quirks-sbcl
(defun get-float-bits (x)
  (etypecase x
    (cl:double-float
     ;; DOUBLE-FLOAT-BITS didn't exist as a thing until recently,
     ;; and even then it only exists if the host is 64-bit.
     (logior (ash (host-sb-kernel:double-float-high-bits x) 32)
             (host-sb-kernel:double-float-low-bits x)))
    (cl:single-float
     (host-sb-kernel:single-float-bits x))))

;;; Convert host float X to target representation.
;;; TOTAL-BITS is the size in bits.
;;; PRECISION includes the hidden bit.
;;; The exceptional symbols for X are acceptable.
(defun float-to-bits (x total-bits precision)
  (when (and (cl:floatp x) (cl:= x 0))
    (return-from float-to-bits 0))
  (flet ((set-sign (sign unsigned-result) ; SIGN is -1 or 0
           (logior (ash sign (1- total-bits)) unsigned-result)))
    ;; The sign consumes 1 bit, but the significand has 1 hidden bit,
    ;; so it nets out the same in terms of how many remain for the exponent.
    (let* ((exponent-bits (- total-bits precision))
           (exp-max (1- (ash 1 (1- exponent-bits))))
           (exp-min (cl:- (1- exp-max))))
      (when (symbolp x)
        (return-from float-to-bits
          (ecase x
            ((:-infinity :+infinity)
             (set-sign (if (eq x :-infinity) -1 0)
                       (ash (1- (ash 1 exponent-bits)) ; all 1s
                            ;; shift left by the number of represented
                            ;; significand bits. i.e. exclude the hidden bit.
                            (1- precision))))
            (:minus-zero
             (set-sign -1 0)))))
      (assert (cl:= (cl:float-radix x) 2))
      ;; If the proxy object (the host's floating-point number) does not have
      ;; at least the number of bits we need, conversion to bits will fail.
      ;; Though that's not strictly true - we might get away with less precision
      ;; in the host for certain values - this is the easiest thing to assert.
      (assert (cl:>= (cl:float-precision x) precision))
      (multiple-value-bind (host-significand host-exponent sign)
          (cl:integer-decode-float x)
        (multiple-value-bind (target-significand target-exponent)
            (compute-mantissa host-significand host-exponent precision)
          (assert (cl:<= exp-min target-exponent exp-max)) ; exponent range
          (let ((answer (set-sign (if (minusp sign) -1 0)
                                  (logior (ash (cl:+ target-exponent exp-max) ; exp.bias = Emax
                                               (1- precision))
                                          target-significand))))
            ;; When self-hosted we can cross-check against the authoritative answer
            ;; assuming the host doesn't have a counteracting bug.
            #+host-quirks-sbcl
            (let ((authoritative-answer
                   (get-float-bits (ecase total-bits
                                    (32 (cl:coerce x 'cl:single-float))
                                    (64 (cl:coerce x 'cl:double-float))))))
              (unless (= answer authoritative-answer)
                (error "discrepancy in float-bits ~s:~% ~v,'0b expect~% ~v,'0b got~%"
                       x
                       total-bits (cl:ldb (cl:byte total-bits 0) authoritative-answer)
                       total-bits (cl:ldb (cl:byte total-bits 0) answer))))
            answer))))))

;;; To ensure that target numbers compare by EQL correctly under the host's
;;; definition of EQL (so that we don't have to intercept it and all else
;;; that uses EQL such as EQUAL, EQUALP and sequence traversers etc),
;;; we enforce that EQL numbers are in fact EQ.
(defvar *interned-numbers* (make-hash-table :test 'equal))

(defun make-flonum (value format)
  (let* ((bits (etypecase value
                 (integer
                  (ecase format
                    (single-float (assert (typep value '(signed-byte 32))))
                    (double-float (assert (typep value '(signed-byte 64)))))
                  value)
                 ((or cl:float keyword)
                  (ecase format
                    (single-float (float-to-bits value 32 24))
                    (double-float (float-to-bits value 64 53))))))
         (flonum (ensure-gethash (cons bits format) *interned-numbers*
                                 (%make-flonum bits format))))
    (unless (integerp value)
      (setf (flonum-%value flonum) value))
    flonum))

(defvar *floating-point-number-buffer* (make-array 100 :element-type 'character))
(defun sb-cold::read-target-float (stream char)
  (let ((buffer *floating-point-number-buffer*)
        (index -1))
    (loop (setq char (read-char stream))
          (cond ((or (digit-char-p char)
                     (member char '(#\+ #\- #\. #\E #\S #\F #\D #\L) :test #'char-equal))
                 (setf (aref buffer (incf index)) char))
                (t
                 (unread-char char stream)
                 (return))))
    (when *read-suppress*
      (return-from sb-cold::read-target-float nil))
    (let* ((string (subseq buffer 0 (1+ index)))
           (marker-pos
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
           (format (ecase exp-marker
                    ((#\F #\S) 'single-float)
                    ((#\D #\L) 'double-float))))
      ;; Since we don't know whether the host will parse -0.0 as negative,
      ;; we can't just parse and then always negate, because that might negate
      ;; twice if the host does support it. So explicitly look for -0 string.
      (if (or (string= significand "-0.0")
              (string= significand "-.0")
              (and (or (string= significand "-0") (string= significand "-0."))
                   (or marker-pos (error "~S has integer syntax" string))))
          (ecase format
            (single-float (make-flonum :minus-zero 'single-float))
            (double-float (make-flonum :minus-zero 'double-float)))
          (make-flonum (let ((sb-cold::*choke-on-host-irrationals* nil))
                         (if marker-pos  ; change it, in case it was #\E
                             (setf (char string marker-pos) exp-marker)
                             (setq string (concatenate 'string string (list exp-marker #\0))))
                         (read-from-string string))
                       format)))))
) ; end EVAL-WHEN

(defun float-format-bits (format)
  (ecase format
   (single-float 32)
   (double-float 64)))

;;; Preload the interned flonum table
(dolist (format '(single-float double-float))
  (let ((x (make-flonum 0 format)))
    (setf (flonum-%value x) (cl:coerce 0 (intern (string format) "CL"))))
  (let ((sign (ash -1 (1- (float-format-bits format)))))
    (let ((x (make-flonum sign format)))
      (setf (flonum-%value x) :minus-zero)))
  (make-flonum :+infinity format)
  (make-flonum :-infinity format))

(defun float-ops-cache-insert (key values table)
  ;; Verify results (when possible) prior to inserting into the hash-table.
  ;; If we were to support different floating-point formats across the various
  ;; backends, this check should confined to the scenarios where the host's
  ;; precision is at least as much as the target's precision.
  #+host-quirks-sbcl
  (let ((fun (car key))
        (args (cdr key)))
    (flet ((native-flonum-value (x &aux (bits (flonum-%bits x)))
             (ecase (flonum-format x)
               (single-float (host-sb-kernel:make-single-float bits))
               (double-float (host-sb-kernel:make-double-float
                              (ash bits -32) (ldb (byte 32 0) bits))))))
      (let ((authoritative-answer
              (multiple-value-list
               (host-sb-kernel::with-float-traps-masked (:overflow :divide-by-zero)
                 (apply (intern (string fun) "CL")
                        (mapcar (lambda (x)
                                  (etypecase x
                                    (float (native-flonum-value x))
                                    (rational x)
                                    (symbol (intern (string x) "CL"))))
                                (ensure-list args)))))))
        (unless (equal authoritative-answer
                       (mapcar (lambda (value)
                                 (if (floatp value)
                                     (native-flonum-value value)
                                     value))
                               values))
          (#+sb-devel cerror #+sb-devel "Ignore"
           #-sb-devel format #-sb-devel t
           "~&//CROSS-FLOAT DISCREPANCY!
// CACHE: ~S -> ~S~%// HOST : ~@[#x~X = ~]~S~%"
                  key values
                  (when (cl:floatp authoritative-answer)
                    (get-float-bits authoritative-answer))
                  authoritative-answer)))))
  (setf (gethash key table) (if (singleton-p values) (car values) (cons '&values values))))

(defun get-float-ops-cache (&aux (cache sb-cold::*math-ops-memoization*))
  (when (atom cache)
    (return-from get-float-ops-cache cache))
  (let ((table (car cache)))
    (when (zerop (hash-table-count table))
      (with-open-file (stream "float-math.lisp-expr" :if-does-not-exist nil)
        (when stream
          ;; Ensure that we're reading the correct variant of the file
          ;; in case there is more than one set of floating-point formats.
          (assert (eq (read stream) :default))
          (let ((*package* (find-package "SB-KERNEL")))
            (dolist (expr (read stream))
              (destructuring-bind (fun args . values) expr
                ;; some symbols, such as SQRT, read as XC-STRICT-CL:SQRT
                ;; from the SB-KERNEL package, but the cache key should
                ;; always use the symbol in the CL package.
                (float-ops-cache-insert (cons (intern (string fun) "CL") args)
                                        (if (and (symbolp (first values))
                                                 (string= (symbol-name (first values))
                                                          "&VALUES"))
                                            (rest values)
                                            values)
                                        table))))
          (setf (cdr cache) (hash-table-count table))
          (when cl:*compile-verbose*
            (format t "~&; Float-ops cache prefill: ~D entries~%" (cdr cache))))))
    table))

(defun record-math-op (key &rest values)
  (let* ((cache sb-cold::*math-ops-memoization*)
         (table (if (atom cache) cache (car cache)))
         (fun (car key))
         (args (cdr key))
         ;; args list is potentially on stack, so copy it
         (key (cons fun (if (listp args) (copy-list args) args))))
    (float-ops-cache-insert key values table))
  (apply #'values values))

(defmacro with-memoized-math-op ((name key-expr) calculation)
  `(dx-let ((cache-key (cons ',(intern (string name) "CL") ,key-expr)))
     (multiple-value-bind (answer foundp) (gethash cache-key (get-float-ops-cache))
       (if foundp
           (if (and (listp answer)
                    (eq (first answer) '&values))
               (values-list (rest answer))
               answer)
           (multiple-value-call #'record-math-op cache-key ,calculation)))))

;;; REAL and IMAG are either host integers (therefore EQL-comparable)
;;; or if not, have already been made EQ-comparable by hashing.
(defun complex (re im)
  (if (or (and (floatp re) (floatp im) (eq (flonum-format re) (flonum-format im)))
          ;; Complex rationals can't have 0 imaginary part.
          ;; It ought to have been canonicalized to a purely real rational.
          ;; This is not done here, though maybe it should be,
          ;; because (cl:complex 1 0) = 1.
          (and (rationalp re) (rationalp im) (/= im 0)))
      (values (ensure-gethash (list re im) *interned-numbers*
                              (%make-complexnum re im)))
      (error "Won't make complex number from ~s ~s" re im)))

(defun float-sign-bit (float)
  (declare (type float float))
  (logand (ash (flonum-%bits float)
               (- (1- (float-format-bits (flonum-format float)))))
          1))

(defun calculate-flonum-value (x &optional (nan-errorp t) &aux (bits (flonum-%bits x)))
  ;; Convert the bits of a target float to an object with which we can perform
  ;; arithmetic in the host.  This will of course be an IEEE-standard float,
  ;; constructed using only standard functions.
  ;; When self-hosted, cross-check the result against the authoritative answer.
  (flet ((bits-to-float (bits format-nbits precision host-type
                         &aux (mantissa-nbits (1- precision))) ; less the hidden bit
           (let* ((exp-nbits (- format-nbits mantissa-nbits 1))
                  (max-exp   (1- (ash 1 exp-nbits)))
                  (exp       (cl:ldb (cl:byte exp-nbits mantissa-nbits) bits))
                  (mantissa  (cl:ldb (cl:byte mantissa-nbits 0) bits)))
             (when (or (= exp max-exp) (= exp 0)) ; infinity or NaN, denormal or 0
               (if (and (= exp max-exp) (not (zerop mantissa)) (not nan-errorp))
                   (return-from calculate-flonum-value :nan)
                   (error "Can't cast bits to float: ~x" bits)))
             (let ((mantissa (logior (ash 1 mantissa-nbits) ; hidden bit
                                     mantissa)))
               ;; Subtract the exponent bias and account for discrepancy in binary
               ;; point placement in the IEEE representation and the CL function
               (decf exp (+ (cl:floor max-exp 2) mantissa-nbits))
               (let ((value (cl:scale-float (cl:coerce mantissa host-type) exp)))
                 (if (logbitp (1- format-nbits) bits) (cl:- value) value))))))
    (ecase (flonum-format x)
      (single-float
       (let ((value (bits-to-float bits 32 24 'cl:single-float)))
         #+host-quirks-sbcl ; check with EQL, not =, to ensure same type and value
         (assert (eql (host-sb-kernel:make-single-float bits) value))
         value))
      (double-float
       (let ((value (bits-to-float bits 64 53 'cl:double-float)))
         #+host-quirks-sbcl ; ditto
         (assert (eql (host-sb-kernel:make-double-float
                       (ash bits -32) (cl:ldb (cl:byte 32 0) bits))
                      value))
         value)))))

;;; Cast target number to a host real number.
;;; HOST-REALIZE sounds like it would be a cute name for this, but it's too cute.
(declaim (inline realnumify realnumify* collapse-zeros))

;;; Turn X into a number that the host can operate on.
;;; Works on rationals and "uninteresting" floats.
(defun realnumify (x)
  (cond ((rationalp x) x)
        ((floatp x)
         (let ((value (flonum-%value x)))
           (cond ((cl:floatp value) value)
                 ((null value) ; must not be one of the exceptional symbols
                  (setf (flonum-%value x) (calculate-flonum-value x)))
                 (t ; infinity or minus-zero
                  (error "~S (~D) has no portable value" value x)))))
        (t (error "Got host float"))))

(defun realnumify* (args) (mapcar #'realnumify args))

(defun collapse-zeros (a b)
  (values (if (zerop a) 0 (realnumify a))
          (if (zerop b) 0 (realnumify b))))

;;; Use these predicate to guard operations that we wish to implement
;;; but for which full support for signed zeros and infinities is incomplete.
(declaim (inline operable-float-p inoperable-float-p operable-num-p))
(defun operable-float-p (x)
  (and (floatp x) (cl:floatp (flonum-%value x))))
(defun inoperable-float-p (x)
  (and (floatp x) (not (cl:floatp (flonum-%value x)))))
(defun operable-num-p (arg)
  (or (rationalp arg) (operable-float-p arg)))

(defun pick-result-format (&rest args)
  (flet ((target-num-fmt (num)
           (cond ((rationalp num) 'rational)
                 ((floatp num) (flonum-format num))
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

(defmacro validate-args (&rest args)
  `(when (or ,@(mapcar (lambda (arg) `(typep ,arg '(or cl:float cl:complex))) args))
     (error "Unexpectedly got host float/complex args")))

(defun rational (x)
  (if (rationalp x)
      x
      (with-memoized-math-op (rational x)
        (multiple-value-bind (whole frac) (cl:ftruncate (realnumify x))
          (if (cl:zerop frac)
              (cl:rational whole)
              ;; We'd have to have the exact same implementation of
              ;; RATIONAL as the target will have in order to guarantee
              ;; that compile-time use of RATIONAL is equivalent
              ;; to runtime use. So don't do it.
              (error "Won't do (RATIONAL ~S) due to possible precision loss" x))))))

(defun rationalize (x)
  (if (rationalp x)
      x
      (with-memoized-math-op (rationalize x)
        (multiple-value-bind (whole frac) (cl:ftruncate (realnumify x))
          (if (cl:zerop frac)
              (cl:rationalize whole)
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
             (or (eq type 'float) (eq (flonum-format object) type)))
    (return-from coerce object))
  (with-memoized-math-op (coerce (list object type))
    (if (realp object)
        (let ((actual-type (if (member type '(double-float long-float))
                               'double-float
                               'single-float))
              (source-value (realnumify object)))
          (make-flonum (cl:coerce source-value
                                  (ecase actual-type
                                    (single-float 'cl:single-float)
                                    (double-float 'cl:double-float)))
                       actual-type))
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
    (if (= (float-sign-bit x) 0) x (sb-xc:- x))))

;;; Signum should return -0 of the correct type for -0 input.
;;; We don't need it currently.
(defun xfloat-signum (x)
  (if (zerop x)
      x
      (coerce (if (= (float-sign-bit x) 1)
                  -1
                  1)
              (flonum-format x))))

;;; This is simple enough that it's not necessary to memoize all calls.
(defun xfloat-zerop (x)
  (if (floatp x)
      (or (eql (flonum-%bits x) 0) (eq (flonum-%value x) :minus-zero))
      (error "non-number?"))) ; or complex (not handled)

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
            (if (not prototypep) 'single-float (flonum-format prototype)))
           (host-format
            (ecase format
              (single-float 'cl:single-float)
              (double-float 'cl:double-float))))
      (make-flonum (cl:coerce (realnumify number) host-format) format))))

;;; Produce a float with the format and magnitude of FLOAT2 and sign of FLOAT1.
(defun float-sign (float1 &optional (float2 (float 1 float1) float2p))
  (validate-args float1 float2)
  (with-memoized-math-op (float-sign (cons float1 (if float2p (list float2))))
    (let ((res (if (= (float-sign-bit float2) (float-sign-bit float1))
                   float2
                   (sb-xc:- float2))))
      (if (eq (flonum-format float1) 'double-float)
          (coerce res 'double-float)
          res))))

(macrolet ((define (name float-fun)
             `(progn
                (declaim (inline ,name))
                (defun ,name (number &optional (divisor 1))
                  (if (and (rationalp number) (rationalp divisor))
                      (,(intern (string name) "CL") number divisor)
                      (,float-fun number divisor)))
                (defun ,float-fun (number divisor)
                  (with-memoized-math-op (,name (list number divisor))
                    (multiple-value-bind (q r)
                        (,(intern (string name) "CL")
                         (realnumify number)
                         (realnumify divisor))
                      (values q
                              (make-flonum r (if (or (eq (flonum-format number) 'double-float)
                                                     (and (floatp divisor)
                                                          (eq (flonum-format divisor) 'double-float)))
                                                 'double-float
                                                 'single-float)))))))))
  (define floor xfloat-floor)
  (define ceiling xfloat-ceiling)
  (define truncate xfloat-truncate)
  (define round xfloat-round))

(defun exp (x)
  (validate-args x)
  (with-memoized-math-op (exp x)
    (cond ((eql x $1.0f0) $2.7182817)
          ((eql x $1.0d0) $2.718281828459045d0))))

(defun expt (base power)
  (cond ((and (rationalp base) (integerp power))
         (cl:expt base power))
        (t
         (if (zerop power)
             (coerce 1 (flonum-format base))
             (with-memoized-math-op (expt (list base power))
               (cond ((and (eql base $2f0) (eql power 63))
                      #.(make-flonum #x5F000000 'single-float))
                     ((and (eql base $2d0) (eql power 63))
                      #.(make-flonum #x43E0000000000000 'double-float))
                     ((and (eql base $10d0) (>= power 322))
                      #.(make-flonum :+infinity 'double-float))
                     ((and (eql base $10d0) (eql power -309))
                      #.(make-flonum #xB8157268FDAF 'double-float))
                     (t
                      (make-flonum (cl:expt (realnumify base) power)
                                   (pick-result-format base power)))))))))

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

(defun cis (number)
  (declare (ignore number))
  (error "Unimplemented."))

(defun scale-float (f ex)
  (validate-args f)
  (with-memoized-math-op (scale-float (list f ex))
    (make-flonum (cl:scale-float (let ((val (realnumify f)))
                                   (assert (cl:floatp val))
                                   val)
                                 ex)
                 (flonum-format f))))

(defun scale-single-float (f ex)
  (validate-args f)
  (scale-float f ex))

(defun scale-double-float (f ex)
  (validate-args f)
  (scale-float f ex))

(defun log (number &optional (base nil base-p))
  (validate-args number base)
  (with-memoized-math-op (log (cons number (if base-p (list base))))
    ;; Now this is intriguing:
    ;; * (log 2d0 2s0) => 0.9999999972521647d0
    ;; * (log 2s0 2d0) => 1.0000000027478353d0
    ;; but
    ;; * (log 2d0 2d0) => 1.0d0
    ;; * (log 2s0 2s0) => 1.0
    (let ((format (pick-result-format number (if base-p base 0))))
      (if (zerop number)
          (make-flonum :-infinity (if (eq format 'rational)
                                      'single-float
                                      format))
          (make-flonum (if base-p
                           (cl:log (realnumify number) (realnumify base))
                           (cl:log (realnumify number)))
                       (if (eq format 'rational) ; neither arg was floating-point
                           'single-float
                           format))))))

;;;;

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

(defun make-single-float (bits)
  (declare (type (signed-byte 32) bits))
  (make-flonum bits 'single-float))

(defun make-double-float (hi lo)
  (declare (type (signed-byte 32) hi)
           (type (unsigned-byte 32) lo))
  (make-flonum (logior (ash hi 32) lo) 'double-float))

;;; This is the preferred constructor for 64-bit machines
(defun %make-double-float (bits)
  (declare (type (signed-byte 64) bits))
  (make-flonum bits 'double-float))

(defun float-infinity-p (x)
  (member (flonum-%value x) '(:-infinity :+infinity)))

(defun float-nan-p (x)
  (and (null (flonum-%value x)) (eq (calculate-flonum-value x nil) :nan)))

;;; Infinity and NaN have the same exponent value, but this definition avoids
;;; making use of that fact, unlike the real definition which does.
(defun float-infinity-or-nan-p (x)
  (or (float-infinity-p x) (float-nan-p x)))

(eval-when (:compile-toplevel :execute) (setq sb-cold::*choke-on-host-irrationals* nil))

;;; These use "#." so that they are dumped as literals rather than having to
;;; call read-from-string at load-time (and failing) due to the reader intercept.
;;      #define __FLT_MAX__ 3.40282347e+38F
(defconstant most-positive-single-float
  #.(make-flonum (read-from-string "+3.40282347F38") 'single-float))
(defconstant most-negative-single-float
  #.(make-flonum (read-from-string "-3.40282347F38") 'single-float))

;;      #define __DBL_MAX__ 1.7976931348623157e+308
(defconstant most-positive-double-float
  #.(make-flonum (read-from-string "+1.7976931348623157D308") 'double-float))
(defconstant most-negative-double-float
  #.(make-flonum (read-from-string "-1.7976931348623157D308") 'double-float))

;;; PI is needed in order to build the cross-compiler mainly so that vm-fndb
;;; can define bounds on irrational functions.
(defconstant pi
  #.(make-flonum (read-from-string "3.14159265358979323846264338327950288419716939937511L0")
                 'double-float))

(eval-when (:compile-toplevel :execute) (setq sb-cold::*choke-on-host-irrationals* t))

;;; These two constants are used in 'type'
(defconstant most-positive-long-float most-positive-double-float)
(defconstant most-negative-long-float most-negative-double-float)

(defun substitute-minus-zero (list)
  (substitute $0.0d0
              (make-flonum :minus-zero 'double-float)
              (substitute $0.0f0
                          (make-flonum :minus-zero 'single-float)
                          list)))

(macrolet ((intercept (symbols lambda-list body-form)
             `(progn ,@(mapcar (lambda (symbol)
                                 `(defun ,(intern (string symbol) "SB-XC") ,lambda-list
                                    (declare (dynamic-extent args))
                                    ,(subst (intern (string symbol) "CL") :me body-form)))
                               symbols)))
           (dispatch (f irrational)
             `(if (every #'rationalp args)
                  (apply #',f args)
                  (with-memoized-math-op (,f args) ,irrational)))
           (flonums-eql-p ()
             `(let ((x (car args)) (y (cadr args)))
                (and (floatp x) (floatp y) (eql (flonum-%bits x) (flonum-%bits y)))))
           (two-zeros-p ()
             `(and (eql nargs 2) (zerop (car args)) (zerop (cadr args))))
           (same-sign-infinities-p ()
             `(and (eql nargs 2)
                   (floatp (car args))
                   (floatp (cadr args))
                   (member (flonum-%value (car args)) '(:-infinity :+infinity))
                   (eq (flonum-%value (cadr args)) (flonum-%value (car args))))))

  ;; Simple case of using the interceptor to return a boolean.  If
  ;; infinity leaks in, the host will choke since those are
  ;; represented as symbols.
  (intercept (= /=) (&rest args)
    (dispatch :me (apply #':me (realnumify* (substitute-minus-zero args)))))

  ;; Simple case of using the interceptor to return a float.
  ;; As above, no funky values allowed.
  (intercept (max min + acos acosh asin asinh atan atanh conjugate cos cosh fceiling ffloor fround ftruncate phase sin sinh tan tanh) (&rest args)
    (dispatch :me
     (make-flonum (apply #':me (realnumify* args))
                  (apply #'pick-result-format args))))

  (intercept (sqrt) (&rest args)
    (destructuring-bind (x) args
      (if (zerop x)
          x
          (dispatch :me
                    (make-flonum (funcall #':me (realnumify x))
                                 (pick-result-format x))))))

  (intercept (*) (&rest args)
    (dispatch :me
              (let ((res (make-flonum (apply #':me (realnumify* (substitute-minus-zero args)))
                                      (apply #'pick-result-format args))))
                (if (evenp (+ (count $-0.0d0 args)
                              (count $-0.0 args)))
                    res
                    (sb-xc:- res)))))

  (intercept (/) (&rest args)
    (if (and (= (length args) 2)
             ;; ugly hack to avoid tripping up overflow trap
             (eql (first args) $1.3407807929942596d154)
             (eql (second args) $8.90029543402881d-308))
        (make-flonum :+infinity (apply #'pick-result-format args))
        (dispatch :me
                  (let ((res (make-flonum (apply #':me (realnumify* (substitute-minus-zero args)))
                                          (apply #'pick-result-format args))))
                    (if (evenp (+ (count $-0.0d0 args)
                                  (count $-0.0 args)))
                        res
                        (sb-xc:- res))))))

  (intercept (-) (&rest args)
    (dispatch :me
              (if (cdr args)
                  (make-flonum (apply #':me (realnumify* args))
                               (apply #'pick-result-format args))
                  (let* ((x (car args)) (format (flonum-format x)))
                    (make-flonum (logxor (ash -1 (1- (float-format-bits format)))
                                         (flonum-%bits x))
                                 format)))))

  (intercept (<) (&rest args  &aux (nargs (length args)))
    (dispatch :me
      (if (and (eql nargs 2) (or (flonums-eql-p) (two-zeros-p)))
          nil ; if eql, or both = 0, then not '<'
          (apply #':me (realnumify* (substitute-minus-zero args))))))

  ;; '>' needs to permit some of the "inoperable" floats because making any constant
  ;; (as in 'early-float') calls CTYPE-OF -> CTYPE-OF-NUMBER -> MAKE-NUMERIC-TYPE
  ;; which calls '>' to check for properly ordered low and high bounds.
  ;; (The resulting object is equivalent to *EMPTY-TYPE* if bounds are reversed)
  (intercept (>) (&rest args &aux (nargs (length args)))
    (dispatch :me
      (if (and (eql nargs 2) (or (flonums-eql-p) (two-zeros-p)))
          nil ; if eql, or both = 0, then not '>'
          (apply #':me (realnumify* (substitute-minus-zero args))))))

  (intercept (>=) (&rest args  &aux (nargs (length args)))
    (dispatch :me
      (cond ((and (eql nargs 2)
                  (destructuring-bind (a b) args
                    ;; from src/code/irrat
                    ;; This is, of course, completely heinous, but so is the practice
                    ;; of creating the number from bits in the first place.
                    (and (eql a #.(make-flonum #x3FE62E42FEFA39EF 'double-float))
                         (eql b $2.0d0))))
             nil)
            ((two-zeros-p) t) ; signed zeros are equal
            ((same-sign-infinities-p) t) ; infinities are =
            ((and (eql nargs 2) (zerop (cadr args)))
             ;; Need this case if the first arg is represented as bits
             (if (rationalp (car args))
                 (plusp (car args))
                 (= (float-sign-bit (car args)) 0))) ; anything positive is >= 0
            ((eql nargs 2)
             (multiple-value-bind (a b) (collapse-zeros (car args) (cadr args))
               (:me a b)))
            (t
             (apply #':me (realnumify* args))))))

  (intercept (<=) (&rest args  &aux (nargs (length args)))
    (dispatch :me
      (cond ((and (eql nargs 3) (every #'floatp args))
             (destructuring-bind (a b c) args
               (if (and (eq (flonum-format a) (flonum-format c))
                        (or (eq (flonum-format b) (flonum-format a))
                            (eq (flonum-format b) 'single-float)))
                   (cond ((and (eql a most-negative-single-float)
                               (eql c most-positive-single-float))
                          (not (float-infinity-p b)))
                         ((and (eql a most-negative-double-float)
                               (eql c most-positive-double-float))
                          (not (float-infinity-p b)))
                         (t
                          (error "Unhandled")))
                   (error "Unhandled"))))
            ((two-zeros-p) t) ; signed zeros are equal
            ((same-sign-infinities-p) t) ; infinities are =
            ((and (eql nargs 2) (zerop (cadr args)))
             ;; Need this case if the first arg is represented as bits
             (if (floatp (car args))
                 (= (float-sign-bit (car args)) 1) ; anything negative is <= 0
                 (minusp (car args))))
            ((eql nargs 2)
             (multiple-value-bind (a b) (collapse-zeros (car args) (cadr args))
               (:me a b)))
            (t
             (apply #':me (realnumify* args))))))

) ; end MACROLET

;;; The full logic of MAYBE-EXACT-RECIPROCAL is defined in 'float-tran'
;;; but we don't want to use that in the cross-compiler (yet, if ever)
;;; because it needs INTEGER-DECODE-FLOAT which we haven't emulated
;;; using our flonum abstraction. So just mock out the answers.
(defun sb-c::maybe-exact-reciprocal (x)
  (cond ((eql x $2.0d0) $.5d0)
        ((eql x $10.0d0) nil)
        ((eql x #.(make-flonum #x3FE62E42FEFA39EF 'double-float))
         nil)
        ((eql x #.(make-flonum #x49742400 'single-float))
         nil)
        (t (error "MAYBE-EXACT-RECIPROCAL: didn't expect ~S" x))))

;;; This is a convenient utility to have - we use it within this file and
;;; genesis quite a bit. The target definition resides in target-hash-table.
(defun %hash-table-alist (hash-table &aux result)
  (maphash (lambda (key value) (push (cons key value) result))
           hash-table)
  result)

;;; Canonicalize and write out the memoization table. Order it by function,
;;; then number of arguments, then each argument's value. Rational precedes
;;; single-float which precedes double-float, then order by bits.
(defun dump-math-memoization-table (table stream)
  (format stream ";;; This file is machine-generated. DO NOT EDIT~2%")
  (format stream ":DEFAULT~%(~%")
  (labels ((classify (x)
             (cond ((symbolp x) 0)
                   ((rationalp x) 1)
                   ((single-float-p x) 2)
                   ((double-float-p x) 3)
                   (t (error "Unclassifiable arg ~S" x))))
           (lessp (expr-a expr-b)
             (let ((f (string (car expr-a)))
                   (g (string (car expr-b))))
               (when (string< f g) (return-from lessp t))
               (when (string> f g) (return-from lessp nil)))
             (let ((args-a (ensure-list (cdr expr-a)))
                   (args-b (ensure-list (cdr expr-b))))
               (let ((m (length args-a))
                     (n (length args-b)))
                 (when (< m n) (return-from lessp t))
                 (when (> m n) (return-from lessp nil)))
               (loop for a in args-a
                     for b in args-b
                     do (let ((a-class (classify a))
                              (b-class (classify b)))
                          (when (< a-class b-class) (return-from lessp t))
                          (when (> a-class b-class) (return-from lessp nil)))
                        (when (symbolp a) ; this case is for the 2nd arg of COERCE
                          (return-from lessp (string< a b)))
                        ;; This might have to be enhanced to compare complex
                        ;; numbers by their realpart and imagpart eventually.
                        (let ((a-val (if (floatp a) (flonum-%bits a) a))
                              (b-val (if (floatp b) (flonum-%bits b) b)))
                          (when (< a-val b-val) (return-from lessp t))
                          (when (> a-val b-val) (return-from lessp nil))))
               (bug "Unreachable"))))
    ;; Record each <fun,args> combination to STREAM
    ;; Though all symbols we print, such as SINGLE-FLOAT, are accessible
    ;; in any of the SB- packages, ABCL would buggily output package prefixes
    ;; if ~S is used here. The intent is to use only SBCL as host to compute
    ;; the table, since we assume that everybody's math routines suck.
    ;; But anyway, this does seem to work in most other lisps.
    (let ((*print-pretty* nil) (*print-base* 16) (*print-radix* t))
      (dolist (pair (sort (%hash-table-alist table) #'lessp :key #'car))
        (destructuring-bind ((fun . args) . result) pair
          (format stream "(~A ~A~{ ~A~})~%"
                  fun args
                  ;; Can't use ENSURE-LIST. We need NIL -> (NIL)
                  (if (consp result) result (list result)))))))
  (format stream ")~%"))

(defun show-interned-numbers (stream)
  (flet ((to-native (x)
            (declare (ignorable x))
            #+host-quirks-sbcl
            (flet ((realize (r)
                     (if (rationalp r)
                         r
                         (case (flonum-format r)
                          (single-float
                           (host-sb-kernel:make-single-float (flonum-%bits r)))
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
      (maphash (lambda (k v)
                 (let ((actual (to-native v)))
                   (format stream "; ~S -> ~S~@[ = ~D~]~%" k v actual)
                   (when actual
                     (when (member actual values)
                       ;; Duplicates means that the host's EQL
                       ;; would not answer correctly for certain inputs.
                       (error "Duplicate float in interned flonum table"))
                     (push actual values))))
               *interned-numbers*))))

;;; Perform some simple checks
(assert (not (eq (make-flonum :minus-zero 'single-float)
                 (make-flonum :minus-zero 'double-float))))
(assert (not (eq (make-flonum :+infinity 'single-float) $0s0)))
(dolist (format '(single-float double-float))
  (assert (zerop (make-flonum :minus-zero format)))
  (assert (float-infinity-p (make-flonum :+infinity format)))
  (assert (float-infinity-p (make-flonum :-infinity format)))
  (assert (eq (make-flonum :minus-zero format) (make-flonum :minus-zero format)))
  (assert (eq (make-flonum :+infinity format) (make-flonum :+infinity format)))
  (assert (eq (make-flonum :-infinity format) (make-flonum :-infinity format)))
  (assert (eq (make-flonum :+infinity format) (sb-xc:- (make-flonum :-infinity format))))
  (assert (eq (make-flonum :-infinity format) (sb-xc:- (make-flonum :+infinity format))))
  (assert (eq (sb-xc:- (coerce 0 format)) (make-flonum :minus-zero format)))
  (let ((*break-on-signals* nil))
  (flet ((assert-not-number (x)
           (handler-case (realnumify x)
             (:no-error (x) (error "Expected an error, got ~S" x))
             (simple-error (x) (declare (ignore x))))))
    (let ((nan (make-single-float #b01111111101000000000000000000000)))
      ;;                             [ exp  ]
      (assert-not-number nan)
      (assert (float-nan-p nan)))
    (dolist (symbol '(:+infinity :-infinity :minus-zero))
      (assert-not-number (make-flonum symbol format))))))

#+host-quirks-sbcl ; Cross-check some more things if we can
(loop for (our-symbol host-single host-double)
      in '((:+infinity host-sb-ext:single-float-positive-infinity
                       host-sb-ext:double-float-positive-infinity)
           (:-infinity host-sb-ext:single-float-negative-infinity
                       host-sb-ext:double-float-negative-infinity))
      do (assert (= (flonum-%bits (make-flonum our-symbol 'single-float))
                    (host-sb-kernel:single-float-bits (symbol-value host-single))))
         (assert (= (double-float-high-bits (make-flonum our-symbol 'double-float))
                    (host-sb-kernel:double-float-high-bits (symbol-value host-double))))
         (assert (= (double-float-low-bits (make-flonum our-symbol 'double-float))
                    (host-sb-kernel:double-float-low-bits (symbol-value host-double)))))

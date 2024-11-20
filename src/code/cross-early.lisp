;;;; cross-compile-time-only stuff that is needed before anything else

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

;;; MAYBE-INLINE, FREEZE-TYPE, and block compilation declarations can be safely ignored
;;; (possibly at some cost in efficiency).
(declaim (declaration freeze-type maybe-inline start-block end-block))

;;; SB-C::LAMBDA-LIST declarations can be ignored.
;;; Cross-compilation does not rely on introspection for anything.
(declaim (declaration sb-c::lambda-list))

(declaim (declaration explicit-check always-bound))

;;; CMUCL warns about NIL not being an integer
#+host-quirks-cmu
(declaim (notinline position position-if position-if-not))

(defvar *default-source-external-format* :default)

(defmacro sb-xc:defconstant (&rest args)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (cl:defconstant ,@args)))

(defgeneric sb-xc:make-load-form (obj &optional env))

;;; Restore normalcy of MOD and RATIONAL as type specifiers.
(deftype mod (n) `(integer 0 ,(1- n)))
(deftype rational (&optional low high) `(cl:rational ,low ,high))

;;; To ensure that target numbers compare by EQL correctly under the
;;; host's definition of EQL (so that we don't have to intercept it
;;; and all else that uses EQL such as EQUAL, EQUALP and sequence
;;; traversers etc), we enforce that EQL numbers are in fact EQ by
;;; hash consing either on the bits for floats or on the real and
;;; imaginary parts for complex numbers.
(defstruct (target-num (:constructor nil)))
(defstruct (float (:include target-num)
                  (:conc-name "FLONUM-")
                  (:constructor nil)
                  (:predicate floatp))
  ;; the bits are canonical as regards hash-consing
  (bits (error "unspecified BITS") :type integer :read-only t))
(defstruct (single-float (:include float
                          (bits (error "unspecified %BITS") :type (signed-byte 32)))
                         (:constructor %%make-single-float (bits))))

(defmethod print-object ((obj single-float) stream)
  (format stream "#.(MAKE-SINGLE-FLOAT #x~x)" (single-float-bits obj)))

(defmethod cl:make-load-form ((obj single-float) &optional env)
  (declare (ignore env))
  `(make-single-float ,(single-float-bits obj)))

(defvar *interned-single-floats* (make-hash-table))

(defun make-single-float (bits)
  (declare (type (signed-byte 32) bits))
  (let ((table *interned-single-floats*))
    (or (gethash bits table)
        (setf (gethash bits table) (%%make-single-float bits)))))

(defstruct (double-float (:include float
                          (bits (error "unspecifier %BITS") :type (signed-byte 64)))
                         (:constructor %%make-double-float (bits))))

(defmethod print-object ((obj double-float) stream)
  (format stream "#.(MAKE-DOUBLE-FLOAT #x~x #x~x)"
          (double-float-high-bits obj)
          (double-float-low-bits obj)))

(defmethod cl:make-load-form ((obj double-float) &optional env)
  (declare (ignore env))
  `(%make-double-float ,(double-float-bits obj)))

(defvar *interned-double-floats* (make-hash-table))

;;; This is the preferred constructor for 64-bit machines
(defun %make-double-float (bits)
  (declare (type (signed-byte 64) bits))
  (let ((table *interned-double-floats*))
    (or (gethash bits table)
        (setf (gethash bits table) (%%make-double-float bits)))))

(defun make-double-float (hi lo)
  (declare (type (signed-byte 32) hi)
           (type (unsigned-byte 32) lo))
  (%make-double-float (logior (ash hi 32) lo)))

(defun double-float-low-bits (x)
  (logand (double-float-bits x) #xffffffff))
(defun double-float-high-bits (x)
  (ash (double-float-bits x) -32))

(deftype real () '(or cl:rational float))
(declaim (inline realp))
(defun realp (x) (cl:typep x 'real))

(declaim (inline long-float-p))
(defun long-float-p   (x) (double-float-p x))
(deftype long-float   () '(satisfies long-float-p))

;;; This is used not only for (COMPLEX FLOAT) but also (COMPLEX RATIONAL)
;;; to eliminate reliance on host complex number type reflection.
(defstruct (complexnum (:include target-num)
                       (:predicate complexp)
                       (:constructor %make-complexnum (real imag)))
  (real nil :type real :read-only t)
  (imag nil :type real :read-only t))

(defmethod print-object ((obj complexnum) stream)
  (write-string "#C( " stream)
  (prin1 (complexnum-real obj) stream)
  (write-char #\Space stream)
  (prin1 (complexnum-imag obj) stream)
  (write-char #\) stream))

(defmethod cl:make-load-form ((obj complexnum) &optional env)
  (declare (ignore env))
  `(complex ,(complexnum-real obj) ,(complexnum-imag obj)))

;;; KLUDGE: this is more-or-less copied from sharpm,
(defun sb-cold::read-target-complex (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (let ((cnum (read stream t nil t)))
    (when *read-suppress* (return-from sb-cold::read-target-complex nil))
    (if (and (listp cnum) (= (length cnum) 2))
        (complex (car cnum) (cadr cnum))
        (error "illegal complex number format: #C~S" cnum))))

(defvar *interned-complex-numbers* (make-hash-table :test #'equal))

;;; REAL and IMAG are either host integers (therefore EQL-comparable)
;;; or if not, have already been made EQ-comparable by hashing.
(defun complex (re im)
  (if (or (and (floatp re) (eq (type-of re) (type-of im)))
          (and (rationalp re) (rationalp im)))
      (if (eql im 0)
          re
          (let ((table *interned-complex-numbers*)
                (key (cons re im)))
            (or (gethash key table)
                (setf (gethash key table)
                      (%make-complexnum re im)))))
      (error "Won't make complex number from ~s ~s" re im)))

(defun complex-single-float-p (x)
  (and (complexp x) (single-float-p (complexnum-real x))))
(defun complex-double-float-p (x)
  (and (complexp x) (double-float-p (complexnum-real x))))
(defun complex-float-p (x)
  (and (complexp x) (floatp (complexnum-real x))))

;;; Unlike for type FLOAT, where we don't in practice need lists as specifiers,
;;; we do use (COMPLEX foo) specifiers all over the place. But this deftype
;;; need not be as complete as (def-type-translator complex).
;;; It's just enough to handle all cases parsed by the host.
(deftype complex (&optional spec)
  (cond ((member spec '(* rational real)) 'complexnum)
        ((eq spec 'single-float) '(satisfies complex-single-float-p))
        ((eq spec 'double-float) '(satisfies complex-double-float-p))
        #+long-float
        ((eq spec 'long-float) '(satisfies complex-long-float-p))
        ((eq spec 'float) '(satisfies complex-float-p))
        (t (error "complex type specifier too complicated: ~s" spec))))

(deftype number () '(or real complex))
(declaim (inline numberp))
(defun numberp (x) (cl:typep x 'number))

(declaim (inline ratiop))
(defun ratiop (x) (cl:typep x 'ratio))

(declaim (inline sequencep))
(defun sequencep (x) (cl:typep x 'sequence))

(declaim (inline zerop))
(defun zerop (x) (if (rationalp x) (= x 0) (sb-xc:= x 0)))

(declaim (inline minusp))
(defun minusp (x) (if (rationalp x) (< x 0) (sb-xc:< x 0)))

(declaim (inline expt))
(defun expt (base power)
  (if (and (rationalp base) (integerp power))
      (cl:expt base power)
      (xfloat-expt base power)))

(macrolet ((define (name)
             `(defun ,name (numerator denominator)
                (declare (integer numerator denominator))
                (cl:/ numerator denominator))))
  (define %make-ratio))

(declaim (inline bignum-ashift-left-fixnum))
(defun bignum-ashift-left-fixnum (fixnum count)
  (ash fixnum count))

(macrolet ((define (name type)
             `(progn
                (declaim (inline ,name))
                (defun ,name (bignum)
                  (coerce bignum ',type)))))
  (define bignum-to-single-float single-float)
  (define bignum-to-double-float double-float))

(macrolet ((define (name)
             `(defun ,name (x exp)
                (declare (ignore x exp))
                (error "Unimplemented."))))
  (define sb-kernel::scale-single-float-maybe-underflow)
  (define sb-kernel::scale-single-float-maybe-overflow)
  (define sb-kernel::scale-double-float-maybe-underflow)
  (define sb-kernel::scale-double-float-maybe-overflow))

(macrolet ((define (name float-fun)
             `(progn
                (declaim (inline ,name))
                (defun ,name (number &optional (divisor 1))
                  (if (and (rationalp number) (rationalp divisor))
                      (,(intern (string name) "CL") number divisor)
                      (,float-fun number divisor))))))
  (define floor xfloat-floor)
  (define ceiling xfloat-ceiling)
  (define truncate xfloat-truncate)
  (define round xfloat-round))

(macrolet ((define (name float-fun)
             `(progn
                (declaim (inline ,name))
                (defun ,name (number divisor)
                  (if (and (rationalp number) (rationalp divisor))
                      (,(intern (string name) "CL") number divisor)
                      (,float-fun number divisor))))))
  (define mod xfloat-mod)
  (define rem xfloat-rem))

(macrolet ((define (name float-fun)
             `(progn
                (declaim (inline ,name))
                (defun ,name (x)
                  (if (rationalp x)
                      (,(intern (string name) "CL") x)
                      (,float-fun x))))))
  (define abs xfloat-abs)
  (define signum xfloat-signum))

(defmacro validate-args (&rest args)
  `(when (or ,@(mapcar (lambda (arg) `(typep ,arg '(or cl:float cl:complex))) args))
     (error "Unexpectedly got host float/complex args")))

(defun coerce (object type)
  ;; OBJECT is validated prior to testing the quick bail out case,
  ;; because supposing that we accidentally got a host complex number
  ;; or float, and we accidentally got CL:FLOAT or something else in
  ;; CL as the type, NUMBER would return NIL because host floats do
  ;; NOT satisfy "our" NUMBERP. But we want this to fail, not succeed.
  (validate-args object)
  (cond ((numberp object)
         (xfloat-coerce object type))
        ((and (or (arrayp object) (listp object))
              (not (or (member type '(list vector simple-vector
                                      simple-string simple-base-string))
                       (equal type '(simple-array character (*))))))
         ;; specializable array
         (sb-xc:make-array (length object) :initial-contents object
                           :element-type
                           (ecase (car type)
                             (simple-array
                              (destructuring-bind (et &optional dims)
                                  (cdr type)
                                (assert (or (eql dims 1)
                                            (equal dims '(*))))
                                et))
                             (vector
                              (destructuring-bind (et)
                                  (cdr type)
                                et)))))
        (t
         (cl:coerce object type))))

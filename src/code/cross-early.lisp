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

(defgeneric sb-xc:make-load-form (obj &optional env))

;;; Restore normalcy of MOD and RATIONAL as type specifiers.
(deftype mod (n) `(integer 0 ,(1- n)))
(deftype rational (&optional low high) `(cl:rational ,low ,high))

;;; Target floating-point and COMPLEX number representation
;;; is defined sufficiently early to avoid "undefined type" warnings.
(defstruct (target-num (:constructor nil)))
(defstruct (float (:include target-num)
                  (:conc-name "FLONUM-")
                  (:constructor nil)
                  (:predicate floatp))
  ;; the bits are canonical as regards hash-consing
  (bits (error "unspecified BITS") :type integer :read-only t))
(defstruct (single-float (:include float
                          (bits (error "unspecified %BITS") :type (signed-byte 32)))
                         (:constructor %make-single-flonum (bits))))

(defmethod print-object ((obj single-float) stream)
  (format stream "#.(MAKE-SINGLE-FLOAT #x~x)" (single-float-bits obj)))

(defun %single-bits-from (sign exponent mantissa)
  (declare (type bit sign)
           (type (unsigned-byte 8) exponent)
           (type (unsigned-byte 23) mantissa))
  (logior (ash (cl:- sign) 31) (ash exponent 23) mantissa))
(defun %single-sign-bit (single)
  (cl:ldb (cl:byte 1 31) (single-float-bits single)))
(defun %single-exponent-bits (single)
  (cl:ldb (cl:byte 8 23) (single-float-bits single)))
(defun %single-mantissa-bits (single)
  (cl:ldb (cl:byte 23 0) (single-float-bits single)))

(defstruct (double-float (:include float
                          (bits (error "unspecifier %BITS") :type (signed-byte 64)))
                         (:constructor %make-double-flonum (bits))))

(defmethod print-object ((obj double-float) stream)
  (format stream "#.(MAKE-DOUBLE-FLOAT #x~x #x~x)"
          (double-float-high-bits obj)
          (double-float-low-bits obj)))

(defun %double-bits-from (sign exponent mantissa)
  (declare (type bit sign)
           (type (unsigned-byte 11) exponent)
           (type (unsigned-byte 52) mantissa))
  (logior (ash (cl:- sign) 63) (ash exponent 52) mantissa))
(defun %double-sign-bit (double)
  (cl:ldb (cl:byte 1 63) (double-float-bits double)))
(defun %double-exponent-bits (double)
  (cl:ldb (cl:byte 11 52) (double-float-bits double)))
(defun %double-mantissa-bits (double)
  (cl:ldb (cl:byte 52 0) (double-float-bits double)))
(defun double-float-low-bits (x)
  (logand (double-float-bits x) #xffffffff))
(defun double-float-high-bits (x)
  (ash (double-float-bits x) -32))

(macrolet ((def (name bits-fun n-exponent-bits n-mantissa-bits)
             (let ((initial-exponent (1- (ash 1 (1- n-exponent-bits))))
                   (max-exponent (1- (ash 1 n-exponent-bits))))
             `(defun ,name (rational)
                (declare (type cl:rational rational))
                (let ((sign (if (cl:= (cl:signum rational) -1) 1 0))
                      (magnitude (cl:abs rational)))
                  (when (cl:= magnitude 0)
                    (return-from ,name (,bits-fun 0 0 0)))
                  (loop with dir = (if (cl:> magnitude 1) -1 1)
                        for exponent = ,initial-exponent then (cl:- exponent dir)
                        for mantissa = magnitude then (cl:* mantissa (cl:expt 2 dir))
                        until (and (cl:<= 1 mantissa) (cl:< mantissa 2))
                        ;; the calls to CL:ROUND in this FINALLY
                        ;; clause are the representation of the FPU
                        ;; rounding mode.
                        finally (let ((%mantissa (cl:round (cl:* (cl:1- mantissa) (cl:expt 2 ,n-mantissa-bits)))))
                                  (when (cl:= %mantissa (cl:expt 2 ,n-mantissa-bits))
                                    (incf exponent)
                                    (setf %mantissa 0))
                                  (when (cl:>= exponent ,max-exponent)
                                    (setf exponent ,max-exponent %mantissa 0))
                                  (when (cl:<= exponent 0)
                                    (setf %mantissa (cl:round (cl:* mantissa (cl:expt 2 (cl:+ -1 ,n-mantissa-bits exponent))))
                                          exponent 0))
                                  (return (,bits-fun sign exponent %mantissa)))))))))
  (def %single-bits-from-rational %single-bits-from 8 23)
  (def %double-bits-from-rational %double-bits-from 11 52))

(defun flonum-sign (flonum)
  (let ((bit (etypecase flonum
               (single-float (%single-sign-bit flonum))
               (double-float (%double-sign-bit flonum)))))
    (cl:- 1 (cl:* 2 bit))))
(defun flonum-exponent (flonum)
  (etypecase flonum
    (single-float
     (let ((exponent-bits (%single-exponent-bits flonum)))
       (when (cl:= exponent-bits 255)
         (error "FLONUM-EXPONENT saturated: ~S" flonum))
       (if (cl:= exponent-bits 0) -126 (cl:- exponent-bits 127))))
    (double-float
     (let ((exponent-bits (%double-exponent-bits flonum)))
       (when (cl:= exponent-bits 2047)
         (error "FLONUM-EXPONENT saturated: ~S" flonum))
       (if (cl:= exponent-bits 0) -1022 (cl:- exponent-bits 1023))))))
(defun flonum-mantissa (flonum)
  (etypecase flonum
    (single-float
     (let ((scale (cl:expt 2 23))
           (exponent-bits (%single-exponent-bits flonum)))
       (if (cl:> exponent-bits 0)
           (cl:1+ (cl:/ (%single-mantissa-bits flonum) scale))
           (cl:/ (%single-mantissa-bits flonum) scale))))
    (double-float
     (let ((scale (cl:expt 2 52))
           (exponent-bits (%double-exponent-bits flonum)))
       (if (cl:> exponent-bits 0)
           (cl:1+ (cl:/ (%double-mantissa-bits flonum) scale))
           (cl:/ (%double-mantissa-bits flonum) scale))))))

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
  (write-string "#.(COMPLEX " stream)
  (prin1 (complexnum-real obj) stream)
  (write-char #\Space stream)
  (prin1 (complexnum-imag obj) stream)
  (write-char #\) stream))

(defun complex-single-float-p (x)
  (and (complexp x) (single-float-p (complexnum-real x))))
(defun complex-double-float-p (x)
  (and (complexp x) (double-float-p (complexnum-real x))))

;;; Unlike for type FLOAT, where we don't in practice need lists as specifiers,
;;; we do use (COMPLEX foo) specifiers all over the place. But this deftype
;;; need not be as complete as (def-type-translator complex).
;;; It's just enough to handle all cases parsed by the host.
(deftype complex (&optional spec)
  (cond ((member spec '(* real)) 'complexnum)
        ((eq spec 'single-float) '(satisfies complex-single-float-p))
        ((eq spec 'double-float) '(satisfies complex-double-float-p))
        #+long-float
        ((eq spec 'long-float) '(satisfies complex-long-float-p))
        (t (error "complex type specifier too complicated: ~s" spec))))

(deftype number () '(or real complex))
(declaim (inline numberp))
(defun numberp (x) (cl:typep x 'number))

(declaim (inline ratiop))
(defun ratiop (x) (cl:typep x 'ratio))

(declaim (inline sequencep))
(defun sequencep (x) (cl:typep x 'sequence))

;;; ZEROP is needed sooner than the rest of the cross-float. (Not sure why exactly)
(declaim (inline zerop))
(defun zerop (x) (if (rationalp x) (= x 0) (sb-xc:= x 0)))

(defmethod cl:make-load-form ((self target-num) &optional env)
  (declare (ignore env))
  (if (complexp self)
      `(complex ,(complexnum-real self) ,(complexnum-imag self))
      `(make-flonum ,(flonum-bits self) ',(type-of self))))

#+weak-vector-readbarrier
(progn (deftype weak-vector () nil) ; nothing is a weak-vector
       (defun sb-int:weak-vector-ref (v i)
         (error "Called WEAK-VECTOR-REF on ~S ~S" v i))
       (defun (setf sb-int:weak-vector-ref) (new v i)
         (error "Called (SETF WEAK-VECTOR-REF) on ~S ~S ~S" new v i))
       (defun sb-int:weak-vector-len (v)
         (error "Called WEAK-VECTOR-LEN on ~S" v)))

(defmacro sb-xc:defconstant (&rest args)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (cl:defconstant ,@args)))

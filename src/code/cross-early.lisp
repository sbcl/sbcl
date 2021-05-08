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

(defmacro sb-xc:declaim (&rest declaration-specifiers)
  #+(or sb-devel sb-fluid)
  (setq declaration-specifiers
        (remove-if (lambda (declaration-specifier)
                     (member (first declaration-specifier)
                             '(#+sb-fluid inline
                               #+sb-fluid maybe-inline
                               #+sb-fluid sb-ext:freeze-type
                               #+sb-devel start-block
                               #+sb-devel end-block)))
                   declaration-specifiers))
  `(cl:declaim ,@declaration-specifiers))

;;; The STRUCTURE!OBJECT abstract class is the base of the hierarchy
;;; of objects that need to be identifiable as SBCL system objects
;;; in the host Lisp. This type does not exist in the target.
(defstruct (structure!object (:constructor nil) (:copier nil) (:predicate nil)))

(declaim (declaration truly-dynamic-extent))

;;; MAYBE-INLINE, FREEZE-TYPE, and block compilation declarations can be safely ignored
;;; (possibly at some cost in efficiency).
(declaim (declaration freeze-type maybe-inline start-block end-block))

;;; SB-C::LAMBDA-LIST declarations can be ignored.
;;; Cross-compilation does not rely on introspection for anything.
(declaim (declaration sb-c::lambda-list))

(declaim (declaration explicit-check always-bound))

(defgeneric sb-xc:make-load-form (obj &optional env))

;;; There's no real reason that the cross-compiler shouldn't get the
;;; same macro as the target for this, except that the host doesn't
;;; compile 'cl-specials', and it's kind of unlikely that we'd have
;;; our own sources not fail in make-host-1 using illegal designators.
;;; As to make-host-2, well, it's not a user-facing problem.
(defmacro check-designator (&rest junk) (declare (ignore junk)))

;;; Restore normalcy of MOD and RATIONAL as type specifiers.
(deftype mod (n) `(integer 0 ,(1- n)))
(deftype rational (&optional low high) `(cl:rational ,low ,high))

;;; Target floating-point and COMPLEX number representation
;;; is defined sufficiently early to avoid "undefined type" warnings.
(defstruct (target-num (:constructor nil)))
(defstruct (float (:include target-num)
                  (:conc-name "FLONUM-")
                  (:constructor %make-flonum (%bits format))
                  (:predicate floatp))
  ;; the bits are canonical as regards hash-consing
  (%bits nil :type integer :read-only t)
  ;; the numerical value is used for computation.
  ;; values that might not be exposed in the host use a symbol instead.
  (%value nil :type (or null cl:float (member :+infinity :-infinity :minus-zero)))
  ;; SHORT-FLOAT and LONG-FLOAT are not choices here,
  ;; since we're trying to exactly model the supported target float formats.
  (format nil :type (member single-float double-float) :read-only t))

(deftype real () '(or cl:rational float))
(declaim (inline realp))
(defun realp (x) (cl:typep x 'real))

(declaim (inline short-float-p single-float-p double-float-p long-float-p))
(defun single-float-p (x) (and (floatp x) (eq (flonum-format x) 'single-float)))
(defun double-float-p (x) (and (floatp x) (eq (flonum-format x) 'double-float)))
(defun short-float-p  (x) (single-float-p x))
(defun long-float-p   (x) (double-float-p x))

(deftype short-float  () '(satisfies short-float-p))
(deftype single-float () '(satisfies single-float-p))
(deftype double-float () '(satisfies double-float-p))
(deftype long-float   () '(satisfies long-float-p))

;;; This is used not only for (COMPLEX FLOAT) but also (COMPLEX RATIONAL)
;;; to eliminate reliance on host complex number type reflection.
(defstruct (complexnum (:include target-num)
                       (:predicate complexp)
                       (:constructor %make-complexnum (real imag)))
  (real nil :type real :read-only t)
  (imag nil :type real :read-only t))

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
        (t (error "complex type specifier too complicated: ~s" spec))))

(deftype number () '(or real complex))
(declaim (inline numberp))
(defun numberp (x) (cl:typep x 'number))

;;; ZEROP is needer sooner than the rest of the cross-float. (Not sure why exactly)
(declaim (inline zerop))
(defun zerop (x) (if (rationalp x) (= x 0) (xfloat-zerop x)))

(defmethod cl:make-load-form ((self target-num) &optional env)
  (declare (ignore env))
  (if (complexp self)
      `(complex ,(complexnum-real self) ,(complexnum-imag self))
      `(make-flonum ,(flonum-%bits self) ',(flonum-format self))))

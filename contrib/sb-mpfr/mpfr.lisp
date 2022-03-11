(defpackage :sb-mpfr
  (:use "COMMON-LISP" "SB-ALIEN")
  (:import-from "SB-GMP"
                #:make-gmp-rstate
                #:make-gmp-rstate-lc
                #:rand-seed
                #:gmp-limb
                #:ui
                #:si)
  (:export
   ;; +mpfr-precision+ is a pseudo-constant
   ;; Do not set via LET but use the two methods below!
   #:+mpfr-precision+
   #:set-precision
   #:with-precision
   ;; parameters and types
   #:*mpfr-rnd*
   #:*mpfr-base*
   #:mpfr-float
   ;; arithmetic operations
   #:make-mpfr-float
   #:mpfr-float-to-string
   #:add
   #:sub
   #:mul
   #:square
   #:div
   #:sqrt
   #:reciprocal-sqrt
   #:cubic-root
   #:k-root
   #:power
   #:negate
   #:abs
   #:dim
   #:mul-2-raised
   #:div-2-raised
   ;; special functions
   #:log
   #:log2
   #:log10
   #:exp
   #:exp2
   #:exp10
   #:cos
   #:sin
   #:tan
   #:sin-cos
   #:sec
   #:csc
   #:cot
   #:acos
   #:asin
   #:atan
   #:cosh
   #:sinh
   #:tanh
   #:sinh-cosh
   #:sech
   #:csch
   #:coth
   #:acosh
   #:asinh
   #:atanh
   #:fac
   #:log1p
   #:expm1
   #:eint
   #:li2
   #:gamma
   #:lngamma
   #:digamma
   #:zeta
   #:erf
   #:erfc
   #:j0
   #:j1
   #:jn
   #:y0
   #:y1
   #:yn
   #:ai
   #:arithmetic-geometric-mean
   #:hypot
   #:fma
   #:fms
   #:sum
   ;; comparison functions and predicates
   #:nan-p
   #:infinityp
   #:integerp
   #:numberp
   #:zerop
   #:regularp
   #:compare
   #:compare-2exp
   #:compare-abs
   #:>
   #:>=
   #:<
   #:<=
   #:=
   #:/=
   #:unorderedp
   ;; constants
   #:const-log2
   #:const-pi
   #:const-euler
   #:const-catalan
   ;; miscellaneous functions
   #:clear-underflow
   #:clear-overflow
   #:clear-div-by-zero
   #:clear-nan-flag
   #:clear-inex-flag
   #:clear-erange-flag
   #:set-underflow-flag
   #:set-overflow-flag
   #:set-div-by-zero-flag
   #:set-nan-flag
   #:set-inex-flag
   #:set-erange-flag
   #:clear-flags
   #:underflowp
   #:overflowp
   #:div-by-zero-p
   #:nanflag-p
   #:inexflag-p
   #:erangeflag-p
   ;; random number generation
   #:urandomb
   #:urandom
   ;; rounding
   #:rounded-int
   #:rounded-int-ceiling
   #:rounded-int-floor
   #:rounded-int-round
   #:rounded-int-truncate
   #:fractional
   #:ceiling
   #:floor
   #:round
   #:truncate
   #:modf
   #:fmod
   #:remainder
   #:remainder-quot
   ;; conversion
   #:coerce
   ;; special constants
   #:*mpfr-version*
   #:*mpfr-features*)
  (:shadow
   #:sqrt
   #:abs
   #:log
   #:exp
   #:cos
   #:sin
   #:tan
   #:acos
   #:asin
   #:atan
   #:cosh
   #:sinh
   #:tanh
   #:acosh
   #:asinh
   #:atanh
   #:numberp
   #:integerp
   #:zerop
   #:>
   #:>=
   #:<
   #:<=
   #:=
   #:/=
   #:ceiling
   #:floor
   #:round
   #:truncate
   #:coerce))

(in-package :sb-mpfr)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (sb-int:system-package-p *package*) t))

(defvar +mpfr-precision+)
(defvar *mpfr-version* nil)
(defvar *mpfr-features* nil)

(defun try-load-shared-object (pathname)
  (handler-case
      (load-shared-object pathname :dont-save t)
    (error ()
      nil)))

(defun %load-mpfr ()
  (or (some #'try-load-shared-object
            #-(or win32 darwin) '("libmpfr.so" "libmpfr.so.4")
            #+darwin '("libmpfr.dylib" "libmpfr.4.dylib")
            #+win32 '("mpfr.dll"))
      (warn "MPFR not loaded.")))

(defun load-mpfr (&key (persistently t))
  (setf *mpfr-version* nil
        *mpfr-features* nil
        *features* (set-difference *features* '(:sb-mpfr)))
  (when persistently
    (pushnew 'load-mpfr sb-ext:*init-hooks*)
    ;(pushnew 'uninstall-mpfr-funs sb-ext:*save-hooks*)
    )
  (let ((success (%load-mpfr)))
    (when success
      (setf *mpfr-version*
            (alien-funcall (extern-alien "mpfr_get_version"
                                         (function c-string)))))
    (cond ((null *mpfr-version*))
          ((string<= *mpfr-version* "3.1")
           (warn "SB-MPFR requires at least MPFR version 3.1")
           (setf success nil))
          (t
           (setf +mpfr-precision+
                 (alien-funcall (extern-alien "mpfr_get_default_prec"
                                              (function long))))
           (pushnew :sb-mpfr *mpfr-features*)
           (setf *features* (union *features* *mpfr-features*))))
    success))

(load-mpfr)

;;; types and initialization

(define-alien-type nil
    (struct mpfrfloat
            (mpfr_prec long)
            (mpfr_sign int)
            (mpfr_exp long)
            (mpfr_d (* gmp-limb))))

(define-alien-type mpfr_rnd_enum
  (enum mpfr_rnd
        (:mpfr_rndna -1)
        (:mpfr_rndn 0)
        (:mpfr_rndz 1)
        (:mpfr_rndu 2)
        (:mpfr_rndd 3)
        (:mpfr_rnda 4)
        (:mpfr_rndf 5)))

(declaim (inline mpfr_init2
                 mpfr_clear
                 mpfr_set
                 mpfr_set_ui
                 mpfr_set_si
                 mpfr_set_flt
                 mpfr_set_d
                 mpfr_set_z
                 mpfr_set_q
                 mpfr_set_nan
                 mpfr_set_inf
                 mpfr_set_zero
                 mpfr_set_default_prec
                 mpfr_get_str))

(define-alien-routine mpfr_init2 void
  (x (* (struct mpfrfloat)))
  (precision long))

(define-alien-routine mpfr_clear void
  (x (* (struct mpfrfloat))))

;;; conversion functions

(define-alien-routine mpfr_set void
  (x (* (struct mpfrfloat)))
  (op (* (struct mpfrfloat)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_set_ui void
  (x (* (struct mpfrfloat)))
  (op unsigned-long)
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_set_si void
  (x (* (struct mpfrfloat)))
  (op long)
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_set_flt void
  (x (* (struct mpfrfloat)))
  (op float)
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_set_d void
  (x (* (struct mpfrfloat)))
  (op double-float)
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_set_z void
  (x (* (struct mpfrfloat)))
  (op (* (struct sb-gmp::gmpint)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_set_q void
  (x (* (struct mpfrfloat)))
  (op (* (struct sb-gmp::gmprat)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_set_nan void
  (x (* (struct mpfrfloat))))

(define-alien-routine mpfr_set_inf void
  (x (* (struct mpfrfloat)))
  (sign int))

(define-alien-routine mpfr_set_zero void
  (x (* (struct mpfrfloat)))
  (sign int))

(define-alien-routine mpfr_set_default_prec void
  (prec long))

(define-alien-routine mpfr_get_str (* char)
  (str (* char))
  (exp (* long))
  (base int)
  (n unsigned)
  (x (* (struct mpfrfloat)))
  (rnd mpfr_rnd_enum))

;;; conversion functions

(declaim (inline mpfr_get_flt
                 mpfr_get_d
                 mpfr_get_si
                 mpfr_get_ui
                 mpfr_get_z
                 mpfr_free_str
                 mpfr_set_str))

(define-alien-routine mpfr_get_flt float
  (op (* (struct mpfrfloat)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_get_d double
  (op (* (struct mpfrfloat)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_get_si long
  (op (* (struct mpfrfloat)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_get_ui unsigned-long
  (op (* (struct mpfrfloat)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_get_z int
  (res (* (struct sb-gmp::gmpint)))
  (op (* (struct mpfrfloat)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_free_str void
  (str (* char)))

(define-alien-routine mpfr_set_str int
  (x (* (struct mpfrfloat)))
  (str c-string)
  (base int)
  (rnd mpfr_rnd_enum))

;;; arithmetic functions

(declaim (inline mpfr_add
                 mpfr_add_ui
                 mpfr_add_si
                 mpfr_add_d
                 mpfr_add_z
                 mpfr_add_q
                 mpfr_sub
                 mpfr_ui_sub
                 mpfr_sub_ui
                 mpfr_si_sub
                 mpfr_sub_si
                 mpfr_d_sub
                 mpfr_sub_d
                 mpfr_z_sub
                 mpfr_sub_z
                 mpfr_sub_q
                 mpfr_mul
                 mpfr_mul_ui
                 mpfr_mul_si
                 mpfr_mul_d
                 mpfr_mul_z
                 mpfr_mul_q
                 mpfr_sqr
                 mpfr_div
                 mpfr_ui_div
                 mpfr_div_ui
                 mpfr_si_div
                 mpfr_div_si
                 mpfr_d_div
                 mpfr_div_d
                 mpfr_div_z
                 mpfr_div_q
                 mpfr_sqrt
                 mpfr_sqrt_ui
                 mpfr_rec_sqrt
                 mpfr_cbrt
                 mpfr_root
                 mpfr_pow
                 mpfr_pow_ui
                 mpfr_pow_si
                 mpfr_pow_z
                 mpfr_ui_pow
                 mpfr_neg
                 mpfr_abs
                 mpfr_dim
                 mpfr_mul_2ui
                 mpfr_mul_2si
                 mpfr_div_2ui
                 mpfr_div_2si))

(define-alien-routine mpfr_add int
  (r (* (struct mpfrfloat)))
  (op1 (* (struct mpfrfloat)))
  (op2 (* (struct mpfrfloat)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_add_ui int
  (r (* (struct mpfrfloat)))
  (op1 (* (struct mpfrfloat)))
  (op2 unsigned-long)
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_add_si int
  (r (* (struct mpfrfloat)))
  (op1 (* (struct mpfrfloat)))
  (op2 long)
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_add_d int
  (r (* (struct mpfrfloat)))
  (op1 (* (struct mpfrfloat)))
  (op2 double-float)
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_add_z int
  (r (* (struct mpfrfloat)))
  (op1 (* (struct mpfrfloat)))
  (op2 (* (struct sb-gmp::gmpint)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_add_q int
  (r (* (struct mpfrfloat)))
  (op1 (* (struct mpfrfloat)))
  (op2 (* (struct sb-gmp::gmprat)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_sub int
  (r (* (struct mpfrfloat)))
  (op1 (* (struct mpfrfloat)))
  (op2 (* (struct mpfrfloat)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_ui_sub int
  (r (* (struct mpfrfloat)))
  (op1 unsigned-long)
  (op2 (* (struct mpfrfloat)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_sub_ui int
  (r (* (struct mpfrfloat)))
  (op1 (* (struct mpfrfloat)))
  (op2 unsigned-long)
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_si_sub int
  (r (* (struct mpfrfloat)))
  (op1 long)
  (op2 (* (struct mpfrfloat)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_sub_si int
  (r (* (struct mpfrfloat)))
  (op1 (* (struct mpfrfloat)))
  (op2 long)
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_d_sub int
  (r (* (struct mpfrfloat)))
  (op1 double-float)
  (op2 (* (struct mpfrfloat)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_sub_d int
  (r (* (struct mpfrfloat)))
  (op1 (* (struct mpfrfloat)))
  (op2 double-float)
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_z_sub int
  (r (* (struct mpfrfloat)))
  (op1 (* (struct sb-gmp::gmpint)))
  (op2 (* (struct mpfrfloat)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_sub_z int
  (r (* (struct mpfrfloat)))
  (op1 (* (struct mpfrfloat)))
  (op2 (* (struct sb-gmp::gmpint)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_sub_q int
  (r (* (struct mpfrfloat)))
  (op1 (* (struct mpfrfloat)))
  (op2 (* (struct sb-gmp::gmprat)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_mul int
  (r (* (struct mpfrfloat)))
  (op1 (* (struct mpfrfloat)))
  (op2 (* (struct mpfrfloat)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_mul_ui int
  (r (* (struct mpfrfloat)))
  (op1 (* (struct mpfrfloat)))
  (op2 unsigned-long)
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_mul_si int
  (r (* (struct mpfrfloat)))
  (op1 (* (struct mpfrfloat)))
  (op2 long)
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_mul_d int
  (r (* (struct mpfrfloat)))
  (op1 (* (struct mpfrfloat)))
  (op2 double-float)
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_mul_z int
  (r (* (struct mpfrfloat)))
  (op1 (* (struct mpfrfloat)))
  (op2 (* (struct sb-gmp::gmpint)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_mul_q int
  (r (* (struct mpfrfloat)))
  (op1 (* (struct mpfrfloat)))
  (op2 (* (struct sb-gmp::gmprat)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_sqr int
  (r (* (struct mpfrfloat)))
  (op (* (struct mpfrfloat)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_div int
  (r (* (struct mpfrfloat)))
  (op1 (* (struct mpfrfloat)))
  (op2 (* (struct mpfrfloat)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_ui_div int
  (r (* (struct mpfrfloat)))
  (op1 unsigned-long)
  (op2 (* (struct mpfrfloat)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_div_ui int
  (r (* (struct mpfrfloat)))
  (op1 (* (struct mpfrfloat)))
  (op2 unsigned-long)
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_si_div int
  (r (* (struct mpfrfloat)))
  (op1 long)
  (op2 (* (struct mpfrfloat)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_div_si int
  (r (* (struct mpfrfloat)))
  (op1 (* (struct mpfrfloat)))
  (op2 long)
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_d_div int
  (r (* (struct mpfrfloat)))
  (op1 double-float)
  (op2 (* (struct mpfrfloat)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_div_d int
  (r (* (struct mpfrfloat)))
  (op1 (* (struct mpfrfloat)))
  (op2 double-float)
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_div_z int
  (r (* (struct mpfrfloat)))
  (op1 (* (struct mpfrfloat)))
  (op2 (* (struct sb-gmp::gmpint)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_div_q int
  (r (* (struct mpfrfloat)))
  (op1 (* (struct mpfrfloat)))
  (op2 (* (struct sb-gmp::gmprat)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_sqrt int
  (r (* (struct mpfrfloat)))
  (op (* (struct mpfrfloat)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_sqrt_ui int
  (r (* (struct mpfrfloat)))
  (op unsigned-long)
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_rec_sqrt int
  (r (* (struct mpfrfloat)))
  (op (* (struct mpfrfloat)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_cbrt int
  (r (* (struct mpfrfloat)))
  (op (* (struct mpfrfloat)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_root int
  (r (* (struct mpfrfloat)))
  (op (* (struct mpfrfloat)))
  (k unsigned-long)
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_pow int
  (r (* (struct mpfrfloat)))
  (op1 (* (struct mpfrfloat)))
  (op2 (* (struct mpfrfloat)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_pow_ui int
  (r (* (struct mpfrfloat)))
  (op1 (* (struct mpfrfloat)))
  (op2 unsigned-long)
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_pow_si int
  (r (* (struct mpfrfloat)))
  (op1 (* (struct mpfrfloat)))
  (op2 long)
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_pow_z int
  (r (* (struct mpfrfloat)))
  (op1 (* (struct mpfrfloat)))
  (op2 (* (struct sb-gmp::gmpint)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_ui_pow int
  (r (* (struct mpfrfloat)))
  (op1 unsigned-long)
  (op2 (* (struct mpfrfloat)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_neg int
  (r (* (struct mpfrfloat)))
  (op (* (struct mpfrfloat)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_abs int
  (r (* (struct mpfrfloat)))
  (op (* (struct mpfrfloat)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_dim int
  (r (* (struct mpfrfloat)))
  (op1 (* (struct mpfrfloat)))
  (op2 (* (struct mpfrfloat)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_mul_2ui int
  (r (* (struct mpfrfloat)))
  (op1 (* (struct mpfrfloat)))
  (op2 unsigned-long)
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_mul_2si int
  (r (* (struct mpfrfloat)))
  (op1 (* (struct mpfrfloat)))
  (op2 long)
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_div_2ui int
  (r (* (struct mpfrfloat)))
  (op1 (* (struct mpfrfloat)))
  (op2 unsigned-long)
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_div_2si int
  (r (* (struct mpfrfloat)))
  (op1 (* (struct mpfrfloat)))
  (op2 long)
  (rnd mpfr_rnd_enum))

;;; special functions

(defmacro define-onearg-mpfr-int (funs)
  (loop for i in funs collect `(define-alien-routine ,i int
                                 (r (* (struct mpfrfloat)))
                                 (op (* (struct mpfrfloat)))
                                 (rnd mpfr_rnd_enum))
          into defines
        finally (return `(progn
                           (declaim (inline ,@funs))
                           ,@defines))))

(define-onearg-mpfr-int
    (mpfr_log
     mpfr_log2
     mpfr_log10
     mpfr_exp
     mpfr_exp2
     mpfr_exp10
     mpfr_cos
     mpfr_sin
     mpfr_tan
     mpfr_sec
     mpfr_csc
     mpfr_cot
     mpfr_acos
     mpfr_asin
     mpfr_atan
     mpfr_cosh
     mpfr_sinh
     mpfr_tanh
     mpfr_sech
     mpfr_csch
     mpfr_coth
     mpfr_acosh
     mpfr_asinh
     mpfr_atanh
     mpfr_log1p
     mpfr_expm1
     mpfr_eint
     mpfr_li2
     mpfr_gamma
     mpfr_lngamma
     mpfr_digamma
     mpfr_zeta
     mpfr_erf
     mpfr_erfc
     mpfr_j0
     mpfr_j1
     mpfr_y0
     mpfr_y1
     mpfr_ai))

(defmacro define-twoarg-mpfr-int (funs)
  (loop for i in funs collect `(define-alien-routine ,i int
                                 (r (* (struct mpfrfloat)))
                                 (op1 (* (struct mpfrfloat)))
                                 (op2 (* (struct mpfrfloat)))
                                 (rnd mpfr_rnd_enum))
          into defines
        finally (return `(progn
                           (declaim (inline ,@funs))
                           ,@defines))))

(define-twoarg-mpfr-int
    (mpfr_sin_cos
     mpfr_atan2
     mpfr_sinh_cosh
     mpfr_agm
     mpfr_hypot))

(declaim (inline mpfr_fac_ui
                 mpfr_zeta_ui
                 mpfr_jn
                 mpfr_yn
                 mpfr_fma
                 mpfr_fms))

(define-alien-routine mpfr_fac_ui int
  (r (* (struct mpfrfloat)))
  (op unsigned-long)
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_zeta_ui int
  (r (* (struct mpfrfloat)))
  (op unsigned-long)
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_jn int
  (r (* (struct mpfrfloat)))
  (n long)
  (op (* (struct mpfrfloat)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_yn int
  (r (* (struct mpfrfloat)))
  (n long)
  (op (* (struct mpfrfloat)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_fma int
  (r (* (struct mpfrfloat)))
  (op1 (* (struct mpfrfloat)))
  (op2 (* (struct mpfrfloat)))
  (op3 (* (struct mpfrfloat)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_fms int
  (r (* (struct mpfrfloat)))
  (op1 (* (struct mpfrfloat)))
  (op2 (* (struct mpfrfloat)))
  (op3 (* (struct mpfrfloat)))
  (rnd mpfr_rnd_enum))


;;; constant initialization

(defmacro define-const-mpfr-int (funs)
  (loop for i in funs collect `(define-alien-routine ,i int
                                 (r (* (struct mpfrfloat)))
                                 (rnd mpfr_rnd_enum))
          into defines
        finally (return `(progn
                           (declaim (inline ,@funs))
                           ,@defines))))

(define-const-mpfr-int
    (mpfr_const_log2
     mpfr_const_pi
     mpfr_const_euler
     mpfr_const_catalan))

(declaim (inline mpfr_sum))
(define-alien-routine mpfr_sum int
  (r (* (struct mpfrfloat)))
  (tab (* (* (struct mpfrfloat))))
  (n unsigned-long)
  (rnd mpfr_rnd_enum))


;;; comparison functions

(declaim (inline mpfr_cmp
                 mpfr_cmp_ui
                 mpfr_cmp_si
                 mpfr_cmp_d
                 mpfr_cmp_z
                 mpfr_cmp_q
                 mpfr_cmp_ui_2exp
                 mpfr_cmp_si_2exp
                 mpfr_cmpabs))

(define-alien-routine mpfr_cmp int
  (op1 (* (struct mpfrfloat)))
  (op2 (* (struct mpfrfloat))))

(define-alien-routine mpfr_cmp_ui int
  (op1 (* (struct mpfrfloat)))
  (op2 unsigned-long))

(define-alien-routine mpfr_cmp_si int
  (op1 (* (struct mpfrfloat)))
  (op2 long))

(define-alien-routine mpfr_cmp_d int
  (op1 (* (struct mpfrfloat)))
  (op2 double))

(define-alien-routine mpfr_cmp_z int
  (op1 (* (struct mpfrfloat)))
  (op2 (* (struct sb-gmp::gmpint))))

(define-alien-routine mpfr_cmp_q int
  (op1 (* (struct mpfrfloat)))
  (op2 (* (struct sb-gmp::gmprat))))

(define-alien-routine mpfr_cmp_ui_2exp int
  (op1 (* (struct mpfrfloat)))
  (op2 unsigned-long)
  (exp long))

(define-alien-routine mpfr_cmp_si_2exp int
  (op1 (* (struct mpfrfloat)))
  (op2 long)
  (exp long))

(define-alien-routine mpfr_cmpabs int
  (op1 (* (struct mpfrfloat)))
  (op2 (* (struct mpfrfloat))))


(defmacro define-onearg-mpfr-bool (funs)
  (loop for i in funs collect `(define-alien-routine ,i boolean
                                 (op (* (struct mpfrfloat))))
          into defines
        finally (return `(progn
                           (declaim (inline ,@funs))
                           ,@defines))))

(define-onearg-mpfr-bool
    (mpfr_nan_p
     mpfr_inf_p
     mpfr_number_p
     mpfr_zero_p
     mpfr_integer_p
     mpfr_regular_p))

(declaim (inline mpfr_sgn))
(define-alien-routine mpfr_sgn int
  (op (* (struct mpfrfloat))))

(defmacro define-twoarg-mpfr-bool (funs)
  (loop for i in funs collect `(define-alien-routine ,i boolean
                                 (op1 (* (struct mpfrfloat)))
                                 (op2 (* (struct mpfrfloat))))
          into defines
        finally (return `(progn
                           (declaim (inline ,@funs))
                           ,@defines))))

(define-twoarg-mpfr-bool
    (mpfr_greater_p
     mpfr_greaterequal_p
     mpfr_less_p
     mpfr_lessequal_p
     mpfr_equal_p
     mpfr_lessgreater_p
     mpfr_unordered_p))


;;; miscellaneous functions

(defmacro define-mpfr-void (funs)
  (loop for i in funs collect `(define-alien-routine ,i void)
          into defines
        finally (return `(progn
                           (declaim (inline ,@funs))
                           ,@defines))))

(define-mpfr-void
    (mpfr_clear_underflow
     mpfr_clear_overflow
     mpfr_clear_divby0
     mpfr_clear_nanflag
     mpfr_clear_inexflag
     mpfr_clear_erangeflag
     mpfr_set_underflow
     mpfr_set_overflow
     mpfr_set_divby0
     mpfr_set_nanflag
     mpfr_set_inexflag
     mpfr_set_erangeflag
     mpfr_clear_flags))

(defmacro define-mpfr-bool (funs)
  (loop for i in funs collect `(define-alien-routine ,i boolean)
          into defines
        finally (return `(progn
                           (declaim (inline ,@funs))
                           ,@defines))))

(define-mpfr-bool
    (mpfr_underflow_p
     mpfr_overflow_p
     mpfr_divby0_p
     mpfr_nanflag_p
     mpfr_inexflag_p
     mpfr_erangeflag_p))


(declaim (inline mpfr_urandomb
                 mpfr_urandom))

(define-alien-routine mpfr_urandomb int
  (op (* (struct mpfrfloat)))
  (s (* (struct sb-gmp::gmprandstate))))

(define-alien-routine mpfr_urandom int
  (op (* (struct mpfrfloat)))
  (s (* (struct sb-gmp::gmprandstate)))
  (rnd mpfr_rnd_enum))


;;; integer and remainder related functions / rounding

(define-onearg-mpfr-int
    (mpfr_rint
     mpfr_rint_ceil
     mpfr_rint_floor
     mpfr_rint_round
     mpfr_rint_trunc
     mpfr_frac))

(declaim (inline mpfr_ceil
                 mpfr_floor
                 mpfr_round
                 mpfr_trunc
                 mpfr_modf
                 mpfr_fmod
                 mpfr_remainder
                 mpfr_remquo))

(define-alien-routine mpfr_ceil int
  (r (* (struct mpfrfloat)))
  (op (* (struct mpfrfloat))))

(define-alien-routine mpfr_floor int
  (r (* (struct mpfrfloat)))
  (op (* (struct mpfrfloat))))

(define-alien-routine mpfr_round int
  (r (* (struct mpfrfloat)))
  (op (* (struct mpfrfloat))))

(define-alien-routine mpfr_trunc int
  (r (* (struct mpfrfloat)))
  (op (* (struct mpfrfloat))))

(define-alien-routine mpfr_modf int
  (ir (* (struct mpfrfloat)))
  (fr (* (struct mpfrfloat)))
  (op (* (struct mpfrfloat)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_fmod int
  (r (* (struct mpfrfloat)))
  (x (* (struct mpfrfloat)))
  (y (* (struct mpfrfloat)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_remainder int
  (r (* (struct mpfrfloat)))
  (x (* (struct mpfrfloat)))
  (y (* (struct mpfrfloat)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_remquo int
  (r (* (struct mpfrfloat)))
  (q (* long))
  (x (* (struct mpfrfloat)))
  (y (* (struct mpfrfloat)))
  (rnd mpfr_rnd_enum))


;;;; lisp interface

(defparameter *mpfr-rnd* :mpfr_rndn)
(defparameter *mpfr-base* 10)

(declaim (inline mpfr-float-ref
                 make-mpfr-float))

(defstruct (mpfr-float (:constructor %make-mpfr-float))
  (ref (make-alien (struct mpfrfloat))
   :type (alien (* (struct mpfrfloat))) :read-only t))

(defun make-mpfr-float (&optional (precision +mpfr-precision+))
  (declare (optimize (speed 3) (space 3)))
  (let* ((float (%make-mpfr-float))
         (ref (mpfr-float-ref float)))
    (mpfr_init2 ref precision)
    (sb-ext:finalize float (lambda ()
                             (declare (optimize (speed 3) (space 3) (safety 0)))
                             (mpfr_clear ref)
                             (free-alien ref)))
    float))

(defmacro with-precision (value &body body)
  `(let* ((old +mpfr-precision+)
          (+mpfr-precision+ ,value))
     (unwind-protect
          (progn
            (mpfr_set_default_prec ,value)
            ,@body)
       (mpfr_set_default_prec old))))

(defun set-precision (value)
  (check-type value ui)
  (mpfr_set_default_prec value)
  (setf +mpfr-precision+ value))

;;; printing and reader syntax

(defmethod print-object ((obj mpfr-float) stream)
  (multiple-value-bind (str exp sign)
      (mpfr-float-to-string obj)
    (declare (type (integer -1 1) sign))
    (cond (*print-readably*
           (write-char #\# stream)
           (write-char #\M stream)
           (write-string str stream)
           (write-char #\@ stream)
           (case sign
             (1 (princ (- exp (length str)) stream))
             (-1 (princ (- exp (1- (length str))) stream))))
          (t
           (case sign
             (0
              (write-char #\0 stream))
             (1
              (write-char #\. stream)
              (write-string str stream))
             (-1
              (write-char #\- stream)
              (write-char #\. stream)
              (write-string str stream :start 1)))
           (write-char #\e stream)
           (princ exp stream)))))

(defun mpfr-float-to-string (x &optional (rnd *mpfr-rnd*))
  (let ((xr (mpfr-float-ref x)))
    (with-alien ((exp long)
                 (str (* char)))
      (setf exp -1)
      (setf str (mpfr_get_str NIL (addr exp) *print-base* 0 xr rnd))
      (multiple-value-prog1
          (values (cast str c-string) exp (mpfr_cmp_ui xr 0))
        (mpfr_free_str str)))))

(defun mpfr-reader (stream subchar arg)
  (declare (ignore subchar arg))
  (let ((mpfr (make-mpfr-float)))
    (mpfr_set_str (mpfr-float-ref mpfr)
                  (let ((peek (peek-char t stream nil nil t)))
                    (if (char= peek #\") ;; The old lazy way
                        (read stream nil nil t)
                        (with-output-to-string (str)
                          (loop for char = (peek-char nil stream nil nil t)
                                while (and char
                                           (not (sb-impl:token-delimiterp char)))
                                do (write-char (read-char stream nil nil t) str)))))
                  *mpfr-base* *mpfr-rnd*)
    mpfr))

(defun enable-mpfr-syntax (readtable)
  (set-dispatch-macro-character #\# #\M #'mpfr-reader readtable))
(enable-mpfr-syntax *readtable*)


;;; arithmetic functions

(defun add (x y &optional (round *mpfr-rnd*))
  (if (typep x 'mpfr-float)
      (let* ((res (make-mpfr-float))
             (r (mpfr-float-ref res))
             (xr (mpfr-float-ref x))
             (i (etypecase y
                  (mpfr-float
                   (mpfr_add r xr (mpfr-float-ref y) round))
                  (ui
                   (mpfr_add_ui r xr y round))
                  (si
                   (mpfr_add_si r xr y round))
                  (double-float
                   (mpfr_add_d r xr y round))
                  (integer
                   (sb-gmp::with-mpz-vars ((y gy))
                     (mpfr_add_z r xr (addr gy) round)))
                  (rational
                   (sb-gmp::with-mpq-var (y qy)
                     (mpfr_add_q r xr (addr qy) round))))))
        (values res i))
      (etypecase y
        (mpfr-float
         (add y x)))))

(defun sub (x y &optional (round *mpfr-rnd*))
  (let* ((res (make-mpfr-float))
         (r (mpfr-float-ref res))
         (i (etypecase x
              (mpfr-float
               (let ((xr (mpfr-float-ref x)))
                 (etypecase y
                   (mpfr-float
                    (mpfr_sub r xr (mpfr-float-ref y) round))
                   (ui
                    (mpfr_sub_ui r xr y round))
                   (si
                    (mpfr_sub_si r xr y round))
                   (double-float
                    (mpfr_sub_d r xr y round))
                   (integer
                    (sb-gmp::with-mpz-vars ((y gy))
                      (mpfr_sub_z r xr (addr gy) round)))
                   (rational
                    (sb-gmp::with-mpq-var (y qy)
                      (mpfr_sub_q r xr (addr qy) round))))))
              (ui
               (etypecase y
                 (mpfr-float
                  (mpfr_ui_sub r x (mpfr-float-ref y) round))))
              (si
               (etypecase y
                 (mpfr-float
                  (mpfr_si_sub r x (mpfr-float-ref y) round))))
              (double-float
               (etypecase y
                 (mpfr-float
                  (mpfr_d_sub r x (mpfr-float-ref y) round))))
              (integer
               (etypecase y
                 (mpfr-float
                  (sb-gmp::with-mpz-vars ((x gx))
                    (mpfr_z_sub r (addr gx) (mpfr-float-ref y) round))))))))
    (values res i)))

(defun mul (x y &optional (round *mpfr-rnd*))
  (if (typep x 'mpfr-float)
      (let* ((res (make-mpfr-float))
             (r (mpfr-float-ref res))
             (xr (mpfr-float-ref x))
             (i (etypecase y
                  (mpfr-float
                   (mpfr_mul r xr (mpfr-float-ref y) round))
                  (ui
                   (mpfr_mul_ui r xr y round))
                  (si
                   (mpfr_mul_si r xr y round))
                  (double-float
                   (mpfr_mul_d r xr y round))
                  (integer
                   (sb-gmp::with-mpz-vars ((y gy))
                     (mpfr_mul_z r xr (addr gy) round)))
                  (rational
                   (sb-gmp::with-mpq-var (y qy)
                     (mpfr_mul_q r xr (addr qy) round))))))
        (values res i))
      (etypecase y
        (mpfr-float
         (mul y x)))))

(defun square (x &optional (round *mpfr-rnd*))
  (let ((r (make-mpfr-float)))
    (values r (mpfr_sqr (mpfr-float-ref r) (mpfr-float-ref x) round))))

(defun div (x y &optional (round *mpfr-rnd*))
  (let* ((res (make-mpfr-float))
         (r (mpfr-float-ref res))
         (i (etypecase x
              (mpfr-float
               (let ((xr (mpfr-float-ref x)))
                 (etypecase y
                   (mpfr-float
                    (mpfr_div r xr (mpfr-float-ref y) round))
                   (ui
                    (mpfr_div_ui r xr y round))
                   (si
                    (mpfr_div_si r xr y round))
                   (double-float
                    (mpfr_div_d r xr y round))
                   (integer
                    (sb-gmp::with-mpz-vars ((y gy))
                      (mpfr_div_z r xr (addr gy) round)))
                   (rational
                    (sb-gmp::with-mpq-var (y qy)
                      (mpfr_div_q r xr (addr qy) round))))))
              (ui
               (etypecase y
                 (mpfr-float
                  (mpfr_ui_div r x (mpfr-float-ref y) round))))
              (si
               (etypecase y
                 (mpfr-float
                  (mpfr_si_div r x (mpfr-float-ref y) round))))
              (double-float
               (etypecase y
                 (mpfr-float
                  (mpfr_d_div r x (mpfr-float-ref y) round)))))))
    (values res i)))

(defun sqrt (x &optional (round *mpfr-rnd*))
  (let* ((res (make-mpfr-float))
         (r (mpfr-float-ref res))
         (i (etypecase x
              (ui
               (mpfr_sqrt_ui r x round))
              (mpfr-float
               (mpfr_sqrt r (mpfr-float-ref x) round)))))
    (values res i)))

(defun reciprocal-sqrt (x &optional (round *mpfr-rnd*))
  (let* ((res (make-mpfr-float))
         (r (mpfr-float-ref res))
         (i (mpfr_rec_sqrt r (mpfr-float-ref x) round)))
    (values res i)))

(defun cubic-root (x &optional (round *mpfr-rnd*))
  (let* ((res (make-mpfr-float))
         (r (mpfr-float-ref res))
         (i (mpfr_cbrt r (mpfr-float-ref x) round)))
    (values res i)))

(defun k-root (x k &optional (round *mpfr-rnd*))
  (check-type k ui)
  (let* ((res (make-mpfr-float))
         (r (mpfr-float-ref res))
         (i (mpfr_root r (mpfr-float-ref x) k round)))
    (values res i)))

(defun power (x y &optional (round *mpfr-rnd*))
  (let* ((res (make-mpfr-float))
         (r (mpfr-float-ref res))
         (i (etypecase x
              (mpfr-float
               (let ((xr (mpfr-float-ref x)))
                 (etypecase y
                   (mpfr-float
                    (mpfr_pow r xr (mpfr-float-ref y) round))
                   (ui
                    (mpfr_pow_ui r xr y round))
                   (si
                    (mpfr_pow_si r xr y round))
                   (integer
                    (sb-gmp::with-mpz-vars ((y gy))
                      (mpfr_pow_z r xr (addr gy) round))))))
              (ui
               (etypecase y
                 (mpfr-float
                  (mpfr_ui_pow r x (mpfr-float-ref y) round)))))))
    (values res i)))

(defun negate (x &optional (round *mpfr-rnd*))
  (let* ((res (make-mpfr-float))
         (r (mpfr-float-ref res))
         (i (mpfr_neg r (mpfr-float-ref x) round)))
    (values res i)))

(defun abs (x &optional (round *mpfr-rnd*))
  (let* ((res (make-mpfr-float))
         (r (mpfr-float-ref res))
         (i (mpfr_abs r (mpfr-float-ref x) round)))
    (values res i)))

(defun dim (x y &optional (round *mpfr-rnd*))
  (let* ((res (make-mpfr-float))
         (r (mpfr-float-ref res))
         (i (mpfr_dim r (mpfr-float-ref x) (mpfr-float-ref y) round)))
    (values res i)))

(defun mul-2-raised (x y &optional (round *mpfr-rnd*))
  "Compute X*(2^Y)."
  (let* ((res (make-mpfr-float))
         (r (mpfr-float-ref res))
         (i (etypecase y
              (ui
               (mpfr_mul_2ui r (mpfr-float-ref x) y round))
              (si
               (mpfr_mul_2si r (mpfr-float-ref x) y round)))))
    (values res i)))

(defun div-2-raised (x y &optional (round *mpfr-rnd*))
  "Compute X/(2^Y)."
  (let* ((res (make-mpfr-float))
         (r (mpfr-float-ref res))
         (i (etypecase y
              (ui
               (mpfr_div_2ui r (mpfr-float-ref x) y round))
              (si
               (mpfr_div_2si r (mpfr-float-ref x) y round)))))
    (values res i)))

;;; special functions

(defmacro define-onearg-mpfr-funs (funs)
  (loop for (clfun mfun) in funs
        collect `(defun ,clfun (x &optional (round *mpfr-rnd*))
                   (let* ((result (make-mpfr-float))
                          (i (,mfun (mpfr-float-ref result)
                                    (mpfr-float-ref x)
                                    round)))
                     (values result i)))
          into defines
        finally (return `(progn
                           ,@defines))))

(define-onearg-mpfr-funs
    ((log mpfr_log)
     (log2 mpfr_log2)
     (log10 mpfr_log10)
     (exp mpfr_exp)
     (exp2 mpfr_exp2)
     (exp10 mpfr_exp10)
     (cos mpfr_cos)
     (sin mpfr_sin)
     (tan mpfr_tan)
     (sec mpfr_sec)
     (csc mpfr_csc)
     (cot mpfr_cot)
     (acos mpfr_acos)
     (asin mpfr_asin)
     (cosh mpfr_cosh)
     (sinh mpfr_sinh)
     (tanh mpfr_tanh)
     (sech mpfr_sech)
     (csch mpfr_csch)
     (coth mpfr_coth)
     (acosh mpfr_acosh)
     (asinh mpfr_asinh)
     (atanh mpfr_atanh)
     (log1p mpfr_log1p)
     (expm1 mpfr_expm1)
     (eint mpfr_eint)
     (li2 mpfr_li2)
     (gamma mpfr_gamma)
     (lngamma mpfr_lngamma)
     (digamma mpfr_digamma)
     (erf mpfr_erf)
     (erfc mpfr_erfc)
     (j0 mpfr_j0)
     (j1 mpfr_j1)
     (y0 mpfr_y0)
     (y1 mpfr_y1)
     (ai mpfr_ai)))

(defun atan (y &optional x (round *mpfr-rnd*))
  (if x
      (let* ((result (make-mpfr-float))
             (i (mpfr_atan2 (mpfr-float-ref result)
                            (mpfr-float-ref y)
                            (mpfr-float-ref x)
                            round)))
        (values result i))
      (let* ((result (make-mpfr-float))
             (i (mpfr_atan (mpfr-float-ref result)
                           (mpfr-float-ref y)
                           round)))
        (values result i))))

(defun sin-cos (x &optional (round *mpfr-rnd*))
  (let* ((sin (make-mpfr-float))
         (cos (make-mpfr-float))
         (i (mpfr_sin_cos (mpfr-float-ref sin)
                          (mpfr-float-ref cos)
                          (mpfr-float-ref x)
                          round)))
    (values sin cos i)))

(defun sinh-cosh (x &optional (round *mpfr-rnd*))
  (let* ((sin (make-mpfr-float))
         (cos (make-mpfr-float))
         (i (mpfr_sinh_cosh (mpfr-float-ref sin)
                            (mpfr-float-ref cos)
                            (mpfr-float-ref x)
                            round)))
    (values sin cos i)))

(defun arithmetic-geometric-mean (u v &optional (round *mpfr-rnd*))
  (let* ((result (make-mpfr-float))
         (i (mpfr_agm (mpfr-float-ref result)
                      (mpfr-float-ref u)
                      (mpfr-float-ref v)
                      round)))
    (values result i)))

(defun hypot (x y &optional (round *mpfr-rnd*))
  (let* ((result (make-mpfr-float))
         (i (mpfr_hypot (mpfr-float-ref result)
                        (mpfr-float-ref x)
                        (mpfr-float-ref y)
                        round)))
    (values result i)))

(defun fac (x &optional (round *mpfr-rnd*))
  (let* ((result (make-mpfr-float))
         (i (mpfr_fac_ui (mpfr-float-ref result)
                         x
                         round)))
    (values result i)))

(defun zeta (x &optional (round *mpfr-rnd*))
  (let* ((result (make-mpfr-float))
         (i (etypecase x
              (mpfr-float
               (mpfr_zeta (mpfr-float-ref result)
                          (mpfr-float-ref x)
                          round))
              (ui
               (mpfr_zeta_ui (mpfr-float-ref result)
                             x
                             round)))))
    (values result i)))

(defun jn (n x &optional (round *mpfr-rnd*))
  (check-type n ui)
  (let* ((result (make-mpfr-float))
         (i (mpfr_jn (mpfr-float-ref result)
                     n
                     (mpfr-float-ref x)
                     round)))
    (values result i)))

(defun yn (n x &optional (round *mpfr-rnd*))
  (check-type n ui)
  (let* ((result (make-mpfr-float))
         (i (mpfr_yn (mpfr-float-ref result)
                     n
                     (mpfr-float-ref x)
                     round)))
    (values result i)))

(defun fma (x y z &optional (round *mpfr-rnd*))
  "fma X Y Z = (X * Y) + Z"
  (let* ((result (make-mpfr-float))
         (i (mpfr_fma (mpfr-float-ref result)
                      (mpfr-float-ref x)
                      (mpfr-float-ref y)
                      (mpfr-float-ref z)
                      round)))
    (values result i)))

(defun fms (x y z &optional (round *mpfr-rnd*))
  "fma X Y Z = (X * Y) - Z"
  (let* ((result (make-mpfr-float))
         (i (mpfr_fms (mpfr-float-ref result)
                      (mpfr-float-ref x)
                      (mpfr-float-ref y)
                      (mpfr-float-ref z)
                      round)))
    (values result i)))

(defun sum (seq &optional (round *mpfr-rnd*))
  (let ((length (length seq))
        (idx -1))
    (declare (type (integer -1 #.most-positive-fixnum) idx))
    (let ((result (make-mpfr-float))
          (ar (make-alien (* (struct mpfrfloat)) length)))
      (unwind-protect
           (progn
             (map nil (lambda (x)
                        (setf (deref ar (incf idx))
                              (mpfr-float-ref x)))
                  seq)
             (let ((i (mpfr_sum (mpfr-float-ref result)
                                ar
                                length
                                round)))
               (values result i)))
        (free-alien ar)))))


;;; constant values

(defmacro define-const-mpfr-funs (funs)
  (loop for (fname mname) in funs
        collect `(defun ,fname (&optional (round *mpfr-rnd*))
                   (let* ((result (make-mpfr-float))
                          (i (,mname (mpfr-float-ref result)
                                     round)))
                     (values result i)))
          into defines
        finally (return `(progn
                           ,@defines))))

(define-const-mpfr-funs
    ((const-log2 mpfr_const_log2)
     (const-pi mpfr_const_pi)
     (const-euler mpfr_const_euler)
     (const-catalan mpfr_const_catalan)))

;;; comparison functions and predicates

(defmacro define-onearg-mpfr-predicates (funs)
  (loop for (fname mname) in funs
        collect `(defun ,fname (x)
                   (,mname (mpfr-float-ref x)))
          into defines
        finally (return `(progn
                           ,@defines))))

(define-onearg-mpfr-predicates
    ((nan-p mpfr_nan_p)
     (infinityp mpfr_inf_p)
     (numberp mpfr_number_p)
     (zerop mpfr_zero_p)
     (integerp mpfr_integer_p)
     (regularp mpfr_regular_p)))

(defmacro define-twoarg-mpfr-predicates (funs)
  (loop for (fname mname) in funs
        collect `(defun ,fname (x y)
                   (,mname (mpfr-float-ref x)
                           (mpfr-float-ref y)))
          into defines
        finally (return `(progn
                           ,@defines))))

(define-twoarg-mpfr-predicates
    ((compare-abs mpfr_cmpabs)
     (> mpfr_greater_p)
     (>= mpfr_greaterequal_p)
     (< mpfr_less_p)
     (<= mpfr_lessequal_p)
     (= mpfr_equal_p)
     (/= mpfr_lessgreater_p)
     (unorderedp mpfr_unordered_p)))

(defun compare (x y)
  (if (typep x 'mpfr-float)
      (etypecase y
        (mpfr-float
         (mpfr_cmp (mpfr-float-ref x)
                   (mpfr-float-ref y)))
        (ui
         (mpfr_cmp_ui (mpfr-float-ref x) y))
        (si
         (mpfr_cmp_si (mpfr-float-ref x) y))
        (double-float
         (mpfr_cmp_d (mpfr-float-ref x) y))
        (integer
         (sb-gmp::with-mpz-vars ((y gy))
           (mpfr_cmp_z (mpfr-float-ref x) (addr gy))))
        (rational
         (sb-gmp::with-mpq-var (y qy)
           (mpfr_cmp_q (mpfr-float-ref x) (addr qy)))))
      (etypecase y
        (mpfr-float
         (compare y x)))))

(defun compare-2exp (x y exp)
  (if (typep x 'mpfr-float)
      (etypecase y
        (ui
         (mpfr_cmp_ui_2exp (mpfr-float-ref x) y exp))
        (si
         (mpfr_cmp_si_2exp (mpfr-float-ref x) y exp)))
      (etypecase y
        (mpfr-float
         (compare-2exp y x exp)))))


;;; miscellaneous functions

(defmacro define-mpfr-flag-funs (funs)
  (loop for (pname mname) in funs
        collect `(defun ,pname ()
                   (declare (optimize (speed 3) (space 3)))
                   (,mname) (values))
          into defines
        collect pname into names
        finally (return `(progn
                           (declaim (inline ,@names))
                           ,@defines))))

(define-mpfr-flag-funs
    ((clear-underflow mpfr_clear_underflow)
     (clear-overflow mpfr_clear_overflow)
     (clear-div-by-zero mpfr_clear_divby0)
     (clear-nan-flag mpfr_clear_nanflag)
     (clear-inex-flag mpfr_clear_inexflag)
     (clear-erange-flag mpfr_clear_erangeflag)
     (set-underflow-flag mpfr_set_underflow)
     (set-overflow-flag mpfr_set_overflow)
     (set-div-by-zero-flag mpfr_set_divby0)
     (set-nan-flag mpfr_set_nanflag)
     (set-inex-flag mpfr_set_inexflag)
     (set-erange-flag mpfr_set_erangeflag)
     (clear-flags mpfr_clear_flags)))


(defmacro define-mpfr-flag-predicates (funs)
  (loop for (pname mname) in funs
        collect `(defun ,pname ()
                   (declare (optimize (speed 3) (space 3)))
                   (,mname))
          into defines
        collect pname into names
        finally (return `(progn
                           (declaim (inline ,@names))
                           ,@defines))))

(define-mpfr-flag-predicates
    ((underflowp mpfr_underflow_p)
     (overflowp mpfr_overflow_p)
     (div-by-zero-p mpfr_divby0_p)
     (nanflag-p mpfr_nanflag_p)
     (inexflag-p mpfr_inexflag_p)
     (erangeflag-p mpfr_erangeflag_p)))


;;; random number generation

(defun urandomb (state)
  (check-type state sb-gmp::gmp-rstate)
  (let* ((ref (sb-gmp::gmp-rstate-ref state))
         (result (make-mpfr-float))
         (i (mpfr_urandomb (mpfr-float-ref result)
                           ref)))
    (values result i)))

(defun urandom (state &optional (round *mpfr-rnd*))
  (check-type state sb-gmp::gmp-rstate)
  (let* ((ref (sb-gmp::gmp-rstate-ref state))
         (result (make-mpfr-float))
         (i (mpfr_urandom (mpfr-float-ref result)
                          ref
                          round)))
    (values result i)))


;;; integer and remainder related functions / rounding

(define-onearg-mpfr-funs
    ((rounded-int mpfr_rint)
     (rounded-int-ceiling mpfr_rint_ceil)
     (rounded-int-floor mpfr_rint_floor)
     (rounded-int-round mpfr_rint_round)
     (rounded-int-truncate mpfr_rint_trunc)
     (fractional mpfr_frac)))

(defmacro define-onearg-no-rnd-mpfr-funs (funs)
  (loop for (clfun mfun) in funs
        collect `(defun ,clfun (x)
                   (let* ((result (make-mpfr-float))
                          (i (,mfun (mpfr-float-ref result)
                                    (mpfr-float-ref x))))
                     (values result i)))
          into defines
        finally (return `(progn
                           ,@defines))))

(define-onearg-no-rnd-mpfr-funs
    ((ceil mpfr_ceil)
     (floor mpfr_floor)
     (round mpfr_round)
     (truncate mpfr_trunc)))

(defun modf (x &optional (round *mpfr-rnd*))
  (let* ((integral (make-mpfr-float))
         (fractional (make-mpfr-float))
         (i (mpfr_modf (mpfr-float-ref integral)
                       (mpfr-float-ref fractional)
                       (mpfr-float-ref x)
                       round)))
    (values integral fractional i)))

(defun fmod (x y &optional (round *mpfr-rnd*))
  (let* ((result (make-mpfr-float))
         (i (mpfr_fmod (mpfr-float-ref result)
                       (mpfr-float-ref x)
                       (mpfr-float-ref y)
                       round)))
    (values result i)))

(defun remainder (x y &optional (round *mpfr-rnd*))
  (let* ((result (make-mpfr-float))
         (i (mpfr_remainder (mpfr-float-ref result)
                            (mpfr-float-ref x)
                            (mpfr-float-ref y)
                            round)))
    (values result i)))

(defun remainder-quot (x y &optional (round *mpfr-rnd*))
  (with-alien ((q long))
    (let* ((result (make-mpfr-float))
           (i (mpfr_remquo (mpfr-float-ref result)
                           (addr q)
                           (mpfr-float-ref x)
                           (mpfr-float-ref y)
                           round)))
      (values result q i))))


;;; conversion

(declaim (inline mpfr_fits_ulong_p mpfr_fits_slong_p))

(define-alien-routine mpfr_fits_ulong_p boolean
  (x (* (struct mpfrfloat)))
  (rnd mpfr_rnd_enum))

(define-alien-routine mpfr_fits_slong_p boolean
  (x (* (struct mpfrfloat)))
  (rnd mpfr_rnd_enum))

(defun coerce (x type &optional (round *mpfr-rnd*))
  (cond
    ((typep x 'mpfr-float)
     (let ((x-ref (mpfr-float-ref x)))
       (case type
         (single-float
          (mpfr_get_flt x-ref round))
         (double-float
          (mpfr_get_d x-ref round))
         (mpfr-float
          (let ((result (make-mpfr-float)))
            (mpfr_set (mpfr-float-ref result) x-ref round)
            result))
         (integer
          (unless (numberp x)
            (error "Cannot coerce ~s to ~s. Argument must be an actual number."
                   x type))
          (cond
            ((mpfr_fits_slong_p x-ref round)
             (mpfr_get_si x-ref round))
            ((mpfr_fits_ulong_p x-ref round)
             (mpfr_get_ui x-ref round))
            (t
             (sb-gmp::with-gmp-mpz-results (rop)
               (mpfr_get_z (addr rop) x-ref round)))))
         (t
          (error "TYPE must be one of SINGLE-FLOAT, DOUBLE-FLOAT or INTEGER.")))))
    ((eql type 'mpfr-float)
     (let ((result (make-mpfr-float)))
       (etypecase x
         (si
          (mpfr_set_si (mpfr-float-ref result) x round))
         (ui
          (mpfr_set_ui (mpfr-float-ref result) x round))
         (integer
          (sb-gmp::with-mpz-vars ((x gx))
            (mpfr_set_z (mpfr-float-ref result) (addr gx) round)))
         (single-float
          (mpfr_set_flt (mpfr-float-ref result) x round))
         (double-float
          (mpfr_set_d (mpfr-float-ref result) x round))
         (ratio
          (sb-gmp::with-mpq-var (x qx)
            (mpfr_set_q (mpfr-float-ref result) (addr qx) round))))
       result))
    (t
     (error "Unable to handle the combination of ~S and ~S." x type))))

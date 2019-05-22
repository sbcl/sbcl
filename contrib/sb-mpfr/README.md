SB-MPFR
=======

MPFR integration for SBCL

The MPFR library [1] provides arbitrary precision arithmetic on
floating-point numbers. It makes use of the GMP library and therefore
SB-MPFR requires SB-GMP [2]. Contrary to SB-GMP the SB-MPFR contrib does
not deeply integrate with any standard Common Lisp functions. However,
the SB-MPFR provides several function with equivalent names of
standard function and similar interface, e.g. SB-MPFR:COERCE,
SB-MPFR:SIN etc.

Rounding
--------

The rounding mode for SB-MPFR is defined by the value of
*MPFR-RND*. Possible values are :MPFR_RNDNA, :MPFR_RNDN, :MPFR_RNDZ,
:MPFR_RNDU, :MPFR_RNDD, :MPFR_RNDA, :MPFR_RNDF which implement the
corresponding rounding modes from MPFR and are members of the type
MPFR_RND_ENUM.

Precision
---------

MPFR allows to define the preicision of a computation or newly created
floating-points. This can either be set via the SB-MPFR:WITH-PRECISION
macro which takes the number of bits and an expression body as
arguments, or via SB-MPFR:SET-PRECISION which sets the precision until
further change.

Example, function to compute PI with 400 bits precision:

    (defun sample-pi ()
      (sb-mpfr:set-precision 400)
      (sb-mpfr:const-pi))

    CL-USER> (sample-pi)
    .31415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679821480865132823066470e+1
    -1

    (defun sample-pi-x (x)
      (sb-mpfr:with-precision x
        (sb-mpfr:const-pi)))

    CL-USER> (sample-pi-x 42)
    .31415926535901e+1
    1
    CL-USER> (sample-pi-x 142)
    .31415926535897932384626433832795028841971697e+1
    1

Second Return Value
-------------------

Almost every MPFR-related function returns a second value. This value
is of the ternary set {-1, 0, 1}. If the ternary value is zero, it
means that the value stored in the destination variable is the exact
result of the corresponding mathematical function. If the ternary
value is positive (resp. negative), it means the value stored in the
destination variable is greater (resp. lower) than the exact
result. The returned value depends on the current rounding mode. Refer
to [1] for further details.


Number Creation
---------------

Creating arbitrary precision floating points can be achieved in two
ways. First, by coercing a standard CL non-complex number with
SB-MPFR:COERCE.

    CL-USER> (sb-mpfr:coerce 1.0d0 'sb-mpfr:mpfr-float)
    .10000000000000000e+1
    CL-USER> (sb-mpfr:coerce 1/2 'sb-mpfr:mpfr-float)
    .50000000000000000e+0

The second method uses the reader macro #M:

    CL-USER> #M1.5
    .15000000000000000e+1
    CL-USER> (setf *print-readably* t)
    T
    CL-USER> #M1.5
    #M15000000000000000@-16

Exports and Functions
---------------------

The following functions, macros and symbols are exported from the SB-MPFR package:

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
    #:*mpfr-features*



[1] http://www.mpfr.org/mpfr-current/mpfr.html
[2] https://github.com/sfrank/sb-gmp

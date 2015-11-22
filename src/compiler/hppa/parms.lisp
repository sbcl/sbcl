(in-package "SB!VM")

;;;; Machine Architecture parameters:
(eval-when (:compile-toplevel :load-toplevel :execute)

;;; number of bits per word where a word holds one lisp descriptor
(def!constant n-word-bits 32)

;;; the natural width of a machine word (as seen in e.g. register width,
;;; address space)
(def!constant n-machine-word-bits 32)

;;; number of bits per byte where a byte is the smallest addressable
;;; object
(def!constant n-byte-bits 8)

(def!constant float-sign-shift 31)

(def!constant single-float-bias 126)
(defconstant-eqx single-float-exponent-byte (byte 8 23) #'equalp)
(defconstant-eqx single-float-significand-byte (byte 23 0) #'equalp)
(def!constant single-float-normal-exponent-min 1)
(def!constant single-float-normal-exponent-max 254)
(def!constant single-float-hidden-bit (ash 1 23))
(def!constant single-float-trapping-nan-bit (ash 1 22))

(def!constant double-float-bias 1022)
(defconstant-eqx double-float-exponent-byte (byte 11 20) #'equalp)
(defconstant-eqx double-float-significand-byte (byte 20 0) #'equalp)
(def!constant double-float-normal-exponent-min 1)
(def!constant double-float-normal-exponent-max #x7FE)
(def!constant double-float-hidden-bit (ash 1 20))
(def!constant double-float-trapping-nan-bit (ash 1 19))

(def!constant single-float-digits
  (+ (byte-size single-float-significand-byte) 1))

(def!constant double-float-digits
  (+ (byte-size double-float-significand-byte) n-word-bits 1))

(def!constant float-inexact-trap-bit (ash 1 0))
(def!constant float-underflow-trap-bit (ash 1 1))
(def!constant float-overflow-trap-bit (ash 1 2))
(def!constant float-divide-by-zero-trap-bit (ash 1 3))
(def!constant float-invalid-trap-bit (ash 1 4))

(def!constant float-round-to-nearest 0)
(def!constant float-round-to-zero 1)
(def!constant float-round-to-positive 2)
(def!constant float-round-to-negative 3)

(defconstant-eqx float-rounding-mode (byte 2 7) #'equalp)
(defconstant-eqx float-sticky-bits (byte 5 27) #'equalp)
(defconstant-eqx float-traps-byte (byte 5 0) #'equalp)
(defconstant-eqx float-exceptions-byte (byte 5 27) #'equalp)
(defconstant-eqx float-condition-bit (ash 1 26) #'equalp)
(def!constant float-fast-bit 0)                   ; No fast mode on HPPA.



;;;; Description of the target address space.

;;; Where to put the different spaces.
;;;
(def!constant read-only-space-start #x4b000000)
(def!constant read-only-space-end   #x4dff0000)

(def!constant static-space-start    #x4e000000)
(def!constant static-space-end      #x4fff0000)

(def!constant dynamic-0-space-start   #x50000000)
(def!constant dynamic-0-space-end     #x5fff0000)
(def!constant dynamic-1-space-start   #x60000000)
(def!constant dynamic-1-space-end     #x6fff0000)

); eval-when

;;; When doing external branching on hppa (e.g. inst ble)
;;; we must know which space we want to jump into (text, code)

;; The space-register holding the lisp heap.
(def!constant lisp-heap-space 5)

;; The space-register holding the C text heap.
(def!constant c-text-space 4)


;;;; Other random constants.

(defenum ()
  atomic-flag
  interrupted-flag)

(defenum (:start 8)
  halt-trap
  pending-interrupt-trap
  error-trap
  cerror-trap
  breakpoint-trap
  fun-end-breakpoint-trap
  single-step-breakpoint-trap
  single-step-around-trap
  single-step-before-trap
  single-step-after-trap)

;;;; Static symbols.

;;; These symbols are loaded into static space directly after NIL so
;;; that the system can compute their address by adding a constant
;;; amount to NIL.
;;;
;;; The fdefn objects for the static functions are loaded into static
;;; space directly after the static symbols.  That way, the raw-addr
;;; can be loaded directly out of them by indirecting relative to NIL.
(defparameter *static-symbols*
  (append
   *common-static-symbols*
   *c-callable-static-symbols*
   '()))

(defparameter *static-funs*
  '(length
    two-arg-+
    two-arg--
    two-arg-*
    two-arg-/
    two-arg-<
    two-arg->
    two-arg-=
    two-arg-<=
    two-arg->=
    two-arg-/=
    eql
    %negate
    two-arg-and
    two-arg-ior
    two-arg-xor
    two-arg-gcd
    two-arg-lcm))


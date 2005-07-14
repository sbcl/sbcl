(in-package "SB!VM")


;;;; Machine Architecture parameters:

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
(defconstant-eqx single-float-exponent-byte (byte 8 23) #'equal)
(defconstant-eqx single-float-significand-byte (byte 23 0) #'equal)
(def!constant single-float-normal-exponent-min 1)
(def!constant single-float-normal-exponent-max 254)
(def!constant single-float-hidden-bit (ash 1 23))
(def!constant single-float-trapping-nan-bit (ash 1 22))

(def!constant double-float-bias 1022)
(defconstant-eqx double-float-exponent-byte (byte 11 20) #'equal)
(defconstant-eqx double-float-significand-byte (byte 20 0) #'equal)
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

(defconstant-eqx float-rounding-mode (byte 2 7) #'equal)
(defconstant-eqx float-sticky-bits (byte 5 27) #'equal)
(defconstant-eqx float-traps-byte (byte 5 0) #'equal)
(defconstant-eqx float-exceptions-byte (byte 5 27) #'equal)
(def!constant float-condition-bit (ash 1 26))
(def!constant float-fast-bit 0)                   ; No fast mode on HPPA.



;;;; Description of the target address space.

;;; Where to put the different spaces.
;;;
(def!constant read-only-space-start #x20000000)
(def!constant read-only-space-end   #x24000000)

(def!constant static-space-start    #x28000000)
(def!constant static-space-end      #x2a000000)

(def!constant dynamic-0-space-start   #x30000000)
(def!constant dynamic-0-space-end     #x37fff000)
(def!constant dynamic-1-space-start   #x38000000)
(def!constant dynamic-1-space-end     #x3ffff000)

;;; FIXME: WTF are these for?

;; The space-register holding the lisp heap.
(def!constant lisp-heap-space 5)

;; The space-register holding the C text segment.
(def!constant c-text-space 4)


;;;; Other random constants.

(defenum (:suffix -trap :start 8)
  halt
  pending-interrupt
  error
  cerror
  breakpoint
  fun-end-breakpoint
  single-step-breakpoint)

(defenum (:prefix trace-table-)
  normal
  call-site
  fun-prologue
  fun-epilogue)



;;;; Static symbols.

;;; These symbols are loaded into static space directly after NIL so
;;; that the system can compute their address by adding a constant
;;; amount to NIL.
;;;
;;; The fdefn objects for the static functions are loaded into static
;;; space directly after the static symbols.  That way, the raw-addr
;;; can be loaded directly out of them by indirecting relative to NIL.
;;;
(defparameter *static-symbols*
  '(t

    ;; The C startup code must fill these in.
    *posix-argv*

    ;; Functions that the C code needs to call
    sb!impl::sub-gc
    sb!kernel::internal-error
    sb!kernel::control-stack-exhausted-error
    sb!kernel::undefined-alien-variable-error
    sb!kernel::undefined-alien-function-error
    sb!di::handle-breakpoint
    sb!impl::fdefinition-object

    ;; Free Pointers.
    *read-only-space-free-pointer*
    *static-space-free-pointer*
    *initial-dynamic-space-free-pointer*

    ;; Things needed for non-local-exit.
    *current-catch-block*
    *current-unwind-protect-block*

    *binding-stack-start*
    *control-stack-start*
    *control-stack-end*

    ;; Interrupt Handling
    *free-interrupt-context-index*
    sb!unix::*interrupts-enabled*
    sb!unix::*interrupt-pending*
    ))

(defparameter *static-funs*
  '(length
    sb!kernel:two-arg-+
    sb!kernel:two-arg--
    sb!kernel:two-arg-*
    sb!kernel:two-arg-/
    sb!kernel:two-arg-<
    sb!kernel:two-arg->
    sb!kernel:two-arg-=
    eql
    sb!kernel:%negate
    sb!kernel:two-arg-and
    sb!kernel:two-arg-ior
    sb!kernel:two-arg-xor
    sb!kernel:two-arg-gcd
    sb!kernel:two-arg-lcm
    ))

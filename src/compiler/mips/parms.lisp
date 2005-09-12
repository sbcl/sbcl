(in-package "SB!VM")

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

(defconstant-eqx float-rounding-mode (byte 2 0) #'equalp)
(defconstant-eqx float-sticky-bits (byte 5 2) #'equalp)
(defconstant-eqx float-traps-byte (byte 5 7) #'equalp)
(defconstant-eqx float-exceptions-byte (byte 5 12) #'equalp)
(defconstant-eqx float-condition-bit (ash 1 23) #'equalp)
(def!constant float-fast-bit 0)                   ; No fast mode on PMAX.


;;;; Description of the target address space.

#!+irix
(progn
  ;; Where to put the different spaces.
  ;; Old definitions, might be still relevant for an IRIX port.
  ;;
  (def!constant read-only-space-start #x01000000)
  (def!constant read-only-space-end   #x05000000)

  (def!constant static-space-start    #x06000000)
  (def!constant static-space-end      #x08000000)

  (def!constant dynamic-0-space-start #x08000000)
  (def!constant dynamic-0-space-end   #x0c000000)
  (def!constant dynamic-1-space-start #x0c000000)
  (def!constant dynamic-1-space-end   #x10000000))

#!+linux
(progn
  ;; Where to put the address spaces on Linux.
  ;;
  ;; C runtime executable segment starts at 0x00400000
  (def!constant read-only-space-start #x01000000)
  (def!constant read-only-space-end   #x08000000)

  (def!constant static-space-start    #x08000000)
  (def!constant static-space-end      #x10000000)
  ;; C runtime read/write segment starts at 0x10000000, heap and DSOs
  ;; start at 0x2a000000
  (def!constant dynamic-0-space-start #x30000000)
  (def!constant dynamic-0-space-end   #x50000000)
  (def!constant dynamic-1-space-start #x50000000)
  (def!constant dynamic-1-space-end   #x70000000)
  ;; C stack grows downward from 0x80000000
  )



;;;; Other non-type constants.

(defenum (:suffix -flag)
  atomic
  interrupted)

(defenum (:suffix -trap :start 8)
  halt
  pending-interrupt
  error
  cerror
  breakpoint
  fun-end-breakpoint
  after-breakpoint
  unused
  pseudo-atomic
  object-not-list
  object-not-instance)

(defenum (:prefix trace-table-)
  normal
  call-site
  fun-prologue
  fun-epilogue)

;;;; Static symbols.

;;; Static symbols are loaded into static space directly after NIL so
;;; that the system can compute their address by adding a constant
;;; amount to NIL.
;;;
;;; The fdefn objects for the static functions are loaded into static
;;; space directly after the static symbols.  That way, the raw-addr
;;; can be loaded directly out of them by indirecting relative to NIL.
;;;
(defparameter *static-symbols*
  '(t

    *posix-argv*

    sb!impl::sub-gc
    sb!kernel::internal-error
    sb!kernel::control-stack-exhausted-error
    sb!kernel::undefined-alien-variable-error
    sb!kernel::undefined-alien-function-error
    sb!di::handle-breakpoint
    sb!impl::fdefinition-object

    ;; Free Pointers
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
    *gc-inhibit*
    *gc-pending*))

(defparameter *static-funs*
  '(sb!kernel:two-arg-+
    sb!kernel:two-arg--
    sb!kernel:two-arg-*
    sb!kernel:two-arg-/
    sb!kernel:two-arg-<
    sb!kernel:two-arg->
    sb!kernel:two-arg-=
    sb!kernel:two-arg-<=
    sb!kernel:two-arg->=
    sb!kernel:two-arg-/=
    eql
    sb!kernel:%negate
    sb!kernel:two-arg-and
    sb!kernel:two-arg-ior
    sb!kernel:two-arg-xor
    length
    sb!kernel:two-arg-gcd
    sb!kernel:two-arg-lcm))

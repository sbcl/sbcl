;;;; This file contains some parameterizations of various VM
;;;; attributes for the PPC.  This file is separate from other stuff so 
;;;; that it can be compiled and loaded earlier. 


(in-package "SB!VM")

(def!constant n-word-bits 32
  "Number of bits per word where a word holds one lisp descriptor.")

(def!constant n-byte-bits 8
  "Number of bits per byte where a byte is the smallest addressable object.")

(def!constant word-shift (1- (integer-length (/ n-word-bits n-byte-bits)))
  "Number of bits to shift between word addresses and byte addresses.")

(def!constant n-word-bytes (/ n-word-bits n-byte-bits)
  "Number of bytes in a word.")


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
(def!constant float-divide-by-zero-trap-bit (ash 1 1))
(def!constant float-underflow-trap-bit (ash 1 2))
(def!constant float-overflow-trap-bit (ash 1 3))
(def!constant float-invalid-trap-bit (ash 1 4))

(def!constant float-round-to-nearest 0)
(def!constant float-round-to-zero 1)
(def!constant float-round-to-positive 2)
(def!constant float-round-to-negative 3)

(defconstant-eqx float-rounding-mode (byte 2 0) #'equalp)	  ; RD 
(defconstant-eqx float-sticky-bits (byte 10 19) #'equalp)
(defconstant-eqx float-traps-byte (byte 6 3) #'equalp)
(defconstant-eqx float-exceptions-byte (byte 5 0) #'equalp)	  ; cexc

(def!constant float-fast-bit 2)		; Non-IEEE mode


;;; NUMBER-STACK-DISPLACEMENT
;;;
;;; The number of bytes reserved above the number stack pointer.  These
;;; slots are required by architecture, mostly (?) to make C backtrace
;;; work.
;;; 
(def!constant number-stack-displacement
  (* 2 sb!vm:n-word-bytes))




;;; Where to put the different spaces.
;;; 

(def!constant read-only-space-start #x01000000)
(def!constant read-only-space-end   #x04ff8000)

(def!constant binding-stack-start   #x06000000)
(def!constant binding-stack-end     #x06ff0000)

(def!constant control-stack-start   #x07000000)
(def!constant control-stack-end     #x07ff0000)

(def!constant static-space-start    #x08000000)
(def!constant static-space-end      #x097fff00)

;;; FIXME: this is a gross violation of OAOO, done purely to support
;;; the #define of DYNAMIC_SPACE_SIZE in validate.c -- CSR, 2002-02-25
;;; (these numbers should match dynamic-0-*)
(def!constant dynamic-space-start   #x40000000)
(def!constant dynamic-space-end     #x47fff000)

;;; nothing _seems_ to be using these addresses 
(def!constant dynamic-0-space-start #x40000000)
(def!constant dynamic-0-space-end   #x47fff000)
(def!constant dynamic-1-space-start #x48000000)
(def!constant dynamic-1-space-end   #x4ffff000)




;;;; Other random constants.

(defenum (:suffix -trap :start 8)
  halt
  pending-interrupt
  error
  cerror
  breakpoint
  fun-end-breakpoint
  after-breakpoint
  fixnum-additive-overflow)

(defenum (:prefix object-not- :suffix -trap :start 16)
  list
  instance)

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
    sb!impl::*initial-fdefn-objects*

    ;; Functions that the C code needs to call
    ;; sb!impl::%initial-fun
    sb!impl::maybe-gc
    sb!kernel::internal-error
    sb!di::handle-breakpoint
    sb!impl::fdefinition-object

    ;; Free Pointers.
    *read-only-space-free-pointer*
    *static-space-free-pointer*
    *initial-dynamic-space-free-pointer*

    ;; Things needed for non-local-exit.
    *current-catch-block*
    *current-unwind-protect-block*

    ;; Interrupt Handling
    *free-interrupt-context-index*
    sb!unix::*interrupts-enabled*
    sb!unix::*interrupt-pending*

    #|sb!kernel::*current-thread*|#
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
    sb!kernel:two-arg-<=
    sb!kernel:two-arg->=   
    sb!kernel:two-arg-/=
    eql
    sb!kernel:%negate
    sb!kernel:two-arg-and
    sb!kernel:two-arg-ior
    sb!kernel:two-arg-xor
    sb!kernel:two-arg-gcd
    sb!kernel:two-arg-lcm))


;;;; Assembler parameters:

;;; The number of bits per element in the assemblers code vector.
;;;
(defparameter *assembly-unit-length* 8)

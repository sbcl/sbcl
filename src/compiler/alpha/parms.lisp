
(in-package "SB!VM")

(eval-when  (:compile-toplevel :load-toplevel :execute)

(defconstant word-bits 32
  #!+sb-doc
  "Number of bits per word where a word holds one lisp descriptor.")

(defconstant byte-bits 8
  #!+sb-doc
  "Number of bits per byte where a byte is the smallest addressable object.")

(defconstant word-shift (1- (integer-length (/ word-bits byte-bits)))
  #!+sb-doc
  "Number of bits to shift between word addresses and byte addresses.")

(defconstant word-bytes (/ word-bits byte-bits)
  #!+sb-doc
  "Number of bytes in a word.")

(defconstant float-sign-shift 31)

(defconstant single-float-bias 126)
(defconstant-eqx single-float-exponent-byte (byte 8 23) #'equalp)
(defconstant-eqx single-float-significand-byte (byte 23 0) #'equalp)
(defconstant single-float-normal-exponent-min 1)
(defconstant single-float-normal-exponent-max 254)
(defconstant single-float-hidden-bit (ash 1 23))
(defconstant single-float-trapping-nan-bit (ash 1 22))

(defconstant double-float-bias 1022)
(defconstant-eqx double-float-exponent-byte (byte 11 20)   #'equalp)
(defconstant-eqx double-float-significand-byte (byte 20 0) #'equalp)
(defconstant double-float-normal-exponent-min 1)
(defconstant double-float-normal-exponent-max #x7FE)
(defconstant double-float-hidden-bit (ash 1 20))
(defconstant double-float-trapping-nan-bit (ash 1 19))

(defconstant single-float-digits
  (+ (byte-size single-float-significand-byte) 1))

(defconstant double-float-digits
  (+ (byte-size double-float-significand-byte) word-bits 1))

;; Values in 17f code seem to be same as HPPA. These values are from
;; DEC Assembly Language Programmers guide. The active bits are
;; actually in (byte 12 52) of the fpcr. (byte 6 52) contain the
;; exception flags. Bit 63 is the bitwise logor of all exceptions.
;; The enable and exception bytes are in a software control word
;; manipulated via OS functions and the bits in the SCP match those
;; defs. This mapping follows <machine/fpu.h>
(defconstant float-inexact-trap-bit        (ash 1 4)) ; rw
(defconstant float-underflow-trap-bit      (ash 1 3)) ; rw
(defconstant float-overflow-trap-bit       (ash 1 2)) ; ro
(defconstant float-divide-by-zero-trap-bit (ash 1 1)) ; ro
(defconstant float-invalid-trap-bit        (ash 1 0)) ; ro

(defconstant float-round-to-zero     0)
(defconstant float-round-to-negative 1)
(defconstant float-round-to-nearest  2)
(defconstant float-round-to-positive 3)

;; These aren't quite correct yet. Work in progress.
(defconstant-eqx float-rounding-mode   (byte 2 58) #'equalp)	; hardware fpcr
(defconstant-eqx float-exceptions-byte (byte 6 52)  #'equalp)	; hardware fpcr
(defconstant-eqx float-sticky-bits     (byte 6 17) #'equalp)	; software (clear only)
(defconstant-eqx float-traps-byte      (byte 6  1) #'equalp)	; software fp control word
(defconstant float-condition-bit   (ash  1 63))	; summary - not used?? XXX
(defconstant float-fast-bit 0)

); eval-when



;;;; Description of the target address space.

;;; Where to put the different spaces.
;;;

#!+linux
(progn
  (defconstant read-only-space-start #x20000000)
  (defconstant read-only-space-end   #x24000000)

  (defconstant static-space-start    #x28000000)
  (defconstant static-space-end      #x2c000000)

  ;; this is used in purify as part of a sloppy check to see if a pointer
  ;; is in dynamic space.  Chocolate brownie for the first person to fix it
  ;; -dan 20010502
  (defconstant dynamic-space-start   #x30000000)
  (defconstant dynamic-space-end     #x38000000)

  (defconstant dynamic-0-space-start   #x30000000)
  (defconstant dynamic-0-space-end     #x38000000)
  
  (defconstant dynamic-1-space-start   #x40000000)
  (defconstant dynamic-1-space-end     #x48000000)

  (defconstant control-stack-start   #x50000000)
  (defconstant control-stack-end     #x51000000)

  (defconstant binding-stack-start    #x70000000)
  (defconstant binding-stack-end      #x71000000))

#!+osf1                                 ;as if
(progn
  (defparameter read-only-space-start #x10000000)
  (defparameter static-space-start    #x28000000)
  (defparameter dynamic-space-start   #x30000000))


;;; FIXME nothing refers to either of these in alpha or x86 cmucl
;;; backend, so they could probably be removed.

;; The space-register holding the lisp heap.
(defconstant lisp-heap-space 4)

;; The space-register holding the C text segment.
(defconstant c-text-space 4)

;;; the X86 port defines *nil-value* as (+ *target-static-space-start* #xB)
;;; here, but it seems to be the only port that needs to know the
;;; location of NIL from lisp.



;;;; Other random constants.

(defenum (:suffix -trap :start 8)
  halt
  pending-interrupt
  error
  cerror
  breakpoint
  function-end-breakpoint
  single-step-breakpoint)

(defenum (:prefix trace-table-)
  normal
  call-site
  function-prologue
  function-epilogue)



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
    ;;lisp::lisp-environment-list
    ;;lisp::lisp-command-line-list
    sb!impl::*!initial-fdefn-objects*

    ;; Functions that the C code needs to call
    sb!impl::%initial-function
    sb!impl::maybe-gc
    sb!kernel::internal-error
    sb!di::handle-breakpoint
    sb!di::handle-function-end-breakpoint
    sb!impl::fdefinition-object

    ;; Free Pointers.
    *read-only-space-free-pointer*
    *static-space-free-pointer*
    *initial-dynamic-space-free-pointer*

    ;; Things needed for non-local-exit.
    sb!impl::*current-catch-block*
    sb!impl::*current-unwind-protect-block*
    sb!c::*eval-stack-top*

    ;; Interrupt Handling
    sb!impl::*free-interrupt-context-index*
    sb!unix::*interrupts-enabled*
    sb!unix::*interrupt-pending*
    ))

(defparameter *static-functions*
  '(length
    sb!kernel:two-arg-+
    sb!kernel:two-arg--
    sb!kernel:two-arg-*
    sb!kernel:two-arg-/
    sb!kernel:two-arg-<
    sb!kernel:two-arg->
    sb!kernel:two-arg-=
    ;; Probably need the following as they are defined in arith.lisp
    ;; two-arg-<= two-arg->= two-arg-/= 
    eql
    sb!kernel:%negate
    sb!kernel:two-arg-and
    sb!kernel:two-arg-ior
    sb!kernel:two-arg-xor
    sb!kernel:two-arg-gcd
    sb!kernel:two-arg-lcm))

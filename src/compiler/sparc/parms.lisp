;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

;;;; Machine Architecture parameters:
(eval-when (:compile-toplevel :load-toplevel :execute)

(defconstant n-word-bits 32
  #!+sb-doc
  "Number of bits per word where a word holds one lisp descriptor.")

(defconstant n-byte-bits 8
  #!+sb-doc
  "Number of bits per byte where a byte is the smallest addressable object.")

(defconstant word-shift (1- (integer-length (/ n-word-bits n-byte-bits)))
  #!+sb-doc
  "Number of bits to shift between word addresses and byte addresses.")

(defconstant n-word-bytes (/ n-word-bits n-byte-bits)
  #!+sb-doc
  "Number of bytes in a word.")

;;; FIXME: The following three should probably be rationalized or at
;;; least prefixed with n- where applicable
(defconstant fixnum-tag-bits (1- n-lowtag-bits)
  #!+sb-doc
  "Number of tag bits used for a fixnum")

(defconstant fixnum-tag-mask (1- (ash 1 fixnum-tag-bits))
  #!+sb-doc
  "Mask to get the fixnum tag")

(defconstant positive-fixnum-bits (- n-word-bits fixnum-tag-bits 1)
  #!+sb-doc
  "Maximum number of bits in a positive fixnum")

(defconstant float-sign-shift 31)

(defconstant single-float-bias 126)
(defconstant-eqx single-float-exponent-byte (byte 8 23) #'equalp)
(defconstant-eqx single-float-significand-byte (byte 23 0) #'equalp)
(defconstant single-float-normal-exponent-min 1)
(defconstant single-float-normal-exponent-max 254)
(defconstant single-float-hidden-bit (ash 1 23))
(defconstant single-float-trapping-nan-bit (ash 1 22))

(defconstant double-float-bias 1022)
(defconstant-eqx double-float-exponent-byte (byte 11 20) #'equalp)
(defconstant-eqx double-float-significand-byte (byte 20 0) #'equalp)
(defconstant double-float-normal-exponent-min 1)
(defconstant double-float-normal-exponent-max #x7FE)
(defconstant double-float-hidden-bit (ash 1 20))
(defconstant double-float-trapping-nan-bit (ash 1 19))

;;; CMUCL COMMENT:
;;;   X These values are for the x86 80 bit format and are no doubt
;;;   incorrect for the sparc.
;;; FIXME
(defconstant long-float-bias 16382)
(defconstant-eqx long-float-exponent-byte (byte 15 0) #'equalp)
(defconstant-eqx long-float-significand-byte (byte 31 0) #'equalp)
(defconstant long-float-normal-exponent-min 1)
(defconstant long-float-normal-exponent-max #x7FFE)
(defconstant long-float-hidden-bit (ash 1 31))
(defconstant long-float-trapping-nan-bit (ash 1 30))

(defconstant single-float-digits
  (+ (byte-size single-float-significand-byte) 1))

(defconstant double-float-digits
  (+ (byte-size double-float-significand-byte) n-word-bits 1))

;;; This looks wrong - CSR
(defconstant long-float-digits
  (+ (byte-size long-float-significand-byte) n-word-bits 1))

(defconstant float-inexact-trap-bit (ash 1 0))
(defconstant float-divide-by-zero-trap-bit (ash 1 1))
(defconstant float-underflow-trap-bit (ash 1 2))
(defconstant float-overflow-trap-bit (ash 1 3))
(defconstant float-invalid-trap-bit (ash 1 4))

(defconstant float-round-to-nearest 0)
(defconstant float-round-to-zero 1)
(defconstant float-round-to-positive 2)
(defconstant float-round-to-negative 3)

(defconstant-eqx float-rounding-mode (byte 2 30) #'equalp)	  ; RD 
(defconstant-eqx float-sticky-bits (byte 5 5) #'equalp)	  ; aexc
(defconstant-eqx float-traps-byte (byte 5 23) #'equalp)	  ; TEM
(defconstant-eqx float-exceptions-byte (byte 5 0) #'equalp)	  ; cexc

;;; According to the SPARC doc (as opposed to FPU doc), the fast mode
;;; bit (EFM) is "reserved", and should always be zero.  However, for
;;; sparc-V8 and sparc-V9, it appears to work, causing denormals to
;;; be truncated to 0 silently.
(defconstant float-fast-bit (ash 1 22))

); eval-when

;;; NUMBER-STACK-DISPLACEMENT
;;;
;;; The number of bytes reserved above the number stack pointer.  These
;;; slots are required by architecture for a place to spill register windows.
;;;
;;; FIXME: Where is this used?
(defconstant number-stack-displacement
  (* 16 n-word-bytes))


;;;; Description of the target address space.

;;; Where to put the different spaces.  Must match the C code!
#!+linux
(progn
  (defconstant read-only-space-start #x10000000)
  (defconstant read-only-space-end #x15000000)

  (defconstant static-space-start    #x28000000)
  (defconstant static-space-end #x2c000000)

  ;; From alpha/parms.lisp:
  ;; this is used in PURIFY as part of a sloppy check to see if a pointer
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

  (defconstant binding-stack-start    #x60000000)
  (defconstant binding-stack-end      #x61000000))

#!+solaris ; maybe someday.
(progn
  (defparameter target-read-only-space-start #x10000000)
  (defparameter target-static-space-start    #x28000000)
  (defparameter target-dynamic-space-start   #x40000000))

;;;; other random constants.

(defenum (:suffix -trap :start 8)
  halt
  pending-interrupt
  error
  cerror
  breakpoint
  fun-end-breakpoint
  after-breakpoint)

(defenum (:prefix object-not- :suffix -trap :start 16)
  list
  instance)

(defenum (:prefix trace-table-)
  normal
  call-site
  fun-prologue
  fun-epilogue)

;;;; static symbols.

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
    maybe-gc
    sb!kernel::internal-error
    sb!di::handle-breakpoint
    sb!di::handle-fun-end-breakpoint

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
    ))

(defparameter *static-funs*
  '(length
    two-arg-+ two-arg-- two-arg-* two-arg-/ two-arg-< two-arg-> two-arg-=
    two-arg-<= two-arg->= two-arg-/= eql %negate
    two-arg-and two-arg-ior two-arg-xor
    two-arg-gcd two-arg-lcm
    ))

;;;; Assembler parameters:

;;; The number of bits per element in the assemblers code vector.
;;;
(defparameter *assembly-unit-length* 8)


;;;; Pseudo-atomic trap number
;;; KLUDGE
#!-linux
(defconstant pseudo-atomic-trap 16)
#!+linux
(defconstant pseudo-atomic-trap #x40)

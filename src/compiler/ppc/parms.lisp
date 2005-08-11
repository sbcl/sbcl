;;;; This file contains some parameterizations of various VM
;;;; attributes for the PPC.  This file is separate from other stuff so
;;;; that it can be compiled and loaded earlier.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

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
(def!constant float-divide-by-zero-trap-bit (ash 1 1))
(def!constant float-underflow-trap-bit (ash 1 2))
(def!constant float-overflow-trap-bit (ash 1 3))
(def!constant float-invalid-trap-bit (ash 1 4))

(def!constant float-round-to-nearest 0)
(def!constant float-round-to-zero 1)
(def!constant float-round-to-positive 2)
(def!constant float-round-to-negative 3)

(defconstant-eqx float-rounding-mode (byte 2 0) #'equalp)         ; RD
;;; FIXME I: Beware, all ye who trespass here. Despite its name,
;;; FLOAT-STICKY-BITS is not the byte specifier for sticky bits in the
;;; floating point control word. It is more like "accrued exceptions"
;;; where FLOAT-EXCEPTIONS-BYTE is "current exceptions". Consequently,
;;; on architectures where there is no "current exceptions"
;;; FLOAT-EXCEPTIONS-BYTE and FLOAT-STICKY-BITS had better be the
;;; same.
;;;
;;; FIXME II: So, I've now documented this in comments in the PowerPC
;;; tree. This may not make it easy to find for when new architectures
;;; get backends written...
;;;
;;; CSR, 2002-06-11
(defconstant-eqx float-sticky-bits (byte 5 25) #'equalp)
(defconstant-eqx float-traps-byte (byte 5 3) #'equalp)
(defconstant-eqx float-exceptions-byte (byte 5 25) #'equalp)      ; cexc

(def!constant float-fast-bit 2)         ; Non-IEEE mode


;;; Where to put the different spaces.

(def!constant read-only-space-start #x01000000)
(def!constant read-only-space-end   #x04ff8000)

(def!constant static-space-start    #x08000000)
(def!constant static-space-end      #x097fff00)

;;; nothing _seems_ to be using these addresses
(def!constant dynamic-0-space-start #x10000000)
(def!constant dynamic-0-space-end   #x3ffff000)
(def!constant dynamic-1-space-start #x40000000)
(def!constant dynamic-1-space-end   #x6ffff000)

#!+darwin
(progn
  (def!constant linkage-table-space-start #x0a000000)
  (def!constant linkage-table-space-end   #x0b000000)
  (def!constant linkage-table-entry-size 16))

;;;; Other miscellaneous constants.

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

    ;; functions that the C code needs to call
    sb!impl::sub-gc
    sb!kernel::internal-error
    sb!kernel::control-stack-exhausted-error
    sb!kernel::undefined-alien-variable-error
    sb!kernel::undefined-alien-function-error
    sb!di::handle-breakpoint
    sb!impl::fdefinition-object

    ;; free pointers
    *read-only-space-free-pointer*
    *static-space-free-pointer*
    *initial-dynamic-space-free-pointer*

    ;; things needed for non-local exit
    *current-catch-block*
    *current-unwind-protect-block*

    *binding-stack-start*
    *control-stack-start*
    *control-stack-end*

    ;; interrupt handling
    *free-interrupt-context-index*
    sb!unix::*interrupts-enabled*
    sb!unix::*interrupt-pending*
    *gc-inhibit*
    *gc-pending*))

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
    sb!kernel:two-arg-eqv
    sb!kernel:two-arg-gcd
    sb!kernel:two-arg-lcm))


;;;; Assembler parameters:

;;; The number of bits per element in the assemblers code vector.
;;;
(defparameter *assembly-unit-length* 8)

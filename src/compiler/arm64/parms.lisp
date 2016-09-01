;;;; This file contains some parameterizations of various VM
;;;; attributes for the ARM.  This file is separate from other stuff so
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
(defconstant n-word-bits 64)

;;; the natural width of a machine word (as seen in e.g. register width,
;;; address space)
(defconstant n-machine-word-bits 64)

;;; number of bits per byte where a byte is the smallest addressable
;;; object
(defconstant n-byte-bits 8)

;;; Floating-point related constants, both format descriptions and FPU
;;; control register descriptions.  These don't exactly match up with
;;; what the machine manuals say because the Common Lisp standard
;;; defines floating-point values somewhat differently than the IEEE
;;; standard does.

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

(defconstant single-float-digits
  (+ (byte-size single-float-significand-byte) 1))

(defconstant double-float-digits
  (+ (byte-size double-float-significand-byte) 32 1))


(progn
  (defconstant float-invalid-trap-bit (ash 1 0))
  (defconstant float-divide-by-zero-trap-bit (ash 1 1))
  (defconstant float-overflow-trap-bit (ash 1 2))
  (defconstant float-underflow-trap-bit (ash 1 3))
  (defconstant float-inexact-trap-bit (ash 1 4))
  (defconstant float-input-denormal-trap-bit (ash 1 7))

  (defconstant float-round-to-nearest 0)
  (defconstant float-round-to-positive 1)
  (defconstant float-round-to-negative 2)
  (defconstant float-round-to-zero 3)

  (defconstant-eqx float-rounding-mode (byte 2 22) #'equalp)

  (defconstant-eqx float-sticky-bits (byte 8 0) #'equalp)
  (defconstant-eqx float-traps-byte (byte 8 8) #'equalp)
  (defconstant-eqx float-exceptions-byte (byte 8 0) #'equalp)

  (defconstant float-fast-bit (ash 1 24))) ;; Flush-to-zero mode

;;;; Where to put the different spaces.

;;; On non-gencgc we need large dynamic and static spaces for PURIFY
#!-gencgc
(progn
  (defconstant read-only-space-start #x04000000)
  (defconstant read-only-space-end   #x07ff8000)
  (defconstant static-space-start    #x08000000)
  (defconstant static-space-end      #x097fff00)

  (defconstant linkage-table-space-start #x0a000000)
  (defconstant linkage-table-space-end   #x0b000000)
  #!+linux
  (progn
    (defconstant dynamic-0-space-start #x4f000000)
    (defconstant dynamic-0-space-end   #x66fff000)
    (defconstant dynamic-1-space-start #x67000000)
    (defconstant dynamic-1-space-end   #x7efff000)))

#!+gencgc
(!gencgc-space-setup #xF0000000 :dynamic-space-start #x1000000000)

(defconstant linkage-table-entry-size 16)

;;;; other miscellaneous constants

(defenum (:start 8)
  halt-trap
  pending-interrupt-trap
  error-trap
  cerror-trap
  breakpoint-trap
  fun-end-breakpoint-trap
  single-step-around-trap
  single-step-before-trap
  invalid-arg-count-trap)

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
  (append
   #!-sb-thread
   '(*binding-stack-pointer*
     *pseudo-atomic-atomic*
     *pseudo-atomic-interrupted*)
   '(*allocation-pointer*
     ;; interrupt handling


     ;; Needed for callbacks to work across saving cores. see
     ;; ALIEN-CALLBACK-ASSEMBLER-WRAPPER in c-call.lisp for gory
     ;; details.
     sb!alien::*enter-alien-callback*
     #!+gencgc *restart-lisp-function*)
   *common-static-symbols*
   *c-callable-static-symbols*))

(defparameter *static-funs*
  '(two-arg-gcd two-arg-lcm
    two-arg-+ two-arg-- two-arg-* two-arg-/
    two-arg-< two-arg-> two-arg-=
    two-arg-and two-arg-ior two-arg-xor two-arg-eqv

    eql
    sb!kernel:%negate))


;;;; Assembler parameters:

;;; The number of bits per element in the assemblers code vector.
;;;
(defparameter *assembly-unit-length* 8)

(defvar *allocation-pointer*)

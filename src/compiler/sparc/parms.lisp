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

;;; number of bits per word where a word holds one lisp descriptor
(def!constant n-word-bits 32)

;;; the natural width of a machine word (as seen in e.g. register width,
;;; address space)
(def!constant n-machine-word-bits 32)

;;; number of bits per byte where a byte is the smallest addressable object
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

;;; CMUCL COMMENT:
;;;   X These values are for the x86 80 bit format and are no doubt
;;;   incorrect for the sparc.
;;; FIXME
(def!constant long-float-bias 16382)
(defconstant-eqx long-float-exponent-byte (byte 15 0) #'equalp)
(defconstant-eqx long-float-significand-byte (byte 31 0) #'equalp)
(def!constant long-float-normal-exponent-min 1)
(def!constant long-float-normal-exponent-max #x7FFE)
(def!constant long-float-hidden-bit (ash 1 31))
(def!constant long-float-trapping-nan-bit (ash 1 30))

(def!constant single-float-digits
  (+ (byte-size single-float-significand-byte) 1))

(def!constant double-float-digits
  (+ (byte-size double-float-significand-byte) n-word-bits 1))

;;; This looks wrong - CSR
(def!constant long-float-digits
  (+ (byte-size long-float-significand-byte) n-word-bits 1))

(def!constant float-inexact-trap-bit (ash 1 0))
(def!constant float-divide-by-zero-trap-bit (ash 1 1))
(def!constant float-underflow-trap-bit (ash 1 2))
(def!constant float-overflow-trap-bit (ash 1 3))
(def!constant float-invalid-trap-bit (ash 1 4))

(def!constant float-round-to-nearest 0)
(def!constant float-round-to-zero 1)
(def!constant float-round-to-positive 2)
(def!constant float-round-to-negative 3)

(defconstant-eqx float-rounding-mode (byte 2 30) #'equalp)        ; RD
(defconstant-eqx float-sticky-bits (byte 5 5) #'equalp)   ; aexc
(defconstant-eqx float-traps-byte (byte 5 23) #'equalp)   ; TEM
(defconstant-eqx float-exceptions-byte (byte 5 0) #'equalp)       ; cexc

;;; According to the SPARC doc (as opposed to FPU doc), the fast mode
;;; bit (EFM) is "reserved", and should always be zero.  However, for
;;; sparc-V8 and sparc-V9, it appears to work, causing denormals to
;;; be truncated to 0 silently.
(def!constant float-fast-bit (ash 1 22))

); eval-when


;;;; Description of the target address space.

;;; Where to put the different spaces.  Must match the C code!
#!+linux
(progn
  (def!constant linkage-table-space-start #x0f800000)
  (def!constant linkage-table-space-end   #x10000000)

  (def!constant read-only-space-start     #x11000000)
  (def!constant read-only-space-end       #x15000000)

  (def!constant static-space-start        #x28000000)
  (def!constant static-space-end          #x2c000000)

  (def!constant dynamic-0-space-start #x30000000)
  (def!constant dynamic-0-space-end   #x38000000)

  (def!constant dynamic-1-space-start #x40000000)
  (def!constant dynamic-1-space-end   #x48000000))

#!+sunos ; might as well start by trying the same numbers
(progn
  (def!constant linkage-table-space-start #x0f800000)
  (def!constant linkage-table-space-end   #x10000000)

  (def!constant read-only-space-start     #x11000000)
  (def!constant read-only-space-end       #x15000000)

  (def!constant static-space-start        #x28000000)
  (def!constant static-space-end          #x2c000000)

  (def!constant dynamic-0-space-start     #x30000000)
  (def!constant dynamic-0-space-end       #x38000000)

  (def!constant dynamic-1-space-start     #x40000000)
  (def!constant dynamic-1-space-end       #x48000000))

#!+netbsd ; Need a gap at 0x4000000 for shared libraries
(progn
  (def!constant linkage-table-space-start #x0f800000)
  (def!constant linkage-table-space-end   #x10000000)

  (def!constant read-only-space-start     #x11000000)
  (def!constant read-only-space-end       #x15000000)

  (def!constant static-space-start        #x18000000)
  (def!constant static-space-end          #x1c000000)

  (def!constant dynamic-0-space-start     #x48000000)
  (def!constant dynamic-0-space-end       #x5ffff000)

  (def!constant dynamic-1-space-start     #x60000000)
  (def!constant dynamic-1-space-end       #x77fff000))

;; Size of one linkage-table entry in bytes. See comment in
;; src/runtime/sparc-arch.c
(def!constant linkage-table-entry-size 16)


;;;; other random constants.

(defenum (:start 8)
  halt-trap
  pending-interrupt-trap
  error-trap
  cerror-trap
  breakpoint-trap
  fun-end-breakpoint-trap
  after-breakpoint-trap
  single-step-around-trap
  single-step-before-trap)

(defenum (:start 24)
  object-not-list-trap
  object-not-instance-trap)

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
  (append
   *common-static-symbols*
   *c-callable-static-symbols*
   '()))

(defparameter *static-funs*
  '(length
    two-arg-+ two-arg-- two-arg-* two-arg-/ two-arg-< two-arg-> two-arg-=
    two-arg-<= two-arg->= two-arg-/= eql %negate
    two-arg-and two-arg-ior two-arg-xor two-arg-eqv
    two-arg-gcd two-arg-lcm
    ))

;;;; Assembler parameters:

;;; The number of bits per element in the assemblers code vector.
(defparameter *assembly-unit-length* 8)


;;;; Pseudo-atomic trap number

;;; KLUDGE: Linux on the SPARC doesn't seem to conform to any kind of
;;; standards at all. So we use an explicitly undefined trap, because
;;; that currently does the right thing. Expect this to break
;;; eventually (but with luck, at that point we'll be able to revert
;;; to the compliant trap number...
;;;
;;; KLUDGE: Maybe this should be called pseudo-atomic-magic-number,
;;; allowing other architectures (which don't necessarily use traps
;;; for pseudo-atomic) to propagate a magic number to C land via
;;; sbcl.h.
#!-linux
(def!constant pseudo-atomic-trap #x10)
#!+linux
(def!constant pseudo-atomic-trap #x40)

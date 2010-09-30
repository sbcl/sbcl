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

;;; flags for the generational garbage collector
(def!constant pseudo-atomic-interrupted-flag 1)
(def!constant pseudo-atomic-flag 4)

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


;;;; Where to put the different spaces.

;;; On non-gencgc we need large dynamic and static spaces for PURIFY
#!-gencgc
(progn
  (def!constant read-only-space-start #x04000000)
  (def!constant read-only-space-end   #x07ff8000)
  (def!constant static-space-start    #x08000000)
  (def!constant static-space-end      #x097fff00)

  (def!constant linkage-table-space-start #x0a000000)
  (def!constant linkage-table-space-end   #x0b000000))

;;; While on gencgc we don't.
#!+gencgc
(progn
  (def!constant read-only-space-start #x04000000)
  (def!constant read-only-space-end   #x040ff000)
  (def!constant static-space-start    #x04100000)
  (def!constant static-space-end      #x041ff000)

  (def!constant linkage-table-space-start #x04200000)
  (def!constant linkage-table-space-end   #x042ff000))

(def!constant linkage-table-entry-size 16)

#!+linux
(progn
  #!+gencgc
  (progn
    (def!constant dynamic-space-start #x4f000000)
    (def!constant dynamic-space-end   (!configure-dynamic-space-end #x7efff000)))
  #!-gencgc
  (progn
    (def!constant dynamic-0-space-start #x4f000000)
    (def!constant dynamic-0-space-end   #x66fff000)
    (def!constant dynamic-1-space-start #x67000000)
    (def!constant dynamic-1-space-end   #x7efff000)))

#!+netbsd
(progn
  #!+gencgc
  (progn
    (def!constant dynamic-space-start #x4f000000)
    (def!constant dynamic-space-end   (!configure-dynamic-space-end #x7efff000)))
  #!-gencgc
  (progn
    (def!constant dynamic-0-space-start #x4f000000)
    (def!constant dynamic-0-space-end   #x66fff000)
    (def!constant dynamic-1-space-start #x67000000)
    (def!constant dynamic-1-space-end   #x7efff000)))

;;; Text and data segments start at #x01800000.  Range for randomized
;;; malloc() starts #x20000000 (MAXDSIZ) after end of data seg and
;;; extends 256 MB.  Use 512 - 64 MB for dynamic space so we can run
;;; under default resource limits.
;;; FIXME: MAXDSIZ is a kernel parameter, and can vary as high as 1GB.
;;; These parameters should probably be tested under such a configuration,
;;; as rare as it might or might not be.
#!+openbsd
(progn
  #!+gencgc
  (progn
    (def!constant dynamic-space-start #x4f000000)
    (def!constant dynamic-space-end   (!configure-dynamic-space-end #x6afff000)))
  #!-gencgc
  (progn
    (def!constant dynamic-0-space-start #x4f000000)
    (def!constant dynamic-0-space-end   #x5cfff000)
    (def!constant dynamic-1-space-start #x5f000000)
    (def!constant dynamic-1-space-end   #x6cfff000)))

#!+darwin
(progn
  #!+gencgc
  (progn
    (def!constant dynamic-space-start #x10000000)
    (def!constant dynamic-space-end   (!configure-dynamic-space-end #x6ffff000)))
  #!-gencgc
  (progn
    (def!constant dynamic-0-space-start #x10000000)
    (def!constant dynamic-0-space-end   #x3ffff000)

    (def!constant dynamic-1-space-start #x40000000)
    (def!constant dynamic-1-space-end   #x6ffff000)))

;;;; Other miscellaneous constants.

(defenum (:start 8)
  halt-trap
  pending-interrupt-trap
  error-trap
  cerror-trap
  breakpoint-trap
  fun-end-breakpoint-trap
  after-breakpoint-trap
  fixnum-additive-overflow-trap
  single-step-around-trap
  single-step-before-trap)

(defenum (:start 24)
  object-not-list-trap
  object-not-instance-trap)

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
   *common-static-symbols*
   *c-callable-static-symbols*
   '(
     #!+gencgc *restart-lisp-function*

     ;; CLH: 20060210 Taken from x86-64/parms.lisp per JES' suggestion
     ;; Needed for callbacks to work across saving cores. see
     ;; ALIEN-CALLBACK-ASSEMBLER-WRAPPER in c-call.lisp for gory
     ;; details.
     sb!alien::*enter-alien-callback*)))

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

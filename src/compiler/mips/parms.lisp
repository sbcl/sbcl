;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

(defconstant sb-assem:assem-scheduler-p t)
(defconstant sb-assem:+inst-alignment-bytes+ 4)
(defconstant sb-assem:+assem-max-locations+ 68)

(defconstant +backend-fasl-file-implementation+ :mips)

  ;; The o32 ABI specifies 4k-64k as page size. We have to pick the
  ;; maximum since mprotect() works only with page granularity.
(defconstant +backend-page-bytes+ 65536)

;;;; Machine Architecture parameters:
(eval-when (:compile-toplevel :load-toplevel :execute)

;;; number of bits per word where a word holds one lisp descriptor
(defconstant n-word-bits 32)

;;; the natural width of a machine word (as seen in e.g. register width,
;;; address space)
(defconstant n-machine-word-bits 32)

(defconstant float-sign-shift 31)

(defconstant single-float-bias 126)
(defconstant-eqx single-float-exponent-byte (byte 8 23) #'equalp)
(defconstant-eqx single-float-significand-byte (byte 23 0) #'equalp)
(defconstant single-float-normal-exponent-min 1)
(defconstant single-float-normal-exponent-max 254)
(defconstant single-float-hidden-bit (ash 1 23))

(defconstant double-float-bias 1022)
(defconstant-eqx double-float-exponent-byte (byte 11 20) #'equalp)
(defconstant-eqx double-float-significand-byte (byte 20 0) #'equalp)
(defconstant double-float-normal-exponent-min 1)
(defconstant double-float-normal-exponent-max #x7FE)
(defconstant double-float-hidden-bit (ash 1 20))

(defconstant single-float-digits
  (+ (byte-size single-float-significand-byte) 1))

(defconstant double-float-digits
  (+ (byte-size double-float-significand-byte) n-word-bits 1))

(defconstant float-inexact-trap-bit (ash 1 0))
(defconstant float-underflow-trap-bit (ash 1 1))
(defconstant float-overflow-trap-bit (ash 1 2))
(defconstant float-divide-by-zero-trap-bit (ash 1 3))
(defconstant float-invalid-trap-bit (ash 1 4))
(defconstant float-unimplemented-trap-bit (ash 1 5))

(defconstant float-round-to-nearest 0)
(defconstant float-round-to-zero 1)
(defconstant float-round-to-positive 2)
(defconstant float-round-to-negative 3)

(defconstant-eqx float-rounding-mode (byte 2 0) #'equalp)
(defconstant-eqx float-sticky-bits (byte 5 2) #'equalp)
(defconstant-eqx float-traps-byte (byte 5 7) #'equalp)
(defconstant-eqx float-exceptions-byte (byte 6 12) #'equalp)
(defconstant-eqx float-condition-bit (ash 1 23) #'equalp)
(defconstant float-fast-bit (ash 1 24))

;;;; Description of the target address space.

#+irix
(progn
  ;; Where to put the different spaces.
  ;; Old definitions, might be still relevant for an IRIX port.
  ;;
  (defconstant read-only-space-start #x01000000)
  (defconstant read-only-space-end   #x05000000)

  (defconstant static-space-start    #x06000000)
  (defconstant static-space-end      #x08000000)

  (defparameter dynamic-0-space-start #x08000000)
  (defparameter dynamic-0-space-end   #x0c000000))

#+linux
(progn
  ;; Where to put the address spaces on Linux.
  ;;
  ;; C runtime executable segment starts at 0x00400000
  (defconstant read-only-space-start #x01000000)
  (defconstant read-only-space-end   #x07ff0000)

  (defconstant static-space-start    #x08000000)
  (defconstant static-space-end      #x0fff0000)
  ;; C runtime read/write segment starts at 0x10000000, heap and DSOs
  ;; start at 0x2a000000
  (defparameter dynamic-0-space-start #x30000000)
  (defparameter dynamic-0-space-end   #x4fff0000)

  (defconstant linkage-table-space-start #x70000000)
  (defconstant linkage-table-space-end   #x71000000)
  (defconstant linkage-table-entry-size 16)

  ;; C stack grows downward from 0x80000000
  )

); eval-when


;;;; Other non-type constants.

(defenum ()
  atomic-flag
  interrupted-flag)

(defenum (:start 8)
  halt-trap
  pending-interrupt-trap
  cerror-trap
  breakpoint-trap
  fun-end-breakpoint-trap
  after-breakpoint-trap
  single-step-around-trap
  single-step-before-trap
  error-trap)

;;;; Static symbols.

;;; Static symbols are loaded into static space directly after NIL so
;;; that the system can compute their address by adding a constant
;;; amount to NIL.
;;;
;;; The fdefn objects for the static functions are loaded into static
;;; space directly after the static symbols.  That way, the raw-addr
;;; can be loaded directly out of them by indirecting relative to NIL.
(defconstant-eqx +static-symbols+
  `#(,@+common-static-symbols+)
  #'equalp)

(defconstant-eqx +static-fdefns+
  #(two-arg-+
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
    length
    two-arg-gcd
    two-arg-lcm)
  #'equalp)

;;;; This file contains some parameterizations of various VM
;;;; attributes for the RV32.  This file is separate from other stuff so
;;;; that it can be compiled and loaded earlier.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

(defconstant sb-assem:assem-scheduler-p nil)
(defconstant sb-assem:+inst-alignment-bytes+ 4) ; FIXME: C

(defconstant +backend-fasl-file-implementation+ :rv32g)

(defconstant +backend-page-bytes+ #+linux 4096 #+netbsd 8192)

;;; number of bits per word where a word holds one lisp descriptor
(defconstant n-word-bits 32)

;;; the natural width of a machine word (as seen in e.g. register width,
;;; address space)
(defconstant n-machine-word-bits 32)

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
  (+ (byte-size double-float-significand-byte) n-word-bits 1))


;;;; Where to put the different spaces.

;;; On non-gencgc we need large dynamic and static spaces for PURIFY
(progn
  (defconstant read-only-space-start #x04000000)
  (defconstant read-only-space-end   #x07ff8000)
  (defconstant static-space-start    #x08000000)
  (defconstant static-space-end      #x097fff00)

  (defconstant linkage-table-space-start #x0a000000)
  (defconstant linkage-table-space-end   #x0b000000))

(defconstant linkage-table-entry-size 16)

#+(or linux netbsd)
(progn
  (progn
    (defparameter dynamic-0-space-start #x4f000000)
    (defparameter dynamic-0-space-end   #x66fff000)))

;;;; other miscellaneous constants

#+nil
(defenum (:start 8)
  halt-trap
  pending-interrupt-trap
  cerror-trap
  breakpoint-trap
  fun-end-breakpoint-trap
  single-step-around-trap
  single-step-before-trap
  error-trap)

;;;; Static symbols.

;;; These symbols are loaded into static space directly after NIL so
;;; that the system can compute their address by adding a constant
;;; amount to NIL.
;;;
;;; The fdefn objects for the static functions are loaded into static
;;; space directly after the static symbols.  That way, the raw-addr
;;; can be loaded directly out of them by indirecting relative to NIL.
;;;
(defconstant-eqx +static-symbols+
 `#(,@+common-static-symbols+
    *allocation-pointer*

     *control-stack-pointer*
     *binding-stack-pointer*
     *interrupted-control-stack-pointer*

     ;; interrupt handling
     *pseudo-atomic-atomic*
     *pseudo-atomic-interrupted*)
  #'equalp)

(defconstant-eqx +static-fdefns+
  #(two-arg-gcd two-arg-lcm
    two-arg-+ two-arg-- two-arg-* two-arg-/
    two-arg-< two-arg-> two-arg-=
    two-arg-and two-arg-ior two-arg-xor two-arg-eqv

    eql
    sb-kernel:%negate)
  #'equalp)


;;;; Assembler parameters:

;;; The number of bits per element in the assemblers code vector.
;;;
(defparameter *assembly-unit-length* 8)

(defvar *allocation-pointer*)

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

(in-package "SB-VM")

(defconstant sb-assem:assem-scheduler-p nil)
(defconstant sb-assem:+inst-alignment-bytes+ 4)

(defconstant +backend-fasl-file-implementation+ :arm)

  ;; Minumum observed value, not authoritative.
(defconstant +backend-page-bytes+ #-netbsd 4096 #+netbsd 8192)

;;; The size in bytes of GENCGC cards, i.e. the granularity at which
;;; writes to old generations are logged.  With mprotect-based write
;;; barriers, this must be a multiple of the OS page size.
(defconstant gencgc-page-bytes +backend-page-bytes+)
;;; The minimum size of new allocation regions.  While it doesn't
;;; currently make a lot of sense to have a card size lower than
;;; the alloc granularity, it will, once we are smarter about finding
;;; the start of objects.
(defconstant gencgc-alloc-granularity 0)
;;; The minimum size at which we release address ranges to the OS.
;;; This must be a multiple of the OS page size.
(defconstant gencgc-release-granularity +backend-page-bytes+)

;;; number of bits per word where a word holds one lisp descriptor
(defconstant n-word-bits 32)

;;; the natural width of a machine word (as seen in e.g. register width,
;;; address space)
(defconstant n-machine-word-bits 32)

#+arm-vfp
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
;; NOTE: As with the FLOAT-REGISTERS SB in vm.lisp, if you define this
;; for non-VFP systems, please use a specific positive feature
;; conditional.
#-arm-vfp
(error "Don't know how to set the FPU control word layout on non-VFP systems")

;;;; Where to put the different spaces.

;;; On non-gencgc we need large dynamic and static spaces for PURIFY
#-gencgc
(progn
  (defconstant read-only-space-start #x04000000)
  (defconstant read-only-space-end   #x07ff8000)
  (defconstant static-space-start    #x08000000)
  (defconstant static-space-end      #x097fff00)

  (defconstant linkage-table-space-start #x0a000000)
  (defconstant linkage-table-space-end   #x0b000000))

#+gencgc
(progn
  #+(or linux netbsd)
  (!gencgc-space-setup #x04000000 :dynamic-space-start #x4f000000)
  #+openbsd
  (!gencgc-space-setup #x04000000 :dynamic-space-start #x10000000))

(defconstant linkage-table-growth-direction :down)
(defconstant linkage-table-entry-size 16)
;;; Link these as data entries so that we store only the address of the
;;; handwritten assembly code in the linkage able, and not a trampoline
;;; to the trampoline. The ALLOCATION macro just wants an address.
#+gencgc
(setq *linkage-space-predefined-entries* '(("alloc_tramp" t)
                                           ("list_alloc_tramp" t)))

#+(or linux netbsd openbsd)
(progn
  #-gencgc
  (progn
    (defparameter dynamic-0-space-start #x4f000000)
    (defparameter dynamic-0-space-end   #x66fff000)))

;;;; other miscellaneous constants

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

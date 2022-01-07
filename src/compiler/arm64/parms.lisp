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

(defconstant +backend-fasl-file-implementation+ :arm64)

  ;; Can be in the range 4K-64K
(defconstant +backend-page-bytes+ 65536)

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
(defconstant n-word-bits 64)

;;; the natural width of a machine word (as seen in e.g. register width,
;;; address space)
(defconstant n-machine-word-bits 64)

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
#-gencgc
(progn
  (defconstant read-only-space-start #x04000000)
  (defconstant read-only-space-end   #x07ff8000)
  (defconstant static-space-start    #x08000000)
  (defconstant static-space-end      #x097fff00)

  (defconstant linkage-table-space-start #x0a000000)
  (defconstant linkage-table-space-end   #x0b000000)
  #+(or linux openbsd)
  (progn
    (defparameter dynamic-0-space-start #x4f000000)
    (defparameter dynamic-0-space-end   #x66fff000)))

#+gencgc
(!gencgc-space-setup #+(or linux openbsd freebsd) #xF0000000
                     #+darwin #x300000000
                     #+netbsd #x2F0000000
                     :dynamic-space-start
                     #-darwin #x1000000000
                     #+darwin #x7003000000)

(defconstant linkage-table-growth-direction :up)
(defconstant linkage-table-entry-size 16)

;;;; other miscellaneous constants

(defenum (:start 8)
  halt-trap
  pending-interrupt-trap
  cerror-trap
  breakpoint-trap
  fun-end-breakpoint-trap
  single-step-around-trap
  single-step-before-trap
  invalid-arg-count-trap
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
 `#(#-sb-thread
    ,@'(*binding-stack-pointer*
        *pseudo-atomic-atomic*
        *pseudo-atomic-interrupted*)
    *allocation-pointer*
     ;; interrupt handling
     ,@+common-static-symbols+)
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

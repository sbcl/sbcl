;;;; This file contains some parameterizations of various VM
;;;; attributes for the RISC-V.  This file is separate from other stuff so
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

(defconstant sb-fasl:+backend-fasl-file-implementation+ #-64-bit :rv32g #+64-bit :rv64g)

(defconstant +backend-page-bytes+ #+linux 4096 #+netbsd 8192)

;;; The size in bytes of GENCGC cards, i.e. the granularity at which
;;; writes to old generations are logged.  With mprotect-based write
;;; barriers, this must be a multiple of the OS page size.
(defconstant gencgc-page-bytes +backend-page-bytes+)
;;; The minimum size of new allocation regions.  While it doesn't
;;; currently make a lot of sense to have a card size lower than
;;; the alloc granularity, it will, once we are smarter about finding
;;; the start of objects.
(defconstant gencgc-alloc-granularity 0)

;;; number of bits per word where a word holds one lisp descriptor
(defconstant n-word-bits #-64-bit 32 #+64-bit 64)

;;; the natural width of a machine word (as seen in e.g. register width,
;;; address space)
(defconstant n-machine-word-bits #-64-bit 32 #+64-bit 64)

(defconstant float-inexact-trap-bit (ash 1 0))
(defconstant float-underflow-trap-bit (ash 1 1))
(defconstant float-overflow-trap-bit (ash 1 2))
(defconstant float-divide-by-zero-trap-bit (ash 1 3))
(defconstant float-invalid-trap-bit (ash 1 4))

(defconstant float-round-to-nearest 0)
(defconstant float-round-to-zero 1)
(defconstant float-round-to-negative 2)
(defconstant float-round-to-positive 3)

(defconstant-eqx float-rounding-mode (byte 3 5) #'equalp)
(defconstant-eqx float-sticky-bits (byte 5 0) #'equalp)
;;;; RISC-V has no explicit floating point traps.
(defconstant-eqx float-traps-byte (byte 5 0) #'equalp)
(defconstant-eqx float-exceptions-byte (byte 5 0) #'equalp)
(defconstant float-fast-bit (ash 1 24)) ;; Flush-to-zero mode


;;;; Where to put the different spaces.

(gc-space-setup #x04000000 :dynamic-space-start #x4f000000)

(defconstant alien-linkage-table-entry-size #-64-bit 8 #+64-bit 24)
(defconstant alien-linkage-table-growth-direction :down)
(setq *alien-linkage-table-predefined-entries* '(("alloc" nil)
                                                 ("alloc_list" nil)))


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


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *runtime-asm-routines* '(call-into-lisp do-pending-interrupt)))

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
    #-sb-thread
    ,@'(*binding-stack-pointer*
        ;; interrupt handling
        *pseudo-atomic-atomic*
        *pseudo-atomic-interrupted*)
    ,@*runtime-asm-routines*)
  #'equalp)

(defconstant-eqx +static-fdefns+ `#(,@common-static-fdefns) #'equalp)


;;;; Assembler parameters:

;;; The number of bits per element in the assemblers code vector.
;;;
(defparameter *assembly-unit-length* 8)

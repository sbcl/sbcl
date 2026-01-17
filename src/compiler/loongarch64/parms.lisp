;;;; This file contains some parameterizations of various VM
;;;; attributes for the LoongArch.  This file is separate from other stuff so
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

(defconstant sb-fasl:+backend-fasl-file-implementation+ :loongarch64)

(defconstant +backend-page-bytes+ 16384)

(defconstant gencgc-page-bytes +backend-page-bytes+)

(defconstant gencgc-alloc-granularity 0)

(defconstant n-word-bits  64)
(defconstant n-machine-word-bits  64)

(defconstant float-inexact-trap-bit (ash 1 0))
(defconstant float-underflow-trap-bit (ash 1 1))
(defconstant float-overflow-trap-bit (ash 1 2))
(defconstant float-divide-by-zero-trap-bit (ash 1 3))
(defconstant float-invalid-trap-bit (ash 1 4))

(defconstant float-round-to-nearest 0)
(defconstant float-round-to-zero 1)
(defconstant float-round-to-negative 3)
(defconstant float-round-to-positive 2)

(defconstant-eqx float-rounding-mode (byte 2 8) #'equalp)
(defconstant-eqx float-sticky-bits (byte 5 16) #'equalp)
(defconstant-eqx float-traps-byte (byte 5 0) #'equalp)
(defconstant-eqx float-exceptions-byte (byte 5 24) #'equalp)
(defconstant float-fast-bit 0)

(gc-space-setup #x04000000 :dynamic-space-start #x50000000)

(defconstant alien-linkage-table-entry-size 32)
(defconstant alien-linkage-table-growth-direction :down)
(setq *alien-linkage-table-predefined-entries*
      '(("alloc" nil)
        ("alloc_list" nil)))

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

(defconstant-eqx +static-symbols+
  `#(,@+common-static-symbols+
     #-sb-thread
     ,@'(*binding-stack-pointer*
         *pseudo-atomic-atomic*
         *pseudo-atomic-interrupted*)
     ,@*runtime-asm-routines*)
  #'equalp)
(defconstant-eqx +static-fdefns+ `#(,@common-static-fdefns) #'equalp)

(defparameter *assembly-unit-length* 8)

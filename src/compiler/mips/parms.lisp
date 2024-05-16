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

(defconstant sb-fasl:+backend-fasl-file-implementation+ :mips)

;; backend-page-size is the granularity at which we try to map/unmap.
;; linux says getpagesize() is 4k so any multiple thereof is fine.
(defconstant +backend-page-bytes+ 16384)
(defconstant gencgc-page-bytes +backend-page-bytes+)
(defconstant cards-per-page 8)
(defconstant gencgc-alloc-granularity 0)

;;;; Machine Architecture parameters:
(eval-when (:compile-toplevel :load-toplevel :execute)

;;; number of bits per word where a word holds one lisp descriptor
(defconstant n-word-bits 32)

;;; the natural width of a machine word (as seen in e.g. register width,
;;; address space)
(defconstant n-machine-word-bits 32)

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

#+linux
(progn
  (gc-space-setup #x04000000 :dynamic-space-start #x4f000000)

  (defconstant alien-linkage-table-entry-size 4)
  (defconstant alien-linkage-table-growth-direction :down)
  (setq *alien-linkage-table-predefined-entries* '(("call_into_c" nil)))

  ;; C stack grows downward from 0x80000000
  )

); eval-when


;;;; Other non-type constants.

(defenum (:start 8)
  halt-trap
  pending-interrupt-trap
  cerror-trap
  invalid-arg-count-trap
  allocation-trap
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
  `#(,@+common-static-symbols+
     *pseudo-atomic-atomic*
     *pseudo-atomic-interrupted*)
  #'equalp)

(defconstant-eqx +static-fdefns+
    `#(two-arg-<= two-arg->= two-arg-/= ,@common-static-fdefns)
  #'equalp)

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

(in-package "SB-VM")

; ok, so we're supposed to use our instruction scheduler, but we don't,
; because of "needs a little more work in the assembler"
(defconstant sb-assem:assem-scheduler-p nil)
(defconstant sb-assem:+inst-alignment-bytes+ 4)

(defconstant sb-fasl:+backend-fasl-file-implementation+ :ppc)
;; Granularity at which memory is mapped
(defconstant +backend-page-bytes+ 65536)

;;; The size in bytes of GENCGC pages, i.e. the granularity at which
;;; threads claim memory from the global heap.
(defconstant gencgc-page-bytes +backend-page-bytes+)
;;; Granularity at which writes to old generations are logged.
(defconstant cards-per-page 32)
;;; The minimum size of new allocation regions.  While it doesn't
;;; currently make a lot of sense to have a card size lower than
;;; the alloc granularity, it will, once we are smarter about finding
;;; the start of objects.
(defconstant gencgc-alloc-granularity 0)

;;; number of bits per word where a word holds one lisp descriptor
(defconstant n-word-bits 64)

;;; the natural width of a machine word (as seen in e.g. register width,
;;; address space)
(defconstant n-machine-word-bits 64)

(defconstant float-inexact-trap-bit (ash 1 0))
(defconstant float-divide-by-zero-trap-bit (ash 1 1))
(defconstant float-underflow-trap-bit (ash 1 2))
(defconstant float-overflow-trap-bit (ash 1 3))
(defconstant float-invalid-trap-bit (ash 1 4))

(defconstant float-round-to-nearest 0)
(defconstant float-round-to-zero 1)
(defconstant float-round-to-positive 2)
(defconstant float-round-to-negative 3)

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
(defconstant-eqx float-invalid-byte (byte 6 19) #'equalp)

(defconstant float-fast-bit 2)         ; Non-IEEE mode


;;;; Where to put the different spaces.

(gc-space-setup #x04000000
                              :read-only-space-size 0
                              :dynamic-space-start #x1000000000)

(defconstant alien-linkage-table-growth-direction :up)
(defconstant alien-linkage-table-entry-size #+little-endian 28 #+big-endian 24)


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


;;; These symbols are loaded into static space directly after NIL so
;;; that the system can compute their address by adding a constant
;;; amount to NIL.
;;;
;;; The fdefn objects for the static functions are loaded into static
;;; space directly after the static symbols.  That way, the raw-addr
;;; can be loaded directly out of them by indirecting relative to NIL.
;;;
(defconstant-eqx +static-symbols+
 `#(,@+common-static-symbols+)
  #'equalp)

(defconstant-eqx +static-fdefns+
  ;; I really don't understand the need for static-fdefns, since it has only to do
  ;; with a slightly differenet way of looking up the fdefn; however ltn decides
  ;; whether to warn or not about "recursion in known fun" based on whether the
  ;; fdefn is static, which isn't expressing exactly the right notion.
    `#(two-arg-<= two-arg->= two-arg-/= %negate ,@common-static-fdefns)
  #'equalp)

#+sb-xc-host (defparameter lisp-linkage-space-addr #x1500000000) ; arbitrary

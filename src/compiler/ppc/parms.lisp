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
;;; needs a little more work in the assembler, to realise that the
;;; delays requested here are not mandatory, so that the assembler
;;; shouldn't fill gaps with NOPs but with real instructions.  -- CSR,
;;; 2003-09-08
#+nil (defconstant sb-assem:+assem-max-locations+ 70)

(defconstant sb-fasl:+backend-fasl-file-implementation+ :ppc)
  ;; On Linux, the ABI specifies the page size to be 4k-64k, use the
  ;; maximum of that range. FIXME: it'd be great if somebody would
  ;; find out whether using exact multiples of the page size actually
  ;; matters in the few places where that's done, or whether we could
  ;; just use 4k everywhere.
(defconstant +backend-page-bytes+ #+linux 65536 #-linux 4096)

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
(defconstant n-word-bits 32)

;;; the natural width of a machine word (as seen in e.g. register width,
;;; address space)
(defconstant n-machine-word-bits 32)

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

(defconstant float-fast-bit 2)         ; Non-IEEE mode


;;;; Where to put the different spaces.

(gc-space-setup #x04000000
                     :read-only-space-size 0
                     :dynamic-space-start
                     #+linux   #x4f000000
                     #+netbsd  #x4f000000
                     #+openbsd #x4f000000
                     #+darwin  #x10000000)

(defconstant alien-linkage-table-growth-direction :up)
(defconstant alien-linkage-table-entry-size 16)


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
    `#(two-arg-<= two-arg->= two-arg-/= ,@common-static-fdefns)
  #'equalp)

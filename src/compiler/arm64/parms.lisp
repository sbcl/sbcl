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

(defconstant sb-fasl:+backend-fasl-file-implementation+ :arm64)

  ;; Can be in the range 4K-64K
(defconstant +backend-page-bytes+ 65536)

;;; The size in bytes of GENCGC pages. A page is essentially
;;; the granularity at which we claim memory for TLABs.
(defconstant gencgc-page-bytes +backend-page-bytes+)
;;; The divisor relative to page-bytes which computes the granularity
;;; at which writes to old generations are logged.
(defconstant cards-per-page
             #+gencgc 32
             #+mark-region-gc (/ +backend-page-bytes+ 128))

;;; The minimum size of new allocation regions.  While it doesn't
;;; currently make a lot of sense to have a card size lower than
;;; the alloc granularity, it will, once we are smarter about finding
;;; the start of objects.
(defconstant gencgc-alloc-granularity 0)
;;; The card size for immobile/low space.
;;; This must be a multiple of the OS page size.
(defconstant immobile-card-bytes +backend-page-bytes+)

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

(gc-space-setup #+(or linux openbsd netbsd freebsd)
                     #x2F0000000
                     #+darwin #x300000000
                     #-darwin :read-only-space-size #-darwin 0
                     :fixedobj-space-size #.(* 65536 1024)
                     :text-space-start #x0A00000000
                     :text-space-size #.(* 2 65536 1024)
                     :dynamic-space-start
                     #-darwin #x1000000000
                     #+darwin #x7003000000)

(defconstant alien-linkage-table-growth-direction :up)
(defconstant alien-linkage-table-entry-size 16)
  ;; text space:
  ;;   | ALIEN LINKAGE | CODE OBJECTS ...
  ;;   |<------------->|
#+(and sb-xc-host immobile-space)
(defparameter alien-linkage-space-start (- text-space-start alien-linkage-space-size))

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
     ;; interrupt handling
     ,@+common-static-symbols+)
  #'equalp)

(defconstant-eqx +static-fdefns+ `#(,@common-static-fdefns) #'equalp)


;;;; Assembler parameters:

;;; The number of bits per element in the assemblers code vector.
;;;
(defparameter *assembly-unit-length* 8)

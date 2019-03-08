;;;; This file contains some parameterizations of various VM
;;;; attributes for the x86. This file is separate from other stuff so
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
(defconstant sb-assem:+inst-alignment-bytes+ 1)

(defconstant +backend-fasl-file-implementation+ :x86-64)
(defconstant-eqx +fixup-kinds+ #(:absolute :relative :absolute64)
  #'equalp)

;;; KLUDGE: It would seem natural to set this by asking our C runtime
;;; code for it, but mostly we need it for GENESIS, which doesn't in
;;; general have our C runtime code running to ask, so instead we set
;;; it by hand. -- WHN 2001-04-15
;;;
;;; Actually any information that we can retrieve C-side would be
;;; useless in SBCL, since it's possible for otherwise binary
;;; compatible systems to return different values for getpagesize().
;;; -- JES, 2007-01-06
(defconstant +backend-page-bytes+ #+win32 65536 #-win32 32768)

;;; The size in bytes of GENCGC cards, i.e. the granularity at which
;;; writes to old generations are logged.  With mprotect-based write
;;; barriers, this must be a multiple of the OS page size.
(defconstant gencgc-card-bytes +backend-page-bytes+)
;;; The minimum size of new allocation regions.  While it doesn't
;;; currently make a lot of sense to have a card size lower than
;;; the alloc granularity, it will, once we are smarter about finding
;;; the start of objects.
(defconstant gencgc-alloc-granularity 0)
;;; The minimum size at which we release address ranges to the OS.
;;; This must be a multiple of the OS page size.
(defconstant gencgc-release-granularity +backend-page-bytes+)
;;; The card size for immobile/low space
(defconstant immobile-card-bytes 4096)

;;; ### Note: we simultaneously use ``word'' to mean a 32 bit quantity
;;; and a 16 bit quantity depending on context. This is because Intel
;;; insists on calling 16 bit things words and 32 bit things
;;; double-words (or dwords). Therefore, in the instruction definition
;;; and register specs, we use the Intel convention. But whenever we
;;; are talking about stuff the rest of the lisp system might be
;;; interested in, we use ``word'' to mean the size of a descriptor
;;; object, which is 64 bits.

;;;; machine architecture parameters

;;; the number of bits per word, where a word holds one lisp descriptor
(defconstant n-word-bits 64)

;;; the natural width of a machine word (as seen in e.g. register width,
;;; address space)
(defconstant n-machine-word-bits 64)

(defconstant float-sign-shift 31)

;;; comment from CMU CL:
;;;   These values were taken from the alpha code. The values for
;;;   bias and exponent min/max are not the same as shown in the 486 book.
;;;   They may be correct for how Python uses them.
(defconstant single-float-bias 126)    ; Intel says 127.
(defconstant-eqx single-float-exponent-byte    (byte 8 23) #'equalp)
(defconstant-eqx single-float-significand-byte (byte 23 0) #'equalp)
;;; comment from CMU CL:
;;;   The 486 book shows the exponent range -126 to +127. The Lisp
;;;   code that uses these values seems to want already biased numbers.
(defconstant single-float-normal-exponent-min 1)
(defconstant single-float-normal-exponent-max 254)
(defconstant single-float-hidden-bit (ash 1 23))

(defconstant double-float-bias 1022)
(defconstant-eqx double-float-exponent-byte    (byte 11 20) #'equalp)
(defconstant-eqx double-float-significand-byte (byte 20 0)  #'equalp)
(defconstant double-float-normal-exponent-min 1)
(defconstant double-float-normal-exponent-max #x7FE)
(defconstant double-float-hidden-bit (ash 1 20))

(defconstant single-float-digits
  (+ (byte-size single-float-significand-byte) 1))

(defconstant double-float-digits
  (+ (byte-size double-float-significand-byte) 32 1))

;;; from AMD64 Architecture manual
(defconstant float-invalid-trap-bit       (ash 1 0))
(defconstant float-denormal-trap-bit       (ash 1 1))
(defconstant float-divide-by-zero-trap-bit (ash 1 2))
(defconstant float-overflow-trap-bit       (ash 1 3))
(defconstant float-underflow-trap-bit      (ash 1 4))
(defconstant float-inexact-trap-bit       (ash 1 5))

(defconstant float-round-to-nearest  0)
(defconstant float-round-to-negative 1)
(defconstant float-round-to-positive 2)
(defconstant float-round-to-zero     3)

(defconstant-eqx float-rounding-mode     (byte 2 13) #'equalp)
(defconstant-eqx float-sticky-bits       (byte 6  0) #'equalp)
(defconstant-eqx float-traps-byte        (byte 6  7) #'equalp)
(defconstant-eqx float-exceptions-byte   (byte 6  0) #'equalp)
(defconstant float-fast-bit 0) ; no fast mode on x86-64

;;;; description of the target address space

;;; where to put the different spaces.

;;; Currently the read-only and static spaces must be located in low
;;; memory (certainly under the 4GB limit, very probably under 2GB
;;; limit). This is due to the inability of using immediate values of
;;; more than 32 bits (31 bits if you take sign extension into
;;; account) in any other instructions except MOV. Removing this limit
;;; would be possible, but probably not worth the time and code bloat
;;; it would cause. -- JES, 2005-12-11

#+linux
(!gencgc-space-setup #x50000000
                     :fixedobj-space-size #.(* 30 1024 1024)
                     :varyobj-space-size #.(* 130 1024 1024)
                     :dynamic-space-start #x1000000000)

;;; The default dynamic space size is lower on OpenBSD to allow SBCL to
;;; run under the default 512M data size limit.

#-linux
(!gencgc-space-setup #x20000000
                     :dynamic-space-start #x1000000000
                     #+openbsd :dynamic-space-size #+openbsd #x1bcf0000)

(defconstant linkage-table-entry-size 16)


(defenum (:start 8)
  halt-trap
  pending-interrupt-trap
  cerror-trap
  breakpoint-trap
  fun-end-breakpoint-trap
  single-step-around-trap
  single-step-before-trap
  invalid-arg-count-trap
  memory-fault-emulation-trap
  #+sb-safepoint global-safepoint-trap
  #+sb-safepoint csp-safepoint-trap
  error-trap)

;;;; static symbols

;;; These symbols are loaded into static space directly after NIL so
;;; that the system can compute their address by adding a constant
;;; amount to NIL.
;;;
;;; The fdefn objects for the static functions are loaded into static
;;; space directly after the static symbols. That way, the raw-addr
;;; can be loaded directly out of them by indirecting relative to NIL.
;;;
;;; we could profitably keep these in registers on x86-64 now we have
;;; r8-r15 as well
;;;     Note these spaces grow from low to high addresses.
(defvar *binding-stack-pointer*)

(defconstant-eqx +static-symbols+
 `#(,@+common-static-symbols+
    #+(and immobile-space (not sb-thread)) function-layout
    #-sb-thread *alien-stack-pointer*    ; a thread slot if #+sb-thread
     ;; interrupt handling
    #-sb-thread *pseudo-atomic-bits*     ; ditto
    #-sb-thread *binding-stack-pointer* ; ditto
    *cpuid-fn1-ecx*)
  #'equalp)

;;; FIXME: with #+immobile-space, this should be the empty list,
;;; because *all* fdefns are permanently placed.
(defconstant-eqx +static-fdefns+
  #(length
    two-arg-+
    two-arg--
    two-arg-*
    two-arg-/
    two-arg-<
    two-arg->
    two-arg-=
    eql
    %negate
    two-arg-and
    two-arg-ior
    two-arg-xor
    two-arg-gcd
    two-arg-lcm
    %coerce-callable-to-fun)
  #'equalp)

#+sb-simd-pack
(defglobal *simd-pack-element-types* '(integer single-float double-float))

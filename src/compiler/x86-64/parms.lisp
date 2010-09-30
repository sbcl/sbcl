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

(in-package "SB!VM")

;;; ### Note: we simultaneously use ``word'' to mean a 32 bit quantity
;;; and a 16 bit quantity depending on context. This is because Intel
;;; insists on calling 16 bit things words and 32 bit things
;;; double-words (or dwords). Therefore, in the instruction definition
;;; and register specs, we use the Intel convention. But whenever we
;;; are talking about stuff the rest of the lisp system might be
;;; interested in, we use ``word'' to mean the size of a descriptor
;;; object, which is 32 bits.

;;;; machine architecture parameters

;;; the number of bits per word, where a word holds one lisp descriptor
(def!constant n-word-bits 64)

;;; the natural width of a machine word (as seen in e.g. register width,
;;; address space)
(def!constant n-machine-word-bits 64)

;;; the number of bits per byte, where a byte is the smallest
;;; addressable object
(def!constant n-byte-bits 8)

;;; The minimum immediate offset in a memory-referencing instruction.
(def!constant minimum-immediate-offset (- (expt 2 31)))

;;; The maximum immediate offset in a memory-referencing instruction.
(def!constant maximum-immediate-offset (1- (expt 2 31)))

(def!constant float-sign-shift 31)

;;; comment from CMU CL:
;;;   These values were taken from the alpha code. The values for
;;;   bias and exponent min/max are not the same as shown in the 486 book.
;;;   They may be correct for how Python uses them.
(def!constant single-float-bias 126)    ; Intel says 127.
(defconstant-eqx single-float-exponent-byte    (byte 8 23) #'equalp)
(defconstant-eqx single-float-significand-byte (byte 23 0) #'equalp)
;;; comment from CMU CL:
;;;   The 486 book shows the exponent range -126 to +127. The Lisp
;;;   code that uses these values seems to want already biased numbers.
(def!constant single-float-normal-exponent-min 1)
(def!constant single-float-normal-exponent-max 254)
(def!constant single-float-hidden-bit (ash 1 23))
(def!constant single-float-trapping-nan-bit (ash 1 22))

(def!constant double-float-bias 1022)
(defconstant-eqx double-float-exponent-byte    (byte 11 20) #'equalp)
(defconstant-eqx double-float-significand-byte (byte 20 0)  #'equalp)
(def!constant double-float-normal-exponent-min 1)
(def!constant double-float-normal-exponent-max #x7FE)
(def!constant double-float-hidden-bit (ash 1 20))
(def!constant double-float-trapping-nan-bit (ash 1 19))

(def!constant single-float-digits
  (+ (byte-size single-float-significand-byte) 1))

(def!constant double-float-digits
  (+ (byte-size double-float-significand-byte) 32 1))

;;; from AMD64 Architecture manual
(def!constant float-invalid-trap-bit       (ash 1 0))
(def!constant float-denormal-trap-bit       (ash 1 1))
(def!constant float-divide-by-zero-trap-bit (ash 1 2))
(def!constant float-overflow-trap-bit       (ash 1 3))
(def!constant float-underflow-trap-bit      (ash 1 4))
(def!constant float-inexact-trap-bit       (ash 1 5))

(def!constant float-round-to-nearest  0)
(def!constant float-round-to-negative 1)
(def!constant float-round-to-positive 2)
(def!constant float-round-to-zero     3)

(defconstant-eqx float-rounding-mode     (byte 2 13) #'equalp)
(defconstant-eqx float-sticky-bits       (byte 6  0) #'equalp)
(defconstant-eqx float-traps-byte        (byte 6  7) #'equalp)
(defconstant-eqx float-exceptions-byte   (byte 6  0) #'equalp)
(def!constant float-fast-bit 0) ; no fast mode on x86-64

;;;; description of the target address space

;;; where to put the different spaces.

;;; Currently the read-only and static spaces must be located in low
;;; memory (certainly under the 4GB limit, very probably under 2GB
;;; limit). This is due to the inability of using immediate values of
;;; more than 32 bits (31 bits if you take sign extension into
;;; account) in any other instructions except MOV. Removing this limit
;;; would be possible, but probably not worth the time and code bloat
;;; it would cause. -- JES, 2005-12-11

(progn
  (def!constant read-only-space-start     #x20000000)
  (def!constant read-only-space-end       #x200ff000)

  (def!constant static-space-start        #x20100000)
  (def!constant static-space-end          #x201ff000)

  (def!constant dynamic-space-start       #x1000000000)
  #!-openbsd
  (def!constant dynamic-space-end         (!configure-dynamic-space-end #x11ffff0000))
  #!+openbsd
  ;; This is lower on OpenBSD to allow SBCL to run under the default
  ;; 512M data size limit.
  (def!constant dynamic-space-end         (!configure-dynamic-space-end #x101bcf0000))

  (def!constant linkage-table-space-start #x20200000)
  (def!constant linkage-table-space-end   #x202ff000)

  (def!constant linkage-table-entry-size 16))


;;;; other miscellaneous constants

(defenum (:start 8)
  halt-trap
  pending-interrupt-trap
  error-trap
  cerror-trap
  breakpoint-trap
  fun-end-breakpoint-trap
  single-step-around-trap
  single-step-before-trap)

(defenum (:start 24)
  object-not-list-trap
  object-not-instance-trap)

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
(defvar *allocation-pointer*)
(defvar *binding-stack-pointer*)

(defparameter *static-symbols*
  (append
   *common-static-symbols*
   *c-callable-static-symbols*
   '(*alien-stack*

     ;; interrupt handling
     *pseudo-atomic-bits*

     *allocation-pointer*
     *binding-stack-pointer*

     ;; the floating point constants
     *fp-constant-0d0*
     *fp-constant-1d0*
     *fp-constant-0f0*
     *fp-constant-1f0*

     ;; For GC-AND-SAVE
     *restart-lisp-function*

     ;; For the UNWIND-TO-FRAME-AND-CALL VOP
     *unwind-to-frame-function*

     ;; Needed for callbacks to work across saving cores. see
     ;; ALIEN-CALLBACK-ASSEMBLER-WRAPPER in c-call.lisp for gory
     ;; details.
     sb!alien::*enter-alien-callback*

     ;; The ..SLOT-UNBOUND.. symbol is static in order to optimise the
     ;; common slot unbound check.
     ;;
     ;; FIXME: In SBCL, the CLOS code has become sufficiently tightly
     ;; integrated into the system that it'd probably make sense to
     ;; use the ordinary unbound marker for this.
     ;;
     ;; FIXME II: if it doesn't make sense, why is this X86-ish only?
     sb!pcl::..slot-unbound..)))

(defparameter *static-funs*
  '(length
    sb!kernel:two-arg-+
    sb!kernel:two-arg--
    sb!kernel:two-arg-*
    sb!kernel:two-arg-/
    sb!kernel:two-arg-<
    sb!kernel:two-arg->
    sb!kernel:two-arg-=
    eql
    sb!kernel:%negate
    sb!kernel:two-arg-and
    sb!kernel:two-arg-ior
    sb!kernel:two-arg-xor
    sb!kernel:two-arg-gcd
    sb!kernel:two-arg-lcm))

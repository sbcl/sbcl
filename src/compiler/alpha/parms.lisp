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

(defconstant +backend-fasl-file-implementation+ :alpha)

(defconstant +backend-page-bytes+ 8192)

(eval-when  (:compile-toplevel :load-toplevel :execute)

;;; number of bits per word where a word holds one lisp descriptor
(defconstant n-word-bits 32)

;;; the natural width of a machine word (as seen in e.g. register width,
;;; address space)
(defconstant n-machine-word-bits 64)

(defconstant float-sign-shift 31)

(defconstant single-float-bias 126)
(defconstant-eqx single-float-exponent-byte (byte 8 23) #'equalp)
(defconstant-eqx single-float-significand-byte (byte 23 0) #'equalp)
(defconstant single-float-normal-exponent-min 1)
(defconstant single-float-normal-exponent-max 254)
(defconstant single-float-hidden-bit (ash 1 23))

(defconstant double-float-bias 1022)
(defconstant-eqx double-float-exponent-byte (byte 11 20)   #'equalp)
(defconstant-eqx double-float-significand-byte (byte 20 0) #'equalp)
(defconstant double-float-normal-exponent-min 1)
(defconstant double-float-normal-exponent-max #x7FE)
(defconstant double-float-hidden-bit (ash 1 20))

(defconstant single-float-digits
  (+ (byte-size single-float-significand-byte) 1))

(defconstant double-float-digits
  (+ (byte-size double-float-significand-byte) n-word-bits 1))

;;; These values are originally from the DEC Assembly Language
;;; Programmers guide.  Where possible we read/write the software
;;; fp_control word, which apparently is necessary for the OS FPU
;;; completion (OS handler which fixes up non-IEEE answers that the
;;; hardware occasionally gives us) to work properly.  The rounding
;;; mode, however, can't be set that way, so we have to deal with that
;;; directly.  (FIXME: we actually don't suport setting the rounding mode
;;; at the moment anyway)

;;; Short guide to floating point trap terminology: an "exception" is
;;; cheap and can happen at almost any time.  An exception will only
;;; generate a trap if that trap is enabled, otherwise a default value
;;; will be substituted.  A "trap" will end up somewhere in the
;;; kernel, which may play by its own rules,  (on Alpha it allegedly
;;; actually fixes up some non-IEEE compliant results to get the
;;; _right_ answer) but if something is really wrong will eventually
;;; signal SIGFPE and let us sort it out.

;;; Old comment follows: The active bits are actually in (byte 12 52)
;;; of the fpcr. (byte 6 52) contain the exception flags. Bit 63 is the
;;; bitwise logor of all exceptions.  The enable and exception bytes
;;; are in a software control word manipulated via OS functions and the
;;; bits in the SCP match those defs. This mapping follows
;;; <machine/fpu.h>

;;; trap enables are set in software (fp_control)
(defconstant float-inexact-trap-bit        (ash 1 4)) ; rw
(defconstant float-underflow-trap-bit      (ash 1 3)) ; rw
(defconstant float-overflow-trap-bit       (ash 1 2)) ; ro
(defconstant float-divide-by-zero-trap-bit (ash 1 1)) ; ro
(defconstant float-invalid-trap-bit        (ash 1 0)) ; ro
(defconstant-eqx float-traps-byte          (byte 6  1) #'equalp)

;;; exceptions are also read/written in software (by syscalls, no less).
;;; This is kind of dumb, but has to be done
(defconstant-eqx float-sticky-bits     (byte 6 17) #'equalp)    ; fp_control

;;; (We don't actually _have_ "current exceptions" on Alpha; the
;;; hardware only ever sets bits.  So, set this the same as accrued
;;; exceptions)
(defconstant-eqx float-exceptions-byte (byte 6 17)  #'equalp)

;;; Rounding modes can only be set by frobbing the hardware fpcr directly
(defconstant float-round-to-zero     0)
(defconstant float-round-to-negative 1)
(defconstant float-round-to-nearest  2)
(defconstant float-round-to-positive 3)
(defconstant-eqx float-rounding-mode   (byte 2 58) #'equalp)

;;; Miscellaneous stuff - I think it's far to say that you deserve
;;; what you get if you ask for fast mode.
(defconstant float-fast-bit 0)

); eval-when



;;;; Description of the target address space.

;;; Where to put the different spaces.
;;;

(defconstant linkage-table-space-start #x1000)
(defconstant linkage-table-space-end   linkage-table-space-start) ; 0 size

#+linux
(progn
  (defconstant read-only-space-start #x20000000)
  (defconstant read-only-space-end   #x24000000))

(defconstant static-space-start    #x28000000)
(defconstant static-space-end      #x2c000000)

(defparameter dynamic-0-space-start  #x30000000)
(defparameter dynamic-0-space-end    #x3fff0000)

;;; FIXME nothing refers to either of these in alpha or x86 cmucl
;;; backend, so they could probably be removed.

;; The space-register holding the lisp heap.
(defconstant lisp-heap-space 4)

;; The space-register holding the C text segment.
(defconstant c-text-space 4)


;;;; other miscellaneous constants

(defenum (:start 8)
  halt-trap
  pending-interrupt-trap
  cerror-trap
  breakpoint-trap
  fun-end-breakpoint-trap
  single-step-breakpoint-trap
  ;; Stepper actually not implemented on Alpha, but these constants
  ;; are still needed to avoid undefined variable warnings during sbcl
  ;; build.
  single-step-around-trap
  single-step-before-trap
  error-trap)

;;;; static symbols

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
  #(length
    two-arg-+
    two-arg--
    two-arg-*
    two-arg-/
    two-arg-<
    two-arg->
    two-arg-=
    ;; FIXME: Is this
    ;;     probably need the following as they are defined in
    ;;     arith.lisp: two-arg-<= two-arg->= two-arg-/=
    ;; a comment from old CMU CL or old old CMU CL or
    ;; the SBCL alpha port or what? Do we need to worry about it,
    ;; or can we delete it?
    two-arg-/=
    eql
    %negate
    two-arg-and
    two-arg-ior
    two-arg-xor
    two-arg-eqv
    two-arg-gcd
    two-arg-lcm)
  #'equalp)

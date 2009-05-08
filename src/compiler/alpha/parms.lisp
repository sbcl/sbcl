;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

(eval-when  (:compile-toplevel :load-toplevel :execute)

;;; number of bits per word where a word holds one lisp descriptor
(def!constant n-word-bits 32)

;;; the natural width of a machine word (as seen in e.g. register width,
;;; address space)
(def!constant n-machine-word-bits 64)

;;; number of bits per byte where a byte is the smallest addressable
;;; object
(def!constant n-byte-bits 8)

(def!constant float-sign-shift 31)

(def!constant single-float-bias 126)
(defconstant-eqx single-float-exponent-byte (byte 8 23) #'equalp)
(defconstant-eqx single-float-significand-byte (byte 23 0) #'equalp)
(def!constant single-float-normal-exponent-min 1)
(def!constant single-float-normal-exponent-max 254)
(def!constant single-float-hidden-bit (ash 1 23))
(def!constant single-float-trapping-nan-bit (ash 1 22))

(def!constant double-float-bias 1022)
(defconstant-eqx double-float-exponent-byte (byte 11 20)   #'equalp)
(defconstant-eqx double-float-significand-byte (byte 20 0) #'equalp)
(def!constant double-float-normal-exponent-min 1)
(def!constant double-float-normal-exponent-max #x7FE)
(def!constant double-float-hidden-bit (ash 1 20))
(def!constant double-float-trapping-nan-bit (ash 1 19))

(def!constant single-float-digits
  (+ (byte-size single-float-significand-byte) 1))

(def!constant double-float-digits
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
(def!constant float-inexact-trap-bit        (ash 1 4)) ; rw
(def!constant float-underflow-trap-bit      (ash 1 3)) ; rw
(def!constant float-overflow-trap-bit       (ash 1 2)) ; ro
(def!constant float-divide-by-zero-trap-bit (ash 1 1)) ; ro
(def!constant float-invalid-trap-bit        (ash 1 0)) ; ro
(defconstant-eqx float-traps-byte          (byte 6  1) #'equalp)

;;; exceptions are also read/written in software (by syscalls, no less).
;;; This is kind of dumb, but has to be done
(defconstant-eqx float-sticky-bits     (byte 6 17) #'equalp)    ; fp_control

;;; (We don't actually _have_ "current exceptions" on Alpha; the
;;; hardware only ever sets bits.  So, set this the same as accrued
;;; exceptions)
(defconstant-eqx float-exceptions-byte (byte 6 17)  #'equalp)

;;; Rounding modes can only be set by frobbing the hardware fpcr directly
(def!constant float-round-to-zero     0)
(def!constant float-round-to-negative 1)
(def!constant float-round-to-nearest  2)
(def!constant float-round-to-positive 3)
(defconstant-eqx float-rounding-mode   (byte 2 58) #'equalp)

;;; Miscellaneous stuff - I think it's far to say that you deserve
;;; what you get if you ask for fast mode.
(def!constant float-fast-bit 0)

); eval-when



;;;; Description of the target address space.

;;; Where to put the different spaces.
;;;

#!+linux
(progn
  (def!constant read-only-space-start #x20000000)
  (def!constant read-only-space-end   #x24000000))

#!+osf1
(progn
  (defconstant read-only-space-start #x10000000)
  (defconstant read-only-space-end   #x25000000))


(def!constant static-space-start    #x28000000)
(def!constant static-space-end      #x2c000000)

(def!constant dynamic-0-space-start   #x30000000)
(def!constant dynamic-0-space-end     #x3fff0000)

(def!constant dynamic-1-space-start   #x40000000)
(def!constant dynamic-1-space-end     #x4fff0000)

;;; FIXME nothing refers to either of these in alpha or x86 cmucl
;;; backend, so they could probably be removed.

;; The space-register holding the lisp heap.
(def!constant lisp-heap-space 4)

;; The space-register holding the C text segment.
(def!constant c-text-space 4)

;;; the X86 port defines *nil-value* as (+ *target-static-space-start* #xB)
;;; here, but it seems to be the only port that needs to know the
;;; location of NIL from lisp.

;;;; other miscellaneous constants

(defenum (:start 8)
  halt-trap
  pending-interrupt-trap
  error-trap
  cerror-trap
  breakpoint-trap
  fun-end-breakpoint-trap
  single-step-breakpoint-trap
  ;; Stepper actually not implemented on Alpha, but these constants
  ;; are still needed to avoid undefined variable warnings during sbcl
  ;; build.
  single-step-around-trap
  single-step-before-trap)

;;;; static symbols

;;; These symbols are loaded into static space directly after NIL so
;;; that the system can compute their address by adding a constant
;;; amount to NIL.
;;;
;;; The fdefn objects for the static functions are loaded into static
;;; space directly after the static symbols.  That way, the raw-addr
;;; can be loaded directly out of them by indirecting relative to NIL.
;;;
(defparameter *static-symbols*
  (append
   *common-static-symbols*
   *c-callable-static-symbols*
   '()))

(defparameter *static-funs*
  '(length
    sb!kernel:two-arg-+
    sb!kernel:two-arg--
    sb!kernel:two-arg-*
    sb!kernel:two-arg-/
    sb!kernel:two-arg-<
    sb!kernel:two-arg->
    sb!kernel:two-arg-=
    ;; FIXME: Is this
    ;;     probably need the following as they are defined in
    ;;     arith.lisp: two-arg-<= two-arg->= two-arg-/=
    ;; a comment from old CMU CL or old old CMU CL or
    ;; the SBCL alpha port or what? Do we need to worry about it,
    ;; or can we delete it?
    sb!kernel:two-arg-/=
    eql
    sb!kernel:%negate
    sb!kernel:two-arg-and
    sb!kernel:two-arg-ior
    sb!kernel:two-arg-xor
    sb!kernel:two-arg-eqv
    sb!kernel:two-arg-gcd
    sb!kernel:two-arg-lcm))

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

(eval-when (:compile-toplevel :load-toplevel :execute)

(defconstant word-bits 32
  #!+sb-doc
  "Number of bits per word where a word holds one lisp descriptor.")

(defconstant byte-bits 8
  #!+sb-doc
  "Number of bits per byte where a byte is the smallest addressable object.")

(defconstant word-shift (1- (integer-length (/ word-bits byte-bits)))
  #!+sb-doc
  "Number of bits to shift between word addresses and byte addresses.")

(defconstant word-bytes (/ word-bits byte-bits)
  #!+sb-doc
  "Number of bytes in a word.")

) ; EVAL-WHEN

(eval-when (:compile-toplevel :load-toplevel :execute)
(defconstant float-sign-shift 31)

;; These values were taken from the alpha code. The values for
;; bias and exponent min/max are not the same as shown in the 486 book.
;; They may be correct for how Python uses them.
(defconstant single-float-bias 126)	; Intel says 127
(defconstant single-float-exponent-byte (byte 8 23))
(defconstant single-float-significand-byte (byte 23 0))
;; The 486 book shows the exponent range -126 to +127. The Lisp
;; code that uses these values seems to want already biased numbers.
(defconstant single-float-normal-exponent-min 1)
(defconstant single-float-normal-exponent-max 254)
(defconstant single-float-hidden-bit (ash 1 23))
(defconstant single-float-trapping-nan-bit (ash 1 22))

(defconstant double-float-bias 1022)
(defconstant double-float-exponent-byte (byte 11 20))
(defconstant double-float-significand-byte (byte 20 0))
(defconstant double-float-normal-exponent-min 1)
(defconstant double-float-normal-exponent-max #x7FE)
(defconstant double-float-hidden-bit (ash 1 20))
(defconstant double-float-trapping-nan-bit (ash 1 19))

(defconstant long-float-bias 16382)
(defconstant long-float-exponent-byte (byte 15 0))
(defconstant long-float-significand-byte (byte 31 0))
(defconstant long-float-normal-exponent-min 1)
(defconstant long-float-normal-exponent-max #x7FFE)
(defconstant long-float-hidden-bit (ash 1 31))		; Actually not hidden
(defconstant long-float-trapping-nan-bit (ash 1 30))

(defconstant single-float-digits
  (+ (byte-size single-float-significand-byte) 1))

(defconstant double-float-digits
  (+ (byte-size double-float-significand-byte) word-bits 1))

(defconstant long-float-digits
  (+ (byte-size long-float-significand-byte) word-bits 1))

;;; pfw -- from i486 microprocessor programmers reference manual
(defconstant float-invalid-trap-bit	(ash 1 0))
(defconstant float-denormal-trap-bit       (ash 1 1))
(defconstant float-divide-by-zero-trap-bit (ash 1 2))
(defconstant float-overflow-trap-bit       (ash 1 3))
(defconstant float-underflow-trap-bit      (ash 1 4))
(defconstant float-inexact-trap-bit	(ash 1 5))

(defconstant float-round-to-nearest  0)
(defconstant float-round-to-negative 1)
(defconstant float-round-to-positive 2)
(defconstant float-round-to-zero     3)

(defconstant float-rounding-mode   (byte 2 10))
(defconstant float-sticky-bits     (byte 6 16))
(defconstant float-traps-byte      (byte 6  0))
(defconstant float-exceptions-byte (byte 6 16))
(defconstant float-precision-control (byte 2 8))
(defconstant float-fast-bit 0) ; No fast mode on x86

); EVAL-WHEN

;;;; description of the target address space

;;; where to put the different spaces
;;;
;;; FIXME: Couldn't/shouldn't these be DEFCONSTANT instead of DEFPARAMETER?
;;;
;;; FIXME: Since SBCL has a different way of distinguishing between target
;;; and host than the old CMU CL code used, the "TARGET-" prefix is
;;; redundant. Perhaps each *TARGET-FOO* should become *FOO*, probably
;;; at the same time that we unscrew the kludgy way that constants are
;;; duplicated between this file and runtime/x86-validate.h.
;;;
;;; Note: Mostly these values are black magic, inherited from CMU CL
;;; without any documentation. However, there were a few explanatory
;;; comments in the CMU CL sources:
;;;   * On Linux,
;;;     ** The space 0x08000000-0x10000000 is "C program and memory allocation".
;;;     ** The space 0x40000000-0x48000000 is reserved for shared libs.
;;;     ** The space >0xE0000000 is "C stack - Alien stack".
;;;   * On FreeBSD,
;;;     ** The space 0x0E000000-0x10000000 is "Foreign segment".
;;;     ** The space 0x20000000-0x30000000 is reserved for shared libs.
;;; And there have been a few changes since the fork:
;;;   * The FreeBSD STATIC-SPACE-START value was bumped up from
;;;     #x28000000 to #x30000000 when FreeBSD ld.so dynamic linking
;;;     support was added for FreeBSD ca. 20000910. This was to keep from
;;;     stomping on an address range that the dynamic libraries want to use. 
;;;     (They want to use this address range even if we try to reserve it
;;;     with a call to validate() as the first operation in main().)

#!+linux
(progn

  (defconstant read-only-space-start #x01000000)
  (defconstant read-only-space-end   #x037ff000)

  (defconstant static-space-start    #x05000000)
  (defconstant static-space-end      #x07fff000)

  (defconstant dynamic-space-start   #x09000000)
  (defconstant dynamic-space-end     #x29000000)

  (defconstant control-stack-start   #x50000000)
  (defconstant control-stack-end     #x57fff000)

  (defconstant binding-stack-start   #x60000000)
  (defconstant binding-stack-end     #x67fff000))

#!+bsd
(progn

  (defconstant read-only-space-start #x10000000)
  (defconstant read-only-space-end   #x1ffff000)

  (defconstant static-space-start
    #!+freebsd #x30000000
    #!+openbsd #x28000000)
  (defconstant static-space-end      #x37fff000)

  (defconstant binding-stack-start   #x38000000)
  (defconstant binding-stack-end     #x3ffff000)

  (defconstant control-stack-start   #x40000000)
  (defconstant control-stack-end     #x47fff000)

  (defconstant dynamic-space-start   #x48000000)
  (defconstant dynamic-space-end     #x88000000))

;;; Given that NIL is the first thing allocated in static space, we
;;; know its value at compile time:
(defconstant nil-value (+ static-space-start #xb))

;;;; other miscellaneous constants

(defenum (:suffix -trap :start 8)
  halt
  pending-interrupt
  error
  cerror
  breakpoint
  function-end-breakpoint
  single-step-breakpoint)
;;; FIXME: It'd be nice to replace all the DEFENUMs with something like
;;;   (WITH-DEF-ENUM (:START 8)
;;;     (DEF-ENUM HALT-TRAP)
;;;     (DEF-ENUM PENDING-INTERRUPT-TRAP)
;;;     ..)
;;; for the benefit of anyone doing a lexical search for definitions
;;; of these symbols.

(defenum (:prefix object-not- :suffix -trap :start 16)
  list
  instance)

(defenum (:prefix trace-table-)
  normal
  call-site
  function-prologue
  function-epilogue)

;;;; static symbols

;;; These symbols are loaded into static space directly after NIL so
;;; that the system can compute their address by adding a constant
;;; amount to NIL.
;;;
;;; The fdefn objects for the static functions are loaded into static
;;; space directly after the static symbols. That way, the raw-addr
;;; can be loaded directly out of them by indirecting relative to NIL.
;;;
;;; pfw X86 doesn't have enough registers to keep these things there.
;;;     Note these spaces grow from low to high addresses.
(defvar *allocation-pointer*)
(defvar *binding-stack-pointer*)
(defvar *x86-cgc-active-p*) ; FIXME: Document this.
(defvar *static-blue-bag* nil)

;;; FIXME: *!INITIAL-FDEFN-OBJECTS* and !COLD-INIT probably don't need
;;; to be in the static symbols table any more. Also, if
;;; *INTERNAL-GC-TRIGGER* really is not used, we can punt it.
(defparameter *static-symbols*
  '(t

    ;; The C startup code must fill these in.
    *posix-argv*
    sb!impl::*!initial-fdefn-objects*

    ;; functions that the C code needs to call
    sb!impl::!cold-init
    sb!impl::maybe-gc
    sb!kernel::internal-error
    sb!di::handle-breakpoint
    sb!impl::fdefinition-object

    ;; free pointers
    ;; 
    ;; Note that these are FIXNUM word counts, not (as one might
    ;; expect) byte counts or SAPs. The reason seems to be that by
    ;; representing them this way, we can avoid consing bignums. 
    ;; -- WHN 2000-10-02
    *read-only-space-free-pointer*
    *static-space-free-pointer*
    *initial-dynamic-space-free-pointer*

    ;; things needed for non-local exit
    sb!impl::*current-catch-block*
    sb!impl::*current-unwind-protect-block*
    sb!c::*eval-stack-top*
    sb!vm::*alien-stack*

    ;; interrupt handling
    sb!impl::*pseudo-atomic-atomic*
    sb!impl::*pseudo-atomic-interrupted*
    sb!unix::*interrupts-enabled*
    sb!unix::*interrupt-pending*
    sb!impl::*free-interrupt-context-index*

    sb!vm::*allocation-pointer*
    sb!vm::*binding-stack-pointer*
    sb!vm::*internal-gc-trigger*   ; Not used.

    ;; the floating point constants
    sb!vm::*fp-constant-0d0*
    sb!vm::*fp-constant-1d0*
    sb!vm::*fp-constant-0s0*
    sb!vm::*fp-constant-1s0*
    ;; The following are all long-floats.
    sb!vm::*fp-constant-0l0*
    sb!vm::*fp-constant-1l0*
    sb!vm::*fp-constant-pi*
    sb!vm::*fp-constant-l2t*
    sb!vm::*fp-constant-l2e*
    sb!vm::*fp-constant-lg2*
    sb!vm::*fp-constant-ln2*

    ;; used by gencgc
    sb!vm::*scavenge-read-only-space*

    ;; The ..SLOT-UNBOUND.. symbol is static in order to optimise the
    ;; common slot unbound check.
    sb!pcl::..slot-unbound..

    ;; spare symbols
    sb!vm::spare-10
    sb!vm::spare-9
    sb!vm::spare-8
    sb!vm::spare-7
    sb!vm::spare-6
    sb!vm::spare-5
    sb!vm::spare-4
    sb!vm::spare-3
    sb!vm::spare-2
    sb!vm::spare-1

    ;; used by cgc
    sb!vm::*x86-cgc-active-p*
    sb!vm::*static-blue-bag*		; must be last or change C code
    ))

(defparameter *static-functions*
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

;;;; stuff added by jrd

;;; FIXME: Is this used? Delete it or document it.
;;; cf the sparc PARMS.LISP
(defparameter *assembly-unit-length* 8)

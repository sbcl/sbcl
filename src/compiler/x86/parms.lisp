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
(def!constant n-word-bits 32)

;;; the natural width of a machine word (as seen in e.g. register width,
;;; address space)
(def!constant n-machine-word-bits 32)

;;; the number of bits per byte, where a byte is the smallest
;;; addressable object
(def!constant n-byte-bits 8)

(def!constant float-sign-shift 31)

;;; comment from CMU CL:
;;;   These values were taken from the alpha code. The values for
;;;   bias and exponent min/max are not the same as shown in the 486 book.
;;;   They may be correct for how Python uses them.
(def!constant single-float-bias 126)	; Intel says 127.
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

(def!constant long-float-bias 16382)
(defconstant-eqx long-float-exponent-byte    (byte 15 0) #'equalp)
(defconstant-eqx long-float-significand-byte (byte 31 0) #'equalp)
(def!constant long-float-normal-exponent-min 1)
(def!constant long-float-normal-exponent-max #x7FFE)
(def!constant long-float-hidden-bit (ash 1 31))		; actually not hidden
(def!constant long-float-trapping-nan-bit (ash 1 30))

(def!constant single-float-digits
  (+ (byte-size single-float-significand-byte) 1))

(def!constant double-float-digits
  (+ (byte-size double-float-significand-byte) n-word-bits 1))

(def!constant long-float-digits
  (+ (byte-size long-float-significand-byte) n-word-bits 1))

;;; pfw -- from i486 microprocessor programmer's reference manual
(def!constant float-invalid-trap-bit	   (ash 1 0))
(def!constant float-denormal-trap-bit       (ash 1 1))
(def!constant float-divide-by-zero-trap-bit (ash 1 2))
(def!constant float-overflow-trap-bit       (ash 1 3))
(def!constant float-underflow-trap-bit      (ash 1 4))
(def!constant float-inexact-trap-bit	   (ash 1 5))

(def!constant float-round-to-nearest  0)
(def!constant float-round-to-negative 1)
(def!constant float-round-to-positive 2)
(def!constant float-round-to-zero     3)

(def!constant float-precision-24-bit 0)
(def!constant float-precision-53-bit 2)
(def!constant float-precision-64-bit 3)

(defconstant-eqx float-rounding-mode     (byte 2 10) #'equalp)
(defconstant-eqx float-sticky-bits       (byte 6 16) #'equalp)
(defconstant-eqx float-traps-byte        (byte 6  0) #'equalp)
(defconstant-eqx float-exceptions-byte   (byte 6 16) #'equalp)
(defconstant-eqx float-precision-control (byte 2  8) #'equalp)
(def!constant float-fast-bit 0) ; no fast mode on x86

;;;; description of the target address space

;;; where to put the different spaces
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
;;; And there have been some changes since the fork from CMU CL:
;;;   * The OpenBSD port is new since the fork. We started with
;;;     the FreeBSD address map, which actually worked until the
;;;     Alpha port patches, for reasons which in retrospect are rather
;;;     mysterious. After the Alpha port patches were added, the
;;;     OpenBSD port suffered memory corruption problems. While
;;;     debugging those, it was discovered that src/runtime/trymap
;;;     failed for the control stack region #x40000000-#x47fff000.
;;;     After the control stack was moved upward out of this region
;;;     (stealing some bytes from dynamic space) the problems went
;;;     away.
;;;   * The FreeBSD STATIC-SPACE-START value was bumped up from
;;;     #x28000000 to #x30000000 when FreeBSD ld.so dynamic linking
;;;     support was added for FreeBSD ca. 20000910. This was to keep from
;;;     stomping on an address range that the dynamic libraries want to
;;;     use. (They want to use this address range even if we try to
;;;     reserve it with a call to validate() as the first operation in
;;;     main().)
;;;   * For NetBSD 2.0, the following ranges are used by normal
;;;     executables and mmap:
;;;     ** Executables are (by default) loaded at 0x08048000.
;;;     ** The break for the sbcl runtime seems to end around 0x08400000
;;;     We set read only space around 0x20000000, static
;;;     space around 0x30000000, all ending below 0x37fff000
;;;     ** ld.so and other mmap'ed stuff like shared libs start around
;;;        0x48000000
;;;     We set dynamic space between 0x60000000 and 0x98000000
;;;     ** Bottom of the stack is typically not below 0xb0000000
;;;     FYI, this can be looked at with the "pmap" program, and if you
;;;     set the top-down mmap allocation option in the kernel (not yet
;;;     the default), all bets are totally off!

#!+linux
(progn
  (def!constant read-only-space-start     #x01000000)
  (def!constant read-only-space-end       #x037ff000)

  (def!constant static-space-start        #x05000000)
  (def!constant static-space-end          #x07fff000)

  (def!constant dynamic-space-start       #x09000000)
  (def!constant dynamic-space-end         #x29000000)

  (def!constant linkage-table-space-start #x70000000)
  (def!constant linkage-table-space-end   #x7ffff000))

#!+freebsd
(progn
  (def!constant read-only-space-start     #x10000000)
  (def!constant read-only-space-end       #x1ffff000)

  (def!constant static-space-start        #x30000000)
  (def!constant static-space-end          #x37fff000)

  (def!constant dynamic-space-start       #x48000000)
  (def!constant dynamic-space-end         #x88000000)

  ;; In CMUCL:  0xB0000000->0xB1000000
  (def!constant linkage-table-space-start #x90000000)
  (def!constant linkage-table-space-end   #x91000000))

#!+openbsd
(progn
  (def!constant read-only-space-start     #x40000000)
  (def!constant read-only-space-end       #x47fff000)

  (def!constant static-space-start        #x50000000)
  (def!constant static-space-end          #x5ffff000)

  (def!constant dynamic-space-start       #x80000000)
  (def!constant dynamic-space-end         #xA0000000)

  ;; In CMUCL: 0xB0000000->0xB1000000
  (def!constant linkage-table-space-start #xA0000000)
  (def!constant linkage-table-space-end   #xA1000000))

#!+netbsd
(progn
  (def!constant read-only-space-start     #x20000000)
  (def!constant read-only-space-end       #x2ffff000)

  (def!constant static-space-start        #x30000000)
  (def!constant static-space-end          #x37fff000)

  (def!constant dynamic-space-start       #x60000000)
  (def!constant dynamic-space-end         #x98000000)

  ;; In CMUCL: 0xB0000000->0xB1000000
  (def!constant linkage-table-space-start #xA0000000)
  (def!constant linkage-table-space-end   #xA1000000))

;;; Size of one linkage-table entry in bytes.
(def!constant linkage-table-entry-size 8)

;;; Given that NIL is the first thing allocated in static space, we
;;; know its value at compile time:
(def!constant nil-value (+ static-space-start #xb))

;;;; other miscellaneous constants

(defenum (:suffix -trap :start 8)
  halt
  pending-interrupt
  error
  cerror
  breakpoint
  fun-end-breakpoint
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
  fun-prologue
  fun-epilogue)

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

;;; FIXME: !COLD-INIT probably doesn't need
;;; to be in the static symbols table any more.
(defparameter *static-symbols*
  '(t

    ;; The C startup code must fill these in.
    *posix-argv*

    ;; functions that the C code needs to call.  When adding to this list,
    ;; also add a `frob' form in genesis.lisp finish-symbols.
    sub-gc
    sb!kernel::internal-error
    sb!kernel::control-stack-exhausted-error
    sb!kernel::undefined-alien-variable-error
    sb!kernel::undefined-alien-function-error
    sb!kernel::memory-fault-error
    sb!di::handle-breakpoint
    fdefinition-object

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
    *current-catch-block*
    *current-unwind-protect-block*
    *alien-stack*

    ;; interrupt handling
    *pseudo-atomic-atomic*
    *pseudo-atomic-interrupted*
    sb!unix::*interrupts-enabled*
    sb!unix::*interrupt-pending*
    *free-interrupt-context-index*

    *free-tls-index*
    
    *allocation-pointer*
    *binding-stack-pointer*
    *binding-stack-start*
    *control-stack-start*
    *control-stack-end*

    *need-to-collect-garbage*

    ;; the floating point constants
    *fp-constant-0d0*
    *fp-constant-1d0*
    *fp-constant-0f0*
    *fp-constant-1f0*
    ;; The following are all long-floats.
    *fp-constant-0l0*
    *fp-constant-1l0*
    *fp-constant-pi*
    *fp-constant-l2t*
    *fp-constant-l2e*
    *fp-constant-lg2*
    *fp-constant-ln2*

    ;; The ..SLOT-UNBOUND.. symbol is static in order to optimise the
    ;; common slot unbound check.
    ;;
    ;; FIXME: In SBCL, the CLOS code has become sufficiently tightly
    ;; integrated into the system that it'd probably make sense to use
    ;; the ordinary unbound marker for this.
    sb!pcl::..slot-unbound..
    ))

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

;;;; stuff added by jrd

;;; FIXME: Is this used? Delete it or document it.
;;; cf the sparc PARMS.LISP
(defparameter *assembly-unit-length* 8)

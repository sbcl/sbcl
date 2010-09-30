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

(def!constant long-float-bias 16382)
(defconstant-eqx long-float-exponent-byte    (byte 15 0) #'equalp)
(defconstant-eqx long-float-significand-byte (byte 31 0) #'equalp)
(def!constant long-float-normal-exponent-min 1)
(def!constant long-float-normal-exponent-max #x7FFE)
(def!constant long-float-hidden-bit (ash 1 31))         ; actually not hidden
(def!constant long-float-trapping-nan-bit (ash 1 30))

(def!constant single-float-digits
  (+ (byte-size single-float-significand-byte) 1))

(def!constant double-float-digits
  (+ (byte-size double-float-significand-byte) n-word-bits 1))

(def!constant long-float-digits
  (+ (byte-size long-float-significand-byte) n-word-bits 1))

;;; pfw -- from i486 microprocessor programmer's reference manual
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
;;;   * For FreeBSD, the requirement of user and kernel space are
;;;     getting larger, and users tend to extend them.
;;;     If MAXDSIZ is extended from 512MB to 1GB, we can't use up to
;;;     around 0x50000000.
;;;     And if KVA_PAGES is extended from 1GB to 1.5GB, we can't use
;;;     down to around 0xA0000000.
;;;     So we use 0x58000000--0x98000000 for dynamic space.
;;;   * OpenBSD address space changes for W^X as well as malloc()
;;;     randomization made the old addresses unsafe.
;;;     ** By default (linked without -Z option):
;;;        The executable's text segment starts at #x1c000000 and the
;;;        data segment MAXDSIZ bytes higher, at #x3c000000. Shared
;;;        library text segments start randomly between #x00002000 and
;;;        #x10002000, with the data segment MAXDSIZ bytes after that.
;;;     ** If the -Z linker option is used:
;;;        The executable's text and data segments simply start at
;;;        #x08048000, data immediately following text. Shared library
;;;        text and data is placed as if allocated by malloc().
;;;     ** In both cases, the randomized range for malloc() starts
;;;        MAXDSIZ bytes after the end of the data segment (#x48048000
;;;        with -Z, #x7c000000 without), and extends 256 MB.
;;;     ** The read only, static, and linkage table spaces should be
;;;        safe with and without -Z if they are located just before
;;;        #x1c000000.
;;;     ** Ideally the dynamic space should be at #x94000000, 64 MB
;;;        after the end of the highest random malloc() address.
;;;        Unfortunately the dynamic space must be in the lower half
;;;        of the address space, where there are no large areas which
;;;        are unused both with and without -Z. So we break -Z by
;;;        starting at #x40000000. By only using 512 - 64 MB we can
;;;        run under the default 512 MB data size resource limit.

#!+win32
(progn

  (def!constant read-only-space-start #x22000000)
  (def!constant read-only-space-end   #x220ff000)

  (def!constant static-space-start    #x22100000)
  (def!constant static-space-end      #x221ff000)

  (def!constant dynamic-space-start   #x22300000)
  (def!constant dynamic-space-end     (!configure-dynamic-space-end #x42300000))

  (def!constant linkage-table-space-start #x22200000)
  (def!constant linkage-table-space-end   #x222ff000))

#!+linux
(progn
  (def!constant read-only-space-start     #x01000000)
  (def!constant read-only-space-end       #x010ff000)

  (def!constant static-space-start        #x01100000)
  (def!constant static-space-end          #x011ff000)

  (def!constant dynamic-space-start       #x09000000)
  (def!constant dynamic-space-end         (!configure-dynamic-space-end #x29000000))

  (def!constant linkage-table-space-start #x01200000)
  (def!constant linkage-table-space-end   #x012ff000))

#!+sunos
(progn
  (def!constant read-only-space-start     #x20000000)
  (def!constant read-only-space-end       #x200ff000)

  (def!constant static-space-start        #x20100000)
  (def!constant static-space-end          #x201ff000)

  (def!constant dynamic-space-start       #x48000000)
  (def!constant dynamic-space-end         (!configure-dynamic-space-end #xA0000000))

  (def!constant linkage-table-space-start #x20200000)
  (def!constant linkage-table-space-end   #x202ff000))

#!+freebsd
(progn
  (def!constant read-only-space-start     #x01000000)
  (def!constant read-only-space-end       #x010ff000)

  (def!constant static-space-start        #x01100000)
  (def!constant static-space-end          #x011ff000)

  (def!constant dynamic-space-start       #x58000000)
  (def!constant dynamic-space-end         (!configure-dynamic-space-end #x98000000))

  (def!constant linkage-table-space-start #x01200000)
  (def!constant linkage-table-space-end   #x012ff000))

#!+openbsd
(progn
  (def!constant read-only-space-start     #x1b000000)
  (def!constant read-only-space-end       #x1b0ff000)

  (def!constant static-space-start        #x1b100000)
  (def!constant static-space-end          #x1b1ff000)

  (def!constant dynamic-space-start       #x40000000)
  (def!constant dynamic-space-end         (!configure-dynamic-space-end #x5bfff000))

  (def!constant linkage-table-space-start #x1b200000)
  (def!constant linkage-table-space-end   #x1b2ff000))

#!+netbsd
(progn
  (def!constant read-only-space-start     #x20000000)
  (def!constant read-only-space-end       #x200ff000)

  (def!constant static-space-start        #x20100000)
  (def!constant static-space-end          #x201ff000)

  (def!constant dynamic-space-start       #x60000000)
  (def!constant dynamic-space-end         (!configure-dynamic-space-end #x98000000))

  ;; In CMUCL: 0xB0000000->0xB1000000
  (def!constant linkage-table-space-start #x20200000)
  (def!constant linkage-table-space-end   #x202ff000))


#!+darwin
(progn
  (def!constant read-only-space-start #x04000000)
  (def!constant read-only-space-end   #x040ff000)

  (def!constant static-space-start    #x04100000)
  (def!constant static-space-end      #x041ff000)

  (def!constant dynamic-space-start #x10000000)
  (def!constant dynamic-space-end   (!configure-dynamic-space-end #x6ffff000))

  (def!constant linkage-table-space-start #x04200000)
  (def!constant linkage-table-space-end   #x042ff000))

;;; Size of one linkage-table entry in bytes.
(def!constant linkage-table-entry-size 8)

;;; Given that NIL is the first thing allocated in static space, we
;;; know its value at compile time:
(def!constant nil-value (+ static-space-start #xb))

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
;;; pfw X86 doesn't have enough registers to keep these things there.
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
     ;; The following are all long-floats.
     *fp-constant-0l0*
     *fp-constant-1l0*
     *fp-constant-pi*
     *fp-constant-l2t*
     *fp-constant-l2e*
     *fp-constant-lg2*
     *fp-constant-ln2*

     ;; For GC-AND-SAVE
     *restart-lisp-function*

     ;; For the UNWIND-TO-FRAME-AND-CALL VOP
     *unwind-to-frame-function*

     ;; Needed for callbacks to work across saving cores. see
     ;; ALIEN-CALLBACK-ASSEMBLER-WRAPPER in c-call.lisp for gory
     ;; details.
     sb!alien::*enter-alien-callback*

     ;; see comments in ../x86-64/parms.lisp
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

;;;; stuff added by jrd

;;; FIXME: Is this used? Delete it or document it.
;;; cf the sparc PARMS.LISP
(defparameter *assembly-unit-length* 8)

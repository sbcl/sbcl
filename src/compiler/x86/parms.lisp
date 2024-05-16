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

(defconstant sb-fasl:+backend-fasl-file-implementation+ :x86)
(defconstant-eqx +fixup-kinds+ #(:absolute :relative) #'equalp)

;;; KLUDGE: It would seem natural to set this by asking our C runtime
;;; code for it, but mostly we need it for GENESIS, which doesn't in
;;; general have our C runtime code running to ask, so instead we set
;;; it by hand. -- WHN 2001-04-15
;;;
;;; Actually any information that we can retrieve C-side would be
;;; useless in SBCL, since it's possible for otherwise binary
;;; compatible systems to return different values for getpagesize().
;;; -- JES, 2007-01-06
(defconstant +backend-page-bytes+ #+win32 65536 #-win32 4096)

;;; The size in bytes of GENCGC cards, i.e. the granularity at which
;;; writes to old generations are logged.  With mprotect-based write
;;; barriers, this must be a multiple of the OS page size.
(defconstant gencgc-page-bytes +backend-page-bytes+)
;;; The minimum size of new allocation regions.  While it doesn't
;;; currently make a lot of sense to have a card size lower than
;;; the alloc granularity, it will, once we are smarter about finding
;;; the start of objects.
(defconstant gencgc-alloc-granularity 0)

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
(defconstant n-word-bits 32)

;;; the natural width of a machine word (as seen in e.g. register width,
;;; address space)
(defconstant n-machine-word-bits 32)

(defconstant long-float-bias 16382)
(defconstant-eqx long-float-exponent-byte    (byte 15 0) #'equalp)
(defconstant-eqx long-float-significand-byte (byte 31 0) #'equalp)
(defconstant long-float-normal-exponent-min 1)
(defconstant long-float-normal-exponent-max #x7FFE)
(defconstant long-float-hidden-bit (ash 1 31))         ; actually not hidden

(defconstant long-float-digits
  (+ (byte-size long-float-significand-byte) n-word-bits 1))

;;; pfw -- from i486 microprocessor programmer's reference manual
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

(defconstant float-precision-24-bit 0)
(defconstant float-precision-53-bit 2)
(defconstant float-precision-64-bit 3)

(defconstant-eqx float-rounding-mode     (byte 2 10) #'equalp)
(defconstant-eqx float-sticky-bits       (byte 6 16) #'equalp)
(defconstant-eqx float-traps-byte        (byte 6  0) #'equalp)
(defconstant-eqx float-exceptions-byte   (byte 6 16) #'equalp)
(defconstant-eqx float-precision-control (byte 2  8) #'equalp)
(defconstant float-fast-bit 0) ; no fast mode on x86

;;;; description of the target address space

;;; where to put the different spaces
;;;
;;; Note: Mostly these values are magic, inherited from CMU CL
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
;;;   * For OpenBSD, the following ranges are used:
;;;     ** Non-PIE executables' text segments start at #x1c000000 and
;;;        data segments 512MB higher at #x3c000000.
;;;     ** Shared library text segments are randomly located between
;;;        #x00002000 and #x10002000, with each data segment located
;;;        512MB higher.
;;;     ** OpenBSD 6.3 and earlier place random malloc/mmap
;;;        allocations in the range starting 1GB after the end of the
;;;        data segment and extending for 256MB.
;;;     ** After OpenBSD 6.3, this range starts 128MB after the end of
;;;        the data segment and extends for 1GB.

;;; NetBSD configuration used to have this comment regarding the linkage
;;; table: "In CMUCL: 0xB0000000->0xB1000000"

(defmacro space-setup (arg &rest more)
  `(gc-space-setup ,arg ,@more))

#+win32     (space-setup #x22000000)
#+linux     (space-setup #x01000000 :dynamic-space-start #x09000000)
#+sunos     (space-setup #x20000000 :dynamic-space-start #x48000000)
#+freebsd   (space-setup #x01000000 :dynamic-space-start #x58000000)
#+dragonfly (space-setup #x01000000 :dynamic-space-start #x58000000)
#+openbsd   (space-setup #x11000000 :dynamic-space-start #x8d000000)
#+netbsd    (space-setup #x20000000 :dynamic-space-start #x60000000)
#+darwin    (space-setup #x04000000 :dynamic-space-start #x10000000)

;;; Size of one alien-linkage-table entry in bytes.
(defconstant alien-linkage-table-entry-size 8)
(defconstant alien-linkage-table-growth-direction :up)


(defenum (:start 8)
  halt-trap
  pending-interrupt-trap
  cerror-trap
  breakpoint-trap
  fun-end-breakpoint-trap
  single-step-around-trap
  single-step-before-trap
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
;;; pfw X86 doesn't have enough registers to keep these things there.
;;;     Note these spaces grow from low to high addresses.
(defvar *binding-stack-pointer*)

(defconstant-eqx +static-symbols+
 `#(,@+common-static-symbols+
    *alien-stack-pointer*

     ;; interrupt handling
     *pseudo-atomic-bits*

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
     *fp-constant-ln2*)
  #'equalp)

(defconstant-eqx +static-fdefns+ `#(,@common-static-fdefns) #'equalp)

#+win32
(defconstant +win32-tib-arbitrary-field-offset+ #.(+ #xE10 (* 4 63)))

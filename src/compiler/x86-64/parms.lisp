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

(defconstant sb-fasl:+backend-fasl-file-implementation+ :x86-64)
(defconstant-eqx +fixup-kinds+ #(:abs32 :*abs32 :rel32 :absolute) #'equalp)

;;; This size is supposed to indicate something about the actual granularity
;;; at which you can map memory.  We just hardwire it, but that may or may not
;;; be necessary any more.
(defconstant +backend-page-bytes+ #+win32 65536 #-win32 32768)

;;; The size in bytes of GENCGC pages. A page is the smallest amount of memory
;;; that a thread can claim for a thread-local region, and also determines
;;; the granularity at which we can find the start of a sequence of objects.
(defconstant gencgc-page-bytes 32768)
;;; The divisor relative to page-bytes which computes the granularity
;;; at which writes to old generations are logged.
#+soft-card-marks (defconstant cards-per-page
                               #+mark-region-gc (/ gencgc-page-bytes 128)
                               #-mark-region-gc 32)
;;; The minimum size of new allocation regions.  While it doesn't
;;; currently make a lot of sense to have a card size lower than
;;; the alloc granularity, it will, once we are smarter about finding
;;; the start of objects.
(defconstant gencgc-alloc-granularity 0)
;;; The card size for immobile/low space
(defconstant immobile-card-bytes 4096)

;;; ### Note: 'lispword' always means 8 bytes, and 'word' usually means
;;; the same as 'lispword', except in the assembler and disassembler,
;;; where 'word' means 2 bytes to match AMD/Intel terminology.

;;;; machine architecture parameters

;;; the number of bits per word, where a word holds one lisp descriptor
(defconstant n-word-bits 64)

;;; the natural width of a machine word (as seen in e.g. register width,
;;; address space)
(defconstant n-machine-word-bits 64)

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

#+(or linux darwin)
(gc-space-setup #x50000000
                     :read-only-space-size 0
                     :fixedobj-space-size #.(* 60 1024 1024)
                     :text-space-size #.(* 160 1024 1024)
                     :text-space-start #xB800000000
                     :dynamic-space-start #x1000000000)

;;; The default dynamic space size is lower on OpenBSD to allow SBCL to
;;; run under the default 1G data size limit.

#-(or linux darwin)
(gc-space-setup #x20000000
                     :read-only-space-size 0
                     :fixedobj-space-size #.(* 60 1024 1024)
                     :text-space-size #.(* 130 1024 1024)
                     :text-space-start #x1000000000
                     :dynamic-space-start #x1100000000
                     #+openbsd :dynamic-space-size #+openbsd #x2fff0000)

(defconstant alien-linkage-table-growth-direction :up)
(defconstant alien-linkage-table-entry-size 16)

#+(and sb-xc-host (not immobile-space))
(defparameter lisp-linkage-space-addr #x1500000000) ; arbitrary
#+(and sb-xc-host immobile-space)
(progn
(defparameter lisp-linkage-space-addr
  ;; text space:
  ;;   | ALIEN LINKAGE | LISP LINKAGE | CODE OBJECTS ...
  ;;   |<------------->|<------------>| ....
  (- text-space-start (* (ash 1 n-linkage-index-bits) 8)))
(defparameter alien-linkage-space-start (- lisp-linkage-space-addr alien-linkage-space-size)))

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
  uninitialized-load-trap
  ;; ERROR-TRAP has to be numerically highest.
  ;; The various internal errors are numbered from here upward.
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

;;; Bit indices into *CPU-FEATURE-BITS*
(defconstant cpu-has-ymm-registers   0)
(defconstant cpu-has-popcnt          1)

(defconstant-eqx +static-symbols+
 `#(,@+common-static-symbols+
    #+(and immobile-space (not sb-thread)) function-layout
    ;; I had trouble making alien_stack_pointer use the thread slot for #-sb-thread
    ;; because WITH-ALIEN binds SB-C:*ALIEN-STACK-POINTER* in an ordinary LET.
    ;; That being so, it has to use the #-sb-thread mechanism of placing the new value
    ;; in the symbol's value slot for compatibility with UNBIND and all else.
    #-sb-thread *alien-stack-pointer*    ; a thread slot if #+sb-thread
    ;; Since the text space and alien linkage table might both get relocated on startup
    ;; under #+immobile-space, an alien callback wrapper can't wire in the address
    ;; of a word that holds the C function pointer to callback_wrapper_trampoline.
    ;; (There is no register that points to a known address when entering the callback)
    ;; A static symbol works well for this, and is sensible considering that
    ;; the assembled wrappers also reside in static space.
    #+(and sb-thread immobile-space) callback-wrapper-trampoline
    *cpu-feature-bits*)
  #'equalp)

;; No static-fdefns are actually needed, but #() here causes the
;; "recursion in known function" error to occur in ltn
(defconstant-eqx +static-fdefns+ `#(,@common-static-fdefns) #'equalp)

#+sb-simd-pack
(progn
  (defconstant-eqx +simd-pack-element-types+
      (coerce
       (hash-cons
        '(single-float double-float
          (unsigned-byte 8) (unsigned-byte 16) (unsigned-byte 32) (unsigned-byte 64)
          (signed-byte 8) (signed-byte 16) (signed-byte 32) (signed-byte 64)))
       'vector)
    #'equalp)
  (defconstant sb-kernel::+simd-pack-wild+
    (ldb (byte (length +simd-pack-element-types+) 0) -1))
  (defconstant-eqx +simd-pack-128-primtypes+
      #(simd-pack-single simd-pack-double
        simd-pack-ub8 simd-pack-ub16 simd-pack-ub32 simd-pack-ub64
        simd-pack-sb8 simd-pack-sb16 simd-pack-sb32 simd-pack-sb64)
    #'equalp)
  (defconstant-eqx +simd-pack-256-primtypes+
      #(simd-pack-256-single simd-pack-256-double
        simd-pack-256-ub8 simd-pack-256-ub16 simd-pack-256-ub32 simd-pack-256-ub64
        simd-pack-256-sb8 simd-pack-256-sb16 simd-pack-256-sb32 simd-pack-256-sb64)
    #'equalp))

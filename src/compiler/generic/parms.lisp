;;;; This file contains some parameterizations of various VM
;;;; attributes common to all architectures.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

;;; When building the cross-compiler (and called by the host), read the
;;; dynamic-space-size file.
;;; When called by the cross-compiler (in the host), use the previously chosen value.
;;; The target function is never called, but if omitted via #-sb-xc-host,
;;; compilation of !GENCGC-SPACE-SETUP would issue an "undefined" warning.
(defun !read-dynamic-space-size ()
  #+sb-xc-host
  (with-open-file (f "output/dynamic-space-size.txt" :if-does-not-exist nil)
    (unless f
      (return-from !read-dynamic-space-size nil))
    (let ((line (read-line f)))
      (multiple-value-bind (number end)
          (parse-integer line :junk-allowed t)
        (when number
            (let* ((ext (subseq line end))
                   (mult (cond ((or (zerop (length ext))
                                    (member ext '("MB" "MIB") :test #'equalp))
                                (expt 2 20))
                               ((member ext '("GB" "GIB") :test #'equalp)
                                (expt 2 30))
                               (t
                                (error "Invalid --dynamic-space-size=~A" line)))))
              (* number mult))))))
  #-sb-xc-host (symbol-value 'default-dynamic-space-size))

#+gencgc
;; Define START/END constants for GENCGC spaces.
;; Assumptions:
;;     We only need very small read-only and static spaces, because
;;     gencgc does not purify any more.  We can count on being able to
;;     allocate them with roughly the same size, and next to each other.
;;
;;     The linkage table (if enabled) can be treated the same way.
;;
;;     Dynamic space traditionally sits elsewhere, so has its own
;;     parameter.  But if not specified, it is allocated right after
;;     the other spaces (used on Windows/x86).
;;
;;     The safepoint page (if enabled) is to be allocated immediately
;;     prior to static page.
(defmacro !gencgc-space-setup
    (small-spaces-start
          ;; These keywords variables have to be careful not to overlap with the
          ;; the DEFCONSTANT of the same name, hence the suffix.
          &key ((:dynamic-space-start dynamic-space-start*))
               ((:dynamic-space-size dynamic-space-size*))
               ;; The immobile-space START parameters should not be used
               ;; except in forcing discontiguous addresses for testing.
               ;; And of course, don't use them if unsupported.
               ((:fixedobj-space-start fixedobj-space-start*))
               ((:fixedobj-space-size  fixedobj-space-size*) (* 24 1024 1024))
               ((:varyobj-space-start  varyobj-space-start*))
               ((:varyobj-space-size   varyobj-space-size*) (* 104 1024 1024))
               (small-space-size #x100000)
               ((:read-only-space-size ro-space-size) small-space-size))
  (declare (ignorable dynamic-space-start*)) ; might be unused in make-host-2
  (flet ((defconstantish (relocatable symbol value)
           (if (not relocatable) ; easy case
               `(defconstant ,symbol ,value)
               ;; Genesis needs to know the gspace start, but it's not constant.
               ;; This value will not be exposed to C code.
               #+sb-xc-host `(defparameter ,symbol ,value)
               ;; Ideally the #-sb-xc-host code be a DEFINE-ALIEN-VARIABLE,
               ;; but can't be due to dependency order problem.
               )))
    (let*
        ((spaces (append `((read-only ,ro-space-size)
                           #+(and win32 x86-64)
                           (seh-data ,(symbol-value '+backend-page-bytes+) win64-seh-data-addr)
                           (linkage-table ,small-space-size)
                           #+sb-safepoint
                           ;; Must be just before NIL.
                           (safepoint ,(symbol-value '+backend-page-bytes+) gc-safepoint-page-addr)
                           (static ,small-space-size)
                           #+darwin-jit
                           (static-code ,small-space-size))
                         #+immobile-space
                         `((fixedobj ,fixedobj-space-size*)
                           (varyobj ,varyobj-space-size*))))
         (ptr small-spaces-start)
         (small-space-forms
           (loop for (space size var-name) in spaces
                 appending
                 (let* ((relocatable
                          ;; TODO: linkage-table could move with code, if the CPU
                          ;; prefers PC-relative jumps, and we emit better code
                          ;; (which we don't- for x86 we jmp via RBX always)
                          (member space '(fixedobj varyobj)))
                        (start ptr)
                        (end (+ ptr size)))
                   (setf ptr end)
                   (if var-name
                       `((defconstant ,var-name ,start))
                       (let ((start-sym (symbolicate space "-SPACE-START")))
                         ;; Allow expressly given addresses / sizes for immobile space.
                         ;; The addresses are for testing only - you should not need them.
                         (case space
                           (varyobj  (setq start (or varyobj-space-start* start)
                                           end (+ start varyobj-space-size*)))
                           (fixedobj (setq start (or fixedobj-space-start* start)
                                           end (+ start fixedobj-space-size*))))
                         `(,(defconstantish relocatable start-sym start)
                           ,(cond ((not relocatable)
                                   `(defconstant ,(symbolicate space "-SPACE-END") ,end))
                                  #-sb-xc-host ((eq space 'varyobj)) ; don't emit anything
                                  (t
                                   `(defconstant ,(symbolicate space "-SPACE-SIZE")
                                      ,(- end start)))))))))))
      `(progn
         ,@small-space-forms
         ,(defconstantish t 'dynamic-space-start
            (or dynamic-space-start* ptr))
         (defconstant default-dynamic-space-size
           ;; Build-time make-config.sh option "--dynamic-space-size" overrides
           ;; keyword argument :dynamic-space-size which overrides general default.
           ;; All are overridden by runtime --dynamic-space-size command-line arg.
           (or ,(or (!read-dynamic-space-size) dynamic-space-size*)
               (ecase n-word-bits
                 (32 (expt 2 29))
                 (64 (expt 2 30)))))
         (defconstant gencgc-card-shift
           (integer-length (1- sb-vm:gencgc-page-bytes)))
         ;; This is a constant during build, but a different value
         ;; can be patched directly into the affected machine code
         ;; when the core is loaded based on dynamic-space-size.
         ;; I think the C runtime does a floor operation rather than ceiling,
         ;; but firstly there's probably no difference, and secondly it's better
         ;; to be safe than sorry - using too many bits rather than too few.
         (defconstant gencgc-card-table-index-nbits
           (integer-length (1- (ceiling sb-vm::default-dynamic-space-size
                                        sb-vm::gencgc-page-bytes))))
         (defconstant gencgc-card-table-index-mask
           (1- (ash 1 gencgc-card-table-index-nbits)))))))

(defconstant-eqx +c-callable-fdefns+
  '(sub-gc
    sb-kernel::post-gc
    internal-error
    sb-kernel::control-stack-exhausted-error
    sb-kernel::binding-stack-exhausted-error
    sb-kernel::alien-stack-exhausted-error
    sb-kernel::heap-exhausted-error
    sb-kernel::undefined-alien-variable-error
    sb-kernel::memory-fault-error
    sb-kernel::unhandled-trap-error
    ;; On these it's called through the internal errors mechanism
    #-(or arm arm64 x86-64) undefined-alien-fun-error
    sb-di::handle-breakpoint
    sb-di::handle-single-step-trap
    #+win32 sb-kernel::handle-win32-exception
    #+sb-safepoint sb-thread::run-interruption
    enter-alien-callback
    #+sb-thread sb-thread::enter-foreign-callback)
  #'equal)

;;; (potentially) static symbols that C code must be able to set/get
;;; as contrasted with static for other reasons such as:
;;;  - garbage collections roots (namely NIL)
;;;  - other symbols that Lisp codegen must hardwire (T)
;;;  - static for efficiency of access but need not be
;;; On #+sb-thread builds, these are not static, because access to them
;;; is via the TLS, not the symbol.
(defconstant-eqx per-thread-c-interface-symbols
  `((*free-interrupt-context-index* 0)
    (sb-sys:*allow-with-interrupts* t)
    (sb-sys:*interrupts-enabled* t)
    sb-sys:*interrupt-pending*
    #+sb-safepoint sb-sys:*thruption-pending*
    *in-without-gcing*
    *gc-inhibit*
    *gc-pending*
    #+sb-safepoint sb-impl::*in-safepoint*
    #+sb-thread *stop-for-gc-pending*
    *pinned-objects*
    #+gencgc (*gc-pin-code-pages* 0)
    ;; things needed for non-local-exit
    (*current-catch-block* 0)
    (*current-unwind-protect-block* 0)
    )
  #'equal)

(defconstant-eqx +common-static-symbols+
  '#.`(t
    ;; These symbols are accessed from C only through TLS,
    ;; never the symbol-value slot
    #-sb-thread ,@(mapcar (lambda (x) (car (ensure-list x)))
                           per-thread-c-interface-symbols)
    ;; NLX variables are thread slots on x86-64 and RISC-V.  A static sym is needed
    ;; for arm64, ppc, and x86 because we haven't implemented TLS index fixups,
    ;; so must lookup the TLS index given the symbol.
    #+(and sb-thread (not x86-64) (not riscv))
    ,@'(*current-catch-block*
        *current-unwind-protect-block*)

    #+immobile-space *immobile-freelist* ; not per-thread (yet...)
    #+metaspace *metaspace-tracts*

    ;; stack pointers
    #-sb-thread *binding-stack-start* ; a thread slot if #+sb-thread
    #-sb-thread *control-stack-start* ; ditto
    #-sb-thread *control-stack-end*   ; ditto

    #-sb-thread *stepping*

    ;; threading support
    #+sb-thread ,@'(sb-thread::*starting-threads* *free-tls-index*)

    ;; runtime linking of lisp->C calls (regardless of whether
    ;; the C function is in a dynamic shared object or not)
    +required-foreign-symbols+

    ;;; The following symbols aren't strictly required to be static
    ;;; - they are not accessed from C - but we make them static in order
    ;;; to (perhaps) micro-optimize access in Lisp.
    ;;; However there is no efficiency gain if we have #+immobile-space.
    #-immobile-space ,@'(
     ;; arbitrary object that changes after each GC
     sb-kernel::*gc-epoch*
     ;; Dispatch tables for generic array access
     %%data-vector-reffers%%
     %%data-vector-reffers/check-bounds%%
     %%data-vector-setters%%
     %%data-vector-setters/check-bounds%%))
  #'equalp)

;;; Refer to the lengthy comment in 'src/runtime/interrupt.h' about
;;; the choice of this number. Rather than have to two copies
;;; of the comment, please see that file before adjusting this.
;;; I think most of the need for a ridiculously large value stemmed from
;;; receive-pending-interrupt after a pseudo-atomic code section.
;;; If that trapped into GC, and then ran finalizers in POST-GC (while still
;;; in a signal handler), which consed, which caused the need for another GC,
;;; you'd receive a nested interrupt, as the GC trap was still on the stack
;;; not having returned to "user" code yet. [See example in src/code/final]
;;;
;;; But now that #+sb-thread creates a dedicated finalizer thread, nesting
;;; seems unlikely to occur, because "your" code doesn't get a chance to run again
;;; until after the interrupt returns. And the finalizer thread won't invoke
;;; run-pending-finalizers in post-GC, it will just pick up the next finalizer
;;; in due turn. You could potentially force a nested GC by consing a lot in
;;; a post-GC hook, but if you do that, your hook function is badly behaved
;;; and you should fix it.
(defconstant max-interrupts
  #+sb-thread    8  ; reasonable value
  #-sb-thread 1024) ; crazy value

;;; Thread slots accessed at negative indices relative to struct thread.
(defconstant thread-header-slots
  ;; This seems to need to be an even number.
  ;; I'm not sure what the constraint on that stems from.
  #+(and x86-64 sb-safepoint) 14 ; the safepoint trap page is at word index -15
  #+(and x86-64 (not sb-safepoint)) 16
  #-x86-64 0)

#+gencgc
(progn
  (defconstant +highest-normal-generation+ 5)
  (defconstant +pseudo-static-generation+ 6))

(defparameter *runtime-asm-routines* nil)
(defparameter *linkage-space-predefined-entries* nil)

;;; Floating-point related constants, both format descriptions and FPU
;;; control register descriptions.  These don't exactly match up with
;;; what the machine manuals say because the Common Lisp standard
;;; defines floating-point values somewhat differently than the IEEE
;;; standard does.

;;; We can currently manipulate only IEEE single and double precision.
;;; Machine-specific formats (such as x86 80-bit and PPC double-double)
;;; are unsupported.

#+ieee-floating-point
(progn
(defconstant float-sign-shift 31)

;;; The exponent bias is the amount up by which the true exponent is
;;; incremented for storage purposes.
;;; (Wikipedia entry for single-float: "an exponent value of 127 represents the actual zero")
;;; 126 works for us because we actually want an exponent of -1 and not 0
;;; when using DECODE-FLOAT. -1 is correct because the implied 1 bit in a normalized
;;; float is the _left_ of of the binary point, so the powers of 2 for the first represented
;;; bit is 2^-1. Similarly for double-float.
(defconstant single-float-bias 126)
(defconstant-eqx single-float-exponent-byte (byte 8 23) #'equalp)
(defconstant-eqx single-float-significand-byte (byte 23 0) #'equalp)
(defconstant single-float-normal-exponent-min 1)
(defconstant single-float-normal-exponent-max 254)
(defconstant single-float-hidden-bit (ash 1 23))

(defconstant double-float-bias 1022)
(defconstant-eqx double-float-exponent-byte (byte 11 20) #'equalp)
(defconstant-eqx double-float-significand-byte (byte 20 0) #'equalp)
(defconstant double-float-normal-exponent-min 1)
(defconstant double-float-normal-exponent-max #x7FE)
(defconstant double-float-hidden-bit (ash 1 20))

(defconstant single-float-digits
  (+ (byte-size single-float-significand-byte) 1))

(defconstant double-float-digits
  (+ (byte-size double-float-significand-byte) 32 1))
)

(push '("SB-VM" +c-callable-fdefns+ +common-static-symbols+)
      *!removable-symbols*)

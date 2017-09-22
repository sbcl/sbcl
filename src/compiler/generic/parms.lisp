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

(in-package "SB!VM")

;;; When building the cross-compiler (and called by the host), read the
;;; dynamic-space-size file.
;;; When called by the cross-compiler (in the host), use the previously chosen value.
;;; The target function is never called, but if omitted via #-sb-xc-host,
;;; compilation of !GENCGC-SPACE-SETUP would issue an "undefined" warning.
(defun !read-dynamic-space-size ()
  (unless (member :sb-xc-host *features*)
    (return-from !read-dynamic-space-size (symbol-value 'default-dynamic-space-size)))
  (with-open-file (f "output/dynamic-space-size.txt")
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
              (* number mult)))))))

#!+gencgc
;; Define START/END constants for GENCGC spaces.
;; Assumptions:
;;     We only need very small read-only and static spaces, because
;;     gencgc does not purify any more.  We can count on being able to
;;     allocate them with roughly the same size, and next to each other.
;;
;;     There is one page of unmapped buffer between them for good measure.
;;
;;     The linkage table (if enabled) can be treated the same way.
;;
;;     Dynamic space traditionally sits elsewhere, so has its own
;;     parameter.  But if not specified, it is allocated right after
;;     the other spaces (used on Windows/x86).
;;
;;     The safepoint page (if enabled) is to be allocated immediately
;;     prior to static page.  For x86(-64) this would not matter, because
;;     they can only reference it using an absolute fixup anyway, but
;;     for RISC platforms we can (and must) do better.
;;
;;     The safepoint page needs to be small enough that the offset from
;;     static space is immediate, e.g. >= -2^12 for SPARC.  #x1000 works
;;     for almost all platforms, but is too small to make VirtualProtect
;;     happy -- hence the need for an extra `alignment' configuration
;;     option below, which parms.lisp can set to #x10000 on Windows.
;;
(defmacro !gencgc-space-setup
    (small-spaces-start
          &key ((:dynamic-space-start dynamic-space-start*))
               ((:default-dynamic-space-size default-dynamic-space-size*))
               #!+immobile-space
               ((:immobile-space-size immobile-space-size*) (* 128 1024 1024))
               #!+immobile-space (immobile-code-space-size (* 104 1024 1024))
               ;; Smallest os_validate()able alignment; used as safepoint
               ;; page size.  Default suitable for POSIX platforms.
               (alignment            #x1000)
               ;; traditional distance between spaces -- including the margin:
               (small-space-spread #x100000)
               ;; traditional margin between spaces
               (margin-size          #x1000))
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
        ((spaces '(read-only static
                   #!+linkage-table linkage-table
                   #!+immobile-space immobile))
         (ptr small-spaces-start)
         safepoint-address
         (small-space-forms
          (loop for (space next-space) on spaces appending
                (let* ((relocatable
                        ;; TODO: immobile and linkage-table should be relocatable
                        #!+relocatable-heap (member space '(immobile)))
                       (next-start
                        (+ ptr (cond #!+immobile-space
                                     ((eq space 'immobile)
                                      ;; We subtract margin-size when
                                      ;; computing FOO-SPACE-END,
                                      ;; so add it in here to compensate.
                                      (+ immobile-space-size* margin-size))
                                     (t
                                      small-space-spread))))
                       (end next-start))
                  (when (eq next-space 'static)
                    ;; margin becomes safepoint page; substract margin again.
                    (decf end alignment)
                    (setf safepoint-address end))
                  (let* ((start-sym (symbolicate space "-SPACE-START"))
                         (start-val ptr)
                         (end-val (- end margin-size)))
                    (setf ptr next-start)
                    `(,(defconstantish relocatable start-sym start-val)
                      ,(if relocatable
                           `(defconstant ,(symbolicate space "-SPACE-SIZE")
                              ,(- end-val start-val))
                           `(defconstant ,(symbolicate space "-SPACE-END")
                              ,end-val)))))))
         (safepoint-page-forms
          (list #!+sb-safepoint
                `(defconstant gc-safepoint-page-addr ,safepoint-address)))
         )
    #+ccl safepoint-address ; workaround for incorrect "Unused" warning
    `(progn
       ,@safepoint-page-forms
       ,@small-space-forms
       #!+immobile-space
       (defconstant immobile-fixedobj-subspace-size
         ,(- immobile-space-size* immobile-code-space-size))
       ,(defconstantish (or #!+relocatable-heap t) 'dynamic-space-start
          (or dynamic-space-start* ptr))
       (defconstant default-dynamic-space-size
         (or ,(!read-dynamic-space-size)
             ,default-dynamic-space-size*
             (ecase n-word-bits
               (32 (expt 2 29))
               (64 (expt 2 30)))))))))

(defconstant-eqx +c-callable-fdefns+
  '(sub-gc
    sb!kernel::post-gc
    internal-error
    sb!kernel::control-stack-exhausted-error
    sb!kernel::binding-stack-exhausted-error
    sb!kernel::alien-stack-exhausted-error
    sb!kernel::heap-exhausted-error
    sb!kernel::undefined-alien-variable-error
    sb!kernel::memory-fault-error
    sb!kernel::unhandled-trap-error
    ;; On these it's called through the internal errors mechanism
    #!-(or arm arm64 x86-64) undefined-alien-fun-error
    sb!di::handle-breakpoint
    sb!di::handle-single-step-trap
    #!+win32 sb!kernel::handle-win32-exception
    #!+sb-thruption sb!thread::run-interruption
    enter-alien-callback
    #!+sb-thread sb!thread::enter-foreign-callback
    #!+(and sb-safepoint-strictly (not win32))
    sb!unix::signal-handler-callback)
  #'equal)

;;; Static symbols that C code must be able to assign to,
;;; as contrasted with static for other reasons such as:
;;;  - garbage collections roots (namely NIL)
;;;  - other symbols that Lisp codegen must hardwire (T)
;;;  - static for efficiency of access but need not be
(defconstant-eqx !per-thread-c-interface-symbols
  `((*free-interrupt-context-index* 0)
    (sb!sys:*allow-with-interrupts* t)
    (sb!sys:*interrupts-enabled* t)
    *alloc-signal*
    sb!sys:*interrupt-pending*
    #!+sb-thruption sb!sys:*thruption-pending*
    #!+sb-thruption sb!kernel:*restart-clusters*
    *in-without-gcing*
    *gc-inhibit*
    *gc-pending*
    #!+sb-safepoint sb!impl::*gc-safe*
    #!+sb-safepoint sb!impl::*in-safepoint*
    #!+sb-thread *stop-for-gc-pending*
    ;; non-x86oid gencgc object pinning
    #!+(and gencgc (not (or x86 x86-64))) *pinned-objects*
    ;; things needed for non-local-exit
    (*current-catch-block* 0)
    (*current-unwind-protect-block* 0)
    )
  #'equal)

(defconstant-eqx +common-static-symbols+
  `(t
    ;; These symbols are accessed from C only through TLS,
    ;; never the symbol-value slot
    #!-sb-thread ,@(mapcar (lambda (x) (car (ensure-list x)))
                           !per-thread-c-interface-symbols)
    ;; NLX variables are thread slots on x86-64.  A static sym is needed
    ;; for arm64, ppc, and x86 because we haven't implemented TLS index fixups,
    ;; so must lookup the TLS index given the symbol.
    #!+(and sb-thread (not x86-64)) ,@'(*current-catch-block*
                                        *current-unwind-protect-block*)

    ;; sb-safepoint in addition to accessing this symbol via TLS,
    ;; uses the symbol itself as a value. Kinda weird.
    #!+sb-safepoint *in-without-gcing*

    #!+immobile-space *immobile-freelist* ; not per-thread (yet...)

    #!+hpux *c-lra*

    ;; stack pointers
    #!-sb-thread *binding-stack-start* ; a thread slot if #!+sb-thread
    #!-sb-thread *control-stack-start* ; ditto
    #!-sb-thread *control-stack-end*   ; ditto

    #!-sb-thread *stepping*

    ;; threading support
    #!+sb-thread *free-tls-index*
    ;; Keep in sync with 'compiler/early-backend.lisp':
    ;;  "only PPC uses a separate symbol for the TLS index lock"
    #!+(and sb-thread ppc) *tls-index-lock*

    ;; dynamic runtime linking support
    #!+sb-dynamic-core +required-foreign-symbols+

    ;; for looking up assembler routine by name
    ;; and patching them on runtime startup
    sb!fasl::*assembler-routines*

    ;;; The following symbols aren't strictly required to be static
    ;;; - they are not accessed from C - but we make them static in order
    ;;; to (perhaps) micro-optimize access in Lisp.
    ;;; However there is no efficiency gain if we have #!+immobile-space.
    #!-immobile-space ,@'(
     ;; arbitrary object that changes after each GC
     sb!kernel::*gc-epoch*
     ;; Dispatch tables for generic array access
     sb!impl::%%data-vector-reffers%%
     sb!impl::%%data-vector-reffers/check-bounds%%
     sb!impl::%%data-vector-setters%%
     sb!impl::%%data-vector-setters/check-bounds%%))
  #'equalp)

;;; Number of entries in the thread local storage. Limits the number
;;; of symbols with thread local bindings.
(defconstant tls-size 4096)
;;; Refer to the lengthy comment in 'src/runtime/interrupt.h' about
;;; the choice of this number. Rather than have to two copies
;;; of the comment, please see that file before adjusting this.
(defconstant max-interrupts 1024)

#!+gencgc
(progn
  (defconstant +highest-normal-generation+ 5)
  (defconstant +pseudo-static-generation+ 6))

(defun !unintern-symbols ()
  '("SB-VM"
    +c-callable-fdefns+
    +common-static-symbols+))

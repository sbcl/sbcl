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

(def!macro !configure-dynamic-space-end (&optional default)
  (with-open-file (f "output/dynamic-space-size.txt")
    (let ((line (read-line f)))
      (multiple-value-bind (number end)
          (parse-integer line :junk-allowed t)
        (if number
            (let* ((ext (subseq line end))
                   (mult (cond ((or (zerop (length ext))
                                    (member ext '("MB MIB") :test #'equalp))
                                (expt 2 20))
                               ((member ext '("GB" "GIB") :test #'equalp)
                                (expt 2 30))
                               (t
                                (error "Invalid --dynamic-space-size=~A" line)))))
              `(+ dynamic-space-start ,(* number mult)))
            (or default
                `(+ dynamic-space-start
                    (ecase n-word-bits
                      (32 (expt 2 29))
                      (64 (expt 2 30))))))))))

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
;; Cosmetic problem:
;;
;;     In the interest of readability, &KEY would be much nicer than
;;     &OPTIONAL.  But is it possible to use keyword arguments to
;;     DEF!MACRO?
;;
(def!macro !gencgc-space-setup
    (small-spaces-start
     &optional dynamic-space-start*
               default-dynamic-space-size
               ;; Smallest os_validate()able alignment; used as safepoint
               ;; page size.  Default suitable for POSIX platforms.
               (alignment            #x1000)
               ;; traditional distance between spaces -- including the margin:
               (small-space-spread #x100000)
               ;; traditional margin between spaces
               (margin-size          #x1000))
  (let* ((spaces '(read-only static #!+linkage-table linkage-table))
         (ptr small-spaces-start)
         safepoint-address
         (small-space-forms
          (loop for (space next-space) on spaces appending
                (let* ((next-start (+ ptr small-space-spread))
                       (end next-start))
                  (when (eq next-space 'static)
                    ;; margin becomes safepoint page; substract margin again.
                    (decf end alignment)
                    (setf safepoint-address end))
                  (prog1
                      `((def!constant ,(symbolicate space "-SPACE-START")
                            ,ptr)
                        (def!constant ,(symbolicate space "-SPACE-END")
                            ,(- end margin-size)))
                    (setf ptr next-start)))))
         (safepoint-page-forms
          (list #!+sb-safepoint
                `(def!constant gc-safepoint-page-addr ,safepoint-address)))
         (dynamic-space-start* (or dynamic-space-start* ptr))
         (optional-dynamic-space-end
          (when default-dynamic-space-size
            (list (+ dynamic-space-start* default-dynamic-space-size)))))
    `(progn
       ,@safepoint-page-forms
       ,@small-space-forms
       (def!constant dynamic-space-start ,dynamic-space-start*)
       (def!constant dynamic-space-end (!configure-dynamic-space-end
                                        ,@optional-dynamic-space-end)))))

(defparameter *c-callable-static-symbols*
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
    #!+sb-thread sb!thread::enter-foreign-callback
    #!+(and sb-safepoint-strictly (not win32))
    sb!unix::signal-handler-callback))

(defparameter *common-static-symbols*
  '(t

    ;; filled in by the C code to propagate to Lisp
    *posix-argv* *core-string*

    ;; free pointers.  Note that these are FIXNUM word counts, not (as
    ;; one might expect) byte counts or SAPs. The reason seems to be
    ;; that by representing them this way, we can avoid consing
    ;; bignums.  -- WHN 2000-10-02
    *read-only-space-free-pointer*
    *static-space-free-pointer*

    ;; things needed for non-local-exit
    *current-catch-block*
    *current-unwind-protect-block*

    #!+hpux *c-lra*

    ;; stack pointers
    *binding-stack-start*
    *control-stack-start*
    *control-stack-end*

    ;; interrupt handling
    *alloc-signal*
    *free-interrupt-context-index*
    sb!unix::*allow-with-interrupts*
    sb!unix::*interrupts-enabled*
    sb!unix::*interrupt-pending*
    #!+sb-thruption sb!unix::*thruption-pending*
    #!+sb-thruption sb!impl::*restart-clusters*
    sb!vm::*in-without-gcing*
    *gc-inhibit*
    *gc-pending*
    #!-sb-thread
    *stepping*
    #!+sb-safepoint sb!impl::*gc-safe*
    #!+sb-safepoint sb!impl::*in-safepoint*

    ;; threading support
    #!+sb-thread *stop-for-gc-pending*
    #!+sb-thread *free-tls-index*
    #!+sb-thread *tls-index-lock*

    ;; dynamic runtime linking support
    #!+sb-dynamic-core *required-runtime-c-symbols*
    sb!kernel::*gc-epoch*

    ;; Dispatch tables for generic array access
    sb!impl::%%data-vector-reffers%%
    sb!impl::%%data-vector-reffers/check-bounds%%
    sb!impl::%%data-vector-setters%%
    sb!impl::%%data-vector-setters/check-bounds%%

    ;; non-x86oid gencgc object pinning
    #!+(and gencgc (not (or x86 x86-64)))
    *pinned-objects*

    ;; hash table weaknesses
    :key
    :value
    :key-and-value
    :key-or-value))

;;; Number of entries in the thread local storage. Limits the number
;;; of symbols with thread local bindings.
(def!constant tls-size 4096)
;;; Refer to the lengthy comment in 'src/runtime/interrupt.h' about
;;; the choice of this number. Rather than have to two copies
;;; of the comment, please see that file before adjusting this.
(def!constant max-interrupts 1024)

#!+gencgc
(progn
  (def!constant +highest-normal-generation+ 5)
  (def!constant +pseudo-static-generation+ 6))

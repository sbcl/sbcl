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

(def!macro !configure-dynamic-space-end (default)
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
            default)))))

(defparameter *c-callable-static-symbols*
  '(sub-gc
    sb!kernel::post-gc
    sb!kernel::internal-error
    sb!kernel::control-stack-exhausted-error
    sb!kernel::binding-stack-exhausted-error
    sb!kernel::alien-stack-exhausted-error
    sb!kernel::heap-exhausted-error
    sb!kernel::undefined-alien-variable-error
    sb!kernel::undefined-alien-function-error
    sb!kernel::memory-fault-error
    sb!kernel::unhandled-trap-error
    sb!di::handle-breakpoint
    sb!di::handle-single-step-trap
    fdefinition-object
    #!+win32 sb!kernel::handle-win32-exception))

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
    *in-without-gcing*
    *gc-inhibit*
    *gc-pending*
    #!-sb-thread
    *stepping*

    ;; threading support
    #!+sb-thread *stop-for-gc-pending*
    #!+sb-thread *free-tls-index*
    #!+sb-thread *tls-index-lock*

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

#!+gencgc
(progn
  (def!constant +highest-normal-generation+ 5)
  (def!constant +pseudo-static-generation+ 6))

(defenum ()
  trace-table-normal
  trace-table-call-site
  trace-table-fun-prologue
  trace-table-fun-epilogue)

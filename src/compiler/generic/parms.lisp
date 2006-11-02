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

(defparameter *c-callable-static-symbols*
  '(sub-gc
    sb!kernel::internal-error
    sb!kernel::control-stack-exhausted-error
    sb!kernel::heap-exhausted-error
    sb!kernel::undefined-alien-variable-error
    sb!kernel::undefined-alien-function-error
    sb!kernel::memory-fault-error
    sb!di::handle-breakpoint
    sb!di::handle-single-step-trap
    fdefinition-object
    #!+sb-thread sb!thread::run-interruption
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

    ;; stack pointers
    *binding-stack-start*
    *control-stack-start*
    *control-stack-end*

    ;; interrupt handling
    *free-interrupt-context-index*
    sb!unix::*interrupts-enabled*
    sb!unix::*interrupt-pending*
    *gc-inhibit*
    *gc-pending*
    #!-sb-thread
    *stepping*

    ;; hash table weaknesses
    :key
    :value
    :key-and-value
    :key-or-value))

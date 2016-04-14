;;;; code for handling UNIX signals

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!UNIX")

(defmacro with-interrupt-bindings (&body body)
  `(let*
       ;; KLUDGE: Whatever is on the PCL stacks before the interrupt
       ;; handler runs doesn't really matter, since we're not on the
       ;; same call stack, really -- and if we don't bind these (esp.
       ;; the cache one) we can get a bogus metacircle if an interrupt
       ;; handler calls a GF that was being computed when the interrupt
       ;; hit.
       ((sb!pcl::*cache-miss-values-stack* nil)
        (sb!pcl::*dfun-miss-gfs-on-stack* nil))
     ,@body))

;;; Evaluate CLEANUP-FORMS iff PROTECTED-FORM does a non-local exit.
(defmacro nlx-protect (protected-form &rest cleanup-froms)
  (with-unique-names (completep)
    `(let ((,completep nil))
       (without-interrupts
         (unwind-protect
              (progn
                (allow-with-interrupts
                  ,protected-form)
                (setq ,completep t))
           (unless ,completep
             ,@cleanup-froms))))))

(defun invoke-interruption (function)
  (without-interrupts
    ;; Reset signal mask: the C-side handler has blocked all
    ;; deferrable signals before funcalling into lisp. They are to be
    ;; unblocked the first time interrupts are enabled. With this
    ;; mechanism there are no extra frames on the stack from a
    ;; previous signal handler when the next signal is delivered
    ;; provided there is no WITH-INTERRUPTS.
    (let ((*unblock-deferrables-on-enabling-interrupts-p* t)
          (sb!debug:*stack-top-hint* (or sb!debug:*stack-top-hint* 'invoke-interruption)))
      (with-interrupt-bindings
        (sb!thread::without-thread-waiting-for (:already-without-interrupts t)
          (allow-with-interrupts
            (nlx-protect (funcall function)
                         ;; We've been running with deferrables
                         ;; blocked in Lisp called by a C signal
                         ;; handler. If we return normally the sigmask
                         ;; in the interrupted context is restored.
                         ;; However, if we do an nlx the operating
                         ;; system will not restore it for us.
                         (when *unblock-deferrables-on-enabling-interrupts-p*
                           ;; This means that storms of interrupts
                           ;; doing an nlx can still run out of stack.
                           (unblock-deferrable-signals)))))))))

(defmacro in-interruption ((&key) &body body)
  #!+sb-doc
  "Convenience macro on top of INVOKE-INTERRUPTION."
  `(dx-flet ((interruption () ,@body))
     (invoke-interruption #'interruption)))

;;;; system calls that deal with signals

;;; Send the signal SIGNAL to the process with process id PID. SIGNAL
;;; should be a valid signal number
#!-sb-fluid (declaim (inline unix-kill))
(define-alien-routine ("kill" unix-kill) int
  (pid int)
  (signal int))

;;; Send the signal SIGNAL to the all the process in process group
;;; PGRP. SIGNAL should be a valid signal number
#!-sb-fluid (declaim (inline unix-killpg))
(define-alien-routine ("killpg" unix-killpg) int
  (pgrp int)
  (signal int))

;;; Reset the current set of masked signals (those being blocked from
;;; delivery).
;;;
;;; (Note: CMU CL had a more general SIGSETMASK call and a SIGMASK
;;; operator to create masks, but since we only ever reset to 0, we no
;;; longer support it. If you need it, you can pull it out of the CMU
;;; CL sources, or the old SBCL sources; but you might also consider
;;; doing things the SBCL way and moving this kind of C-level work
;;; down to C wrapper functions.)

(declaim (inline %unblock-deferrable-signals %unblock-gc-signals))
(define-alien-routine ("unblock_deferrable_signals"
                       %unblock-deferrable-signals)
  void
  (where unsigned-long)
  (old unsigned-long))
#!-sb-safepoint
(define-alien-routine ("unblock_gc_signals" %unblock-gc-signals)
    void
  (where unsigned-long)
  (old unsigned-long))

(defun unblock-deferrable-signals ()
  (%unblock-deferrable-signals 0 0))

#!-sb-safepoint
(defun unblock-gc-signals ()
  (%unblock-gc-signals 0 0))


;;;; C routines that actually do all the work of establishing signal handlers
(define-alien-routine ("install_handler" install-handler)
  unsigned-long
  (signal int)
  (handler unsigned-long)
  (synchronous boolean))

;;;; interface to enabling and disabling signal handlers

;;; Note on the SYNCHRONOUS argument: On builds without pseudo-atomic,
;;; we have no way of knowing whether interrupted code was in an
;;; allocation sequence, and cannot delay signals until after
;;; allocation.  Any signal that can occur asynchronously must be
;;; considered unsafe for immediate execution, and the invocation of its
;;; lisp handler will get delayed into a newly spawned signal handler
;;; thread.  However, there are signals which we must handle
;;; immediately, because they occur synchonously (hence the boolean flag
;;; SYNCHRONOUS to this function), luckily implying that the signal
;;; happens only in specific places (illegal instructions, floating
;;; point instructions, certain system calls), hopefully ruling out the
;;; possibility that we would trigger it during allocation.

(defun enable-interrupt (signal handler &key synchronous)
  (declare (type (or function fixnum (member :default :ignore)) handler))
  (/show0 "enable-interrupt")
  (flet ((run-handler (&rest args)
           (declare (truly-dynamic-extent args))
           (in-interruption ()
             (apply handler args))))
    (without-gcing
      (let ((result (install-handler signal
                                     (case handler
                                       (:default sig-dfl)
                                       (:ignore sig-ign)
                                       (t
                                        (sb!kernel:get-lisp-obj-address
                                         #'run-handler)))
                                     synchronous)))
        (cond ((= result sig-dfl) :default)
              ((= result sig-ign) :ignore)
              (t ;; MAKE-LISP-OBJ returns 2 values, which gets
                 ;; "too complex to check". We don't want the second value.
               (values (the (or function fixnum)
                         (sb!kernel:make-lisp-obj result)))))))))

(defun default-interrupt (signal)
  (enable-interrupt signal :default))

(defun ignore-interrupt (signal)
  (enable-interrupt signal :ignore))

;;;; Support for signal handlers which aren't.
;;;;
;;;; On safepoint builds, user-defined Lisp signal handlers do not run
;;;; in the handler for their signal, because we have no pseudo atomic
;;;; mechanism to prevent handlers from hitting during allocation.
;;;; Rather, the signal spawns off a fresh native thread, which calls
;;;; into lisp with a fake context through this callback:

#!+(and sb-safepoint-strictly (not win32))
(defun signal-handler-callback (run-handler signal args)
  ;; SAPs are dx allocated, close over the values, not the SAPs.
  (let ((info (sap-ref-sap args 0))
        (context (sap-ref-sap args sb!vm:n-word-bytes)))
    (sb!thread::initial-thread-function-trampoline
     (sb!thread::make-signal-handling-thread :name "signal handler"
                                             :signal-number signal)
     nil (lambda ()
           (funcall run-handler signal info context))
     nil
     nil nil nil nil)))


;;;; default LISP signal handlers
;;;;
;;;; Most of these just call ERROR to report the presence of the signal.

;;; SIGINT is handled like BREAK, except that ANSI BREAK ignores
;;; *DEBUGGER-HOOK*, but we want SIGINT's BREAK to respect it, so that
;;; SIGINT in --disable-debugger mode will cleanly terminate the system
;;; (by respecting the *DEBUGGER-HOOK* established in that mode).
(eval-when (:compile-toplevel :execute)
  (sb!xc:defmacro define-signal-handler (name what &optional (function 'error))
    `(defun ,name (signal info context)
       (declare (ignore signal info))
       (declare (type system-area-pointer context))
       (/show "in Lisp-level signal handler" ,(symbol-name name)
              (sap-int context))
       (with-interrupts
         (,function ,(concatenate 'simple-string what " at #X~X")
                    (with-alien ((context (* os-context-t) context))
                      (sap-int (sb!vm:context-pc context))))))))

(define-signal-handler sigill-handler "illegal instruction")
#!-(or linux android)
(define-signal-handler sigemt-handler "SIGEMT")
(define-signal-handler sigbus-handler "bus error")
#!-(or linux android)
(define-signal-handler sigsys-handler "bad argument to a system call")

(defun sigint-handler (signal info context)
  (declare (ignore signal info))
  (declare (type system-area-pointer context))
  (/show "in Lisp-level SIGINT handler" (sap-int context))
  ;; Copy CONTEXT, since the SAP is stack allocated and it's going
  ;; to be passed to another thread. See the below comment on the
  ;; general idea whether it's a good thing to do at all.
  (let ((context (int-sap (sap-int context))))
    (flet ((interrupt-it ()
             ;; This seems wrong to me on multi-threaded builds.  The
             ;; closed-over signal context belongs to a SIGINT handler.
             ;; But this function gets run through INTERRUPT-THREAD,
             ;; i.e. in in a SIGPIPE handler, at a different point in time
             ;; or even a different thread.  How do we know that the
             ;; SIGINT's context structure from the other thread is still
             ;; alive and meaningful?  Why do we care?  If we even need
             ;; the context and PC, shouldn't they come from the SIGPIPE's
             ;; context? --DFL
             (with-alien ((context (* os-context-t) context))
               (with-interrupts
                 (let ((int (make-condition 'interactive-interrupt
                                            :context context
                                            :address (sap-int (sb!vm:context-pc context)))))
                   ;; First SIGNAL, so that handlers can run.
                   (signal int)
                   ;; Then enter the debugger like BREAK.
                   (%break 'sigint int))))))
      #!+sb-safepoint
      (let ((target (sb!thread::foreground-thread)))
        ;; Note that INTERRUPT-THREAD on *CURRENT-THREAD* doesn't actually
        ;; interrupt right away, because deferrables are blocked.  Rather,
        ;; the kernel would arrange for the SIGPIPE to hit when the SIGINT
        ;; handler is done.  However, on safepoint builds, we don't use
        ;; SIGPIPE and lack an appropriate mechanism to handle pending
        ;; thruptions upon exit from signal handlers (and this situation is
        ;; unlike WITHOUT-INTERRUPTS, which handles pending interrupts
        ;; explicitly at the end).  Only as long as safepoint builds pretend
        ;; to cooperate with signals -- that is, as long as SIGINT-HANDLER
        ;; is used at all -- detect this situation and work around it.
        (if (eq target sb!thread:*current-thread*)
            (interrupt-it)
            (sb!thread:interrupt-thread target #'interrupt-it)))
      #!-sb-safepoint
      (sb!thread:interrupt-thread (sb!thread::foreground-thread)
                                  #'interrupt-it))))

#!-sb-wtimer
(defun sigalrm-handler (signal info context)
  (declare (ignore signal info context))
  (declare (type system-area-pointer context))
  (sb!impl::run-expired-timers))

(defun sigterm-handler (signal code context)
  (declare (ignore signal code context))
  (exit))

#!-sb-thruption
;;; SIGPIPE is not used in SBCL for its original purpose, instead it's
;;; for signalling a thread that it should look at its interruption
;;; queue. The handler (RUN_INTERRUPTION) just returns if there is
;;; nothing to do so it's safe to receive spurious SIGPIPEs coming
;;; from the kernel.
(defun sigpipe-handler (signal code context)
  (declare (ignore signal code context))
  (sb!thread::run-interruption))

;;; the handler for SIGCHLD signals for RUN-PROGRAM
(defun sigchld-handler  (signal code context)
  (declare (ignore signal code context))
  (sb!impl::get-processes-status-changes))

(defun sb!kernel:signal-cold-init-or-reinit ()
  #!+sb-doc
  "Enable all the default signals that Lisp knows how to deal with."
  (enable-interrupt sigint #'sigint-handler)
  (enable-interrupt sigterm #'sigterm-handler)
  (enable-interrupt sigill #'sigill-handler :synchronous t)
  #!-(or linux android)
  (enable-interrupt sigemt #'sigemt-handler)
  (enable-interrupt sigfpe #'sb!vm:sigfpe-handler :synchronous t)
  (enable-interrupt sigbus #'sigbus-handler :synchronous t)
  #!-(or linux android)
  (enable-interrupt sigsys #'sigsys-handler :synchronous t)
  #!-sb-wtimer
  (enable-interrupt sigalrm #'sigalrm-handler)
  #!-sb-thruption
  (enable-interrupt sigpipe #'sigpipe-handler)
  (enable-interrupt sigchld #'sigchld-handler)
  #!+hpux (ignore-interrupt sigxcpu)
  #!-sb-safepoint (unblock-gc-signals)
  (unblock-deferrable-signals)
  (values))

;;;; etc.

;;; extract si_code from siginfo_t
(define-alien-routine ("siginfo_code" siginfo-code) int
  (info system-area-pointer))

;;; CMU CL comment:
;;;   Magically converted by the compiler into a break instruction.
(defun receive-pending-interrupt ()
  (receive-pending-interrupt))

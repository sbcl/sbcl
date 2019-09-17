;;;; code for handling UNIX signals

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-UNIX")

;;;; system calls that deal with signals

;;; Send the signal SIGNAL to the process with process id PID. SIGNAL
;;; should be a valid signal number
#-sb-fluid (declaim (inline unix-kill))
(define-alien-routine ("kill" unix-kill) int
  (pid int)
  (signal int))

;;; Send the signal SIGNAL to the all the process in process group
;;; PGRP. SIGNAL should be a valid signal number
#-sb-fluid (declaim (inline unix-killpg))
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

#-sb-safepoint
(defun unblock-gc-signals ()
  (with-alien ((%unblock-gc-signals (function void) :extern "unblock_gc_signals"))
    (alien-funcall %unblock-gc-signals)
    nil))


;;;; C routines that actually do all the work of establishing signal handlers
(define-alien-routine ("install_handler" install-handler)
  unsigned-long
  (signal int)
  (handler unsigned-long)
  (ohandler unsigned-long)
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
           #-(or c-stack-is-control-stack sb-safepoint) ;; able to do that in interrupt_handle_now()
           (unblock-gc-signals)
           (in-interruption ()
             (apply handler args))))
    (dx-let ((ohandler (make-array 1 :initial-element nil)))
      ;; Pin OHANDLER in case the backend heap-allocates it
      (with-pinned-objects (#'run-handler ohandler)
        ;; 0 and 1 probably coincide with SIG_DFL and SIG_IGN, but those
        ;; constants are opaque. We use our own explicit translation
        ;; of them in the C install_handler() argument and return convention.
        (let ((result (install-handler
                       signal
                       (case handler
                         (:default 0)
                         (:ignore 1)
                         (t (sb-kernel:get-lisp-obj-address #'run-handler)))
                       ;; VECTOR-SAP does not work on SIMPLE-VECTOR
                       (sb-kernel:get-lisp-obj-address ohandler)
                       synchronous)))
          (cond ((= result 0) :default)
                ((= result 1) :ignore)
                (t
                 ;; The value in OHANDLER, if a lisp function, is not the right thing to
                 ;; return, but we do it anyway. It's always the RUN-HANDLER closure
                 ;; instead of what was supplied before as HANDLER. We can only hope that
                 ;; users don't pass the result of ENABLE-INTERRUPT as the argument to
                 ;; another call, as that would create a chain of closures.
                 ;; I wonder if the fact that we at some point decided that we need
                 ;; to allow signal nesting to about 1024 levels deep had anything
                 ;; to do with this bug?
                 (the (or function fixnum) (aref ohandler 0)))))))))

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

#+sb-safepoint-strictly
(defun signal-handler-callback (run-handler signal args)
  ;; SAPs are dx allocated, close over the values, not the SAPs.
  (let ((thread (without-gcing
                  ;; Hold off GCing until *current-thread* is set up
                  (setf sb-thread:*current-thread*
                        (sb-thread::make-signal-handling-thread :name "signal handler"
                                                                :signal-number signal))))
        (info (sap-ref-sap args 0))
        (context (sap-ref-sap args sb-vm:n-word-bytes)))
    (dx-flet ((callback ()
                (funcall run-handler signal info context)))
      (sb-thread::new-lisp-thread-trampoline thread nil #'callback nil))))


;;;; default LISP signal handlers
;;;;
;;;; Most of these just call ERROR to report the presence of the signal.

;;; SIGINT is handled like BREAK, except that ANSI BREAK ignores
;;; *DEBUGGER-HOOK*, but we want SIGINT's BREAK to respect it, so that
;;; SIGINT in --disable-debugger mode will cleanly terminate the system
;;; (by respecting the *DEBUGGER-HOOK* established in that mode).
(macrolet
  ((define-signal-handler (name what &optional (function 'error))
    `(defun ,name (signal info context)
       (declare (ignore signal info))
       (declare (type system-area-pointer context))
       (/show "in Lisp-level signal handler" ,(symbol-name name)
              (sap-int context))
       (with-interrupts
         (,function ,(concatenate 'simple-string what " at #X~X")
                    (with-alien ((context (* os-context-t) context))
                      (sap-int (sb-vm:context-pc context))))))))

(define-signal-handler sigill-handler "illegal instruction")
#-(or linux android haiku)
(define-signal-handler sigemt-handler "SIGEMT")
(define-signal-handler sigbus-handler "bus error")
#-(or linux android)
(define-signal-handler sigsys-handler "bad argument to a system call")
) ; end MACROLET

(defun sigint-handler (signal info
                       sb-kernel:*current-internal-error-context*)
  (declare (ignore signal info))
  (flet ((interrupt-it ()
           ;; SB-KERNEL:*CURRENT-INTERNAL-ERROR-CONTEXT* will
           ;; either be bound in this thread by SIGINT-HANDLER or
           ;; in the target thread by SIGPIPE-HANDLER.
           (with-alien ((context (* os-context-t)
                                 sb-kernel:*current-internal-error-context*))
             (with-interrupts
               (let ((int (make-condition 'interactive-interrupt
                                          :context context
                                          :address (sap-int (sb-vm:context-pc context)))))
                 ;; First SIGNAL, so that handlers can run.
                 (signal int)
                 ;; Then enter the debugger like BREAK.
                 (%break 'sigint int))))))
    #+sb-safepoint
    (let ((target (sb-thread::foreground-thread)))
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
      (if (eq target sb-thread:*current-thread*)
          (interrupt-it)
          (sb-thread:interrupt-thread target #'interrupt-it)))
    #-sb-safepoint
    (sb-thread:interrupt-thread (sb-thread::foreground-thread)
                                #'interrupt-it)))

#-sb-wtimer
(defun sigalrm-handler (signal info context)
  (declare (ignore signal info context))
  (declare (type system-area-pointer context))
  (sb-impl::run-expired-timers))

(defun sigterm-handler (signal code context)
  (declare (ignore signal code context))
  (exit))

#-sb-thruption
;;; SIGPIPE is not used in SBCL for its original purpose, instead it's
;;; for signalling a thread that it should look at its interruption
;;; queue. The handler (RUN_INTERRUPTION) just returns if there is
;;; nothing to do so it's safe to receive spurious SIGPIPEs coming
;;; from the kernel.
(defun sigpipe-handler (signal code sb-kernel:*current-internal-error-context*)
  (declare (ignore signal code))
  (sb-thread::run-interruption))

;;; the handler for SIGCHLD signals for RUN-PROGRAM
(defun sigchld-handler  (signal code context)
  (declare (ignore signal code context))
  (sb-impl::get-processes-status-changes))

(defun sb-kernel:signal-cold-init-or-reinit ()
  "Enable all the default signals that Lisp knows how to deal with."
  (enable-interrupt sigint #'sigint-handler)
  (enable-interrupt sigterm #'sigterm-handler)
  (enable-interrupt sigill #'sigill-handler :synchronous t)
  #-(or linux android haiku)
  (enable-interrupt sigemt #'sigemt-handler)
  (enable-interrupt sigfpe #'sb-vm:sigfpe-handler :synchronous t)
  (if (/= (extern-alien "install_sig_memory_fault_handler" int) 0)
      (enable-interrupt sigbus #'sigbus-handler :synchronous t)
      (write-string ";;;; SIGBUS handler not installed
" sb-sys:*stderr*))
  #-(or linux android)
  (enable-interrupt sigsys #'sigsys-handler :synchronous t)
  #-sb-wtimer
  (enable-interrupt sigalrm #'sigalrm-handler)
  #-sb-thruption
  (enable-interrupt sigpipe #'sigpipe-handler)
  (enable-interrupt sigchld #'sigchld-handler)
  #+hpux (ignore-interrupt sigxcpu)
  #-sb-safepoint (unblock-gc-signals)
  (unblock-deferrable-signals)
  (values))

;;;; etc.

;;; extract si_code from siginfo_t
(define-alien-routine ("siginfo_code" siginfo-code) int
  (info system-area-pointer))

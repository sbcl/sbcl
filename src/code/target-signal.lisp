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
(declaim (inline unix-kill))
(define-alien-routine ("kill" unix-kill) int
  (pid int)
  (signal int))

;;; Send the signal SIGNAL to the all the process in process group
;;; PGRP. SIGNAL should be a valid signal number
(declaim (inline unix-killpg))
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

;;; FIXME: terrible name. doesn't actually "enable" in the sense of unmasking.
(defun enable-interrupt (signal handler &key synchronous)
  (declare (type (or function (member :default :ignore)) handler))
  (when synchronous
    (error ":SYNCHRONOUS is broken and should not be used"))
  (%install-handler signal handler)
  ;; This used to return the previously installed handler, if any.
  ;; It no longer does, but the old handler can be obtained via SAP-REF-LISPOBJ
  ;; on 'lisp_sig_handlers'. The reason for not returning it is that the value was
  ;; always a closure, and there's no easy/portable way to ask what function was
  ;; really meant.  And you can't use that closure to reinstall the old thing
  ;; because it would install a new closure on the old closure which, though
  ;; feasible, is definitely not what was intended.
  nil)

(defun %install-handler (signal handler)
  (flet ((run-handler (signo info-sap context-sap)
           #-(or c-stack-is-control-stack sb-safepoint) ;; able to do that in interrupt_handle_now()
           (unblock-gc-signals)
           (in-interruption ()
             (funcall handler signo info-sap context-sap))))
    (with-pinned-objects (#'run-handler)
      ;; 0 and 1 probably coincide with SIG_DFL and SIG_IGN, but those
      ;; constants are opaque. We use our own explicit translation
      ;; of them in the C install_handler() argument convention.
      (with-alien ((%sigaction (function void int unsigned) :extern "install_handler"))
        (alien-funcall %sigaction
                       signal
                       (case handler
                         (:default 0)
                         (:ignore 1)
                         (t (sb-kernel:get-lisp-obj-address #'run-handler)))))))
  nil)

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
      (sb-thread::run thread nil #'callback nil))))


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
           ;; in the target thread by SIGURG-HANDLER.
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
      ;; the kernel would arrange for the SIGURG to hit when the SIGINT
      ;; handler is done.  However, on safepoint builds, we don't use
      ;; SIGURG and lack an appropriate mechanism to handle pending
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
;;; SIGURG is not used in SBCL for its original purpose, instead it's
;;; for signalling a thread that it should look at its interruption
;;; queue. The handler (RUN_INTERRUPTION) just returns if there is
;;; nothing to do so it's safe to receive spurious SIGURGs coming
;;; from the kernel.
(defun sigurg-handler (signal code sb-kernel:*current-internal-error-context*)
  (declare (ignore signal code))
  (sb-thread::run-interruption))

;;; the handler for SIGCHLD signals for RUN-PROGRAM
(defun sigchld-handler  (signal code context)
  (declare (ignore signal code context))
  (sb-impl::get-processes-status-changes))

(defmacro pthread-sigmask (how new old)
  `(let ((how ,how) (new ,new) (old ,old))
     (alien-funcall (extern-alien
                     #+sb-thread ,(or #+unix "pthread_sigmask" #-unix "sb_pthread_sigmask")
                     #-sb-thread ,(or #+netbsd "sb_sigprocmask" #-netbsd "sigprocmask")
                     (function void int system-area-pointer system-area-pointer))
                    how
                    (cond ((system-area-pointer-p new) new)
                          (new (vector-sap new))
                          (t (int-sap 0)))
                    (if old (vector-sap old) (int-sap 0)))))

(defun sb-kernel:signal-cold-init-or-reinit ()
  "Enable all the default signals that Lisp knows how to deal with."
  (%install-handler sigint #'sigint-handler)
  (%install-handler sigterm #'sigterm-handler)
  (%install-handler sigill #'sigill-handler)
  #-(or linux android haiku) (%install-handler sigemt #'sigemt-handler)
  (%install-handler sigfpe #'sb-vm:sigfpe-handler)
  (if (/= (extern-alien "install_sig_memory_fault_handler" int) 0)
      (%install-handler sigbus #'sigbus-handler)
      (write-string ";;;; SIGBUS handler not installed
" sb-sys:*stderr*))
  #-(or linux android) (%install-handler sigsys #'sigsys-handler)
  #-sb-wtimer (%install-handler sigalrm #'sigalrm-handler)
  #-sb-thruption (%install-handler sigurg #'sigurg-handler)
  (%install-handler sigchld #'sigchld-handler)
  ;; Don't want to silently quit on broken pipes.
  (%install-handler sigpipe :ignore)
  ;; Undo the effect of block_blockable_signals() from right at the top of sbcl_main()
  ;; and (if pertinent) blocking stop-for-GC somewhere thereafter.
  (dx-let ((mask (make-array sb-unix::sizeof-sigset_t :element-type '(unsigned-byte 8)
                                                      :initial-element 0)))
    (with-pinned-objects (mask)
      (pthread-sigmask sb-unix::SIG_SETMASK mask nil)))
  (values))

;;;; etc.

;;; extract si_code from siginfo_t
(define-alien-routine ("siginfo_code" siginfo-code) int
  (info system-area-pointer))

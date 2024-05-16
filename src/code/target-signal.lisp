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

;;; A macro, because OS-THREAD is a WORD which could cause boxing if passed
;;; to a function.
(defmacro pthread-kill (os-thread signal)
  (declare (ignorable os-thread))
  ;; If no threads, pthread_kill() won't exist since we didn't link with -lpthread.
  ;; And raise() - as we use it - can't fail, so just ignore the result.
  ;; (The signal number is always SIGURG or SIGINT, and the process is obviously not dead)
  ;; This isn't used on win32 so we don't need to change the symbol to sb_pthr_kill there.
  #-sb-thread `(raise ,signal)
  #+sb-thread
  `(unless (= 0 (alien-funcall (extern-alien "sb_thread_kill"
                                             (function int unsigned int))
                               ,os-thread ,signal))
     (error "pthread_kill() failed")))

;;; raise() is defined to send the signal to pthread_self() if multithreaded.
(defmacro raise (signal)
  `(alien-funcall (extern-alien "raise" (function int int)) ,signal))

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
(defun unblock-stop-for-gc-signal ()
  (with-alien ((%unblock (function void) :extern "unblock_gc_stop_signal"))
    (alien-funcall %unblock)
    nil))

;;;; interface to installing signal handlers

(defun enable-interrupt (signal handler)
  (declare (type (or function (member :default :ignore)) handler))
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
  ;; 0 and 1 should coincide with SIG_DFL and SIG_IGN, but in theory those values
  ;; are opaque. We use our own explicit translation of 0 and 1 to them
  ;; in the C install_handler() argument passing convention.
  (with-alien ((%sigaction (function void int unsigned) :extern "install_handler"))
    #+sb-safepoint
    (alien-funcall %sigaction signal
                   (case handler
                     (:default 0)
                     (:ignore 1)
                     (t (sb-kernel:get-lisp-obj-address handler))))
    #-sb-safepoint
    (flet ((run-handler (signo info-sap context-sap)
             #-(or c-stack-is-control-stack sb-safepoint) ;; able to do that in interrupt_handle_now()
             (unblock-stop-for-gc-signal)
             (in-interruption () (funcall handler signo info-sap context-sap))))
      (with-pinned-objects (#'run-handler)
        (alien-funcall %sigaction signal
                       (case handler
                         (:default 0)
                         (:ignore 1)
                         (t (sb-kernel:get-lisp-obj-address #'run-handler)))))))
  nil)


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

(defun sigterm-handler (signal code context)
  (declare (ignore signal code context))
  (exit))

#-sb-safepoint
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
  (when (fboundp 'sigalrm-handler) ; defined in warm build
    (%install-handler sigalrm (symbol-function 'sigalrm-handler)))
  #-sb-safepoint (%install-handler sigurg #'sigurg-handler)
  (%install-handler sigchld #'sigchld-handler)
  ;; Don't want to silently quit on broken pipes.
  (%install-handler sigpipe :ignore)
  ;; Undo the effect of block_blockable_signals() from right at the top of sbcl_main()
  ;; and (if pertinent) blocking stop-for-GC somewhere thereafter.
  (dx-let ((mask (make-array sizeof-sigset_t :element-type '(unsigned-byte 8)
                                             :initial-element 0)))
    (with-pinned-objects (mask)
      #+(and unix sb-safepoint)
      ;; For safepoints we unblock SIGURG (to receive interrupt-thread),
      ;; SIGPROF (because why not), and SIGPIPE (because it's synchronous
      ;; and not in deferrables). Everything else stays blocked.
      (with-alien ((sigaddset (function int system-area-pointer int) :extern "sigaddset"))
        (alien-funcall sigaddset (vector-sap mask) sigurg)
        (alien-funcall sigaddset (vector-sap mask) sigprof)
        (alien-funcall sigaddset (vector-sap mask) sigpipe)
        (pthread-sigmask SIG_UNBLOCK mask nil))
      ;; The normal thing is to start with no signals blocked
      #-(and unix sb-safepoint) (pthread-sigmask SIG_SETMASK mask nil)))
  (values))

;;;; etc.

;;; extract si_code from siginfo_t
(define-alien-routine ("siginfo_code" siginfo-code) int
  (info system-area-pointer))

;;;; On safepoint builds, user-defined Lisp signal handlers all run
;;;; in one thread that waits for signals.
;;;; Keyboard interrupts work by forwarding SIGINT from that thread
;;;; to the foreground thread via INTERRUPT-THREAD, which is the only
;;;; quasi-asynchronous signal allowed to run in arbitrary threads.
#+sb-safepoint
(progn
(define-load-time-global *sighandler-thread* nil)
(declaim (type (or sb-thread:thread null) *sighandler-thread*))
(defun signal-handler-loop ()
  ;; We could potentially use sigwaitinfo() to obtain more information about the signal,
  ;; but I don't see the point. This is just minimal functionality.
  (with-alien ((sigwait (function int system-area-pointer (* int)) :extern "sigwait")
               (mask (array (unsigned 8) #.sizeof-sigset_t))
               (num int))
    (pthread-sigmask SIG_BLOCK nil (alien-sap mask)) ; Retrieve current mask
    (loop (let ((result (alien-funcall sigwait (alien-sap mask) (addr num))))
            (when (and (= result 0) (= num sigterm) (not *sighandler-thread*))
              (return))
            (when (and (= result 0) (> num 0))
              (let ((fun (sap-ref-lispobj (foreign-symbol-sap "lisp_sig_handlers" t)
                                          (ash num sb-vm:word-shift))))
                (when (functionp fun)
                  (funcall fun num nil nil)))))))))

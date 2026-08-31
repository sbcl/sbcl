;;;; The common stuff for signals and exceptions (win32).

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS

(in-package "SB-UNIX")

;;; CMU CL comment:
;;;   Magically converted by the compiler into a break instruction.
(defun receive-pending-interrupt ()
  (receive-pending-interrupt))

(sb-impl:define-thread-local sb-pcl::*cache-miss-values-stack* nil)
(sb-impl:define-thread-local sb-pcl::*dfun-miss-gfs-on-stack* nil)

;;; TODO: this macro should probably disappear from the pristine image, right?
;;; (well it doesn't)
(defmacro with-interrupt-bindings ((style) &body body)
  (declare (type (member :synchronous :asynchronous) style)
           (ignorable style))
       ;; KLUDGE: Whatever is on the PCL stacks before the interrupt
       ;; handler runs doesn't really matter, since we're not on the
       ;; same call stack, really -- and if we don't bind these (esp.
       ;; the cache one) we can get a bogus metacircle if an interrupt
       ;; handler calls a GF that was being computed when the interrupt
       ;; hit.
  #-tls-based-mv-return
  `(let*
       ((sb-pcl::*cache-miss-values-stack* nil)
        (sb-pcl::*dfun-miss-gfs-on-stack* nil))
     ,@body)
  #+tls-based-mv-return
  ;; The multi-value return area needs to be preserved around async interrupts
  ;; and each slot has the nature of a distinct special binding.
  ;; In theory these could go on the binding stack since they are accessible
  ;; at TLS indices - from C as well - however I prefer to use only 1 cleanup for
  ;; them all. We have at least 5 layers of cleanup now (WITHOUT-ARENA,
  ;; WITHOUT-THREAD-WAITING-FOR, NLX-PROTECT, plus bindings in WITHOUT-INTERRUPTS
  ;; and here) and so I think that because INTERRUPT-THREAD is questionable to use
  ;; in production code in the first place, performance is not a consideration.
  ;; What's one more cleanup? Let's use UWP to simulate an array of bindings.
  ;; Also note that as we're inside WITHOUT-INTERRUPTS, scanning the MV area
  ;; from Lisp is immune to further destruction at this point.
  (ecase style
    (:synchronous
     ;; Moving values between TLS <-> control stack won't incur synchronous traps,
     ;; so use the simpler #-tls-based-mv-return code. Sorry for the repetition
     `(let* ((sb-pcl::*cache-miss-values-stack* nil)
             (sb-pcl::*dfun-miss-gfs-on-stack* nil))
        ,@body))
    (:asynchronous
     (flet ((mv-count-byte ()
              `(sap-ref-8 (sb-thread:current-thread-sap)
                          ,(1+ (ash sb-vm:thread-state-word-slot
                                    sb-vm:word-shift))))
            (spill/restore (form)
              `(loop for i from ,(ash sb-vm::thread-mv-return-values-slot
                                      sb-vm:word-shift) by ,sb-vm:n-word-bytes
                     for j below (length mv-spill)
                     do ,form))
            (mv-slot-at (n)
              `(sap-ref-lispobj (sb-thread:current-thread-sap) ,n)))
       `(let* ((mv-count ,(mv-count-byte))
               (mv-spill (make-array
                          (truly-the (mod #.sb-xc:multiple-values-limit)
                                     (max 0 (- mv-count ,sb-vm::register-arg-count)))))
               (sb-pcl::*cache-miss-values-stack* nil) ; and again
               (sb-pcl::*dfun-miss-gfs-on-stack* nil))
          (declare (dynamic-extent mv-spill))
          ,(spill/restore `(setf (aref mv-spill j) ,(mv-slot-at 'i)))
          (unwind-protect (progn ,@body)
            (setf ,(mv-count-byte) mv-count)
            ,(spill/restore `(setf ,(mv-slot-at 'i) (aref mv-spill j)))))))))

(defun unblock-deferrable-signals ()
  (with-alien ((%unblock-deferrable-signals
                (function void unsigned-long) :extern
                "unblock_deferrable_signals"))
    (alien-funcall %unblock-deferrable-signals 0)
    nil))

(defun with-deferrable-signals-unblocked (enable-interrupts function)
  (cond ((and enable-interrupts
              *unblock-deferrables-on-enabling-interrupts-p*)
         (unwind-protect
              (let (*unblock-deferrables-on-enabling-interrupts-p*)
                (unblock-deferrable-signals)
                (when (or *interrupt-pending*
                          #+sb-safepoint *thruption-pending*)
                  (receive-pending-interrupt))
                (funcall function))
           (alien-funcall (extern-alien "block_deferrable_signals"
                                        (function (values) int))
                          0)))
        (t
         (when (and enable-interrupts
                    (or *interrupt-pending*
                        #+sb-safepoint *thruption-pending*))
           (receive-pending-interrupt))
         (funcall function))))

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
                    (cond ((system-area-pointer-p old) old)
                          (old (vector-sap old))
                          (t (int-sap 0))))))

;;; TLDR: everything about signals and NLX is dangerous.
;;; Despite the abudance of caution below (notably NLX-PROTECT),
;;; nonlocal exit through a signal handler is potentially crash-prone due to a
;;; lingering garbage word in the topmost entry of sigcontexts[] for the thread.
;;; undo_fake_foreign_function_call used to have a comment stressing the importance
;;; of zeroing the array element corresponding to *FREE-INTERRUPT-CONTEXT-INDEX*.
;;; The index itself is correctly decremented by 1 via special unbinding,
;;; whether normally or nonlocally, so that's fine.  However the change adding the
;;; comment about nullptr (rev af3be8e1) addressed a slightly different problem, namely,
;;; if you increment the index first, then GC could see a 0 where there should be a
;;; non-null pointer. So the question remains: if you decrement the context index without
;;; zeroing the unused word, and the next interrupt increments the index before storing
;;; the new context pointer, can GC be confused by seeing an old value as an alleged
;;; context pointer? With precise GC, it certainly seems possible. With conservative GC,
;;; it is unlikely to be a problem as long as the pointer itself is valid, though
;;; even that can see a dangling pointer in one of two ways: (1) the pointer itself
;;; could be to anywhere, because it is unspecified where the kernel creates sigcontexts.
;;; (2) on macOS for x86-64, a sigcontext contains *pointers* to machine context parts.
;;; See arch_os_context_mxcsr_addr() in x86-64-darwin-os.h for example. Therefore,
;;; a stale (alleged) context pointer can in fact contain wild pointers that
;;; must not be dereferenced.
;;;
;;; Possible fixes could be as simple as clobbering the sigcontext[] element in
;;; the *UNBLOCK-DEFERRABLES-ON-ENABLING-INTERRUPTS-P* flow below,
;;; or ensuring that we are always uninterruptible when adding to or removing
;;; from the array (which might already be true in fact).
;;; A final possibility would be to turn the array into a linked list where the cells
;;; are stack-allocated from C in the manner of DX_ALLOC_SAP, and in that situation
;;; rather than unbinding *FREE-INTERRUPT-CONTEXT-INDEX* we would unbind the special
;;; var that holds the current head of the linked list of contexts.
;;;
;;; And to add the to fun: although we literally advise against using INTERRUPT-THREAD
;;; for anything but debugging (in its docstring), we also advertise MAKE-TIMER which
;;; uses INTERRUPT-THREAD, and then in regression tests (e.g. hash-cache.pure) we make
;;; a timer that performs an NLX, which as per the above should not be done.
(defun invoke-interruption (function)
  (without-interrupts
    ;; Reset signal mask: the C-side handler has blocked all
    ;; deferrable signals before funcalling into lisp. They are to be
    ;; unblocked the first time interrupts are enabled. With this
    ;; mechanism there are no extra frames on the stack from a
    ;; previous signal handler when the next signal is delivered
    ;; provided there is no WITH-INTERRUPTS.
    (let ((*unblock-deferrables-on-enabling-interrupts-p* t)
          (sb-debug:*stack-top-hint* (or sb-debug:*stack-top-hint* 'invoke-interruption)))
      (sb-vm:without-arena
        (with-interrupt-bindings (:asynchronous)
          (sb-thread::without-thread-waiting-for (:already-without-interrupts t)
            (allow-with-interrupts
              (nlx-protect (funcall function)
                           ;; We've been running with blockable
                           ;; blocked in Lisp called by a C signal
                           ;; handler. If we return normally the sigmask
                           ;; in the interrupted context is restored.
                           ;; However, if we do an nlx the operating
                           ;; system will not restore it for us.
                           (when *unblock-deferrables-on-enabling-interrupts-p*
                             ;; This means that storms of interrupts
                             ;; doing an nlx can still run out of stack.
                             (pthread-sigmask SIG_UNBLOCK
                                              (foreign-symbol-sap "blockable_sigset" t)
                                              nil)))
              ;; The return value doesn't matter, just return 0
              0)))))))

(defmacro in-interruption ((&key) &body body)
  "Convenience macro on top of INVOKE-INTERRUPTION."
  `(dx-flet ((interruption () ,@body))
     (invoke-interruption #'interruption)))

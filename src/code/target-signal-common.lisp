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

(defmacro with-interrupt-bindings (&body body)
  `(let*
       ;; KLUDGE: Whatever is on the PCL stacks before the interrupt
       ;; handler runs doesn't really matter, since we're not on the
       ;; same call stack, really -- and if we don't bind these (esp.
       ;; the cache one) we can get a bogus metacircle if an interrupt
       ;; handler calls a GF that was being computed when the interrupt
       ;; hit.
       ((sb-pcl::*cache-miss-values-stack* nil)
        (sb-pcl::*dfun-miss-gfs-on-stack* nil))
     (declare (special sb-pcl::*cache-miss-values-stack*
                       sb-pcl::*dfun-miss-gfs-on-stack*))
     ,@body))

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
        (with-interrupt-bindings
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

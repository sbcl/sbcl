;;;; handling UNIX signals

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!UNIX")

;;;; macros for dynamically enabling and disabling signal handling

;;; Notes on how the without-interrupts/with-interrupts stuff works:
;;;
;;; Before invoking the supplied handler for any of the signals that
;;; can be blocked, the C interrupt support code checks to see whether
;;; *interrupts-enabled* has been bound to NIL. If so, it saves the
;;; signal number and the value of the signal mask (from the signal
;;; context), sets the signal mask to block all blockable signals,
;;; sets *interrupt-pending* and returns without handling the signal.
;;;
;;; When we drop out the without interrupts, we check to see whether
;;; *INTERRUPT-PENDING* has been set. If so, we call
;;; RECEIVE-PENDING-INTERRUPT, which generates a SIGTRAP. The C code
;;; invokes the handler for the saved signal instead of the SIGTRAP
;;; after replacing the signal mask in the signal context with the
;;; saved value. When that hander returns, the original signal mask is
;;; installed, allowing any other pending signals to be handled.
;;;
;;; This means that the cost of WITHOUT-INTERRUPTS is just a special
;;; binding in the case when no signals are delivered (the normal
;;; case). It's only when a signal is actually delivered that we use
;;; any system calls, and by then the cost of the extra system calls
;;; are lost in the noise when compared with the cost of delivering
;;; the signal in the first place.
;;;
;;; The conditional bindings done by this code here are worth the
;;; trouble as binding is more expensive then read & test -- so
;;;  (if *foo*
;;;      (foo)
;;;      (let ((*foo* t))
;;;        (foo)))
;;; is faster then
;;;  (let ((*foo* t))
;;;    (foo))
;;; provided that the first branch is true "often enough".

(!defvar *interrupts-enabled* t)
(!defvar *interrupt-pending* nil)
#!+sb-thruption (!defvar *thruption-pending* nil)
(!defvar *allow-with-interrupts* t)
;;; This is to support signal handlers that want to return to the
;;; interrupted context without leaving anything extra on the stack. A
;;; simple
;;;
;;;  (without-interrupts
;;;   (unblock-deferrable-signals)
;;;   (allow-with-interrupts ...))
;;;
;;; would not cut it, as upon leaving WITHOUT-INTERRUPTS the pending
;;; handlers is run with stuff from the function in which this is
;;; still on the stack.
(!defvar *unblock-deferrables-on-enabling-interrupts-p* nil)

(defmacro without-interrupts (&body body)
  #!+sb-doc
  "Executes BODY with all deferrable interrupts disabled. Deferrable
interrupts arriving during execution of the BODY take effect after BODY has
been executed.

Deferrable interrupts include most blockable POSIX signals, and
SB-THREAD:INTERRUPT-THREAD. Does not interfere with garbage collection, and
unlike in many traditional Lisps using userspace threads, in SBCL
WITHOUT-INTERRUPTS does not inhibit scheduling of other threads.

Binds ALLOW-WITH-INTERRUPTS and WITH-LOCAL-INTERRUPTS as a local macros.

ALLOW-WITH-INTERRUPTS allows the WITH-INTERRUPTS to take effect during the
dynamic scope of its body, unless there is an outer WITHOUT-INTERRUPTS without
a corresponding ALLOW-WITH-INTERRUPTS.

WITH-LOCAL-INTERRUPTS executes its body with interrupts enabled provided that
for there is an ALLOW-WITH-INTERRUPTS for every WITHOUT-INTERRUPTS surrounding
the current one. WITH-LOCAL-INTERRUPTS is equivalent to:

  (allow-with-interrupts (with-interrupts ...))

Care must be taken not to let either ALLOW-WITH-INTERRUPTS or
WITH-LOCAL-INTERRUPTS appear in a function that escapes from inside the
WITHOUT-INTERRUPTS in:

  (without-interrupts
    ;; The body of the lambda would be executed with WITH-INTERRUPTS allowed
    ;; regardless of the interrupt policy in effect when it is called.
    (lambda () (allow-with-interrupts ...)))

  (without-interrupts
    ;; The body of the lambda would be executed with interrupts enabled
    ;; regardless of the interrupt policy in effect when it is called.
    (lambda () (with-local-interrupts ...)))
"
  (with-unique-names (outer-allow-with-interrupts without-interrupts-body)
    `(dx-flet ((,without-interrupts-body ()
              (declare (disable-package-locks allow-with-interrupts
                                              with-local-interrupts))
              (macrolet
                  ((allow-with-interrupts
                     (&body allow-forms)
                     `(let ((*allow-with-interrupts*
                             ,',outer-allow-with-interrupts))
                        ,@allow-forms))
                   (with-local-interrupts
                     (&body with-forms)
                     `(let ((*allow-with-interrupts*
                             ,',outer-allow-with-interrupts)
                            (*interrupts-enabled*
                             ,',outer-allow-with-interrupts))
                        (when ,',outer-allow-with-interrupts
                          (when *unblock-deferrables-on-enabling-interrupts-p*
                            (setq *unblock-deferrables-on-enabling-interrupts-p*
                                  nil)
                            (sb!unix::unblock-deferrable-signals))
                          (when (or *interrupt-pending*
                                    #!+sb-thruption *thruption-pending*)
                            (receive-pending-interrupt)))
                        (locally ,@with-forms))))
                (let ((*interrupts-enabled* nil)
                      (,outer-allow-with-interrupts *allow-with-interrupts*)
                      (*allow-with-interrupts* nil))
                  (declare (ignorable ,outer-allow-with-interrupts))
                  (declare (enable-package-locks allow-with-interrupts
                                                 with-local-interrupts))
                  ,@body))))
       (if *interrupts-enabled*
           (unwind-protect
                (,without-interrupts-body)
             ;; If we were interrupted in the protected section,
             ;; then the interrupts are still blocked and it remains
             ;; so until the pending interrupt is handled.
             ;;
             ;; If we were not interrupted in the protected section,
             ;; but here, then even if the interrupt handler enters
             ;; another WITHOUT-INTERRUPTS, the pending interrupt will be
             ;; handled immediately upon exit from said
             ;; WITHOUT-INTERRUPTS, so it is as if nothing has happened.
             (when (or *interrupt-pending*
                       #!+sb-thruption *thruption-pending*)
               (receive-pending-interrupt)))
           (,without-interrupts-body)))))

(defmacro with-interrupts (&body body)
  #!+sb-doc
  "Executes BODY with deferrable interrupts conditionally enabled. If there
are pending interrupts they take effect prior to executing BODY.

As interrupts are normally allowed WITH-INTERRUPTS only makes sense if there
is an outer WITHOUT-INTERRUPTS with a corresponding ALLOW-WITH-INTERRUPTS:
interrupts are not enabled if any outer WITHOUT-INTERRUPTS is not accompanied
by ALLOW-WITH-INTERRUPTS."
  (with-unique-names (allowp enablep)
    ;; We could manage without ENABLEP here, but that would require
    ;; taking extra care not to ever have *ALLOW-WITH-INTERRUPTS* NIL
    ;; and *INTERRUPTS-ENABLED* T -- instead of risking future breakage
    ;; we take the tiny hit here.
    `(let* ((,allowp *allow-with-interrupts*)
            (,enablep *interrupts-enabled*)
            (*interrupts-enabled* (or ,enablep ,allowp)))
       (when (and ,allowp (not ,enablep))
         (when *unblock-deferrables-on-enabling-interrupts-p*
           (setq *unblock-deferrables-on-enabling-interrupts-p* nil)
           (sb!unix::unblock-deferrable-signals))
         (when (or *interrupt-pending*
                   #!+sb-thruption *thruption-pending*)
           (receive-pending-interrupt)))
       (locally ,@body))))

(defmacro allow-with-interrupts (&body body)
  (declare (ignore body))
  (error "~S is valid only inside ~S."
         'allow-with-interrupts 'without-interrupts))

(defmacro with-local-interrupts (&body body)
  (declare (ignore body))
  (error "~S is valid only inside ~S."
         'with-local-interrupts 'without-interrupts))

;;; A low-level operation that assumes that *INTERRUPTS-ENABLED* is
;;; false, *ALLOW-WITH-INTERRUPTS* is true and deferrable signals are
;;; unblocked.
(defun %check-interrupts ()
  ;; Here we check for pending interrupts first, because reading a
  ;; special is faster then binding it!
  (when (or *interrupt-pending* #!+sb-thruption *thruption-pending*)
    (let ((*interrupts-enabled* t))
      (receive-pending-interrupt))))

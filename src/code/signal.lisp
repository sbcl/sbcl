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

(defvar *interrupts-enabled* t)
(defvar *interrupt-pending* nil)
(defvar *allow-with-interrupts* t)

(sb!xc:defmacro without-interrupts (&body body)
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
  (with-unique-names (outer-allow-with-interrupts)
    `(call-with-dx-function (call-without-interrupts
                             ,outer-allow-with-interrupts)
       (declare (disable-package-locks allow-with-interrupts with-interrupts)
                (ignorable ,outer-allow-with-interrupts))
       (macrolet ((allow-with-interrupts (&body allow-forms)
                    `(call-allowing-with-interrupts
                      (lambda () ,@allow-forms)
                      ,',outer-allow-with-interrupts))
                  (with-local-interrupts (&body with-forms)
                    `(call-with-local-interrupts
                      (lambda () ,@with-forms)
                      ,',outer-allow-with-interrupts)))
         (declare (enable-package-locks allow-with-interrupts with-interrupts))
         ,@body))))

;;; Helper for making the DX closure allocation in WITHOUT-INTERRUPTS
;;; less ugly.
;;;
;;; TODO: generalize for cases where FUNCTION takes more arguments
;;; than just the thunk; use in other WITH-FOO macros that expand to a
;;; CALL-WITH-FOO. I just did WITHOUT-INTERRUPTS since it's
;;; performance critical (for example each call to GETHASH was consing
;;; 48 bytes of WITHOUT-INTERRUPTS closures). --JES, 2007-06-08
(sb!xc:defmacro call-with-dx-function ((function &rest args) &body body)
  (with-unique-names (fun1 fun2)
    `(flet ((,fun1 (,@args)
              ,@body))
       (declare (optimize sb!c::stack-allocate-dynamic-extent))
       (flet ((,fun2 (,@args)
                ;; Avoid consing up a closure: FUN1 will be inlined
                ;; and FUN2 will be stack-allocated, so we avoid
                ;; consing up a closure. This is split into two
                ;; separate functions to ensure that the body doesn't
                ;; get compiled with (OPTIMIZE
                ;; SB!C::STACK-ALLOCATE-DYNAMIC-EXTENT), which could
                ;; cause problems e.g. when the body contains
                ;; DYNAMIC-EXTENT declarations and the code is being
                ;; compiled with (SAFETY 3).
                (,fun1 ,@args)))
         (declare (dynamic-extent (function ,fun2)))
         (,function (function ,fun2))))))

(sb!xc:defmacro with-interrupts (&body body)
  #!+sb-doc
  "Executes BODY with deferrable interrupts conditionally enabled. If there
are pending interrupts they take effect prior to executing BODY.

As interrupts are normally allowed WITH-INTERRUPTS only makes sense if there
is an outer WITHOUT-INTERRUPTS with a corresponding ALLOW-WITH-INTERRUPTS:
interrupts are not enabled if any outer WITHOUT-INTERRUPTS is not accompanied
by ALLOW-WITH-INTERRUPTS."
  `(call-with-interrupts
    (lambda () ,@body)
    (and (not *interrupts-enabled*) *allow-with-interrupts*)))

(defun call-allowing-with-interrupts (function allowp)
  (declare (function function))
  (if allowp
      (let ((*allow-with-interrupts* t))
        (funcall function))
      (funcall function)))

(defun call-with-interrupts (function allowp)
  (declare (function function))
  (if allowp
      (let ((*interrupts-enabled* t))
        (when *interrupt-pending*
          (receive-pending-interrupt))
        (funcall function))
      (funcall function)))

;; Distinct from CALL-WITH-INTERRUPTS as it needs to bind both *A-W-I*
;; and *I-E*.
(defun call-with-local-interrupts (function allowp)
  (declare (function function))
  (if allowp
      (let* ((*allow-with-interrupts* t)
             (*interrupts-enabled* t))
        (when *interrupt-pending*
          (receive-pending-interrupt))
        (funcall function))
      (funcall function)))

(defun call-without-interrupts (function)
  (declare (function function))
  (flet ((run-without-interrupts ()
           (if *allow-with-interrupts*
               (let ((*allow-with-interrupts* nil))
                 (funcall function t))
               (funcall function nil))))
    (if *interrupts-enabled*
        (unwind-protect
             (let ((*interrupts-enabled* nil))
               (run-without-interrupts))
          ;; If we were interrupted in the protected section, then the
          ;; interrupts are still blocked and it remains so until the
          ;; pending interrupt is handled.
          ;;
          ;; If we were not interrupted in the protected section, but
          ;; here, then even if the interrupt handler enters another
          ;; WITHOUT-INTERRUPTS, the pending interrupt will be handled
          ;; immediately upon exit from said WITHOUT-INTERRUPTS, so it
          ;; is as if nothing has happened.
          (when *interrupt-pending*
            (receive-pending-interrupt)))
        (run-without-interrupts))))

;;; A low-level operation that assumes that *INTERRUPTS-ENABLED* is false,
;;; and *ALLOW-WITH-INTERRUPTS* is true.
(defun %check-interrupts ()
  ;; Here we check for pending interrupts first, because reading a special
  ;; is faster then binding it!
  (when *interrupt-pending*
    (let ((*interrupts-enabled* t))
      (receive-pending-interrupt))))

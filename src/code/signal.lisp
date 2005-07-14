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

(defvar *interrupts-enabled* t)
(defvar *interrupt-pending* nil)

(sb!xc:defmacro without-interrupts (&body body)
  #!+sb-doc
  "Execute BODY in a context impervious to interrupts."
  (let ((name (gensym "WITHOUT-INTERRUPTS-BODY-")))
    `(flet ((,name () ,@body))
      (if *interrupts-enabled*
          (unwind-protect
               (let ((*interrupts-enabled* nil))
                 (,name))
            ;; If we were interrupted in the protected section, then
            ;; the interrupts are still blocked and it remains so
            ;; until the pending interrupt is handled.
            ;;
            ;; If we were not interrupted in the protected section,
            ;; but here, then even if the interrupt handler enters
            ;; another WITHOUT-INTERRUPTS, the pending interrupt will
            ;; be handled immediately upon exit from said
            ;; WITHOUT-INTERRUPTS, so it is as if nothing has
            ;; happened.
            (when *interrupt-pending*
              (receive-pending-interrupt)))
          (,name)))))

(sb!xc:defmacro with-interrupts (&body body)
  #!+sb-doc
  "Allow interrupts while executing BODY. As interrupts are normally allowed,
  this is only useful inside a WITHOUT-INTERRUPTS."
  (let ((name (gensym)))
    `(flet ((,name () ,@body))
       (if *interrupts-enabled*
           (,name)
           (let ((*interrupts-enabled* t))
             (when *interrupt-pending*
               (receive-pending-interrupt))
             (,name))))))

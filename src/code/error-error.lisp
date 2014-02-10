;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

;;; These specials are used by ERROR-ERROR to track the success of recovery
;;; attempts. Not to be confused with SB-KERNEL::*CURRENT-ERROR-DEPTH*.
(defvar *error-error-depth* 0)

;;; ERROR-ERROR can be called when the error system is in trouble and needs to
;;; punt fast. It prints a message without using FORMAT. If we get into this
;;; recursively, then we halt.
(defun error-error (&rest messages)
  ;; Be very conservative until nesting is dealt with, including consing.
  (let ((*error-error-depth* (1+ *error-error-depth*)))
    (when (> *error-error-depth* 4)
      (/show0 "*ERROR-ERROR-DEPTH* too big, trying HALT")
      (%primitive sb!c:halt)
      (/show0 "*ERROR-ERROR-DEPTH* too big, trying THROW")
      (throw 'toplevel-catcher nil))
    ;; We can be somewhat more relaxed now, the above will save us.
    (with-standard-io-syntax
      (let ((*print-readably* nil))
        (flet
            ((try-to-invoke-debugger ()
               ;; Try to print something useful for debugging and try to invoke the debugger.
               ;; Many things can trigger potentially nested errors beyond this point.
               (dolist (item messages)
                 (princ item *terminal-io*))
               (terpri *terminal-io*)
               (sb!debug:print-backtrace :stream *terminal-io* :emergency-best-effort t)
               (force-output *terminal-io*)
               (invoke-debugger
                (coerce-to-condition "Maximum error nesting depth exceeded" nil
                                     'simple-error
                                     'error)))
             (safely-print (message)
               (handler-case
                   (progn
                     (princ "Help! ERROR-ERROR is " *terminal-io*)
                     (princ *error-error-depth* *terminal-io*)
                     (princ " levels deep. " *terminal-io*)
                     (princ message *terminal-io*)
                     (terpri *terminal-io*))
                 (t ()))))
          (declare (inline safely-print))
          (case *error-error-depth*
            (1
             ;; Don't scream on the first invocation.
             (try-to-invoke-debugger))
            (2
             (safely-print "Will try to reset the IO streams by calling STREAM-COLD-INIT-OR-RESET.")
             (stream-cold-init-or-reset)
             (try-to-invoke-debugger))
            (3
             (safely-print "Will try to THROW this thread to the toplevel.")
             (/show0 "*ERROR-ERROR-DEPTH* too big, trying THROW")
             ;; FIXME: TOPLEVEL-REPL installs a (catch 'toplevel-catcher ...)
             ;; inside its LOOP. If any error nesting happens while inside that
             ;; LOOP (e.g. due to a "Broken pipe" on stderr, which is used by the
             ;; debugger), then when we get here this throw below will only unwind
             ;; until inside this LOOP, and thus the repl will loop forever busy
             ;; printing errors. https://bugs.launchpad.net/sbcl/+bug/1520694
             (throw 'toplevel-catcher nil))
            (t
             (safely-print "Will try to halt this thread as a last resort.")
             (/show0 "*ERROR-ERROR-DEPTH* too big, trying HALT")
             (%primitive sb!c:halt)
             (/show0 "*ERROR-ERROR-DEPTH* too big, trying THROW")
             (throw '%abort-thread nil))))))))

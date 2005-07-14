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
;;; attempts.
(defvar *error-error-depth* 0)

;;; ERROR-ERROR can be called when the error system is in trouble and needs to
;;; punt fast. It prints a message without using FORMAT. If we get into this
;;; recursively, then we halt.
(defun error-error (&rest messages)
  (let ((*error-error-depth* (1+ *error-error-depth*)))
    (case *error-error-depth*
      (1)
      (2
       (stream-cold-init-or-reset))
      (3
       (/show0 "*ERROR-ERROR-DEPTH* too big, trying THROW")
       (throw 'toplevel-catcher nil))
      (t
       (/show0 "*ERROR-ERROR-DEPTH* too big, trying HALT")
       (%primitive sb!c:halt)
       (/show0 "*ERROR-ERROR-DEPTH* too big, trying THROW")
       (throw 'toplevel-catcher nil)))

    (with-standard-io-syntax
      (let ((*print-readably* nil))
        (dolist (item messages)
          (princ item *terminal-io*))
        (terpri *terminal-io*)
        (sb!debug:backtrace most-positive-fixnum *terminal-io*)
        (force-output *terminal-io*)
        (invoke-debugger
         (coerce-to-condition "Maximum error nesting depth exceeded" nil
                              'simple-error
                              'error))))))

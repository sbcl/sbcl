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
(defvar *error-throw-up-count* 0)

;;; ERROR-ERROR can be called when the error system is in trouble and needs to
;;; punt fast. It prints a message without using FORMAT. If we get into this
;;; recursively, then we halt.
(defun error-error (&rest messages)
  (let ((*error-error-depth* (1+ *error-error-depth*)))
    (when (> *error-throw-up-count* 50)
      (/show0 "*ERROR-THROW-UP-COUNT* too big, trying HALT")
      (%primitive sb!c:halt)
      (/show0 "*ERROR-THROW-UP-COUNT* too big, trying THROW")
      (throw 'sb!impl::toplevel-catcher nil))
    (case *error-error-depth*
      (1)
      (2
       (stream-cold-init-or-reset))
      (3
       (incf *error-throw-up-count*)
       (/show0 "*ERROR-ERROR-DEPTH* too big, trying THROW")
       (throw 'sb!impl::toplevel-catcher nil))
      (t
       (/show0 "*ERROR-ERROR-DEPTH* too big, trying HALT")
       (%primitive sb!c:halt)
       (/show0 "*ERROR-ERROR-DEPTH* too big, trying THROW")
       (throw 'sb!impl::toplevel-catcher nil)))

    (with-standard-io-syntax
      (let ((*print-readably* nil))
	(dolist (item messages)
	  (princ item *terminal-io*))
	(sb!debug:internal-debug)))))

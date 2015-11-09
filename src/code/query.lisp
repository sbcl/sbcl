;;;; querying the user: Y-OR-N-P, YES-OR-NO-P

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

(defun query-read-char ()
  (clear-input *query-io*)
  (prog1 (read-char *query-io*)
    (clear-input *query-io*)))

(defun query-read-line ()
  (force-output *query-io*)
  (string-trim " " (read-line *query-io*)))

(defun maybe-print-query (hint format-string &rest format-args)
  (fresh-line *query-io*)
  (when format-string
    (apply #'format *query-io* format-string format-args)
    (write-char #\Space *query-io*))
  (format *query-io* "~A " hint)
  (finish-output *query-io*))

(defun clarify-legal-query-input (yes no)
  (format *query-io* "~&Please type \"~A\" for yes or \"~A\" for no.~%"
          yes no))

(defun y-or-n-p (&optional format-string &rest arguments)
  #!+sb-doc
  "Y-OR-N-P prints the message, if any, and reads characters from
   *QUERY-IO* until the user enters y or Y as an affirmative, or either
   n or N as a negative answer. It asks again if you enter any other
   characters."
  (declare (explicit-check))
  (flet ((print-query ()
           (apply #'maybe-print-query "(y or n)" format-string arguments)))
    (loop (print-query)
          (case (query-read-char)
            ((#\y #\Y) (return t))
            ((#\n #\N) (return nil))
            (t (clarify-legal-query-input "y" "n"))))))

(defun yes-or-no-p (&optional format-string &rest arguments)
  #!+sb-doc
  "YES-OR-NO-P is similar to Y-OR-N-P, except that it clears the
   input buffer, beeps, and uses READ-LINE to get the strings
   YES or NO."
  (declare (explicit-check))
  (flet ((print-query ()
           (apply #'maybe-print-query "(yes or no)" format-string arguments)))
    (beep *query-io*)
    (loop (print-query)
          (let ((input (query-read-line)))
            (cond
              ((string-equal input "yes") (return t))
              ((string-equal input "no") (return nil))
              (t (clarify-legal-query-input "yes" "no")))))))

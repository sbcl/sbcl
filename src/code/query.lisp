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

(defun query-readline ()
  (force-output *query-io*)
  (string-trim " 	" (read-line *query-io*)))

;;; FIXME: The ANSI documentation for these says that they
;;; prompt with strings like "(Y or N)" or "(Yes or No)", but
;;; these implementations don't.

(defun y-or-n-p (&optional format-string &rest arguments)
  #!+sb-doc
  "Y-OR-N-P prints the message, if any, and reads characters from *QUERY-IO*
   until the user enters y or Y as an affirmative, or either n or N as a
   negative answer. It ignores preceding whitespace and asks again if you
   enter any other characters."
  (when format-string
    (fresh-line *query-io*)
    (apply #'format *query-io* format-string arguments))
  (loop
    (let* ((line (query-readline))
	   (ans (if (string= line "")
		    #\? ;Force CASE below to issue instruction.
		    (schar line 0))))
      (unless (sb!impl::whitespacep ans)
	(case ans
	  ((#\y #\Y) (return t))
	  ((#\n #\N) (return nil))
	  (t
	   (write-line "Please type \"y\" for yes or \"n\" for no. "
		       *query-io*)
	   (when format-string
	     (apply #'format *query-io* format-string arguments))
	   (force-output *query-io*)))))))

(defun yes-or-no-p (&optional format-string &rest arguments)
  #!+sb-doc
  "YES-OR-NO-P is similar to Y-OR-N-P, except that it clears the
   input buffer, beeps, and uses READ-LINE to get the strings
   YES or NO."
  (clear-input *query-io*)
  (beep)
  (when format-string
    (fresh-line *query-io*)
    (apply #'format *query-io* format-string arguments))
  (do ((ans (query-readline) (query-readline)))
      (())
    (cond ((string-equal ans "YES") (return t))
	  ((string-equal ans "NO") (return nil))
	  (t
	   (write-line "Please type \"yes\" for yes or \"no\" for no. "
		       *query-io*)
	   (when format-string
	     (apply #'format *query-io* format-string arguments))))))

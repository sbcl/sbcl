;;;; This file contains things for the extensions packages (SB-EXT and
;;;; also "internal extensions" SB-INT) which can't be built at
;;;; cross-compile time, and perhaps also some things which might as
;;;; well not be built at cross-compile time because they're not
;;;; needed then. Things which can't be built at cross-compile time
;;;; (e.g. because they need machinery which only exists inside SBCL's
;;;; implementation of the LISP package) do not belong in this file.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

;;;; variables related to saving core files
;;;;
;;;; (Most of the save-a-core functionality is defined later, in its
;;;; own file, but we'd like to have these symbols declared special
;;;; and initialized ASAP.)

(defvar *before-save-initializations* nil
  #!+sb-doc
  "This is a list of functions which are called before creating a saved core
  image. These functions are executed in the child process which has no ports,
  so they cannot do anything that tries to talk to the outside world.")

(defvar *after-save-initializations* nil
  #!+sb-doc
  "This is a list of functions which are called when a saved core image starts
  up. The system itself should be initialized at this point, but applications
  might not be.")

;;; like LISTEN, but any whitespace in the input stream will be flushed
(defun listen-skip-whitespace (&optional (stream *standard-input*))
  (do ((char (read-char-no-hang stream nil nil nil)
	     (read-char-no-hang stream nil nil nil)))
      ((null char) nil)
    (cond ((not (whitespacep char))
	   (unread-char char stream)
	   (return t)))))

;;;; helpers for C library calls

;;; Signal a SIMPLE-CONDITION/ERROR condition associated with an ANSI C
;;; errno problem, arranging for the condition's print representation
;;; to be similar to the ANSI C perror(3) style.
(defun simple-perror (prefix-string
		      &key
		      (errno (get-errno))
		      (simple-error 'simple-error)
		      other-condition-args)
  (declare (type symbol simple-error))
  (aver (subtypep simple-error 'simple-condition))
  (aver (subtypep simple-error 'error))
  (apply #'error
	 simple-error
	 :format-control "~@<~A: ~2I~_~A~:>"
	 :format-arguments (list prefix-string (strerror errno))
	 other-condition-args))

;;;; OS interface functions for CMU CL under BSD Unix.

;;;; This code was written as part of the CMU Common Lisp project at
;;;; Carnegie Mellon University, and has been placed in the public
;;;; domain.

(in-package "SB!SYS")

;;;; Check that target machine features are set up consistently with
;;;; this file.
#!-bsd (eval-when (:compile-toplevel :load-toplevel :execute)
	 (error "The :BSD feature is missing, we shouldn't be doing this code."))

(defun software-type ()
  #!+sb-doc
  "Return a string describing the supporting software."
  (the string ; (to force error in case of unsupported BSD variant)
       #!+FreeBSD "FreeBSD"
       #!+OpenBSD "OpenBSD"))

(defvar *software-version* nil)

(defun software-version ()
  #!+sb-doc
  "Return a string describing version of the supporting software, or NIL
   if not available."
  (or *software-version*
      (setf *software-version*
	    (string-trim '(#\newline)
			 (with-output-to-string (stream)
			   (sb!ext:run-program "/usr/bin/uname" `("-r")
					       :output stream))))))

(defun os-cold-init-or-reinit ()
  (setf *software-version* nil)
  (setf *default-pathname-defaults*
	;; (temporary value, so that #'PATHNAME won't blow up when
	;; we call it below:)
	(make-trivial-default-pathname)
	*default-pathname-defaults*
	;; (final value, constructed using #'PATHNAME:)
	(pathname (sb!unix:posix-getcwd/))))

;;; Return system time, user time and number of page faults.
(defun get-system-info ()
  (multiple-value-bind (err? utime stime maxrss ixrss idrss
			     isrss minflt majflt)
		       (sb!unix:unix-getrusage sb!unix:rusage_self)
    (declare (ignore maxrss ixrss idrss isrss minflt))
    (unless err?
      (simple-perror "Unix system call getrusage() failed" :errno utime))
    
    (values utime stime majflt)))

;;; Return the system page size.
(defun get-page-size ()
  ;; FIXME: probably should call getpagesize()
  4096)

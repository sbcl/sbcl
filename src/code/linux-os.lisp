;;;; OS interface functions for CMU CL under Linux

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!SYS")

;;; Check that target machine features are set up consistently with
;;; this file.
#!-linux (error "missing :LINUX feature")

(defun software-type ()
  #!+sb-doc
  "Return a string describing the supporting software."
  (values "Linux"))

(defvar *software-version* nil)

(defun software-version ()
  #!+sb-doc
  "Return a string describing version of the supporting software, or NIL
  if not available."
  (or *software-version*
      (setf *software-version*
	    (string-trim '(#\newline)
			 (with-output-to-string (stream)
			   (sb!ext:run-program "/bin/uname" `("-r")
					       :output stream))))))

;;; OS-COLD-INIT-OR-REINIT initializes our operating-system interface.
;;; It sets the values of the global port variables to what they
;;; should be and calls the functions that set up the argument blocks
;;; for the server interfaces.
(defun os-cold-init-or-reinit () ; KLUDGE: don't know what to do here
  #!+sparc ;; Can't use #x20000000 thru #xDFFFFFFF, but mach tries to let us.
  (sb!sys:allocate-system-memory-at (sb!sys:int-sap #x20000000) #xc0000000))

;;; Return system time, user time and number of page faults.
(defun get-system-info ()
  (multiple-value-bind
      (err? utime stime maxrss ixrss idrss isrss minflt majflt)
      (sb!unix:unix-getrusage sb!unix:rusage_self)
    (declare (ignore maxrss ixrss idrss isrss minflt))
    (unless err? ; FIXME: nonmnemonic (reversed) name for ERR?
      (error "Unix system call getrusage failed: ~A." (strerror utime)))
    (values utime stime majflt)))

;;; Return the system page size.
(defun get-page-size ()
  ;; probably should call getpagesize()
  ;; FIXME: Or we could just get rid of this, since the uses of it look
  ;; disposable.
  4096)

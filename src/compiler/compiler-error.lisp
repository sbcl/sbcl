;;;; the bare essentials of compiler error handling
;;;;
;;;; (Logically, this might belong in early-c.lisp, since it's stuff
;;;; which might as well be visible to all compiler code. However,
;;;; physically its DEFINE-CONDITION forms depend on the condition
;;;; system being set up before it can be cold loaded, so we keep it
;;;; in this separate, loaded-later file instead of in early-c.lisp.)

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

;;;; error-handling definitions which are easy to define early and
;;;; which are nice to have visible everywhere

;;; a function that is called to unwind out of COMPILER-ERROR
(declaim (type (function () nil) *compiler-error-bailout*))
(defvar *compiler-error-bailout*
  (lambda () (error "COMPILER-ERROR with no bailout")))

;;; an application programmer's error caught by the compiler
;;;
;;; We want a separate condition for application programmer errors so
;;; that we can distinguish them from system programming errors (bugs
;;; in SBCL itself). Application programmer errors should be caught
;;; and turned into diagnostic output and a FAILURE-P return value
;;; from COMPILE or COMPILE-FILE. Bugs in SBCL itself throw us into
;;; the debugger.
(define-condition compiler-error (simple-error) ())

;;; Signal the appropriate condition. COMPILER-ERROR calls the bailout
;;; function so that it never returns (but compilation continues).
;;; COMPILER-ABORT falls through to the default error handling, so
;;; compilation terminates. 
(declaim (ftype (function (string &rest t) nil) compiler-error compiler-abort))
(declaim (ftype (function (string &rest t) (values))
		compiler-warning compiler-style-warning))
(defun compiler-abort (format-string &rest format-args)
  (error 'compiler-error
	 :format-control format-string
	 :format-arguments format-args))
(defun compiler-error (format-string &rest format-args)
  (cerror "Replace form with call to ERROR."
	  'compiler-error
	  :format-control format-string
	  :format-arguments format-args)
  (funcall *compiler-error-bailout*)
  (bug "Control returned from *COMPILER-ERROR-BAILOUT*."))
(defun compiler-warn (format-string &rest format-args)
  (apply #'warn format-string format-args)
  (values))
(defun compiler-style-warn (format-string &rest format-args)
  (apply #'style-warn format-string format-args)
  (values))

;;; the condition of COMPILE-FILE being unable to READ from the
;;; source file
;;;
;;; This is not a COMPILER-ERROR, since we don't try to recover from
;;; it and keep chugging along, but instead immediately bail out of
;;; the entire COMPILE-FILE.
;;;
;;; (The old CMU CL code did try to recover from this condition, but
;;; the code for doing that was messy and didn't always work right.
;;; Since in Common Lisp the simple act of reading and compiling code
;;; (even without ever loading the compiled result) can have side
;;; effects, it's a little scary to go on reading code when you're
;;; deeply confused, so we violate what'd otherwise be good compiler
;;; practice by not trying to recover from this error and bailing out
;;; instead.)
(define-condition input-error-in-compile-file (error)
  (;; the original error which was trapped to produce this condition
   (error :reader input-error-in-compile-file-error
	  :initarg :error)
   ;; the position where the bad READ began, or NIL if unavailable,
   ;; redundant, or irrelevant
   (position :reader input-error-in-compile-file-position
	     :initarg :position
	     :initform nil))
  (:report
   (lambda (condition stream)
     (format stream
	     "~@<~S failure in ~S~@[ at character ~W~]: ~2I~_~A~:>"
	     'read
	     'compile-file
	     (input-error-in-compile-file-position condition)
	     (input-error-in-compile-file-error condition)))))

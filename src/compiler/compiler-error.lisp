;;;; the bare essentials of compiler error handling (FIXME: to be
;;;; moved to early-c.lisp when stable)

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

;;; a function that is called to unwind out of COMPILER-ERROR
(declaim (type (function () nil) *compiler-error-bailout*))
(defvar *compiler-error-bailout*
  (lambda () (error "COMPILER-ERROR with no bailout")))

;;; We have a separate COMPILER-ERROR condition to allow us to
;;; distinguish internal compiler errors from user errors.
;;; Non-compiler errors put us in the debugger.
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
  ;; FIXME: It might be nice to define a BUG or OOPS function for "shouldn't
  ;; happen" cases like this.
  (error "internal error, control returned from *COMPILER-ERROR-BAILOUT*"))
(defun compiler-warning (format-string &rest format-args)
  (apply #'warn format-string format-args)
  (values))
(defun compiler-style-warning (format-string &rest format-args)
  (apply #'style-warn format-string format-args)
  (values))

;;;; side-effect-free tests of the condition system

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; While most of SBCL is derived from the CMU CL system, the test
;;;; files (like this one) were written from scratch after the fork
;;;; from CMU CL.
;;;; 
;;;; This software is in the public domain and is provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for
;;;; more information.

(cl:in-package :cl-user)

;;; Until 0.7.7.21, (MAKE-CONDITION 'FILE-ERROR :PATHNAME "FOO")
;;; wasn't printable, because the REPORT function for FILE-ERROR
;;; referred to unbound slots. This was reported and fixed by Antonio
;;; Martinez (sbcl-devel 2002-09-10).
(format t
	"~&printable now: ~A~%"
	(make-condition 'file-error :pathname "foo"))

;;;; tests of the system's ability to catch resource exhaustion errors

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

;;; Prior to sbcl-0.7.1.38, doing something like (RECURSE), even in
;;; safe code, would crash the entire Lisp process. Now it should
;;; signal an error in a context where the soft stack limit has been
;;; relaxed enough that the error can be handled.
(locally
  (declare (optimize safety))
  (defun recurse () (recurse) (recurse))
  (ignore-errors (recurse)))

;;; success
(quit :unix-status 104)

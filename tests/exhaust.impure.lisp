;;;; tests of the system's ability to catch resource exhaustion problems

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
;;; safe code, would crash the entire Lisp process. Then the soft
;;; stack checking was introduced, which checked (in safe code) for
;;; stack exhaustion at each lambda.

;;; Post 0.7.6.1, this was rewritten to use mprotect()-based stack
;;; protection which does not require lisp code to check anything,
;;; and works at all optimization settings.  However, it now signals a
;;; STORAGE-CONDITION instead of an ERROR, so this test needs revising
(locally
  (defun recurse () (recurse) (recurse))
  (handler-case
    (recurse)
    (storage-condition (c) (declare (ignore c)) (quit :unix-status 104))))

;;; oops
(quit :unix-status 1)

;;;; This file is for compiler tests which have side effects (e.g.
;;;; executing DEFUN) but which don't need any special side-effecting
;;;; environmental stuff (e.g. DECLAIM of particular optimization
;;;; settings). Similar tests which *do* expect special settings may
;;;; be in files compiler-1.impure.lisp, compiler-2.impure.lisp, etc.

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

;;; In sbcl-0.6.10, Douglas Brebner reported that (SETF EXTERN-ALIEN)
;;; was messed up so badly that trying to execute expressions like
;;; this signalled an error.
(setf (sb-alien:extern-alien "current_control_stack_pointer" sb-alien:unsigned)
      (sb-alien:extern-alien "current_control_stack_pointer" sb-alien:unsigned))

;;; success
(quit :unix-status 104)

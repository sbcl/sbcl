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

;;; bug 133, fixed in 0.7.0.5: Somewhere in 0.pre7.*, C void returns
;;; were broken ("unable to use values types here") when
;;; auto-PROCLAIM-of-return-value was added to DEFINE-ALIEN-ROUTINE.
(sb-alien:define-alien-routine ("free" free) void (ptr (* t) :in))

;;; Types of alien functions were being incorrectly DECLAIMED when
;;; docstrings were included in the definition until sbcl-0.7.6.15.
(sb-alien:define-alien-routine ("getenv" ftype-correctness) c-string
  "docstring"
  (name c-string))

(multiple-value-bind (function warningsp failurep)
    (compile nil '(lambda () (ftype-correctness)))
  (assert warningsp))

(multiple-value-bind (function warningsp failurep)
    (compile nil '(lambda () (ftype-correctness "FOO")))
  (assert (not warningsp)))

(multiple-value-bind (function warningsp failurep)
    (compile nil '(lambda () (ftype-correctness "FOO" "BAR")))
  (assert warningsp))

;;; success
(quit :unix-status 104)

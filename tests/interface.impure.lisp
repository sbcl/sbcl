;;;; tests for problems in the interface presented to the user/programmer

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

(load "assertoid.lisp")
(use-package "ASSERTOID")

(defun (setf foo) (x)
  "(setf foo) documentation"
  x)

(assert (string= (documentation '(setf foo) 'function)
                 "(setf foo) documentation"))
(assert (string= (documentation #'(setf foo) 'function)
                 "(setf foo) documentation"))

(defun (sb-pcl::class-predicate foo) (x)
  "(class-predicate foo) documentation"
  x)

(assert (string= (documentation '(setf foo) 'function)
                 "(setf foo) documentation"))
(assert (string= (documentation #'(setf foo) 'function)
                 "(setf foo) documentation"))
(assert (string= (documentation '(sb-pcl::class-predicate foo) 'function)
                 "(class-predicate foo) documentation"))
(assert (string= (documentation #'(sb-pcl::class-predicate foo) 'function)
                 "(class-predicate foo) documentation"))

;;; DISASSEMBLE shouldn't fail on closures or unpurified functions
(defun disassemble-fun (x) x)
(disassemble 'disassemble-fun)

(let ((x 1)) (defun disassemble-closure (y) (if y (setq x y) x)))
(disassemble 'disassemble-closure)

;;;; success
(sb-ext:quit :unix-status 104)

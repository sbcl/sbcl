;;;; tests related to setf

;;;; This file is impure because we want to be able to use DEFUN.

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

(in-package :cl-user)

(defvar *foo* nil)
(defun (setf foo) (bar)
    (setf *foo* bar))

;;; Regression test for get-setf-expansion without explicit
;;; environment object.
(assert (multiple-value-list (get-setf-expansion '(foo))))

;;; Regression test for SHIFTF of values.
(let ((x (list 1))
      (y (list 2)))
  (shiftf (values (car x) (car y)) (values (car y) (car x)))
  (assert (equal (list x y) '((2) (1)))))

;;; success
(quit :unix-status 104)

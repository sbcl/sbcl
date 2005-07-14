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

;;; SETF of values with multiple-value place forms
(let ((a t) (b t) (c t) (d t))
  (let ((list (multiple-value-list
               (setf (values (values a b) (values c d)) (values 1 2 3 4)))))
    (assert (equal list '(1 2)))
    (assert (eql a 1))
    (assert (eql c 2))
    (assert (null b))
    (assert (null d))))

;;; SETF of THE with VALUES.
(let (x y)
  (setf (the (values fixnum fixnum) (values x y))
        (values 1 2))
  (assert (= x 1))
  (assert (= y 2)))

;;; SETF of MACRO-FUNCTION must accept a NIL environment
(let ((fun (constantly 'ok)))
  (setf (macro-function 'nothing-at-all nil) fun)
  (assert (eq fun (macro-function 'nothing-at-all nil))))

;;; success
(quit :unix-status 104)

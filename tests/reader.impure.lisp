;;;; tests related to the Lisp reader

;;;; This file is impure because we want to modify the readtable and stuff.

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

;;; Bug 30, involving mistakes in binding the read table, made this
;;; code fail.
(defun read-vector (stream char)
  (declare (ignorable char))
  (coerce (read-delimited-list #\] stream t) 'vector))
(set-macro-character #\[ #'read-vector nil)
(set-macro-character #\] (get-macro-character #\)) nil)
(multiple-value-bind (res pos)
    (read-from-string "[1 2 3]") ; ==> #(1 2 3), 7
  (assert (equalp res #(1 2 3)))
  (assert (= pos 7)))
(multiple-value-bind (res pos)
    (read-from-string "#\\x") ; ==> #\x, 3
  (assert (equalp res #\x))
  (assert (= pos 3)))
(multiple-value-bind (res pos)
    (read-from-string "[#\\x]")
  (assert (equalp res #(#\x)))
  (assert (= pos 5)))

;;; success
(quit :unix-status 104)

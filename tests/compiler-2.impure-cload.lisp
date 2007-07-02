;;;; -*- lisp -*-

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

;;;; recognize self-calls
(declaim (optimize speed))

;;;; These three forms should be equivalent.

;;; This used to be a bug in the handling of null-lexenv vs toplevel
;;; policy: LOCALLY and MACROLET hid the toplevel policy from view.

(locally
    (defun foo (n)
      (frob 'foo)
      (if (<= n 0)
          n
          (foo (1- n)))))

(progn
  (defun bar (n)
    (frob 'bar)
    (if (<= n 0)
        n
        (bar (1- n)))))

(macrolet ()
  (defun quux (n)
    (frob 'quux)
    (if (<= n 0)
        n
        (quux (1- n)))))

(defun frob (x)
  (setf (fdefinition x) (constantly 13)))

(defun test ()
  (list (foo 1) (bar 1) (quux 1)))

(assert (equal (test) '(0 0 0)))
(assert (equal (test) '(13 13 13))) ; sanity check

;;; Bug in 1.0.2 and 1.0.3, where the XEP was compiled with the wrong
;;; policy. (Test-case derived from code posted by alexander.ekart in
;;; comp.lang.lisp).

(locally
    (declare (optimize (safety 0)))
  (defun bubblesort (x y)
    (declare (type (simple-array fixnum (*)) x)
             (type fixnum y)
             (optimize (speed 3) (safety 3) (space 0) (debug 0)))
    (aref x y)))

(assert (raises-error? (bubblesort (make-array 10) 9)))

;;;; tests, with side effects, of DESTRUCTURING-BIND-ish functionality

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

;;; In sbcl-1.0.7.8, the printer for the ERROR condition signalled from
;;;   (DESTRUCTURING-BIND (...) 1 ...)
;;; contained the implicit assumption that the bad datum was a list,
;;; so that attempting to print the condition caused a new error.
(defun frob-1-0-7-8 (x)
  (destructuring-bind (y . z) x
    (print y)
    (print z)))
(multiple-value-bind (whatever error)
    (ignore-errors (frob-1-0-7-8 1))
  (declare (ignore whatever))
  (princ error)) ; shouldn't cause an error

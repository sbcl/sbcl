;;;; various CHARACTER tests without side effects

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

(load "assertoid.lisp")

;;; ANSI's specification of #'CHAR-NAME imposes these constraints.
;;;
;;; (Obviously, the numeric values in this test implicitly assume
;;; we're using an ASCII-based character set.)
(dolist (i '(("Newline" 10)
	     ;; (ANSI also imposes a constraint on the "semi-standard
	     ;; character" "Linefeed", but in ASCII as interpreted by
	     ;; Unix it's shadowed by "Newline" and so doesn't exist
	     ;; as a separate character.)
	     ("Space" 32)
	     ("Tab" 9)
	     ("Page" 12)
	     ("Rubout" 127)
	     ("Return" 13)
	     ("Backspace" 8)))
  (destructuring-bind (name code) i
    (let ((named-char (name-char name))
	  (coded-char (code-char code)))
      (assert (eql named-char coded-char))
      (assert (characterp named-char))
      (let ((coded-char-name (char-name coded-char)))
	(assert (string= name coded-char-name))))))

;;; bug 230: CHAR= didn't check types of &REST arguments
(dolist (form '((code-char char-code-limit)
                (standard-char-p "a")
                (graphic-char-p "a")
                (alpha-char-p "a")
                (upper-case-p "a")
                (lower-case-p "a")
                (both-case-p "a")
                (digit-char-p "a")
                (alphanumericp "a")
                (char= #\a "a")
                (char/= #\a "a")
                (char< #\a #\b "c")
                (char-equal #\a #\a "b")
                (digit-char -1)
                (digit-char 4 1)
                (digit-char 4 37)))
  (assert (raises-error? (apply (car form) (mapcar 'eval (cdr form))) type-error)))

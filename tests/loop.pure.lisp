;;;; miscellaneous tests of LOOP-related stuff

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

(in-package "CL-USER")

;;; The bug reported by Alexei Dejneka on sbcl-devel 2001-09-03
;;; is fixed now.
(assert (equal (let ((hash (make-hash-table)))
		 (setf (gethash 'key1 hash) 'val1)
		 (setf (gethash 'key2 hash) 'val2)
		 (sort (loop for key being each hash-key in hash
			     collect key)
		       #'string<))
	       '(key1 key2)))

;;; Bug 81, reported by Wolfhard Buss on cmucl-help 2001-02-14, was
;;; fixed by Alexey Dejneka's patch on sbcl-devel 2001-09-30.
(assert (equal '(0.0 1.0 2.0 3.0)
	       (loop with (a . b) of-type float = '(0.0 . 1.0)
		     and (c . d) of-type float = '(2.0 . 3.0)
		     return (list a b c d))))

;;; a bug reported and fixed by Alexey Dejneka sbcl-devel 2001-10-05:
;;; The type declarations should apply, hence under Python's
;;; declarations-are-assertions rule, the code should signal a type
;;; error.
(assert (typep (nth-value 1
			  (ignore-errors
			    (funcall (lambda ()
				       (loop with (a . b)
					     of-type float = '(5 . 5)
					     return (list a b))))))
	       'type-error))

;;; bug 103, reported by Arthur Lemmens sbcl-devel 2001-05-05,
;;; fixed by Alexey Dejneka patch sbcl-devel 2001-10-05:
;;; LOOP syntax requires that forms after INITIALLY, FINALLY, and DO
;;; must be compound forms.
(multiple-value-bind (function warnings-p failure-p)
    (compile nil
	     '(lambda ()
		(loop while t do
		      *print-level*
		      (print t))))
  (declare (ignore function warnings-p))
  (assert failure-p))

;;; a bug reported by Paul F. Dietz (in his ANSI test suite):
;;; duplicate bindings in LOOP must signal errors of type
;;; PROGRAM-ERROR.
(assert (typep (nth-value 1
			  (ignore-errors
			    (funcall (lambda ()
				       (loop for (a . a) in '((1 . 2) (3 . 4))
					     return a)))))
	       'program-error))

;;; similar to gcl/ansi-test LOOP.1.27, and fixed at the same time:
(assert (equal (loop for x downto 7 by 2 from 13 collect x) '(13 11 9 7)))

;;; some more from gcl/ansi-test:
(let ((table (make-hash-table)))
  (setf (gethash 'foo table) '(bar baz))
  (assert (= (loop for nil being the hash-keys of table count t) 1))
  (assert (equal (loop for nil being the hash-keys of table
		               using (hash-value (v1 . v2))
		       when v1
		         return v2)
		 '(baz))))

(assert (= (loop for nil being the external-symbols of :cl count t) 978))
(assert (= (loop for x being the external-symbols of :cl count x) 977))

(let ((*package* (find-package :cl)))
  (assert (= (loop for x being each external-symbol count t) 978)))

(assert (eq (loop for a = (return t) return nil) t))

(multiple-value-bind (result error)
    (ignore-errors
      (loop for nil being the external-symbols of :nonexistent-package
	    count t))
  (assert (null result))
  (assert (typep error 'package-error)))

(assert (equal (loop for i from 1 repeat (the (integer 7 7) 7) collect i)
               '(1 2 3 4 5 6 7)))

(multiple-value-bind (result error)
    (ignore-errors
      (eval '(loop for i from 1 repeat 7 of-type fixnum collect i)))
  (assert (null result))
  (assert (typep error 'program-error)))

(assert (equal
	 (ignore-errors (loop for i from 1 repeat 6.5 collect i))
	 (ignore-errors (loop for i from 1 repeat (eval '6.5) collect i))))

(assert (eq (block nil
	      (loop named foo do (loop-finish) finally (return :good))
	      :bad)
	    :good))

(assert (= (loop with (a nil) = '(1 2) return a) 1))
(assert (= (loop with (nil a) = '(1 2) return a) 2))
(assert (= (loop with (a . nil) = '(1 2) return a) 1))
(assert (equal (loop with (nil . a) = '(1 2) return a) '(2)))

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

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

(multiple-value-bind (result error)
    (ignore-errors
      (loop for i in '(1 2 3) collect i always (< i 4)))
  (assert (null result))
  (assert (typep error 'program-error)))
(assert (equal
	 (loop for i in '(1 2 3) collect i into foo always (< i 4)
	       finally (return foo))
	 '(1 2 3)))
(assert (equal
	 (loop for i in '(1 2 3) collect i into foo always (= i 4)
	       finally (return foo))
	 nil))
(multiple-value-bind (result error)
    (ignore-errors
      (loop for i in '(1 2 3) always (< i 4) collect i))
  (assert (null result))
  (assert (typep error 'program-error)))
(assert (equal
	 (loop for i in '(1 2 3) always (< i 4) collect i into foo
	       finally (return foo))
	 '(1 2 3)))
(assert (equal
	 (loop for i in '(1 2 3) always (= i 4) collect i into foo
	       finally (return foo))
	 nil))
(multiple-value-bind (result error)
    (ignore-errors
      (loop for i in '(1 2 3) thereis (= i 3) collect i))
  (assert (null result))
  (assert (typep error 'program-error)))

(multiple-value-bind (result error)
    (ignore-errors
      (loop with i = 1 for x from 1 to 3 collect x into i))
  (assert (null result))
  (assert (typep error 'program-error)))
(multiple-value-bind (result error)
    ;; this one has a plausible interpretation in terms of LET*, but
    ;; ANSI seems specifically to disallow it
    (ignore-errors
      (loop with i = 1 with i = (1+ i)
	    for x from 1 to 3
	    collect (+ x i)))
  (assert (null result))
  (assert (typep error 'program-error)))

(let ((it 'z))
  (assert (equal
	   ;; this one just seems weird.  Nevertheless...
	   (loop for i in '(a b c d)
		 when i
		   collect it
		   and collect it)
	   '(a z b z c z d z))))

(let ((ht (make-hash-table)))
  (setf (gethash 1 ht) 3)
  (setf (gethash 7 ht) 15)
  (assert (= (loop for v fixnum being each hash-key in ht sum v) 8))
  (assert (= (loop for v fixnum being each hash-value in ht sum v) 18))
  (assert (raises-error? (loop for v float being each hash-value in ht sum v)
                         type-error)))

;; arithmetic indexes can be NIL or symbols.
(assert (equal (loop for nil from 0 to 2 collect nil)
	       '(nil nil nil)))
(assert (equal (loop for nil to 2 collect nil)
	       '(nil nil nil)))

;; although allowed by the loop syntax definition in 6.2/LOOP,
;; 6.1.2.1.1 says: "The variable var is bound to the value of form1 in
;; the first iteration[...]"; since we can't bind (i j) to anything,
;; we give a program error.
(multiple-value-bind (function warnings-p failure-p)
    (compile nil
	     `(lambda ()
		(loop for (i j) from 4 to 6 collect nil)))
  (assert failure-p))

;; ...and another for indexes without FROM forms (these are treated
;; differently by the loop code right now
(multiple-value-bind (function warnings-p failure-p)
    (compile nil
	     `(lambda ()
		(loop for (i j) to 6 collect nil)))
  (assert failure-p))

(assert
 (equal
  (let ((x 2d0))
    (loop for d of-type double-float from 0d0 to 10d0 by x collect d))
  '(0d0 2d0 4d0 6d0 8d0 10d0)))
(assert
 (equal
  (let ((x 2d0))
    (loop for d of-type double-float downfrom 10d0 to 0d0 by x collect d))
  '(10d0 8d0 6d0 4d0 2d0 0d0)))

(let ((fn (handler-case
	      (compile nil '(lambda ()
			     (declare (special x y))
			     (loop thereis (pop x) thereis (pop y))))
	    (warning (c) (error "Warned: ~S" c)))))
  (let ((x (list nil nil 1))
	(y (list nil 2 nil)))
    (declare (special x y))
    (assert (= (funcall fn) 2))))

;;;; various compiler tests without side effects

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

;;; Exercise a compiler bug (by crashing the compiler).
;;;
;;; This test code is from Douglas Crosher's simplified TICKLE-BUG
;;; (2000-09-06 on cmucl-imp).
;;;
;;; The bug was fixed by Douglas Crosher's patch, massaged for SBCL by
;;; Martin Atzmueller (2000-09-13 on sbcl-devel).
(funcall (compile nil
		  '(lambda ()
		     (labels ((fun1 ()
				(fun2))
			      (fun2 ()
				(when nil
				  (tagbody
				   tag
				   (fun2)
				   (go tag)))
				(when nil
				  (tagbody
				   tag
				   (fun1)
				   (go tag)))))

		       (fun1)
		       nil))))

;;; Exercise a compiler bug (by crashing the compiler).
;;;
;;; Tim Moore gave a patch for this bug in CMU CL 2000-05-24 on 
;;; cmucl-imp, and Martin Atzmueller applied it to SBCL.
(funcall (compile nil
		  '(lambda (x)
		     (or (integerp x)
			 (block used-by-some-y?
			   (flet ((frob (stk)
				    (dolist (y stk)
				      (unless (rejected? y)
					(return-from used-by-some-y? t)))))
			     (declare (inline frob))
			     (frob (rstk x))
			     (frob (mrstk x)))
			   nil))))
	 13)

;;; bug 112, reported by Martin Atzmueller 2001-06-25 (originally
;;; from Bruno Haible in CMU CL bugs collection), fixed by
;;; Alexey Dejneka 2002-01-27
(assert (= 1 ; (used to give 0 under bug 112)
	   (let ((x 0))
	     (declare (special x))
	     (let ((x 1))
	       (let ((y x))
		 (declare (special x)) y)))))
(assert (= 1 ; (used to give 1 even under bug 112, still works after fix)
	   (let ((x 0))
	     (declare (special x))
	     (let ((x 1))
	       (let ((y x) (x 5))
		 (declare (special x)) y)))))

;;; another LET-related bug fixed by Alexey Dejneka at the same
;;; time as bug 112
(multiple-value-bind (value error)
    (ignore-errors
      ;; should complain about duplicate variable names in LET binding
      (compile nil
	       '(lambda ()
		  (let (x
			(x 1))
		    (list x)))))
  (assert (null value))
  (assert (typep error 'error)))

;;; bug 169 (reported by Alexey Dejneka 2002-05-12, fixed by David
;;; Lichteblau 2002-05-21)
(progn
  (multiple-value-bind (fun warnings-p failure-p)
      (compile nil
	       ;; Compiling this code should cause a STYLE-WARNING
	       ;; about *X* looking like a special variable but not
	       ;; being one.
	       '(lambda (n)
		  (let ((*x* n))
		    (funcall (symbol-function 'x-getter))
		    (print *x*))))
    (assert (functionp fun))
    (assert warnings-p)
    (assert (not failure-p)))
  (multiple-value-bind (fun warnings-p failure-p)
      (compile nil
	       ;; Compiling this code should not cause a warning
	       ;; (because the DECLARE turns *X* into a special
	       ;; variable as its name suggests it should be).
	       '(lambda (n)
		  (let ((*x* n))
		    (declare (special *x*))
		    (funcall (symbol-function 'x-getter))
		    (print *x*))))
    (assert (functionp fun))
    (assert (not warnings-p))
    (assert (not failure-p))))

;;; a bug in 0.7.4.11
(dolist (i '(a b 1 2 "x" "y"))
  ;; In sbcl-0.7.4.11, the compiler tried to source-transform the
  ;; TYPEP here but got confused and died, doing
  ;;   (ASSOC '(AND INTEGERP (SATISFIES PLUSP)))
  ;;          *BACKEND-TYPE-PREDICATES*
  ;;          :TEST #'TYPE=)
  ;; and blowing up because TYPE= tried to call PLUSP on the
  ;; characters of the MEMBER-TYPE representing STANDARD-CHAR.
  (when (typep i '(and integer (satisfies oddp)))
    (print i)))
(dotimes (i 14)
  (when (typep i '(and integer (satisfies oddp)))
    (print i)))

;;; bug 156 (reported by APD sbcl-devel 2002-04-12, fixed by CSR patch
;;; sbcl-devel 2002-07-02): FUNCTION-LAMBDA-EXPRESSION of
;;; interactively-compiled functions was broken by sleaziness and
;;; confusion in the assault on 0.7.0, so this expression used to
;;; signal TYPE-ERROR when it found NIL instead of a DEBUG-SOURCE.
(eval '(function-lambda-expression #'(lambda (x) x)))

;;; bug caught and fixed by Raymond Toy cmucl-imp 2002-07-10: &REST
;;; variable is not optional.
(assert (null (ignore-errors (eval '(funcall (lambda (&rest) 12))))))

;;; on the PPC, we got the magic numbers in undefined_tramp wrong for
;;; a while; fixed by CSR 2002-07-18
(multiple-value-bind (value error)
    (ignore-errors (some-undefined-function))
  (assert (null value))
  (assert (eq (cell-error-name error) 'some-undefined-function)))

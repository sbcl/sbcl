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

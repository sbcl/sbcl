(cl:in-package :cl-user)

;;; Exercise a compiler bug by (crashing the compiler).
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

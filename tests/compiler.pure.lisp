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

;;; Non-symbols shouldn't be allowed as VARs in lambda lists. (Where VAR
;;; is a variable name, as in section 3.4.1 of the ANSI spec.)
(assert (null (ignore-errors (eval '(lambda ("foo") 12)))))
(assert (ignore-errors (eval '(lambda (foo) 12))))
(assert (null (ignore-errors (eval '(lambda (&optional 12) "foo")))))
(assert (ignore-errors (eval '(lambda (&optional twelve) "foo"))))
(assert (null (ignore-errors (eval '(lambda (&optional (12 12)) "foo")))))
(assert (ignore-errors (eval '(lambda (&optional (twelve 12)) "foo"))))
(assert (null (ignore-errors (eval '(lambda (&key #\c) "foo")))))
(assert (ignore-errors (eval '(lambda (&key c) "foo"))))
(assert (null (ignore-errors (eval '(lambda (&key (#\c #\c)) "foo")))))
(assert (ignore-errors (eval '(lambda (&key (c #\c)) "foo"))))
(assert (null (ignore-errors (eval '(lambda (&key ((#\c #\c) #\c)) "foo")))))
(assert (ignore-errors (eval '(lambda (&key ((:c cbyanyothername) #\c)) "foo"))))

;;; As reported and fixed by Antonio Martinez-Shotton sbcl-devel
;;; 2002-09-12, this failed in sbcl-0.7.7.23. (with failed AVER
;;; "(LEAF-HAS-SOURCE-NAME-P LEAF)")
(assert (= (funcall (eval `(lambda (x) (funcall ,(lambda (y) (+ y 3)) x))) 14)
	   17))

;;; bug 181: bad type specifier dropped compiler into debugger
(assert (list (compile nil '(lambda (x)
                             (declare (type (0) x))
                             x))))

(let ((f (compile nil '(lambda (x)
                        (make-array 1 :element-type '(0))))))
  (assert (null (ignore-errors (funcall f)))))

;;; the following functions must not be flushable
(dolist (form '((make-sequence 'fixnum 10)
                (concatenate 'fixnum nil)
                (map 'fixnum #'identity nil)
                (merge 'fixnum nil nil #'<)))
  (assert (not (eval `(locally (declare (optimize (safety 0)))
                        (ignore-errors (progn ,form t)))))))

(dolist (form '((values-list (car (list '(1 . 2))))
                (fboundp '(set bet))
                (atan #c(1 1) (car (list #c(2 2))))
                (nthcdr (car (list (floor (cos 3)))) '(1 2 3 4 5))
                (nthcdr (car (list 5)) '(1 2 . 3))))
  (assert (not (eval `(locally (declare (optimize (safety 3)))
                        (ignore-errors (progn ,form t)))))))

;;; a bug in the MAP deftransform caused non-VECTOR array specifiers
;;; to cause errors in the compiler.  Fixed by CSR in 0.7.8.10
(assert (list (compile nil '(lambda (x) (map 'simple-array 'identity x)))))

;;; bug 129: insufficient syntax checking in MACROLET
(multiple-value-bind (result error)
    (ignore-errors (eval '(macrolet ((foo x `',x)) (foo 1 2 3))))
  (assert (null result))
  (assert (typep error 'error)))

;;; bug 124: environment of MACROLET-introduced macro expanders
(assert (equal
         (macrolet ((mext (x) `(cons :mext ,x)))
           (macrolet ((mint (y) `'(:mint ,(mext y))))
             (list (mext '(1 2))
                   (mint (1 2)))))
         '((:MEXT 1 2) (:MINT (:MEXT 1 2)))))

;;; bug 48c: SYMBOL-MACROLET should signal PROGRAM-ERROR if introduced
;;; symbol is declared to be SPECIAL
(multiple-value-bind (result error)
    (ignore-errors (funcall (lambda ()
                              (symbol-macrolet ((s '(1 2)))
                                  (declare (special s))
                                s))))
  (assert (null result))
  (assert (typep error 'program-error)))

;;; ECASE should treat a bare T as a literal key
(multiple-value-bind (result error)
    (ignore-errors (ecase 1 (t 0)))
  (assert (null result))
  (assert (typep error 'type-error)))

(multiple-value-bind (result error)
    (ignore-errors (ecase 1 (t 0) (1 2)))
  (assert (eql result 2))
  (assert (null error)))
	  
;;; FTYPE should accept any functional type specifier
(compile nil '(lambda (x) (declare (ftype function f)) (f x)))

;;; FUNCALL of special operators and macros should signal an
;;; UNDEFINED-FUNCTION error
(multiple-value-bind (result error)
    (ignore-errors (funcall 'quote 1))
  (assert (null result))
  (assert (typep error 'undefined-function))
  (assert (eq (cell-error-name error) 'quote)))
(multiple-value-bind (result error)
    (ignore-errors (funcall 'and 1))
  (assert (null result))
  (assert (typep error 'undefined-function))
  (assert (eq (cell-error-name error) 'and)))

;;; PSETQ should behave when given complex symbol-macro arguments
(multiple-value-bind (sequence index)
    (symbol-macrolet ((x (aref a (incf i)))
		      (y (aref a (incf i))))
	(let ((a (copy-seq #(0 1 2 3 4 5 6 7 8 9)))
	      (i 0))
	  (psetq x (aref a (incf i))
		 y (aref a (incf i)))
	  (values a i)))
  (assert (equalp sequence #(0 2 2 4 4 5 6 7 8 9)))
  (assert (= index 4)))

(multiple-value-bind (result error)
    (ignore-errors
      (let ((x (list 1 2)))
	(psetq (car x) 3)
	x))
  (assert (null result))
  (assert (typep error 'program-error)))

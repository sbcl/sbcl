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
(multiple-value-bind (fun warnings-p failure-p)
    ;; should complain about duplicate variable names in LET binding
    (compile nil
	     '(lambda ()
	       (let (x
		     (x 1))
		 (list x))))
  (declare (ignore warnings-p))
  (assert (functionp fun))
  (assert failure-p))

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

;;; COPY-SEQ should work on known-complex vectors:
(assert (equalp #(1)
		(let ((v (make-array 0 :fill-pointer 0)))
		  (vector-push-extend 1 v)
		  (copy-seq v))))

;;; to support INLINE functions inside MACROLET, it is necessary for
;;; FUNCTION-LAMBDA-EXPRESSION to return a proper lambda expression in
;;; certain circumstances, one of which is when compile is called from
;;; top-level.
(assert (equal
	 (function-lambda-expression
	  (compile nil '(lambda (x) (block nil (print x)))))
	 '(lambda (x) (block nil (print x)))))

;;; bug 62: too cautious type inference in a loop
(assert (nth-value
         2
         (compile nil
                  '(lambda (a)
                    (declare (optimize speed (safety 0)))
                    (typecase a
                      (array (loop (print (car a)))))))))

;;; Bug reported by Robert E. Brown sbcl-devel 2003-02-02: compiler
;;; failure
(compile nil
         '(lambda (key tree collect-path-p)
           (let ((lessp (key-lessp tree))
                 (equalp (key-equalp tree)))
             (declare (type (function (t t) boolean) lessp equalp))
             (let ((path '(nil)))
               (loop for node = (root-node tree)
                  then (if (funcall lessp key (node-key node))
                           (left-child node)
                           (right-child node))
                  when (null node)
                  do (return (values nil nil nil))
                  do (when collect-path-p
                       (push node path))
                  (when (funcall equalp key (node-key node))
                    (return (values node path t))))))))

;;; CONSTANTLY should return a side-effect-free function (bug caught
;;; by Paul Dietz' test suite)
(let ((i 0))
  (let ((fn (constantly (progn (incf i) 1))))
    (assert (= i 1))
    (assert (= (funcall fn) 1))
    (assert (= i 1))
    (assert (= (funcall fn) 1))
    (assert (= i 1))))

;;; Bug 240 reported by tonyms on #lisp IRC 2003-02-25 (modified version)
(loop for (fun warns-p) in
     '(((lambda (&optional *x*) *x*) t)
       ((lambda (&optional *x* &rest y) (values *x* y)) t)
       ((lambda (&optional *print-length*) (values *print-length*)) nil)
       ((lambda (&optional *print-length* &rest y) (values *print-length* y)) nil)
       ((lambda (&optional *x*) (declare (special *x*)) (values *x*)) nil)
       ((lambda (&optional *x* &rest y) (declare (special *x*)) (values *x* y)) nil))
   for real-warns-p = (nth-value 1 (compile nil fun))
   do (assert (eq warns-p real-warns-p)))

;;; Bug reported by Gilbert Baumann on #lisp IRC 2003-03-26
(assert (equal (funcall (eval '(lambda (x &optional (y (pop x))) (list x y)))
                        '(1 2))
               '((2) 1)))

;;; Bug reported by Paul Dietz on cmucl-imp and fixed by Gerd
;;; Moellmann: CONVERT-MORE-CALL failed on the following call
(assert (eq (eval '((lambda (&key) 'u) :allow-other-keys nil)) 'u))

(raises-error? (multiple-value-bind (a b c)
                   (eval '(truncate 3 4))
                 (declare (integer c))
                 (list a b c))
               type-error)

(assert (equal (multiple-value-list (the (values &rest integer)
                                      (eval '(values 3))))
               '(3)))

;;; Bug relating to confused representation for the wild function
;;; type:
(assert (null (funcall (eval '(lambda () (multiple-value-list (values)))))))

;;; &ENVIRONMENT parameter should be bound first (from Paul Dietz'
;;; test suite)
(assert (eql (macrolet ((foo () 1))
               (macrolet ((%f (&optional (x (macroexpand '(foo) env)) &environment env)
                            x))
                 (%f)))
             1))

;;; MACROLET should check for duplicated names
(dolist (ll '((x (z x))
              (x y &optional z x w)
              (x y &optional z z)
              (x &rest x)
              (x &rest (y x))
              (x &optional (y nil x))
              (x &optional (y nil y))
              (x &key x)
              (x &key (y nil x))
              (&key (y nil z) (z nil w))
              (&whole x &optional x)
              (&environment x &whole x)))
  (assert (nth-value 2
                     (handler-case
                         (compile nil
                                  `(lambda ()
                                     (macrolet ((foo ,ll nil)
                                                (bar (&environment env)
                                                  `',(macro-function 'foo env)))
                                       (bar))))
                       (error (c)
                         (values nil t t))))))

(assert (typep (eval `(the arithmetic-error
		           ',(make-condition 'arithmetic-error)))
	       'arithmetic-error))

(assert (not (nth-value
              2 (compile nil '(lambda ()
                               (make-array nil :initial-element 11))))))

(assert (raises-error? (funcall (eval #'open) "assertoid.lisp"
                                :external-format '#:nonsense)))
(assert (raises-error? (funcall (eval #'load) "assertoid.lisp"
                                :external-format '#:nonsense)))

(assert (= (the (values integer symbol) (values 1 'foo 13)) 1))

(let ((f (compile nil
                  '(lambda (v)
                    (declare (optimize (safety 3)))
                    (list (the fixnum (the (real 0) (eval v))))))))
  (assert (raises-error? (funcall f 0.1) type-error))
  (assert (raises-error? (funcall f -1) type-error)))

;;; the implicit block does not enclose lambda list
(let ((forms '((defmacro #1=#:foo (&optional (x (return-from #1#))))
               #+nil(macrolet ((#2=#:foo (&optional (x (return-from #2#))))))
               (define-compiler-macro #3=#:foo (&optional (x (return-from #3#))))
               (deftype #4=#:foo (&optional (x (return-from #4#))))
               (define-setf-expander #5=#:foo (&optional (x (return-from #5#))))
               (defsetf #6=#:foo (&optional (x (return-from #6#))) ()))))
  (dolist (form forms)
    (assert (nth-value 2 (compile nil `(lambda () ,form))))))

(assert (nth-value 2 (compile nil
                              '(lambda ()
                                (svref (make-array '(8 9) :adjustable t) 1)))))

;;; CHAR= did not check types of its arguments (reported by Adam Warner)
(raises-error? (funcall (compile nil '(lambda (x y z) (char= x y z)))
                        #\a #\b nil)
               type-error)
(raises-error? (funcall (compile nil
                                 '(lambda (x y z)
                                   (declare (optimize (speed 3) (safety 3)))
                                   (char/= x y z)))
                        nil #\a #\a)
               type-error)

;;; Compiler lost return type of MAPCAR and friends
(dolist (fun '(mapcar mapc maplist mapl))
  (assert (nth-value 2 (compile nil
                                `(lambda (x)
                                   (1+ (,fun #'print x)))))))

(assert (nth-value 2 (compile nil
                              '(lambda ()
                                (declare (notinline mapcar))
                                (1+ (mapcar #'print '(1 2 3)))))))

;;; bug found by Paul Dietz: (SETF AREF) for bit vectors with constant
;;; index was effectless
(let ((f (compile nil '(lambda (a v)
                        (declare (type simple-bit-vector a) (type bit v))
                        (declare (optimize (speed 3) (safety 0)))
                        (setf (aref a 0) v)
                        a))))
  (let ((y (make-array 2 :element-type 'bit :initial-element 0)))
    (assert (equal y #*00))
    (funcall f y 1)
    (assert (equal y #*10))))

(handler-bind ((sb-ext:compiler-note #'error))
  (compile nil '(lambda (x)
		 (declare (type (simple-array (simple-string 3) (5)) x))
		 (aref (aref x 0) 0))))

;;; compiler failure
(let ((f (compile nil '(lambda (x) (typep x '(not (member 0d0)))))))
  (assert (funcall f 1d0)))

(compile nil '(lambda (x)
	       (declare (double-float x))
	       (let ((y (* x pi)))
		 (atan y y))))

;;; bogus optimization of BIT-NOT
(multiple-value-bind (result x)
    (eval '(let ((x (eval #*1001)))
            (declare (optimize (speed 2) (space 3))
                     (type (bit-vector) x))
            (values (bit-not x nil) x)))
  (assert (equal x #*1001))
  (assert (equal result #*0110)))

;;; the VECTOR type in CONCATENATE/MERGE/MAKE-SEQUENCE means (VECTOR T).
(handler-bind ((sb-ext:compiler-note #'error))
  (assert (equalp (funcall
		   (compile
		    nil
		    '(lambda ()
		      (let ((x (make-sequence 'vector 10 :initial-element 'a)))
			(setf (aref x 4) 'b)
			x))))
		  #(a a a a b a a a a a))))

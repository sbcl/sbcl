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

;;; this is not a check for a bug, but rather a test of compiler
;;; quality
(dolist (type '((integer 0 *)           ; upper bound
                (real (-1) *)
                float                   ; class
                (real * (-10))          ; lower bound
                ))
  (assert (nth-value
           1 (compile nil
                      `(lambda (n)
                         (declare (optimize (speed 3) (compilation-speed 0)))
                         (loop for i from 1 to (the (integer -17 10) n) by 2
                               collect (when (> (random 10) 5)
                                         (the ,type (- i 11)))))))))

;;; bug 278b
;;;
;;; We suppose that INTEGER arithmetic cannot be efficient, and the
;;; compiler has an optimized VOP for +; so this code should cause an
;;; efficiency note.
(assert (eq (block nil
              (handler-case
                  (compile nil '(lambda (i)
                                 (declare (optimize speed))
                                 (declare (type integer i))
                                 (+ i 2)))
                (sb-ext:compiler-note (c) (return :good))))
            :good))

;;; bug 277: IGNORE/IGNORABLE declarations should be acceptable for
;;; symbol macros
(assert (not (nth-value 1 (compile nil '(lambda (u v)
                                         (symbol-macrolet ((x u)
                                                           (y v))
                                             (declare (ignore x)
                                                      (ignorable y))
                                           (list u v)))))))

;;; bug reported by Paul Dietz: wrong optimizer for (EXPT ... 0)
(loop for (x type) in
      '((14 integer)
        (14 rational)
        (-14/3 (rational -8 11))
        (3s0 short-float)
        (4f0 single-float)
        (5d0 double-float)
        (6l0 long-float)
        (14 real)
        (13/2 real)
        (2s0 real)
        (2d0 real)
        (#c(-3 4) (complex fixnum))
        (#c(-3 4) (complex rational))
        (#c(-3/7 4) (complex rational))
        (#c(2s0 3s0) (complex short-float))
        (#c(2f0 3f0) (complex single-float))
        (#c(2d0 3d0) (complex double-float))
        (#c(2l0 3l0) (complex long-float))
        (#c(2d0 3s0) (complex float))
        (#c(2 3f0) (complex real))
        (#c(2 3d0) (complex real))
        (#c(-3/7 4) (complex real))
        (#c(-3/7 4) complex)
        (#c(2 3l0) complex))
      do (dolist (zero '(0 0s0 0f0 0d0 0l0))
           (dolist (real-zero (list zero (- zero)))
             (let* ((src `(lambda (x) (expt (the ,type x) ,real-zero)))
                    (fun (compile nil src))
                    (result (1+ (funcall (eval #'*) x real-zero))))
               (assert (eql result (funcall fun x)))))))

;;; (SIGNED-BYTE 1) [ returned from the logxor derive-type optimizer ]
;;; wasn't recognized as a good type specifier.
(let ((fun (lambda (x y)
	     (declare (type (integer -1 0) x y) (optimize speed))
	     (logxor x y))))
  (assert (= (funcall fun 0 0) 0))
  (assert (= (funcall fun 0 -1) -1))
  (assert (= (funcall fun -1 -1) 0)))

;;; from PFD's torture test, triggering a bug in our effective address
;;; treatment.
(compile
 nil
 `(lambda (a b)
    (declare (type (integer 8 22337) b))
    (logandc2
     (logandc2
      (* (logandc1 (max -29303 b) 4) b)
      (abs (logorc1 (+ (logandc1 -11 b) 2607688420) -31153924)))
     (logeqv (max a 0) b))))

;;; Alpha floating point modes weren't being reset after an exception,
;;; leading to an exception on the second compile, below.
(compile nil '(lambda (x y) (declare (type (double-float 0.0d0) x y)) (/ x y)))
(handler-case (/ 1.0 0.0)
  ;; provoke an exception
  (arithmetic-error ()))
(compile nil '(lambda (x y) (declare (type (double-float 0.0d0) x y)) (/ x y)))

;;; bug reported by Paul Dietz: component last block does not have
;;; start ctran
(compile nil
         '(lambda ()
           (declare (notinline + logand)
            (optimize (speed 0)))
           (LOGAND
            (BLOCK B5
              (FLET ((%F1 ()
                       (RETURN-FROM B5 -220)))
                (LET ((V7 (%F1)))
                  (+ 359749 35728422))))
            -24076)))

;;; bug 294 reported by Paul Dietz: miscompilation of REM and MOD
(assert (= (funcall (compile nil `(lambda (b)
                                    (declare (optimize (speed 3))
                                             (type (integer 2 152044363) b))
                                    (rem b (min -16 0))))
                    108251912)
           8))

(assert (= (funcall (compile nil `(lambda (c)
                                    (declare (optimize (speed 3))
                                             (type (integer 23062188 149459656) c))
                                    (mod c (min -2 0))))
                    95019853)
           -1))

;;; bug reported by Paul Dietz: block splitting inside FLUSH-DEAD-CODE
(compile nil
         '(LAMBDA (A B C)
           (BLOCK B6
             (LOGEQV (REM C -6758)
                     (REM B (MAX 44 (RETURN-FROM B6 A)))))))

(compile nil '(lambda ()
               (block nil
                 (flet ((foo (x y) (if (> x y) (print x) (print y))))
                   (foo 1 2)
                   (bar)
                   (foo (return 14) 2)))))

;;; bug in Alpha backend: not enough sanity checking of arguments to
;;; instructions
(assert (= (funcall (compile nil 
			     '(lambda (x) 
				(declare (fixnum x)) 
				(ash x -257)))
		    1024)
	   0))

;;; bug found by WHN and pfdietz: compiler failure while referencing
;;; an entry point inside a deleted lambda
(compile nil '(lambda ()
               (let (r3533)
                 (flet ((bbfn ()
                          (setf r3533
                                (progn
                                  (flet ((truly (fn bbd)
                                           (let (r3534)
                                             (let ((p3537 nil))
                                               (unwind-protect
                                                    (multiple-value-prog1
                                                        (progn
                                                          (setf r3534
                                                                (progn
                                                                  (bubf bbd t)
                                                                  (flet ((c-3536 ()
                                                                           (funcall fn)))
                                                                    (cdec #'c-3536
                                                                          (vector bbd))))))
                                                      (setf p3537 t))
                                                 (unless p3537
                                                   (error "j"))))
                                             r3534))
                                         (c (pd) (pdc pd)))
                                    (let ((a (smock a))
                                          (b (smock b))
                                          (b (smock c)))))))))
                   (wum #'bbfn "hc3" (list)))
                 r3533)))
(compile nil '(lambda () (flet ((%f () (unwind-protect nil))) nil)))

;;; the strength reduction of constant multiplication used (before
;;; sbcl-0.8.4.x) to lie to the compiler.  This meant that, under
;;; certain circumstances, the compiler would derive that a perfectly
;;; reasonable multiplication never returned, causing chaos.  Fixed by
;;; explicitly doing modular arithmetic, and relying on the backends
;;; being smart.
(assert (= (funcall 
	    (compile nil 
		     '(lambda (x)
			(declare (type (integer 178956970 178956970) x)
				 (optimize speed)) 
			(* x 24)))
	    178956970)
	   4294967280))

;;; bug in modular arithmetic and type specifiers
(assert (= (funcall (compile nil (lambda (x) (logand x x 0)))
		    -1)
	   0))

;;; MISC.99 from Paul Dietz' random tester: FAST-ASH-MOD32-C VOP
;;; produced wrong result for shift >=32 on X86
(assert (= 0 (funcall
              (compile nil
                       '(lambda (a)
                         (declare (type (integer 4303063 101130078) a))
                         (mask-field (byte 18 2) (ash a 77))))
              57132532)))

;;; MISC.101 and MISC.103: FLUSH-DEST did not mark the USE's block for
;;; type check regeneration
(assert (eql (funcall
              (compile nil '(lambda (a c)
                             (declare (type (integer 185501219873 303014665162) a))
                             (declare (type (integer -160758 255724) c))
                             (declare (optimize (speed 3)))
                             (let ((v8
                                    (- -554046873252388011622614991634432
                                       (ignore-errors c)
                                       (unwind-protect 2791485))))
                               (max (ignore-errors a)
                                    (let ((v6 (- v8 (restart-case 980))))
                                      (min v8 v6))))))
              259448422916 173715)
             259448422916))
(assert (eql (funcall
              (compile nil '(lambda (a b)
                             (min -80
                              (abs
                               (ignore-errors
                                 (+
                                  (logeqv b
                                          (block b6
                                            (return-from b6
                                              (load-time-value -6876935))))
                                  (if (logbitp 1 a) b (setq a -1522022182249))))))))
              -1802767029877 -12374959963)
             -80))

;;; various MISC.*, related to NODEs/LVARs with derived type NIL
(assert (eql (funcall (compile nil '(lambda (c)
                                     (declare (type (integer -3924 1001809828) c))
                                     (declare (optimize (speed 3)))
                                     (min 47 (if (ldb-test (byte 2 14) c)
                                                 -570344431
                                                 (ignore-errors -732893970)))))
                      705347625)
             -570344431))
(assert (eql (funcall
              (compile nil '(lambda (b)
                             (declare (type (integer -1598566306 2941) b))
                             (declare (optimize (speed 3)))
                             (max -148949 (ignore-errors b))))
              0)
             0))
(assert (eql (funcall
              (compile nil '(lambda (b c)
                             (declare (type (integer -4 -3) c))
                             (block b7
                               (flet ((%f1 (f1-1 f1-2 f1-3)
                                        (if (logbitp 0 (return-from b7
                                                         (- -815145138 f1-2)))
                                            (return-from b7 -2611670)
                                            99345)))
                                 (let ((v2 (%f1 -2464 (%f1 -1146 c c) -2)))
                                   b)))))
              2950453607 -4)
             -815145134))
(assert (eql (funcall
              (compile nil
                       '(lambda (b c)
                         (declare (type (integer -29742055786 23602182204) b))
                         (declare (type (integer -7409 -2075) c))
                         (declare (optimize (speed 3)))
                         (floor
                          (labels ((%f2 ()
                                     (block b6
                                       (ignore-errors (return-from b6
                                                        (if (= c 8) b 82674))))))
                            (%f2)))))
              22992834060 -5833)
             82674))
(assert (equal (multiple-value-list
                (funcall
                 (compile nil '(lambda (a)
                                (declare (type (integer -944 -472) a))
                                (declare (optimize (speed 3)))
                                (round
                                 (block b3
                                   (return-from b3
                                     (if (= 55957 a) -117 (ignore-errors
                                                            (return-from b3 a))))))))
                 -589))
               '(-589 0)))

;;; MISC.158
(assert (zerop (funcall
                (compile nil
                         '(lambda (a b c)
                           (declare (type (integer 79828 2625480458) a))
                           (declare (type (integer -4363283 8171697) b))
                           (declare (type (integer -301 0) c))
                           (if (equal 6392154 (logxor a b))
                               1706
                               (let ((v5 (abs c)))
                                 (logand v5
                                         (logior (logandc2 c v5)
                                                 (common-lisp:handler-case
                                                     (ash a (min 36 22477)))))))))
                100000 0 0)))

;;; MISC.152, 153: deleted code and iteration var type inference
(assert (eql (funcall
              (compile nil
                       '(lambda (a)
                         (block b5
                           (let ((v1 (let ((v8 (unwind-protect 9365)))
                                       8862008)))
                             (*
                              (return-from b5
                                (labels ((%f11 (f11-1) f11-1))
                                  (%f11 87246015)))
                              (return-from b5
                                (setq v1
                                      (labels ((%f6 (f6-1 f6-2 f6-3) v1))
                                        (dpb (unwind-protect a)
                                             (byte 18 13)
                                             (labels ((%f4 () 27322826))
                                               (%f6 -2 -108626545 (%f4))))))))))))
              -6)
             87246015))

(assert (eql (funcall
              (compile nil
                       '(lambda (a)
                         (if (logbitp 3
                                      (case -2
                                        ((-96879 -1035 -57680 -106404 -94516 -125088)
                                         (unwind-protect 90309179))
                                        ((-20811 -86901 -9368 -98520 -71594)
                                         (let ((v9 (unwind-protect 136707)))
                                           (block b3
                                             (setq v9
                                                   (let ((v4 (return-from b3 v9)))
                                                     (- (ignore-errors (return-from b3 v4))))))))
                                        (t -50)))
                             -20343
                             a)))
              0)
             -20343))

;;; MISC.165
(assert (eql (funcall
              (compile
               nil
               '(lambda (a b c)
                 (block b3
                   (flet ((%f15
                              (f15-1 f15-2 f15-3
                                     &optional
                                     (f15-4
                                      (flet ((%f17
                                                 (f17-1 f17-2 f17-3
                                                        &optional (f17-4 185155520) (f17-5 c)
                                                        (f17-6 37))
                                               c))
                                        (%f17 -1046 a 1115306 (%f17 b -146330 422) -337817)))
                                     (f15-5 a) (f15-6 -40))
                            (return-from b3 -16)))
                     (multiple-value-call #'%f15 (values -519354 a 121 c -1905))))))
              0 0 -5)
             -16))

;;; MISC.172
(assert (eql (funcall
              (compile
               nil
               '(lambda (a b c)
                 (declare (notinline list apply))
                 (declare (optimize (safety 3)))
                 (declare (optimize (speed 0)))
                 (declare (optimize (debug 0)))
                 (labels ((%f12 (f12-1 f12-2)
                            (labels ((%f2 (f2-1 f2-2)
                                       (flet ((%f6 ()
						(flet ((%f18
                                                           (f18-1
                                                            &optional (f18-2 a)
                                                            (f18-3 -207465075)
                                                            (f18-4 a))
                                                         (return-from %f12 b)))
						  (%f18 -3489553
							-7
							(%f18 (%f18 150 -64 f12-1)
							      (%f18 (%f18 -8531)
								    11410)
							      b)
							56362666))))
                                         (labels ((%f7
                                                      (f7-1 f7-2
                                                            &optional (f7-3 (%f6)))
                                                    7767415))
                                           f12-1))))
                              (%f2 b -36582571))))
                   (apply #'%f12 (list 774 -4413)))))
              0 1 2)
             774))

;;; MISC.173
(assert (eql (funcall
              (compile
               nil
               '(lambda (a b c)
                 (declare (notinline values))
                 (declare (optimize (safety 3)))
                 (declare (optimize (speed 0)))
                 (declare (optimize (debug 0)))
                 (flet ((%f11
                            (f11-1 f11-2
                                   &optional (f11-3 c) (f11-4 7947114)
                                   (f11-5
                                    (flet ((%f3 (f3-1 &optional (f3-2 b) (f3-3 5529))
                                             8134))
                                      (multiple-value-call #'%f3
                                        (values (%f3 -30637724 b) c)))))
                          (setq c 555910)))
                   (if (and nil (%f11 a a))
                       (if (%f11 a 421778 4030 1)
                           (labels ((%f7
                                        (f7-1 f7-2
                                              &optional
                                              (f7-3
                                               (%f11 -79192293
                                                     (%f11 c a c -4 214720)
                                                     b
                                                     b
                                                     (%f11 b 985)))
                                              (f7-4 a))
                                      b))
                             (%f11 c b -25644))
                           54)
                       -32326608))))
              1 2 3)
             -32326608))

;;; MISC.177, 182: IR2 copy propagation missed a hidden write to a
;;; local lambda argument
(assert
 (equal
  (funcall
   (compile nil
            '(lambda (a b c)
              (declare (type (integer 804561 7640697) a))
              (declare (type (integer -1 10441401) b))
              (declare (type (integer -864634669 55189745) c))
              (declare (ignorable a b c))
              (declare (optimize (speed 3)))
              (declare (optimize (safety 1)))
              (declare (optimize (debug 1)))
              (flet ((%f11
                         (f11-1 f11-2)
                       (labels ((%f4 () (round 200048 (max 99 c))))
                         (logand
                          f11-1
                          (labels ((%f3 (f3-1) -162967612))
                            (%f3 (let* ((v8 (%f4)))
                                   (setq f11-1 (%f4)))))))))
                (%f11 -120429363 (%f11 62362 b)))))
   6714367 9645616 -637681868)
  -264223548))

;;; Bug reported by Paul F. Dietz caused by derive type loss in VALUE
;;; transform
(assert (equal (multiple-value-list
                (funcall
                 (compile nil '(lambda ()
                                (declare (optimize (speed 1) (space 0) (safety 3) (debug 3) (compilation-speed 1)))
                                (ceiling
                                 (ceiling
                                  (flet ((%f16 () 0)) (%f16))))))))
               '(0 0)))

;;; MISC.184
(assert (zerop
         (funcall
          (compile
           nil
           '(lambda (a b c)
             (declare (type (integer 867934833 3293695878) a))
             (declare (type (integer -82111 1776797) b))
             (declare (type (integer -1432413516 54121964) c))
             (declare (optimize (speed 3)))
             (declare (optimize (safety 1)))
             (declare (optimize (debug 1)))
             (if nil
                 (flet ((%f15 (f15-1 &optional (f15-2 c))
                          (labels ((%f1 (f1-1 f1-2) 0))
                            (%f1 a 0))))
                   (flet ((%f4 ()
                            (multiple-value-call #'%f15
                              (values (%f15 c 0) (%f15 0)))))
                     (if nil (%f4)
                         (flet ((%f8 (f8-1 &optional (f8-2 (%f4)) (f8-3 0))
                                  f8-3))
                           0))))
                 0)))
          3040851270 1664281 -1340106197)))

;;; MISC.249
(assert (zerop
         (funcall
          (compile
           nil
           '(lambda (a b)
             (declare (notinline <=))
             (declare (optimize (speed 2) (space 3) (safety 0)
                       (debug 1) (compilation-speed 3)))
             (if (if (<= 0) nil nil)
                 (labels ((%f9 (f9-1 f9-2 f9-3)
                            (ignore-errors 0)))
                   (dotimes (iv4 5 a) (%f9 0 0 b)))
                 0)))
          1 2)))

;;; MISC.259-264 (aka "CSR screwed up implementing *-MOD32")
(assert
 (= (funcall
     (compile
      nil
      '(lambda (a)
         (declare (type (integer 177547470 226026978) a))
         (declare (optimize (speed 3) (space 0) (safety 0) (debug 0)
                            (compilation-speed 1)))
         (logand a (* a 438810))))
     215067723)
    13739018))


;;;; Bugs in stack analysis
;;; bug 299 (reported by PFD)
(assert
 (equal (funcall
         (compile
          nil
          '(lambda ()
            (declare (optimize (debug 1)))
            (multiple-value-call #'list
              (if (eval t) (eval '(values :a :b :c)) nil)
              (catch 'foo (throw 'foo (values :x :y)))))))
        '(:a :b :c :x :y)))
;;; bug 298 (= MISC.183)
(assert (zerop (funcall
                (compile
                 nil
                 '(lambda (a b c)
                   (declare (type (integer -368154 377964) a))
                   (declare (type (integer 5044 14959) b))
                   (declare (type (integer -184859815 -8066427) c))
                   (declare (ignorable a b c))
                   (declare (optimize (speed 3)))
                   (declare (optimize (safety 1)))
                   (declare (optimize (debug 1)))
                   (block b7
                     (flet ((%f3 (f3-1 f3-2 f3-3) 0))
                       (apply #'%f3 0 (catch 'foo (return-from b7 (%f3 0 b c))) c nil)))))
                0 6000 -9000000)))
(assert (equal (eval '(let () (apply #'list 1 (list (catch 'a (throw 'a (block b 2)))))))
               '(1 2)))
(let ((f (compile
          nil
          '(lambda (x)
            (block foo
              (multiple-value-call #'list
                :a
                (block bar
                  (return-from foo
                    (multiple-value-call #'list
                      :b
                      (block quux
                        (return-from bar
                          (catch 'baz
                            (if x
                                (return-from quux 1)
                                (throw 'baz 2))))))))))))))
  (assert (equal (funcall f t) '(:b 1)))
  (assert (equal (funcall f nil) '(:a 2))))

;;; MISC.185
(assert (equal
         (funcall
          (compile
           nil
           '(lambda (a b c)
             (declare (type (integer 5 155656586618) a))
             (declare (type (integer -15492 196529) b))
             (declare (type (integer 7 10) c))
             (declare (optimize (speed 3)))
             (declare (optimize (safety 1)))
             (declare (optimize (debug 1)))
             (flet ((%f3
                        (f3-1 f3-2 f3-3
                              &optional (f3-4 a) (f3-5 0)
                              (f3-6
                               (labels ((%f10 (f10-1 f10-2 f10-3)
                                          0))
                                 (apply #'%f10
                                        0
                                        a
                                        (- (if (equal a b) b (%f10 c a 0))
                                           (catch 'ct2 (throw 'ct2 c)))
                                        nil))))
                      0))
               (%f3 (%f3 (%f3 b 0 0 0) a 0) a b b b c)))) 5 0 7)
         0))
;;; MISC.186
(assert (eq
         (eval
          '(let* ((form '(labels ((%f3 (f3-1 f3-2) f3-1))
                          (apply #'%f3 b (catch 'ct8 (throw 'ct8 (logeqv (%f3 c 0)))) nil)))
                  (vars '(b c))
                  (fn1 `(lambda ,vars
                          (declare (type (integer -2 19) b)
                                   (type (integer -1520 218978) c)
                                   (optimize (speed 3) (safety 1) (debug 1)))
                          ,form))
                  (fn2 `(lambda ,vars
                          (declare (notinline logeqv apply)
                                   (optimize (safety 3) (speed 0) (debug 0)))
                          ,form))
                  (cf1 (compile nil fn1))
                  (cf2 (compile nil fn2))
                  (result1 (multiple-value-list (funcall cf1 2 18886)))
                  (result2 (multiple-value-list (funcall cf2 2 18886))))
            (if (equal result1 result2)
                :good
                (values result1 result2))))
         :good))

;;; MISC.290
(assert (zerop
         (funcall
          (compile
           nil
           '(lambda ()
             (declare
              (optimize (speed 3) (space 3) (safety 1)
               (debug 2) (compilation-speed 0)))
             (apply (constantly 0) (catch 'ct2 0) 0 (catch 'ct2 0) nil))))))

;;; MISC.292
(assert (zerop (funcall
                (compile
                 nil
                 '(lambda (a b)
                   (declare (optimize (speed 2) (space 0) (safety 3) (debug 1)
                             (compilation-speed 2)))
                   (apply (constantly 0)
                    a
                    0
                    (catch 'ct6
                      (apply (constantly 0)
                             0
                             0
                             (let* ((v1
                                     (let ((*s7* 0))
                                       b)))
                               0)
                             0
                             nil))
                    0
                    nil)))
                1 2)))

;;; misc.295
(assert (eql
         (funcall
          (compile
           nil
           '(lambda ()
             (declare (optimize (speed 1) (space 0) (safety 0) (debug 0)))
             (multiple-value-prog1
                 (the integer (catch 'ct8 (catch 'ct7 15867134)))
               (catch 'ct1 (throw 'ct1 0))))))
         15867134))

;;; misc.361: replacing CAST with (m-v-call #'%compile-time-type-error)
;;; could transform known-values LVAR to UVL
(assert (zerop (funcall
   (compile
    nil
    '(lambda (a b c)
       (declare (notinline boole values denominator list))
       (declare
	(optimize (speed 2)
		  (space 0)
		  (safety 1)
		  (debug 0)
		  (compilation-speed 2)))
       (catch 'ct6
	 (progv
	     '(*s8*)
	     (list 0)
	   (let ((v9 (ignore-errors (throw 'ct6 0))))
	     (denominator
	      (progv nil nil (values (boole boole-and 0 v9)))))))))
   1 2 3)))

;;; non-continuous dead UVL blocks
(defun non-continuous-stack-test (x)
  (multiple-value-call #'list
    (eval '(values 11 12))
    (eval '(values 13 14))
    (block ext
      (return-from non-continuous-stack-test
        (multiple-value-call #'list
          (eval '(values :b1 :b2))
          (eval '(values :b3 :b4))
          (block int
            (return-from ext
              (multiple-value-call (eval #'values)
                (eval '(values 1 2))
                (eval '(values 3 4))
                (block ext
                  (return-from int
                    (multiple-value-call (eval #'values)
                      (eval '(values :a1 :a2))
                      (eval '(values :a3 :a4))
                      (block int
                        (return-from ext
                          (multiple-value-call (eval #'values)
                            (eval '(values 5 6))
                            (eval '(values 7 8))
                            (if x
                                :ext
                                (return-from int :int))))))))))))))))
(assert (equal (non-continuous-stack-test t) '(11 12 13 14 1 2 3 4 5 6 7 8 :ext)))
(assert (equal (non-continuous-stack-test nil) '(:b1 :b2 :b3 :b4 :a1 :a2 :a3 :a4 :int)))

;;; MISC.362: environment of UNWIND-PROTECTor is different from that
;;; if ENTRY.
(assert (equal (multiple-value-list (funcall
   (compile
    nil
    '(lambda (b g h)
       (declare (optimize (speed 3) (space 3) (safety 2)
			  (debug 2) (compilation-speed 3)))
       (catch 'ct5
	 (unwind-protect
	     (labels ((%f15 (f15-1 f15-2 f15-3)
                            (rational (throw 'ct5 0))))
	       (%f15 0
		     (apply #'%f15
			    0
			    h
			    (progn
			      (progv '(*s2* *s5*) (list 0 (%f15 0 g 0)) b)
			      0)
			    nil)
		     0))
	   (common-lisp:handler-case 0)))))
   1 2 3))
 '(0)))


;;; MISC.275
(assert
 (zerop
  (funcall
   (compile
    nil
    '(lambda (b)
      (declare (notinline funcall min coerce))
      (declare
       (optimize (speed 1)
        (space 2)
        (safety 2)
        (debug 1)
        (compilation-speed 1)))
      (flet ((%f12 (f12-1)
               (coerce
                (min
                 (if f12-1 (multiple-value-prog1
                               b (return-from %f12 0))
                     0))
                'integer)))
        (funcall #'%f12 0))))
   -33)))

;;; Discussion of a CMUCL PCL bug on Sparc with Raymond Toy revealed a
;;; potential problem: optimizers and type derivers for MAX and MIN
;;; were not consistent in treating EQUALP, but not EQL, arguments.
(dolist (f '(min max))
  (loop for complex-arg-args in '((1d0 2d0) (0d0 1d0))
        for complex-arg = `(if x ,@complex-arg-args)
        do
        (loop for args in `((1 ,complex-arg)
                            (,complex-arg 1))
              for form = `(,f ,@args)
              for f1 = (compile nil `(lambda (x) ,form))
              and f2 = (compile nil `(lambda (x) (declare (notinline min max))
                                             ,form))
              do
              (dolist (x '(nil t))
                (assert (eql (funcall f1 x) (funcall f2 x)))))))

;;;
(handler-case (compile nil '(lambda (x)
                             (declare (optimize (speed 3) (safety 0)))
                             (the double-float (sqrt (the double-float x)))))
  (sb-ext:compiler-note ()
    (error "Compiler does not trust result type assertion.")))

(let ((f (compile nil '(lambda (x)
                        (declare (optimize speed (safety 0)))
                        (block nil
                          (the double-float
                            (multiple-value-prog1
                                (sqrt (the double-float x))
                              (when (< x 0)
                                (return :minus)))))))))
  (assert (eql (funcall f -1d0) :minus))
  (assert (eql (funcall f 4d0) 2d0)))

;;; bug 304: SBCL produced something similar to (/ (ASH x 4) 8)
(handler-case
    (compile nil '(lambda (a i)
                   (locally
                     (declare (optimize (speed 3) (safety 0) (space 0) (debug 0)
                                        (inhibit-warnings 0)))
                     (declare (type (alien (* (unsigned 8))) a)
                              (type (unsigned-byte 32) i))
                     (deref a i))))
  (compiler-note () (error "The code is not optimized.")))

(handler-case
    (compile nil '(lambda (x)
		   (declare (type (integer -100 100) x))
		   (declare (optimize speed))
		   (declare (notinline identity))
		   (1+ (identity x))))
  (compiler-note () (error "IDENTITY derive-type not applied.")))

(assert (null (funcall (compile nil '(lambda (x) (funcall #'cddr x))) nil)))

;;; MISC.293 = easy variant of bug 303: repeated write to the same
;;; LVAR; here the first write may be cleared before the second is
;;; made.
(assert
 (zerop
  (funcall
   (compile
    nil
    '(lambda ()
      (declare (notinline complex))
      (declare (optimize (speed 1) (space 0) (safety 1)
                (debug 3) (compilation-speed 3)))
      (flet ((%f () (multiple-value-prog1 0 (return-from %f 0))))
        (complex (%f) 0)))))))

;;; MISC.110A: CAST optimizer forgot to flush LVAR derived type
(assert (zerop (funcall
  (compile
   nil
   '(lambda (a c)
     (declare (type (integer -1294746569 1640996137) a))
     (declare (type (integer -807801310 3) c))
     (declare (optimize (speed 3) (space 3) (safety 0) (debug 0) (compilation-speed 3)))
     (catch 'ct7
       (if
        (logbitp 0
                 (if (/= 0 a)
                     c
                     (ignore-errors
                       (progn (if (ldb-test (byte 0 0) (rational (throw 'ct7 0))) 0 0) 0))))
        0 0))))
   391833530 -32785211)))

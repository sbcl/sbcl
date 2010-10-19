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

(load "compiler-test-util.lisp")

;; The tests in this file assume that EVAL will use the compiler
(when (eq sb-ext:*evaluator-mode* :interpret)
  (invoke-restart 'run-tests::skip-file))

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

;;; feature: we shall complain if functions which are only useful for
;;; their result are called and their result ignored.
(loop for (form expected-des) in
        '(((progn (nreverse (list 1 2)) t)
           "The return value of NREVERSE should not be discarded.")
          ((progn (nreconc (list 1 2) (list 3 4)) t)
           "The return value of NRECONC should not be discarded.")
          ((locally
             (declare (inline sort))
             (sort (list 1 2) #'<) t)
           ;; FIXME: it would be nice if this warned on non-inlined sort
           ;; but the current simple boolean function attribute
           ;; can't express the condition that would be required.
           "The return value of STABLE-SORT-LIST should not be discarded.")
          ((progn (sort (vector 1 2) #'<) t)
           ;; Apparently, SBCL (but not CL) guarantees in-place vector
           ;; sort, so no warning.
           nil)
          ((progn (delete 2 (list 1 2)) t)
           "The return value of DELETE should not be discarded.")
          ((progn (delete-if #'evenp (list 1 2)) t)
           ("The return value of DELETE-IF should not be discarded."))
          ((progn (delete-if #'evenp (vector 1 2)) t)
           ("The return value of DELETE-IF should not be discarded."))
          ((progn (delete-if-not #'evenp (list 1 2)) t)
           "The return value of DELETE-IF-NOT should not be discarded.")
          ((progn (delete-duplicates (list 1 2)) t)
           "The return value of DELETE-DUPLICATES should not be discarded.")
          ((progn (merge 'list (list 1 3) (list 2 4) #'<) t)
           "The return value of MERGE should not be discarded.")
          ((progn (nreconc (list 1 3) (list 2 4)) t)
           "The return value of NRECONC should not be discarded.")
          ((progn (nunion (list 1 3) (list 2 4)) t)
           "The return value of NUNION should not be discarded.")
          ((progn (nintersection (list 1 3) (list 2 4)) t)
           "The return value of NINTERSECTION should not be discarded.")
          ((progn (nset-difference (list 1 3) (list 2 4)) t)
           "The return value of NSET-DIFFERENCE should not be discarded.")
          ((progn (nset-exclusive-or (list 1 3) (list 2 4)) t)
           "The return value of NSET-EXCLUSIVE-OR should not be discarded."))
      for expected = (if (listp expected-des)
                       expected-des
                       (list expected-des))
      do
  (multiple-value-bind (fun warnings-p failure-p)
      (handler-bind ((style-warning (lambda (c)
                      (if expected
                        (let ((expect-one (pop expected)))
                          (assert (search expect-one
                                          (with-standard-io-syntax
                                            (let ((*print-right-margin* nil))
                                              (princ-to-string c))))
                                  ()
                                  "~S should have warned ~S, but instead warned: ~A"
                                  form expect-one c))
                        (error "~S shouldn't give a(nother) warning, but did: ~A" form c)))))
        (compile nil `(lambda () ,form)))
  (declare (ignore warnings-p))
  (assert (functionp fun))
  (assert (null expected)
          ()
          "~S should have warned ~S, but didn't."
          form expected)
  (assert (not failure-p))))

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

(assert
 (raises-error? (multiple-value-bind (a b c)
                    (eval '(truncate 3 4))
                  (declare (integer c))
                  (list a b c))
                type-error))

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

;;; use of declared array types
(handler-bind ((sb-ext:compiler-note #'error))
  (compile nil '(lambda (x)
                 (declare (type (simple-array (simple-string 3) (5)) x)
                          (optimize speed))
                 (aref (aref x 0) 0))))

(handler-bind ((sb-ext:compiler-note #'error))
  (compile nil '(lambda (x)
                 (declare (type (simple-array (simple-array bit (10)) (10)) x)
                          (optimize speed))
                 (1+ (aref (aref x 0) 0)))))

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
;;; rewrite the test case to get the unsigned-byte 32/64
;;; implementation even after implementing some modular arithmetic
;;; with signed-byte 30:
(assert (= 0 (funcall
              (compile nil
                       '(lambda (a)
                         (declare (type (integer 4303063 101130078) a))
                         (mask-field (byte 30 2) (ash a 77))))
              57132532)))
(assert (= 0 (funcall
              (compile nil
                       '(lambda (a)
                         (declare (type (integer 4303063 101130078) a))
                         (mask-field (byte 64 2) (ash a 77))))
              57132532)))
;;; and a similar test case for the signed masking extension (not the
;;; final interface, so change the call when necessary):
(assert (= 0 (funcall
              (compile nil
                       '(lambda (a)
                         (declare (type (integer 4303063 101130078) a))
                         (sb-c::mask-signed-field 30 (ash a 77))))
              57132532)))
(assert (= 0 (funcall
              (compile nil
                       '(lambda (a)
                         (declare (type (integer 4303063 101130078) a))
                         (sb-c::mask-signed-field 61 (ash a 77))))
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
  (sb-ext:compiler-note (c)
    ;; Ignore the note for the float -> pointer conversion of the
    ;; return value.
    (unless (string= (car (last (sb-c::simple-condition-format-arguments c)))
                     "<return value>")
      (error "Compiler does not trust result type assertion."))))

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

;;; efficiency notes for ordinary code
(macrolet ((frob (arglist &body body)
             `(progn
               (handler-case
                   (compile nil '(lambda ,arglist ,@body))
                 (sb-ext:compiler-note (e)
                   (error "bad compiler note for ~S:~%  ~A" ',body e)))
               (catch :got-note
                 (handler-case
                     (compile nil '(lambda ,arglist (declare (optimize speed))
                                    ,@body))
                   (sb-ext:compiler-note (e) (throw :got-note nil)))
                 (error "missing compiler note for ~S" ',body)))))
  (frob (x) (funcall x))
  (frob (x y) (find x y))
  (frob (x y) (find-if x y))
  (frob (x y) (find-if-not x y))
  (frob (x y) (position x y))
  (frob (x y) (position-if x y))
  (frob (x y) (position-if-not x y))
  (frob (x) (aref x 0)))

(macrolet ((frob (style-warn-p form)
             (if style-warn-p
                 `(catch :got-style-warning
                   (handler-case
                       (eval ',form)
                     (style-warning (e) (throw :got-style-warning nil)))
                   (error "missing style-warning for ~S" ',form))
                 `(handler-case
                   (eval ',form)
                   (style-warning (e)
                    (error "bad style-warning for ~S: ~A" ',form e))))))
  (frob t (lambda (x &optional y &key z) (list x y z)))
  (frob nil (lambda (x &optional y z) (list x y z)))
  (frob nil (lambda (x &key y z) (list x y z)))
  (frob t (defgeneric #:foo (x &optional y &key z)))
  (frob nil (defgeneric #:foo (x &optional y z)))
  (frob nil (defgeneric #:foo (x &key y z)))
  (frob t (defun #:foo (x) (flet ((foo (x &optional y &key z) (list x y z))) (foo x x :z x)))))

;;; this was a bug in the LOGXOR type deriver.  The top form gave a
;;; note, because the system failed to derive the fact that the return
;;; from LOGXOR was small and negative, though the bottom one worked.
(handler-bind ((sb-ext:compiler-note #'error))
  (compile nil '(lambda ()
                 (declare (optimize speed (safety 0)))
                 (lambda (x y)
                   (declare (type (integer 3 6) x)
                            (type (integer -6 -3) y))
                   (+ (logxor x y) most-positive-fixnum)))))
(handler-bind ((sb-ext:compiler-note #'error))
  (compile nil '(lambda ()
                 (declare (optimize speed (safety 0)))
                 (lambda (x y)
                   (declare (type (integer 3 6) y)
                            (type (integer -6 -3) x))
                   (+ (logxor x y) most-positive-fixnum)))))

;;; check that modular ash gives the right answer, to protect against
;;; possible misunderstandings about the hardware shift instruction.
(assert (zerop (funcall
                (compile nil '(lambda (x y)
                               (declare (optimize speed)
                                        (type (unsigned-byte 32) x y))
                               (logand #xffffffff (ash x y))))
                1 257)))

;;; code instrumenting problems
(compile nil
  '(lambda ()
    (declare (optimize (debug 3)))
    (list (the integer (if nil 14 t)))))

(compile nil
  '(LAMBDA (A B C D)
    (DECLARE (NOTINLINE LOGORC1 BYTE MASK-FIELD))
    (DECLARE
     (OPTIMIZE (SPEED 1)
      (SPACE 1)
      (SAFETY 1)
      (DEBUG 3)
      (COMPILATION-SPEED 0)))
    (MASK-FIELD (BYTE 7 26)
     (PROGN
       (TAGBODY (THE INTEGER (CATCH 'CT4 (LOGORC1 C -15950))) 1)
       B))))

(compile nil
  '(lambda (buffer i end)
    (declare (optimize (debug 3)))
    (loop (when (not (eql 0 end)) (return)))
    (let ((s (make-string end)))
      (setf (schar s i) (schar buffer i))
      s)))

;;; check that constant string prefix and suffix don't cause the
;;; compiler to emit code deletion notes.
(handler-bind ((sb-ext:code-deletion-note #'error))
  (compile nil '(lambda (s x)
                 (pprint-logical-block (s x :prefix "(")
                   (print x s))))
  (compile nil '(lambda (s x)
                 (pprint-logical-block (s x :per-line-prefix ";")
                   (print x s))))
  (compile nil '(lambda (s x)
                 (pprint-logical-block (s x :suffix ">")
                   (print x s)))))

;;; MISC.427: loop analysis requires complete DFO structure
(assert (eql 17 (funcall
  (compile
   nil
   '(lambda (a)
     (declare (notinline list reduce logior))
     (declare (optimize (safety 2) (compilation-speed 1)
               (speed 3) (space 2) (debug 2)))
     (logior
      (let* ((v5 (reduce #'+ (list 0 a))))
        (declare (dynamic-extent v5))
        v5))))
    17)))

;;;  MISC.434
(assert (zerop (funcall
   (compile
    nil
    '(lambda (a b)
       (declare (type (integer -8431780939320 1571817471932) a))
       (declare (type (integer -4085 0) b))
       (declare (ignorable a b))
       (declare
        (optimize (space 2)
                  (compilation-speed 0)
                  #+sbcl (sb-c:insert-step-conditions 0)
                  (debug 2)
                  (safety 0)
                  (speed 3)))
       (let ((*s5* 0))
         (dotimes (iv1 2 0)
           (let ((*s5*
                  (elt '(1954479092053)
                       (min 0
                            (max 0
                                 (if (< iv1 iv1)
                                     (lognand iv1 (ash iv1 (min 53 iv1)))
                                   iv1))))))
             0)))))
   -7639589303599 -1368)))

(compile
 nil
 '(lambda (a b)
   (declare (type (integer) a))
   (declare (type (integer) b))
   (declare (ignorable a b))
   (declare (optimize (space 2) (compilation-speed 0)
             (debug 0) (safety 0) (speed 3)))
   (dotimes (iv1 2 0)
     (when (< iv1 2) (print 'x)) ;; request for second constraint propagation pass
     (print (if (< iv1 iv1)
                (logand (ash iv1 iv1) 1)
                iv1)))))

;;; MISC.435: lambda var substitution in a deleted code.
(assert (zerop (funcall
   (compile
    nil
    '(lambda (a b c d)
       (declare (notinline aref logandc2 gcd make-array))
       (declare
        (optimize (space 0) (safety 0) (compilation-speed 3)
                  (speed 3) (debug 1)))
       (progn
         (tagbody
          (let* ((v2 (make-array nil :initial-element (catch 'ct1 (go tag2)))))
            (declare (dynamic-extent v2))
            (gcd (go tag2) (logandc2 (catch 'ct2 c) (aref v2))))
          tag2)
         0)))
   3021871717588 -866608 -2 -17194)))

;;; MISC.436, 438: lost reoptimization
(assert (zerop (funcall
   (compile
    nil
    '(lambda (a b)
       (declare (type (integer -2917822 2783884) a))
       (declare (type (integer 0 160159) b))
       (declare (ignorable a b))
       (declare
        (optimize (compilation-speed 1)
                  (speed 3)
                  (safety 3)
                  (space 0)
                  ; #+sbcl (sb-c:insert-step-conditions 0)
                  (debug 0)))
       (if
           (oddp
            (loop for
                  lv1
                  below
                  2
                  count
                  (logbitp 0
                           (1-
                            (ash b
                                 (min 8
                                      (count 0
                                             '(-10197561 486 430631291
                                                         9674068))))))))
           b
         0)))
   1265797 110757)))

(assert (zerop (funcall
   (compile
    nil
    ' (lambda (a)
        (declare (type (integer 0 1696) a))
        ; (declare (ignorable a))
        (declare (optimize (space 2) (debug 0) (safety 1)
                   (compilation-speed 0) (speed 1)))
        (if (logbitp 0 (ash (1- a) (min 11 a))) 0 0)))
   805)))

;;; bug #302
(assert (compile
         nil
         '(lambda (s ei x y)
           (declare (type (simple-array function (2)) s) (type ei ei))
           (funcall (aref s ei) x y))))

;;; MISC.320: ir1-transform can create an intercomponent reference to
;;; a DEFINED-FUN.
(assert (eql 102 (funcall
  (compile
   nil
   '(lambda ()
     (declare (optimize (speed 3) (space 0) (safety 2)
               (debug 2) (compilation-speed 0)))
     (catch 'ct2
       (elt '(102)
            (flet ((%f12 () (rem 0 -43)))
              (multiple-value-call #'%f12 (values))))))))))

;;; MISC.437: lost reoptimization after FLUSH-DEST
(assert (zerop (funcall
  (compile
   nil
   '(lambda (a b c d e)
     (declare (notinline values complex eql))
     (declare
      (optimize (compilation-speed 3)
       (speed 3)
       (debug 1)
       (safety 1)
       (space 0)))
     (flet ((%f10
                (f10-1 f10-2 f10-3
                       &optional (f10-4 (ignore-errors 0)) (f10-5 0)
                       &key &allow-other-keys)
              (if (or (eql 0 0) t) 0 (if f10-1 0 0))))
       (complex (multiple-value-call #'%f10 (values a c b 0 0)) 0))))
   80043 74953652306 33658947 -63099937105 -27842393)))

;;; bug #351 -- program-error for malformed LET and LET*, including those
;;; resulting from SETF of LET.
(dolist (fun (list (compile nil '(lambda () (let :bogus-let :oops)))
                   (compile nil '(lambda () (let* :bogus-let* :oops)))
                   (compile nil '(lambda (x) (push x (let ((y 0)) y))))))
  (assert (functionp fun))
  (multiple-value-bind (res err) (ignore-errors (funcall fun))
    (assert (not res))
    (assert (typep err 'program-error))))

(let ((fun (compile nil '(lambda (x) (random (if x 10 20))))))
  (dotimes (i 100 (error "bad RANDOM distribution"))
    (when (> (funcall fun nil) 9)
      (return t)))
  (dotimes (i 100)
    (when (> (funcall fun t) 9)
      (error "bad RANDOM event"))))

;;; 0.8.17.28-sma.1 lost derived type information.
(with-test (:name "0.8.17.28-sma.1" :fails-on :sparc)
  (handler-bind ((sb-ext:compiler-note (lambda (c) (error "~A" c))))
    (compile nil
      '(lambda (x y v)
        (declare (optimize (speed 3) (safety 0)))
        (declare (type (integer 0 80) x)
         (type (integer 0 11) y)
         (type (simple-array (unsigned-byte 32) (*)) v))
        (setf (aref v 0) (* (* x #.(floor (ash 1 32) (* 11 80))) y))
        nil))))

;;; Bug reported by Robert J. Macomber: instrumenting of more-entry
;;; prevented open coding of %LISTIFY-REST-ARGS.
(let ((f (compile nil '(lambda ()
                        (declare (optimize (debug 3)))
                        (with-simple-restart (blah "blah") (error "blah"))))))
  (handler-bind ((error (lambda (c) (invoke-restart 'blah))))
    (assert (equal (multiple-value-list (funcall f)) '(nil t)))))

;;; Bug reported by Timmy Douglas: overflow in bit vector setter with
;;; constant index and value.
(loop for n-bits = 1 then (* n-bits 2)
      for type = `(unsigned-byte ,n-bits)
      and v-max = (1- (ash 1 n-bits))
      while (<= n-bits sb-vm:n-word-bits)
      do
      (let* ((n (* 2 (1+ (- sb-vm::n-word-bits n-bits))))
             (array1 (make-array n :element-type type))
             (array2 (make-array n :element-type type)))
        (dotimes (i n)
          (dolist (v (list 0 v-max))
            (let ((f (compile nil `(lambda (a)
                                     (declare (type (simple-array ,type (,n)) a))
                                     (setf (aref a ,i) ,v)))))
              (fill array1 (- v-max v))
              (fill array2 (- v-max v))
              (funcall f array1)
              (setf (aref array2 i) v)
              (assert (every #'= array1 array2)))))))

(let ((fn (compile nil '(lambda (x)
                          (declare (type bit x))
                          (declare (optimize speed))
                          (let ((b (make-array 64 :element-type 'bit
                                               :initial-element 0)))
                            (count x b))))))
  (assert (= (funcall fn 0) 64))
  (assert (= (funcall fn 1) 0)))

(let ((fn (compile nil '(lambda (x y)
                          (declare (type simple-bit-vector x y))
                          (declare (optimize speed))
                          (equal x y)))))
  (assert (funcall
           fn
           (make-array 64 :element-type 'bit :initial-element 0)
           (make-array 64 :element-type 'bit :initial-element 0)))
  (assert (not
           (funcall
            fn
            (make-array 64 :element-type 'bit :initial-element 0)
            (let ((b (make-array 64 :element-type 'bit :initial-element 0)))
              (setf (sbit b 63) 1)
              b)))))

;;; MISC.535: compiler failure
(let ((c0 #c(4196.088977268509d0 -15943.3603515625d0)))
    (assert (not (funcall
     (compile
      nil
      `(lambda (p1 p2)
         (declare (optimize speed (safety 1))
                  (type (eql ,c0) p1)
                  (type number p2))
         (eql (the (complex double-float) p1) p2)))
     c0 #c(12 612/979)))))

;;; reported by Lutz Euler: we shouldn't signal a compiler note for
;;; simple-bit-vector functions.
(handler-bind ((sb-ext:compiler-note #'error))
  (compile nil '(lambda (x)
                 (declare (type simple-bit-vector x))
                 (count 1 x))))
(handler-bind ((sb-ext:compiler-note #'error))
  (compile nil '(lambda (x y)
                 (declare (type simple-bit-vector x y))
                 (equal x y))))

;;; MISC.550: CAST merging in IR1 finalization caused unexpected
;;; code transformations.
(assert (eql (funcall
  (compile
   nil
   '(lambda (p1 p2)
     (declare (optimize (speed 3) (safety 2) (debug 3) (space 3))
      (type atom p1)
      (type symbol p2))
     (or p1 (the (eql t) p2))))
   nil t)
  t))

;;; MISC.548: type check weakening converts required type into
;;; optional
(assert (eql t
  (funcall
   (compile
    nil
    '(lambda (p1)
      (declare (optimize (speed 2) (safety 1) (debug 3) (space 2)))
      (atom (the (member f assoc-if write-line t w) p1))))
   t)))

;;; Free special bindings only apply to the body of the binding form, not
;;; the initialization forms.
(assert (eq :good
            (funcall (compile 'nil
                              (lambda ()
                                (let ((x :bad))
                                  (declare (special x))
                                  (let ((x :good))
                                    ((lambda (&optional (y x))
                                       (declare (special x)) y)))))))))

;;; Bug from pfdietz's random tester: the compiler knew that IMAGPART of
;;; a rational was zero, but didn't do the substitution, leading to a
;;; crash in the ASH vop (since a shift of 57 wouldn't fit in the
;;; machine's ASH instruction's immediate field) that the compiler
;;; thought was legitimate.
;;;
;;; FIXME: this has been recorded as bug 383.  The attempted fix (sbcl
;;; 0.9.2.6) led to lots of spurious optimization notes.  So the bug stil
;;; exist and this test case serves as a reminder of the problem.
;;;   --njf, 2005-07-05
#+nil
(compile 'nil
         (LAMBDA (B)
           (DECLARE (TYPE (INTEGER -2 14) B))
           (DECLARE (IGNORABLE B))
           (ASH (IMAGPART B) 57)))

;;; bug reported by Eduardo Mu\~noz
(multiple-value-bind (fun warnings failure)
    (compile nil '(lambda (struct first)
                   (declare (optimize speed))
                   (let* ((nodes (nodes struct))
                          (bars (bars struct))
                          (length (length nodes))
                          (new (make-array length :fill-pointer 0)))
                     (vector-push first new)
                     (loop with i fixnum = 0
                           for newl fixnum = (length new)
                           while (< newl length) do
                           (let ((oldl (length new)))
                             (loop for j fixnum from i below newl do
                                   (dolist (n (node-neighbours (aref new j) bars))
                                     (unless (find n new)
                                       (vector-push n new))))
                             (setq i oldl)))
                     new)))
  (declare (ignore fun warnings failure))
  (assert (not failure)))

;;; bug #389: "0.0 can't be converted to type NIL."  (Brian Rowe
;;; sbcl-devel)
(compile nil '(lambda (x y a b c)
               (- y (* (signum x) (sqrt (abs (- (* b x) c)))))))

;;; Type inference from CHECK-TYPE
(let ((count0 0) (count1 0))
  (handler-bind ((sb-ext:compiler-note (lambda (c) (incf count0))))
    (compile nil '(lambda (x)
                   (declare (optimize (speed 3)))
                   (1+ x))))
  ;; forced-to-do GENERIC-+, etc, possible word -> bignum conversion note
  (assert (> count0 1))
  (handler-bind ((sb-ext:compiler-note (lambda (c) (incf count1))))
    (compile nil '(lambda (x)
                   (declare (optimize (speed 3)))
                   (check-type x fixnum)
                   (1+ x))))
  ;; Only the posssible word -> bignum conversion note
  (assert (= count1 1)))

;;; Up to 0.9.8.22 x86-64 had broken return value handling in the
;;; %SET-SAP-REF-DOUBLE/SINGLE VOPs.
(with-test (:name :sap-ref-float)
  (compile nil '(lambda (sap)
                 (let ((x (setf (sb-vm::sap-ref-double sap 0) 1d0)))
                   (1+ x))))
  (compile nil '(lambda (sap)
                 (let ((x (setf (sb-vm::sap-ref-single sap 0) 1d0)))
                   (1+ x)))))

;;; bug #399
(with-test (:name :string-union-types)
  (compile nil '(lambda (x)
                 (declare (type (or (simple-array character (6))
                                    (simple-array character (5))) x))
                 (aref x 0))))

;;; MISC.623: missing functions for constant-folding
(assert (eql 0
             (funcall
              (compile
               nil
               '(lambda ()
                 (declare (optimize (space 2) (speed 0) (debug 2)
                           (compilation-speed 3) (safety 0)))
                 (loop for lv3 below 1
                    count (minusp
                           (loop for lv2 below 2
                              count (logbitp 0
                                             (bit #*1001101001001
                                                  (min 12 (max 0 lv3))))))))))))

;;; MISC.624: erroneous AVER in x86's %LOGBITP VOPs
(assert (eql 0
             (funcall
              (compile
               nil
               '(lambda (a)
                 (declare (type (integer 21 28) a))
                 (declare       (optimize (compilation-speed 1) (safety 2)
                                 (speed 0) (debug 0) (space 1)))
                 (let* ((v7 (flet ((%f3 (f3-1 f3-2)
                                     (loop for lv2 below 1
                                        count
                                        (logbitp 29
                                                 (sbit #*10101111
                                                       (min 7 (max 0 (eval '0))))))))
                              (%f3 0 a))))
                   0)))
              22)))

;;; MISC.626: bandaged AVER was still wrong
(assert (eql -829253
             (funcall
              (compile
               nil
               '(lambda (a)
                  (declare (type (integer -902970 2) a))
                  (declare (optimize (space 2) (debug 0) (compilation-speed 1)
                                     (speed 0) (safety 3)))
                  (prog2 (if (logbitp 30 a) 0 (block b3 0)) a)))
              -829253)))

;; MISC.628: constant-folding %LOGBITP was buggy
(assert (eql t
             (funcall
              (compile
               nil
               '(lambda ()
                  (declare (optimize (safety 3) (space 3) (compilation-speed 3)
                                     (speed 0) (debug 1)))
                  (not (not (logbitp 0 (floor 2147483651 (min -23 0))))))))))

;; mistyping found by random-tester
(assert (zerop
  (funcall
   (compile
    nil
    '(lambda ()
      (declare (optimize (speed 1) (debug 0)
                (space 2) (safety 0) (compilation-speed 0)))
      (unwind-protect 0
        (* (/ (multiple-value-prog1 -29457482 -5602513511) 1))))))))

;; aggressive constant folding (bug #400)
(assert
 (eq t (funcall (compile nil '(lambda () (or t (the integer (/ 1 0))))))))

(with-test (:name (:compiler :constraint-propagation :var-eql-to-non-var-1))
  (assert
   (handler-case
       (compile nil '(lambda (x y)
                       (when (eql x (length y))
                         (locally
                             (declare (optimize (speed 3)))
                           (1+ x)))))
     (compiler-note () (error "The code is not optimized.")))))

(with-test (:name (:compiler :constraint-propagation :var-eql-to-non-var-2))
  (assert
   (handler-case
       (compile nil '(lambda (x y)
                       (when (eql (length y) x)
                         (locally
                             (declare (optimize (speed 3)))
                           (1+ x)))))
     (compiler-note () (error "The code is not optimized.")))))

(with-test (:name (:compiler :constraint-propagation :float-bounds-1))
  (handler-case
      (compile nil '(lambda (x)
                      (declare (type (single-float * (3.0)) x))
                      (when (<= x 2.0)
                        (when (<= 2.0 x)
                          x))))
    (compiler-note () (error "Deleted reachable code."))))

(with-test (:name (:compiler :constraint-propagation :float-bounds-2))
  (catch :note
    (handler-case
        (compile nil '(lambda (x)
                        (declare (type single-float x))
                        (when (< 1.0 x)
                          (when (<= x 1.0)
                            (error "This is unreachable.")))))
      (compiler-note () (throw :note nil)))
    (error "Unreachable code undetected.")))

(with-test (:name (:compiler :constraint-propagation :var-eql-to-var-1))
  (catch :note
    (handler-case
        (compile nil '(lambda (x y)
                        (when (typep y 'fixnum)
                          (when (eql x y)
                            (unless (typep x 'fixnum)
                              (error "This is unreachable"))
                            (setq y nil)))))
      (compiler-note () (throw :note nil)))
    (error "Unreachable code undetected.")))

(with-test (:name (:compiler :constraint-propagation :var-eql-to-var-2))
  (catch :note
    (handler-case
        (compile nil '(lambda (x y)
                        (when (typep y 'fixnum)
                          (when (eql y x)
                            (unless (typep x 'fixnum)
                              (error "This is unreachable"))
                            (setq y nil)))))
      (compiler-note () (throw :note nil)))
    (error "Unreachable code undetected.")))

;; Reported by John Wiseman, sbcl-devel
;; Subject: [Sbcl-devel] float type derivation bug?
;; Date: Tue, 4 Apr 2006 15:28:15 -0700
(with-test (:name (:type-derivation :float-bounds))
  (compile nil '(lambda (bits)
                 (let* ((s (if (= (ash bits -31) 0) 1 -1))
                        (e (logand (ash bits -23) #xff))
                        (m (if (= e 0)
                               (ash (logand bits #x7fffff) 1)
                               (logior (logand bits #x7fffff) #x800000))))
                   (float (* s m (expt 2 (- e 150))))))))

;; Reported by James Knight
;; Subject: [Sbcl-devel] AVER: "(EQ (SB-NAME (SC-SB (TN-SC TN))) 'REGISTERS)"
;; Date: Fri, 24 Mar 2006 19:30:00 -0500
(with-test (:name :logbitp-vop)
  (compile nil
           '(lambda (days shift)
             (declare (type fixnum shift days))
             (let* ((result 0)
                    (canonicalized-shift (+ shift 1))
                    (first-wrapping-day (- 1 canonicalized-shift)))
               (declare (type fixnum result))
               (dotimes (source-day 7)
                 (declare (type (integer 0 6) source-day))
                 (when (logbitp source-day days)
                   (setf result
                         (logior result
                                 (the fixnum
                                   (if (< source-day first-wrapping-day)
                                       (+ source-day canonicalized-shift)
                                       (- (+ source-day
                                             canonicalized-shift) 7)))))))
               result))))

;;; MISC.637: incorrect delaying of conversion of optional entries
;;; with hairy constant defaults
(let ((f '(lambda ()
  (labels ((%f11 (f11-2 &key key1)
             (labels ((%f8 (f8-2 &optional (f8-5 (if nil (return-from %f11 0) 0)))
                        :bad1))
               (%f8 (%f8 0)))
             :bad2))
    :good))))
  (assert (eq (funcall (compile nil f)) :good)))

;;; MISC.555: new reference to an already-optimized local function
(let* ((l '(lambda (p1)
    (declare (optimize (speed 1) (safety 2) (debug 2) (space 0)) (type keyword p1))
    (keywordp p1)))
       (f (compile nil l)))
  (assert (funcall f :good))
  (assert (nth-value 1 (ignore-errors (funcall f 42)))))

;;; Check that the compiler doesn't munge *RANDOM-STATE*.
(let* ((state (make-random-state))
       (*random-state* (make-random-state state))
       (a (random most-positive-fixnum)))
  (setf *random-state* state)
  (compile nil `(lambda (x a)
                  (declare (single-float x)
                           (type (simple-array double-float) a))
                  (+ (loop for i across a
                           summing i)
                     x)))
  (assert (= a (random most-positive-fixnum))))

;;; MISC.641: LET-conversion after physical environment analysis lost NLX-INFOs
(let ((form '(lambda ()
              (declare (optimize (speed 1) (space 0) (debug 2)
                           (compilation-speed 0) (safety 1)))
              (flet ((%f3 (f3-1 &key (key1 (count (floor 0 (min -74 0)) #())))
                          0))
                   (apply #'%f3 0 nil)))))
  (assert (zerop (funcall (compile nil form)))))

;;;  size mismatch: #<SB-VM::EA :DWORD base=#<SB-C:TN t1[RDX]> disp=1> is a :DWORD and #<SB-C:TN t2[RAX]> is a :QWORD. on x86-64
(compile nil '(lambda ()
               (let ((x (make-array '(1) :element-type '(signed-byte 32))))
                 (setf (aref x 0) 1))))

;;; step instrumentation confusing the compiler, reported by Faré
(handler-bind ((warning #'error))
  (compile nil '(lambda ()
                 (declare (optimize (debug 2))) ; not debug 3!
                 (let ((val "foobar"))
                   (map-into (make-array (list (length val))
                                         :element-type '(unsigned-byte 8))
                             #'char-code val)))))

;;; overconfident primitive type computation leading to bogus type
;;; checking.
(let* ((form1 '(lambda (x)
                (declare (type (and condition function) x))
                x))
       (fun1 (compile nil form1))
       (form2 '(lambda (x)
                (declare (type (and standard-object function) x))
                x))
       (fun2 (compile nil form2)))
  (assert (raises-error? (funcall fun1 (make-condition 'error))))
  (assert (raises-error? (funcall fun1 fun1)))
  (assert (raises-error? (funcall fun2 fun2)))
  (assert (eq (funcall fun2 #'print-object) #'print-object)))

;;; LET* + VALUES declaration: while the declaration is a non-standard
;;; and possibly a non-conforming extension, as long as we do support
;;; it, we might as well get it right.
;;;
;;; Bug reported by Kaersten Poeck on sbcl-devel 20061023.
(compile nil '(lambda () (let* () (declare (values list)))))


;;; test for some problems with too large immediates in x86-64 modular
;;; arithmetic vops
(compile nil '(lambda (x) (declare (fixnum x))
               (logand most-positive-fixnum (logxor x most-positive-fixnum))))

(compile nil '(lambda (x) (declare (fixnum x))
               (logand most-positive-fixnum (+ x most-positive-fixnum))))

(compile nil '(lambda (x) (declare (fixnum x))
               (logand most-positive-fixnum (* x most-positive-fixnum))))

;;; bug 256.b
(assert (let (warned-p)
            (handler-bind ((warning (lambda (w) (setf warned-p t))))
              (compile nil
                         '(lambda (x)
                           (list (let ((y (the real x)))
                                   (unless (floatp y) (error ""))
                                   y)
                                 (integer-length x)))))
            warned-p))

;; Dead / in safe code
(with-test (:name :safe-dead-/)
  (assert (eq :error
              (handler-case
                  (funcall (compile nil
                                    '(lambda (x y)
                                      (declare (optimize (safety 3)))
                                      (/ x y)
                                      (+ x y)))
                           1
                           0)
                (division-by-zero ()
                  :error)))))

;;; Dead unbound variable (bug 412)
(with-test (:name :dead-unbound)
  (assert (eq :error
              (handler-case
                  (funcall (compile nil
                                    '(lambda ()
                                      #:unbound
                                      42)))
                (unbound-variable ()
                  :error)))))

;;; No compiler notes from compiling SUBSEQ SIMPLE-VECTOR.
(handler-bind ((sb-ext:compiler-note 'error))
  (assert
   (equalp #(2 3)
           (funcall (compile nil `(lambda (s p e)
                                    (declare (optimize speed)
                                             (simple-vector s))
                                    (subseq s p e)))
                    (vector 1 2 3 4)
                    1
                    3))))

;;; No compiler notes from compiling COPY-SEQ SIMPLE-VECTOR.
(handler-bind ((sb-ext:compiler-note 'error))
  (assert
   (equalp #(1 2 3 4)
           (funcall (compile nil `(lambda (s)
                                    (declare (optimize speed)
                                             (simple-vector s))
                                    (copy-seq s)))
                    (vector 1 2 3 4)))))

;;; bug in adding DATA-VECTOR-REF-WITH-OFFSET to x86-64
(assert (not (mismatch #(1.0f0 2.0f0) (make-array 2 :element-type 'single-float :initial-contents (list 1.0f0 2.0f0)))))

;;; bug in interval-arithmetic used by the compiler: needless attempt to coerce too
;;; large bignums to floats
(dolist (op '(* / + -))
  (let ((fun (compile
              nil
              `(lambda (x)
                 (declare (type (integer 0 #.(* 2 (truncate most-positive-double-float))) x))
                 (,op 0.0d0 x)))))
    (loop repeat 10
          do (let ((arg (random (truncate most-positive-double-float))))
               (assert (eql (funcall fun arg)
                            (funcall op 0.0d0 arg)))))))

(with-test (:name :high-debug-known-function-inlining)
  (let ((fun (compile nil
                      '(lambda ()
                        (declare (optimize (debug 3)) (inline append))
                        (let ((fun (lambda (body)
                                     (append
                                      (first body)
                                      nil))))
                          (funcall fun
                                   '((foo (bar)))))))))
    (funcall fun)))

(with-test (:name :high-debug-known-function-transform-with-optional-arguments)
  (compile nil '(lambda (x y)
               (declare (optimize sb-c::preserve-single-use-debug-variables))
               (if (block nil
                     (some-unknown-function
                      (lambda ()
                        (return (member x y))))
                     t)
                   t
                   (error "~a" y)))))

;;; Compiling W-P-O when the pinned objects are known to be fixnums
;;; or characters.
(compile nil '(lambda (x y)
               (declare (fixnum y) (character x))
               (sb-sys:with-pinned-objects (x y)
                 (some-random-function))))

;;; *CHECK-CONSISTENCY* and TRULY-THE

(with-test (:name :bug-423)
  (let ((sb-c::*check-consistency* t))
    (handler-bind ((warning #'error))
      (flet ((make-lambda (type)
               `(lambda (x)
                  ((lambda (z)
                     (if (listp z)
                         (let ((q (truly-the list z)))
                           (length q))
                         (if (arrayp z)
                             (let ((q (truly-the vector z)))
                               (length q))
                             (error "oops"))))
                   (the ,type x)))))
        (compile nil (make-lambda 'list))
        (compile nil (make-lambda 'vector))))))

;;; this caused a momentary regression when an ill-adviced fix to
;;; bug 427 made ANY-REG suitable for primitive-type T:
;;;
;;; no :MOVE-ARG VOP defined to move #<SB-C:TN t1> (SC SB-VM::SINGLE-REG) to #<SB-C:TN t2> (SC SB-VM::ANY-REG)
;;;    [Condition of type SIMPLE-ERROR]
(compile nil
         '(lambda (frob)
           (labels
               ((%zig (frob)
                  (typecase frob
                    (double-float
                     (setf (sb-alien:deref (sb-alien:cast (sb-alien:sap-alien (unknown1) (* unsigned-char))
                                                          (* double-float))) frob))
                    (hash-table
                     (%zig (the (values (single-float (0.0) 1.0) &optional) (unknown2)))
                     nil))))
             (%zig))))

;;; non-required arguments in HANDLER-BIND
(assert (eq :oops (car (funcall (compile nil
                                         '(lambda (x)
                                           (block nil
                                             (handler-bind ((error (lambda (&rest args) (return (cons :oops args)))))
                                               (/ 2 x)))))
                                0))))

;;; NIL is a legal function name
(assert (eq 'a (flet ((nil () 'a)) (nil))))

;;; misc.528
(assert (null (let* ((x 296.3066f0)
                     (y 22717067)
                     (form `(lambda (r p2)
                              (declare (optimize speed (safety 1))
                                       (type (simple-array single-float nil) r)
                                       (type (integer -9369756340 22717335) p2))
                              (setf (aref r) (* ,x (the (eql 22717067) p2)))
                           (values)))
                     (r (make-array nil :element-type 'single-float))
                     (expected (* x y)))
                (funcall (compile nil form) r y)
                (let ((actual (aref r)))
                  (unless (eql expected actual)
                    (list expected actual))))))
;;; misc.529
(assert (null (let* ((x -2367.3296f0)
                     (y 46790178)
                     (form `(lambda (r p2)
                              (declare (optimize speed (safety 1))
                                       (type (simple-array single-float nil) r)
                                       (type (eql 46790178) p2))
                              (setf (aref r) (+ ,x (the (integer 45893897) p2)))
                              (values)))
                     (r (make-array nil :element-type 'single-float))
                     (expected (+ x y)))
                (funcall (compile nil form) r y)
                (let ((actual (aref r)))
                  (unless (eql expected actual)
                    (list expected actual))))))

;;; misc.556
(assert (eql -1
             (funcall
              (compile nil '(lambda (p1 p2)
                             (declare
                              (optimize (speed 1) (safety 0)
                               (debug 0) (space 0))
                              (type (member 8174.8604) p1)
                              (type (member -95195347) p2))
                             (floor p1 p2)))
              8174.8604 -95195347)))

;;; misc.557
(assert (eql -1
             (funcall
              (compile
               nil
               '(lambda (p1)
                 (declare (optimize (speed 3) (safety 0) (debug 3) (space 1))
                  (type (member -94430.086f0) p1))
                 (floor (the single-float p1) 19311235)))
              -94430.086f0)))

;;; misc.558
(assert (eql -1.0f0
             (funcall
              (compile
               nil
               '(lambda (p1)
                 (declare (optimize (speed 1) (safety 2)
                           (debug 2) (space 3))
                  (type (eql -39466.56f0) p1))
                 (ffloor p1 305598613)))
              -39466.56f0)))

;;; misc.559
(assert (eql 1
             (funcall
              (compile
               nil
               '(lambda (p1)
                 (declare (optimize (speed 1) (safety 1) (debug 1) (space 2))
                  (type (eql -83232.09f0) p1))
                 (ceiling p1 -83381228)))
              -83232.09f0)))

;;; misc.560
(assert (eql 1
             (funcall
              (compile
               nil
               '(lambda (p1)
                 (declare (optimize (speed 1) (safety 1)
                           (debug 1) (space 0))
                  (type (member -66414.414f0) p1))
                 (ceiling p1 -63019173f0)))
              -66414.414f0)))

;;; misc.561
(assert (eql 1.0f0
             (funcall
              (compile
               nil
               '(lambda (p1)
                 (declare (optimize (speed 0) (safety 1)
                           (debug 0) (space 1))
                  (type (eql 20851.398f0) p1))
                 (fceiling p1 80839863)))
              20851.398f0)))

;;; misc.581
(assert (floatp
         (funcall
          (compile nil '(lambda (x)
                         (declare (type (eql -5067.2056) x))
                         (+ 213734822 x)))
          -5067.2056)))

;;; misc.581a
(assert (typep
         (funcall
          (compile nil '(lambda (x) (declare (type (eql -1.0) x))
                         (+ #x1000001 x)))
          -1.0f0)
         'single-float))

;;; misc.582
(assert (plusp (funcall
                (compile
                 nil
                 ' (lambda (p1)
                     (declare (optimize (speed 0) (safety 1) (debug 1) (space 1))
                              (type (eql -39887.645) p1))
                     (mod p1 382352925)))
              -39887.645)))

;;; misc.587
(assert (let ((result (funcall
                       (compile
                        nil
                        '(lambda (p2)
                          (declare (optimize (speed 0) (safety 3) (debug 1) (space 0))
                           (type (eql 33558541) p2))
                          (- 92215.266 p2)))
                       33558541)))
          (typep result 'single-float)))

;;; misc.635
(assert (eql 1
             (let* ((form '(lambda (p2)
                            (declare (optimize (speed 0) (safety 1)
                                      (debug 2) (space 2))
                             (type (member -19261719) p2))
                            (ceiling -46022.094 p2))))
               (values (funcall (compile nil form) -19261719)))))

;;; misc.636
(assert (let* ((x 26899.875)
               (form `(lambda (p2)
                        (declare (optimize (speed 3) (safety 1) (debug 3) (space 1))
                                 (type (member ,x #:g5437 char-code #:g5438) p2))
                        (* 104102267 p2))))
          (floatp (funcall (compile nil form) x))))

;;; misc.622
(assert (eql
         (funcall
           (compile
            nil
            '(lambda (p2)
              (declare (optimize (speed 3) (safety 2) (debug 3) (space 0))
               (type real p2))
              (+ 81535869 (the (member 17549.955 #:g35917) p2))))
           17549.955)
          (+ 81535869 17549.955)))

;;; misc.654
(assert (eql 2
             (let ((form '(lambda (p2)
                           (declare (optimize (speed 0) (safety 2) (debug 0) (space 2))
                            (type (member integer eql) p2))
                           (coerce 2 p2))))
               (funcall (compile nil form) 'integer))))

;;; misc.656
(assert (eql 2
             (let ((form '(lambda (p2)
                           (declare (optimize (speed 0) (safety 2) (debug 0) (space 2))
                            (type (member integer mod) p2))
                           (coerce 2 p2))))
               (funcall (compile nil form) 'integer))))

;;; misc.657
(assert (eql 2
         (let ((form '(lambda (p2)
                       (declare (optimize (speed 0) (safety 2) (debug 0) (space 2))
                        (type (member integer values) p2))
                       (coerce 2 p2))))
           (funcall (compile nil form) 'integer))))

(with-test (:name :string-aref-type)
 (assert (eq 'character
             (funcall (compile nil
                               '(lambda (s)
                                 (ctu:compiler-derived-type (aref (the string s) 0))))
                      "foo"))))

(with-test (:name :base-string-aref-type)
 (assert (eq #+sb-unicode 'base-char
             #-sb-unicode 'character
             (funcall (compile nil
                               '(lambda (s)
                                 (ctu:compiler-derived-type (aref (the base-string s) 0))))
                      (coerce "foo" 'base-string)))))

(with-test (:name :dolist-constant-type-derivation)
  (assert (equal '(integer 1 3)
                 (funcall (compile nil
                                   '(lambda (x)
                                     (dolist (y '(1 2 3))
                                       (when x
                                         (return (ctu:compiler-derived-type y))))))
                          t))))

(with-test (:name :dolist-simple-list-type-derivation)
  (assert (equal '(integer 1 3)
                 (funcall (compile nil
                                   '(lambda (x)
                                     (dolist (y (list 1 2 3))
                                       (when x
                                         (return (ctu:compiler-derived-type y))))))
                          t))))

(with-test (:name :dolist-dotted-constant-list-type-derivation)
  (let* ((warned nil)
         (fun (handler-bind ((style-warning (lambda (c) (push c warned))))
                (compile nil
                         '(lambda (x)
                           (dolist (y '(1 2 3 . 4) :foo)
                             (when x
                               (return (ctu:compiler-derived-type y)))))))))
    (assert (equal '(integer 1 3) (funcall fun t)))
    (assert (= 1 (length warned)))
    (multiple-value-bind (res err) (ignore-errors (funcall fun nil))
      (assert (not res))
      (assert (typep err 'type-error)))))

(with-test (:name :constant-list-destructuring)
  (handler-bind ((sb-ext:compiler-note #'error))
    (progn
      (assert (= 10
                 (funcall
                  (compile nil
                           '(lambda ()
                             (destructuring-bind (a (b c) d) '(1 (2 3) 4)
                               (+ a b c d)))))))
      (assert (eq :feh
                  (funcall
                   (compile nil
                            '(lambda (x)
                              (or x
                               (destructuring-bind (a (b c) d) '(1 "foo" 4)
                                 (+ a b c d)))))
                   :feh))))))

;;; Functions with non-required arguments used to end up with
;;; (&OPTIONAL-DISPATCH ...) as their names.
(with-test (:name :hairy-function-name)
  (assert (eq 'read-line (nth-value 2 (function-lambda-expression #'read-line))))
  (assert (equal "#<FUNCTION READ-LINE>" (princ-to-string #'read-line))))

;;; PROGV + RESTRICT-COMPILER-POLICY
(with-test (:name :progv-and-restrict-compiler-policy)
  (let ((sb-c::*policy-restrictions* sb-c::*policy-restrictions*))
    (restrict-compiler-policy 'debug 3)
    (let ((fun (compile nil '(lambda (x)
                              (let ((i x))
                                (declare (special i))
                                (list i
                                      (progv '(i) (list (+ i 1))
                                        i)
                                      i))))))
      (assert (equal '(1 2 1) (funcall fun 1))))))

;;; It used to be possible to confuse the compiler into
;;; IR2-converting such a call to CONS
(with-test (:name :late-bound-primitive)
  (compile nil `(lambda ()
                  (funcall 'cons 1))))

(with-test (:name :hairy-array-element-type-derivation)
  (compile nil '(lambda (x)
                 (declare (type (and simple-string (satisfies array-has-fill-pointer-p)) x))
                 (array-element-type x))))

(with-test (:name :rest-list-type-derivation)
  (multiple-value-bind (type derivedp)
      (funcall (compile nil `(lambda (&rest args)
                               (ctu:compiler-derived-type args)))
               nil)
    (assert (eq 'list type))
    (assert derivedp)))

(with-test (:name :rest-list-type-derivation2)
  (multiple-value-bind (type derivedp)
      (funcall (funcall (compile nil `(lambda ()
                                        (lambda (&rest args)
                                          (ctu:compiler-derived-type args))))))
    (assert (eq 'list type))
    (assert derivedp)))

(with-test (:name :rest-list-type-derivation3)
  (multiple-value-bind (type derivedp)
      (funcall (funcall (compile nil `(lambda ()
                                        (lambda (&optional x &rest args)
                                          (unless x (error "oops"))
                                          (ctu:compiler-derived-type args)))))
               t)
    (assert (eq 'list type))
    (assert derivedp)))

(with-test (:name :rest-list-type-derivation4)
  (multiple-value-bind (type derivedp)
      (funcall (funcall (compile nil `(lambda ()
                                        (lambda (&optional x &rest args)
                                          (declare (type (or null integer) x))
                                          (when x (setf args x))
                                          (ctu:compiler-derived-type args)))))
               42)
    (assert (equal '(or cons null integer) type))
    (assert derivedp)))

(with-test (:name :base-char-typep-elimination)
  (assert (eq (funcall (lambda (ch)
                         (declare (type base-char ch) (optimize (speed 3) (safety 0)))
                         (typep ch 'base-char))
                       t)
              t)))

(with-test (:name :regression-1.0.24.37)
  (compile nil '(lambda (&key (test (constantly t)))
                 (when (funcall test)
                   :quux))))

;;; Attempt to test a decent cross section of conditions
;;; and values types to move conditionally.
(macrolet
    ((test-comparison (comparator type x y)
       `(progn
          ,@(loop for (result-type a b)
                    in '((nil t   nil)
                         (nil 0   1)
                         (nil 0.0 1.0)
                         (nil 0d0 0d0)
                         (nil 0.0 0d0)
                         (nil #c(1.0 1.0) #c(2.0 2.0))

                         (t      t  nil)
                         (fixnum 0 1)
                         ((unsigned-byte #.sb-vm:n-word-bits)
                          (1+ most-positive-fixnum)
                          (+ 2 most-positive-fixnum))
                         ((signed-byte #.sb-vm:n-word-bits)
                          -1 (* 2 most-negative-fixnum))
                         (single-float 0.0 1.0)
                         (double-float 0d0 1d0))
                  for lambda = (if result-type
                                   `(lambda (x y a b)
                                      (declare (,type x y)
                                               (,result-type a b))
                                      (if (,comparator x y)
                                          a b))
                                   `(lambda (x y)
                                      (declare (,type x y))
                                      (if (,comparator x y)
                                          ,a ,b)))
                  for args = `(,x ,y ,@(and result-type
                                            `(,a ,b)))
                  collect
                  `(progn
                     (eql (funcall (compile nil ',lambda)
                                   ,@args)
                          (eval '(,lambda ,@args))))))))
  (sb-vm::with-float-traps-masked
      (:divide-by-zero :overflow :inexact :invalid)
    (let ((sb-ext:*evaluator-mode* :interpret))
      (declare (sb-ext:muffle-conditions style-warning))
      (test-comparison eql t t nil)
      (test-comparison eql t t t)

      (test-comparison =   t 1 0)
      (test-comparison =   t 1 1)
      (test-comparison =   t (1+ most-positive-fixnum) (+ 2 most-positive-fixnum))
      (test-comparison =   fixnum 1 0)
      (test-comparison =   fixnum 0 0)
      (test-comparison =   (unsigned-byte #.sb-vm:n-word-bits) 1 0)
      (test-comparison =   (unsigned-byte #.sb-vm:n-word-bits) 0 0)
      (test-comparison =   (signed-byte #.sb-vm:n-word-bits)   1 0)
      (test-comparison =   (signed-byte #.sb-vm:n-word-bits)   1 1)

      (test-comparison =   single-float 0.0 1.0)
      (test-comparison =   single-float 1.0 1.0)
      (test-comparison =   single-float (/ 1.0 0.0) (/ 1.0 0.0))
      (test-comparison =   single-float (/ 1.0 0.0) 1.0)
      (test-comparison =   single-float (/ 0.0 0.0) (/ 0.0 0.0))
      (test-comparison =   single-float (/ 0.0 0.0) 0.0)

      (test-comparison =   double-float 0d0 1d0)
      (test-comparison =   double-float 1d0 1d0)
      (test-comparison =   double-float (/ 1d0 0d0) (/ 1d0 0d0))
      (test-comparison =   double-float (/ 1d0 0d0) 1d0)
      (test-comparison =   double-float (/ 0d0 0d0) (/ 0d0 0d0))
      (test-comparison =   double-float (/ 0d0 0d0) 0d0)

      (test-comparison <   t 1 0)
      (test-comparison <   t 0 1)
      (test-comparison <   t 1 1)
      (test-comparison <   t (1+ most-positive-fixnum)  (+ 2 most-positive-fixnum))
      (test-comparison <   t (+ 2 most-positive-fixnum) (1+ most-positive-fixnum))
      (test-comparison <   fixnum 1 0)
      (test-comparison <   fixnum 0 1)
      (test-comparison <   fixnum 0 0)
      (test-comparison <   (unsigned-byte #.sb-vm:n-word-bits) 1 0)
      (test-comparison <   (unsigned-byte #.sb-vm:n-word-bits) 0 1)
      (test-comparison <   (unsigned-byte #.sb-vm:n-word-bits) 0 0)
      (test-comparison <   (signed-byte #.sb-vm:n-word-bits)   1 0)
      (test-comparison <   (signed-byte #.sb-vm:n-word-bits)   0 1)
      (test-comparison <   (signed-byte #.sb-vm:n-word-bits)   1 1)

      (test-comparison <   single-float 0.0 1.0)
      (test-comparison <   single-float 1.0 0.0)
      (test-comparison <   single-float 1.0 1.0)
      (test-comparison <   single-float (/ 1.0 0.0) (/ 1.0 0.0))
      (test-comparison <   single-float (/ 1.0 0.0) 1.0)
      (test-comparison <   single-float 1.0 (/ 1.0 0.0))
      (test-comparison <   single-float (/ 0.0 0.0) (/ 0.0 0.0))
      (test-comparison <   single-float (/ 0.0 0.0) 0.0)

      (test-comparison <   double-float 0d0 1d0)
      (test-comparison <   double-float 1d0 0d0)
      (test-comparison <   double-float 1d0 1d0)
      (test-comparison <   double-float (/ 1d0 0d0) (/ 1d0 0d0))
      (test-comparison <   double-float (/ 1d0 0d0) 1d0)
      (test-comparison <   double-float 1d0 (/ 1d0 0d0))
      (test-comparison <   double-float (/ 0d0 0d0) (/ 0d0 0d0))
      (test-comparison <   double-float (/ 0d0 0d0) 0d0)
      (test-comparison <   double-float 0d0 (/ 0d0 0d0))

      (test-comparison >   t 1 0)
      (test-comparison >   t 0 1)
      (test-comparison >   t 1 1)
      (test-comparison >   t (1+ most-positive-fixnum)  (+ 2 most-positive-fixnum))
      (test-comparison >   t (+ 2 most-positive-fixnum) (1+ most-positive-fixnum))
      (test-comparison >   fixnum 1 0)
      (test-comparison >   fixnum 0 1)
      (test-comparison >   fixnum 0 0)
      (test-comparison >   (unsigned-byte #.sb-vm:n-word-bits) 1 0)
      (test-comparison >   (unsigned-byte #.sb-vm:n-word-bits) 0 1)
      (test-comparison >   (unsigned-byte #.sb-vm:n-word-bits) 0 0)
      (test-comparison >   (signed-byte #.sb-vm:n-word-bits)   1 0)
      (test-comparison >   (signed-byte #.sb-vm:n-word-bits)   0 1)
      (test-comparison >   (signed-byte #.sb-vm:n-word-bits)   1 1)

      (test-comparison >   single-float 0.0 1.0)
      (test-comparison >   single-float 1.0 0.0)
      (test-comparison >   single-float 1.0 1.0)
      (test-comparison >   single-float (/ 1.0 0.0) (/ 1.0 0.0))
      (test-comparison >   single-float (/ 1.0 0.0) 1.0)
      (test-comparison >   single-float 1.0 (/ 1.0 0.0))
      (test-comparison >   single-float (/ 0.0 0.0) (/ 0.0 0.0))
      (test-comparison >   single-float (/ 0.0 0.0) 0.0)

      (test-comparison >   double-float 0d0 1d0)
      (test-comparison >   double-float 1d0 0d0)
      (test-comparison >   double-float 1d0 1d0)
      (test-comparison >   double-float (/ 1d0 0d0) (/ 1d0 0d0))
      (test-comparison >   double-float (/ 1d0 0d0) 1d0)
      (test-comparison >   double-float 1d0 (/ 1d0 0d0))
      (test-comparison >   double-float (/ 0d0 0d0) (/ 0d0 0d0))
      (test-comparison >   double-float (/ 0d0 0d0) 0d0)
      (test-comparison >   double-float 0d0 (/ 0d0 0d0)))))

(with-test (:name :car-and-cdr-type-derivation-conservative)
  (let ((f1 (compile nil
                     `(lambda (y)
                        (declare (optimize speed))
                        (let ((x (the (cons fixnum fixnum) (cons 1 2))))
                          (declare (type (cons t fixnum) x))
                          (rplaca x y)
                          (+ (car x) (cdr x))))))
        (f2 (compile nil
                     `(lambda (y)
                        (declare (optimize speed))
                        (let ((x (the (cons fixnum fixnum) (cons 1 2))))
                          (setf (cdr x) y)
                          (+ (car x) (cdr x)))))))
    (flet ((test-error (e value)
             (assert (typep e 'type-error))
             (assert (eq 'number (type-error-expected-type e)))
             (assert (eq value (type-error-datum e)))))
      (let ((v1 "foo")
            (v2 "bar"))
        (multiple-value-bind (res err) (ignore-errors (funcall f1 v1))
          (assert (not res))
          (test-error err v1))
        (multiple-value-bind (res err) (ignore-errors (funcall f2 v2))
          (assert (not res))
          (test-error err v2))))))

(with-test (:name :array-dimension-derivation-conservative)
  (let ((f (compile nil
                    `(lambda (x)
                       (declare (optimize speed))
                       (declare (type (array * (4 4)) x))
                       (let ((y x))
                         (setq x (make-array '(4 4)))
                         (adjust-array y '(3 5))
                         (array-dimension y 0))))))
    (assert (= 3 (funcall f (make-array '(4 4) :adjustable t))))))

(with-test (:name :with-timeout-code-deletion-note)
  (handler-bind ((sb-ext:code-deletion-note #'error))
    (compile nil `(lambda ()
                    (sb-ext:with-timeout 0
                      (sleep 1))))))

(with-test (:name :full-warning-for-undefined-type-in-cl)
  (assert (eq :full
              (handler-case
                  (compile nil `(lambda (x) (the replace x)))
                (style-warning ()
                  :style)
                (warning ()
                  :full)))))

(with-test (:name :single-warning-for-single-undefined-type)
  (let ((n 0))
    (handler-bind ((warning (lambda (c)
                              (declare (ignore c))
                              (incf n))))
      (compile nil `(lambda (x) (the #:no-type x)))
      (assert (= 1 n))
      (compile nil `(lambda (x) (the 'fixnum x)))
      (assert (= 2 n)))))

(with-test (:name :complex-subtype-dumping-in-xc)
  (assert
   (= sb-vm:complex-single-float-widetag
      (sb-kernel:widetag-of
       (sb-vm:saetp-initial-element-default (sb-c::find-saetp '(complex single-float))))))
  (assert
   (= sb-vm:complex-double-float-widetag
      (sb-kernel:widetag-of
       (sb-vm:saetp-initial-element-default (sb-c::find-saetp '(complex double-float)))))))

(with-test (:name :complex-single-float-fill)
  (assert (every (lambda (x) (= #c(1.0 2.0) x))
                 (funcall
                  (compile nil
                           `(lambda (n x)
                              (make-array (list n)
                                          :element-type '(complex single-float)
                                          :initial-element x)))
                  10
                  #c(1.0 2.0)))))

(with-test (:name :regression-1.0.28.21)
  (let ((fun (compile nil `(lambda (x) (typep x '(simple-array * 1))))))
    (assert (funcall fun (vector 1 2 3)))
    (assert (funcall fun "abc"))
    (assert (not (funcall fun (make-array '(2 2)))))))

(with-test (:name :no-silly-compiler-notes-from-character-function)
  (let (current)
    (handler-bind ((compiler-note (lambda (e) (error "~S: ~A" current e))))
      (dolist (name '(char-code char-int character char-name standard-char-p
                      graphic-char-p alpha-char-p upper-case-p lower-case-p
                      both-case-p digit-char-p alphanumericp digit-char-p))
        (setf current name)
        (compile nil `(lambda (x)
                        (declare (character x) (optimize speed))
                        (,name x))))
      (dolist (name '(char= char/= char< char> char<= char>= char-equal
                      char-not-equal char-lessp char-greaterp char-not-greaterp
                      char-not-lessp))
        (setf current name)
        (compile nil `(lambda (x y)
                        (declare (character x y) (optimize speed))
                        (,name x y)))))))

;;; optimizing make-array
(with-test (:name (make-array :open-code-initial-contents))
  (assert (not (ctu:find-named-callees
                (compile nil
                         `(lambda (x y z)
                            (make-array '(3) :initial-contents (list x y z)))))))
  (assert (not (ctu:find-named-callees
                (compile nil
                         `(lambda (x y z)
                            (make-array '3 :initial-contents (vector x y z)))))))
  (assert (not (ctu:find-named-callees
                (compile nil
                         `(lambda (x y z)
                            (make-array '3 :initial-contents `(,x ,y ,z))))))))

;;; optimizing array-in-bounds-p
(with-test (:name :optimize-array-in-bounds-p)
  (locally
    (macrolet ((find-callees (&body body)
                 `(ctu:find-named-callees
                    (compile nil
                             '(lambda ()
                                ,@body))
                    :name 'array-in-bounds-p))
               (must-optimize (&body exprs)
                 `(progn
                    ,@(loop for expr in exprs
                            collect `(assert (not (find-callees
                                                   ,expr))))))
               (must-not-optimize (&body exprs)
                 `(progn
                    ,@(loop for expr in exprs
                            collect `(assert (find-callees
                                              ,expr))))))
      (must-optimize
        ;; in bounds
        (let ((a (make-array '(1))))
          (array-in-bounds-p a 0))
        ;; exceeds upper bound (constant)
        (let ((a (make-array '(1))))
          (array-in-bounds-p a 1))
        ;; exceeds upper bound (interval)
        (let ((a (make-array '(1))))
          (array-in-bounds-p a (+ 1 (random 2))))
        ;; negative lower bound (constant)
        (let ((a (make-array '(1))))
          (array-in-bounds-p a -1))
        ;; negative lower bound (interval)
        (let ((a (make-array 3))
              (i (- (random 1) 20)))
          (array-in-bounds-p a i))
        ;; multiple known dimensions
        (let ((a (make-array '(1 1))))
          (array-in-bounds-p a 0 0))
        ;; union types
        (let ((s (the (simple-string 10) (eval "0123456789"))))
          (array-in-bounds-p s 9)))
      (must-not-optimize
       ;; don't trust non-simple array length in safety=1
       (let ((a (the (array * (10)) (make-array 10 :adjustable t))))
         (eval `(adjust-array ,a 0))
         (array-in-bounds-p a 9))
       ;; same for a union type
       (let ((s (the (string 10) (make-array 10
                                             :element-type 'character
                                             :adjustable t))))
         (eval `(adjust-array ,s 0))
         (array-in-bounds-p s 9))
       ;; single unknown dimension
       (let ((a (make-array (random 20))))
         (array-in-bounds-p a 10))
       ;; multiple unknown dimensions
       (let ((a (make-array (list (random 20) (random 5)))))
         (array-in-bounds-p a 5 2))
       ;; some other known dimensions
       (let ((a (make-array (list 1 (random 5)))))
         (array-in-bounds-p a 0 2))
       ;; subscript might be negative
       (let ((a (make-array 5)))
         (array-in-bounds-p a (- (random 3) 2)))
       ;; subscript might be too large
       (let ((a (make-array 5)))
         (array-in-bounds-p a (random 6)))
       ;; unknown upper bound
       (let ((a (make-array 5)))
         (array-in-bounds-p a (get-universal-time)))
       ;; unknown lower bound
       (let ((a (make-array 5)))
         (array-in-bounds-p a (- (get-universal-time))))
       ;; in theory we should be able to optimize
       ;; the following but the current implementation
       ;; doesn't cut it because the array type's
       ;; dimensions get reported as (* *).
       (let ((a (make-array (list (random 20) 1))))
         (array-in-bounds-p a 5 2))))))

;;; optimizing (EXPT -1 INTEGER)
(test-util:with-test (:name (expt minus-one integer))
  (dolist (x '(-1 -1.0 -1.0d0))
    (let ((fun (compile nil `(lambda (x) (expt ,x (the fixnum x))))))
      (assert (not (ctu:find-named-callees fun)))
      (dotimes (i 12)
        (if (oddp i)
            (assert (eql x (funcall fun i)))
            (assert (eql (- x) (funcall fun i))))))))

(with-test (:name (load-time-value :type-derivation))
  (flet ((test (type form value-cell-p)
           (let ((derived (funcall (compile
                                    nil
                                    `(lambda ()
                                       (ctu:compiler-derived-type
                                        (load-time-value ,form)))))))
             (unless (equal type derived)
              (error "wanted ~S, got ~S" type derived)))))
    (let ((* 10))
      (test '(integer 11 11) '(+ * 1) nil))
    (let ((* "fooo"))
      (test '(integer 4 4) '(length *) t))))

(with-test (:name :float-division-using-exact-reciprocal)
  (flet ((test (lambda-form arg res &key (check-insts t))
           (let* ((fun (compile nil lambda-form))
                  (disassembly (with-output-to-string (s)
                                  (disassemble fun :stream s))))
             ;; Let's make sure there is no division at runtime: for x86 and
             ;; x86-64 that implies an FDIV, DIVSS, or DIVSD instruction, so
             ;; look for DIV in the disassembly. It's a terrible KLUDGE, but
             ;; it works.
             #+(or x86 x86-64)
             (when check-insts
               (assert (not (search "DIV" disassembly))))
             ;; No generic arithmetic!
             (assert (not (search "GENERIC" disassembly)))
             (assert (eql res (funcall fun arg))))))
    (dolist (c '(128 64 32 16 8 4 2 1 1/2 1/4 1/8 1/16 1/32 1/64))
      (dolist (type '(single-float double-float))
        (let* ((cf (coerce c type))
               (arg (- (random (* 2 cf)) cf))
               (r1 (eval `(/ ,arg ,cf)))
               (r2 (eval `(/ ,arg ,(- cf)))))
          (test `(lambda (x) (declare (,type x)) (/ x ,cf)) arg r1)
          (test `(lambda (x) (declare (,type x)) (/ x ,(- cf))) arg r2)
          ;; rational args should get optimized as well
          (test `(lambda (x) (declare (,type x)) (/ x ,c)) arg r1)
          (test `(lambda (x) (declare (,type x)) (/ x ,(- c))) arg r2))))
    ;; Also check that inexact reciprocals (1) are not used by default (2) are
    ;; used with FLOAT-ACCURACY=0.
    (dolist (type '(single-float double-float))
      (let ((trey (coerce 3 type))
            (one (coerce 1 type)))
        (test `(lambda (x) (declare (,type x)) (/ x 3)) trey one
              :check-insts nil)
        (test `(lambda (x)
                 (declare (,type x)
                          (optimize (sb-c::float-accuracy 0)))
                 (/ x 3))
              trey (eval `(* ,trey (/ ,trey))))))))

(with-test (:name :float-multiplication-by-one)
  (flet ((test (lambda-form arg &optional (result arg))
           (let* ((fun1 (compile nil lambda-form))
                  (fun2 (funcall (compile nil `(lambda ()
                                                 (declare (optimize (sb-c::float-accuracy 0)))
                                                 ,lambda-form))))
                  (disassembly1 (with-output-to-string (s)
                                  (disassemble fun1 :stream s)))
                  (disassembly2 (with-output-to-string (s)
                                  (disassemble fun2 :stream s))))
             ;; Multiplication at runtime should be eliminated only with
             ;; FLOAT-ACCURACY=0. (To catch SNaNs.)
             #+(or x86 x86-64)
             (assert (and (search "MUL" disassembly1)
                          (not (search "MUL" disassembly2))))
             ;; Not generic arithmetic, please!
             (assert (and (not (search "GENERIC" disassembly1))
                          (not (search "GENERIC" disassembly2))))
             (assert (eql result (funcall fun1 arg)))
             (assert (eql result (funcall fun2 arg))))))
    (dolist (type '(single-float double-float))
      (let* ((one (coerce 1 type))
             (arg (random (* 2 one)))
             (-r (- arg)))
        (test `(lambda (x) (declare (,type x)) (* x 1)) arg)
        (test `(lambda (x) (declare (,type x)) (* x -1)) arg -r)
        (test `(lambda (x) (declare (,type x)) (* x ,one)) arg)
        (test `(lambda (x) (declare (,type x)) (* x ,(- one))) arg -r)))))

(with-test (:name :float-addition-of-zero)
  (flet ((test (lambda-form arg &optional (result arg))
           (let* ((fun1 (compile nil lambda-form))
                  (fun2 (funcall (compile nil `(lambda ()
                                                 (declare (optimize (sb-c::float-accuracy 0)))
                                                 ,lambda-form))))
                  (disassembly1 (with-output-to-string (s)
                                  (disassemble fun1 :stream s)))
                  (disassembly2 (with-output-to-string (s)
                                  (disassemble fun2 :stream s))))
             ;; Let's make sure there is no addition at runtime: for x86 and
             ;; x86-64 that implies an FADD, ADDSS, or ADDSD instruction, so
             ;; look for the ADDs in the disassembly. It's a terrible KLUDGE,
             ;; but it works. Unless FLOAT-ACCURACY is zero, we leave the
             ;; addition in to catch SNaNs.
             #+x86
             (assert (and (search "FADD" disassembly1)
                          (not (search "FADD" disassembly2))))
             #+x86-64
             (let ((inst (if (typep result 'double-float)
                             "ADDSD" "ADDSS")))
               (assert (and (search inst disassembly1)
                            (not (search inst disassembly2)))))
             (assert (eql result (funcall fun1 arg)))
             (assert (eql result (funcall fun2 arg))))))
    (test `(lambda (x) (declare (single-float x)) (+ x 0)) 123.45)
    (test `(lambda (x) (declare (single-float x)) (+ x 0.0)) 543.21)
    (test `(lambda (x) (declare (single-float x)) (+ x 0.0d0)) 42.00 42.d0)
    (test `(lambda (x) (declare (double-float x)) (+ x 0)) 123.45d0)
    (test `(lambda (x) (declare (double-float x)) (+ x 0.0)) 543.21d0)
    (test `(lambda (x) (declare (double-float x)) (+ x 0.0d0)) 42.d0)))

(with-test (:name :float-substraction-of-zero)
  (flet ((test (lambda-form arg &optional (result arg))
           (let* ((fun1 (compile nil lambda-form))
                  (fun2 (funcall (compile nil `(lambda ()
                                                 (declare (optimize (sb-c::float-accuracy 0)))
                                                 ,lambda-form))))
                  (disassembly1 (with-output-to-string (s)
                                  (disassemble fun1 :stream s)))
                  (disassembly2 (with-output-to-string (s)
                                  (disassemble fun2 :stream s))))
             ;; Let's make sure there is no substraction at runtime: for x86
             ;; and x86-64 that implies an FSUB, SUBSS, or SUBSD instruction,
             ;; so look for SUB in the disassembly. It's a terrible KLUDGE,
             ;; but it works. Unless FLOAT-ACCURACY is zero, we leave the
             ;; substraction in in to catch SNaNs.
             #+x86
             (assert (and (search "FSUB" disassembly1)
                          (not (search "FSUB" disassembly2))))
             #+x86-64
             (let ((inst (if (typep result 'double-float)
                             "SUBSD" "SUBSS")))
               (assert (and (search inst disassembly1)
                            (not (search inst disassembly2)))))
             (assert (eql result (funcall fun1 arg)))
             (assert (eql result (funcall fun2 arg))))))
    (test `(lambda (x) (declare (single-float x)) (- x 0)) 123.45)
    (test `(lambda (x) (declare (single-float x)) (- x 0.0)) 543.21)
    (test `(lambda (x) (declare (single-float x)) (- x 0.0d0)) 42.00 42.d0)
    (test `(lambda (x) (declare (double-float x)) (- x 0)) 123.45d0)
    (test `(lambda (x) (declare (double-float x)) (- x 0.0)) 543.21d0)
    (test `(lambda (x) (declare (double-float x)) (- x 0.0d0)) 42.d0)))

(with-test (:name :float-multiplication-by-two)
  (flet ((test (lambda-form arg &optional (result arg))
           (let* ((fun1 (compile nil lambda-form))
                  (fun2 (funcall (compile nil `(lambda ()
                                                 (declare (optimize (sb-c::float-accuracy 0)))
                                                 ,lambda-form))))
                  (disassembly1 (with-output-to-string (s)
                                  (disassemble fun1 :stream s)))
                  (disassembly2 (with-output-to-string (s)
                                  (disassemble fun2 :stream s))))
             ;; Let's make sure there is no multiplication at runtime: for x86
             ;; and x86-64 that implies an FMUL, MULSS, or MULSD instruction,
             ;; so look for MUL in the disassembly. It's a terrible KLUDGE,
             ;; but it works.
             #+(or x86 x86-64)
             (assert (and (not (search "MUL" disassembly1))
                          (not (search "MUL" disassembly2))))
             (assert (eql result (funcall fun1 arg)))
             (assert (eql result (funcall fun2 arg))))))
    (test `(lambda (x) (declare (single-float x)) (* x 2)) 123.45 246.9)
    (test `(lambda (x) (declare (single-float x)) (* x 2.0)) 543.21 1086.42)
    (test `(lambda (x) (declare (single-float x)) (* x 2.0d0)) 42.00 84.d0)
    (test `(lambda (x) (declare (double-float x)) (* x 2)) 123.45d0 246.9d0)
    (test `(lambda (x) (declare (double-float x)) (* x 2.0)) 543.21d0 1086.42d0)
    (test `(lambda (x) (declare (double-float x)) (* x 2.0d0)) 42.0d0 84.0d0)))

(with-test (:name :bug-392203)
  ;; Used to hit an AVER in COMVERT-MV-CALL.
  (assert (zerop
           (funcall
            (compile nil
                     `(lambda ()
                        (flet ((k (&rest x) (declare (ignore x)) 0))
                          (multiple-value-call #'k #'k))))))))

(with-test (:name :allocate-closures-failing-aver)
  (let ((f (compile nil `(lambda ()
                           (labels ((k (&optional x) #'k)))))))
    (assert (null (funcall f)))))

(with-test (:name :flush-vector-creation)
  (let ((f (compile nil `(lambda ()
                           (dotimes (i 1024)
                             (vector i i i))
                           t))))
    (ctu:assert-no-consing (funcall f))))

(with-test (:name :array-type-predicates)
  (dolist (et sb-kernel::*specialized-array-element-types*)
    (when et
      (let* ((v (make-array 3 :element-type et))
             (fun (compile nil `(lambda ()
                                  (list
                                   (if (typep ,v '(simple-array ,et (*)))
                                       :good
                                       :bad)
                                   (if (typep (elt ,v 0) '(simple-array ,et (*)))
                                       :bad
                                       :good))))))
        (assert (equal '(:good :good) (funcall fun)))))))

(with-test (:name :truncate-float)
  (let ((s (compile nil `(lambda (x)
                           (declare (single-float x))
                           (truncate x))))
        (d (compile nil `(lambda (x)
                           (declare (double-float x))
                           (truncate x))))
        (s-inlined (compile nil '(lambda (x)
                                  (declare (type (single-float 0.0s0 1.0s0) x))
                                  (truncate x))))
        (d-inlined (compile nil '(lambda (x)
                                  (declare (type (double-float 0.0d0 1.0d0) x))
                                  (truncate x)))))
    ;; Check that there is no generic arithmetic
    (assert (not (search "GENERIC"
                         (with-output-to-string (out)
                           (disassemble s :stream out)))))
    (assert (not (search "GENERIC"
                         (with-output-to-string (out)
                           (disassemble d :stream out)))))
    ;; Check that we actually inlined the call when we were supposed to.
    (assert (not (search "UNARY-TRUNCATE"
                         (with-output-to-string (out)
                           (disassemble s-inlined :stream out)))))
    (assert (not (search "UNARY-TRUNCATE"
                         (with-output-to-string (out)
                           (disassemble d-inlined :stream out)))))))

(with-test (:name :make-array-unnamed-dimension-leaf)
  (let ((fun (compile nil `(lambda (stuff)
                             (make-array (map 'list 'length stuff))))))
    (assert (equalp #2A((0 0 0) (0 0 0))
                    (funcall fun '((1 2) (1 2 3)))))))

(with-test (:name :fp-decoding-funs-not-flushable-in-safe-code)
  (dolist (name '(float-sign float-radix float-digits float-precision decode-float
                  integer-decode-float))
    (let ((fun (compile nil `(lambda (x)
                               (declare (optimize safety))
                               (,name x)
                               nil))))
      (flet ((test (arg)
               (unless (eq :error
                           (handler-case
                               (funcall fun arg)
                             (error () :error)))
                 (error "(~S ~S) did not error"
                        name arg))))
        ;; No error
        (funcall fun 1.0)
        ;; Error
        (test 'not-a-float)
        (when (member name '(decode-float integer-decode-float))
          (test sb-ext:single-float-positive-infinity))))))

(with-test (:name :sap-ref-16)
  (let* ((fun (compile nil `(lambda (x y)
                              (declare (type sb-sys:system-area-pointer x)
                                       (type (integer 0 100) y))
                              (sb-sys:sap-ref-16 x (+ 4 y)))))
         (vector (coerce '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
                         '(simple-array (unsigned-byte 8) (*))))
         (sap (sb-sys:vector-sap vector))
         (ret (funcall fun sap 0)))
    ;; test for either endianness
    (assert (or (= ret (+ (* 5 256) 4)) (= ret (+ (* 4 256) 5))))))

(with-test (:name :coerce-type-warning)
  (dolist (type '(t (unsigned-byte 8) (unsigned-byte 16) (unsigned-byte 32)
                  (signed-byte 8) (signed-byte 16) (signed-byte 32)))
    (multiple-value-bind (fun warningsp failurep)
        (compile nil `(lambda (x)
                        (declare (type simple-vector x))
                        (coerce x '(vector ,type))))
      (assert (null warningsp))
      (assert (null failurep))
      (assert (typep (funcall fun #(1)) `(simple-array ,type (*)))))))

(with-test (:name :truncate-double-float)
  (let ((fun (compile nil `(lambda (x)
                             (multiple-value-bind (q r)
                                 (truncate (coerce x 'double-float))
                               (declare (type unsigned-byte q)
                                        (type double-float r))
                               (list q r))))))
    (assert (equal (funcall fun 1.0d0) '(1 0.0d0)))))

(with-test (:name :set-slot-value-no-warning)
  (let ((notes 0))
    (handler-bind ((warning #'error)
                   (sb-ext:compiler-note (lambda (c)
                                           (declare (ignore c))
                                           (incf notes))))
      (compile nil `(lambda (x y)
                      (declare (optimize speed safety))
                      (setf (slot-value x 'bar) y))))
    (assert (= 1 notes))))

(with-test (:name :concatenate-string-opt)
  (flet ((test (type grep)
           (let* ((fun (compile nil `(lambda (a b c d e)
                                      (concatenate ',type a b c d e))))
                  (args '("foo" #(#\.) "bar" (#\-) "quux"))
                  (res (apply fun args)))
             (assert (search grep (with-output-to-string (out)
                                    (disassemble fun :stream out))))
             (assert (equal (apply #'concatenate type args)
                            res))
             (assert (typep res type)))))
    (test 'string "%CONCATENATE-TO-STRING")
    (test 'simple-string "%CONCATENATE-TO-STRING")
    (test 'base-string "%CONCATENATE-TO-BASE-STRING")
    (test 'simple-base-string "%CONCATENATE-TO-BASE-STRING")))

(with-test (:name :satisfies-no-local-fun)
  (let ((fun (compile nil `(lambda (arg)
                             (labels ((local-not-global-bug (x)
                                        t)
                                      (bar (x)
                                        (typep x '(satisfies local-not-global-bug))))
                               (bar arg))))))
    (assert (eq 'local-not-global-bug
                (handler-case
                    (funcall fun 42)
                  (undefined-function (c)
                    (cell-error-name c)))))))

;;; Prior to 1.0.32.x, dumping a fasl with a function with a default
;;; argument that is a complex structure (needing make-load-form
;;; processing) failed an AVER.  The first attempt at a fix caused
;;; doing the same in-core to break.
(with-test (:name :bug-310132)
  (compile nil '(lambda (&optional (foo #p"foo/bar")))))

(with-test (:name :bug-309129)
  (let* ((src '(lambda (v) (values (svref v 0) (vector-pop v))))
         (warningp nil)
         (fun (handler-bind ((warning (lambda (c)
                                        (setf warningp t) (muffle-warning c))))
                (compile nil src))))
    (assert warningp)
    (handler-case (funcall fun #(1))
      (type-error (c)
        ;; we used to put simply VECTOR into EXPECTED-TYPE, rather
        ;; than explicitly (AND VECTOR (NOT SIMPLE-ARRAY))
        (assert (not (typep (type-error-datum c) (type-error-expected-type c)))))
      (:no-error (&rest values)
        (declare (ignore values))
        (error "no error")))))

(with-test (:name :unary-round-type-derivation)
  (let* ((src '(lambda (zone)
                (multiple-value-bind (h m) (truncate (abs zone) 1.0)
                  (declare (ignore h))
                  (round (* 60.0 m)))))
         (fun (compile nil src)))
    (assert (= (funcall fun 0.5) 30))))

(with-test (:name :bug-525949)
  (let* ((src '(lambda ()
                (labels ((always-one () 1)
                         (f (z)
                           (let ((n (funcall z)))
                             (declare (fixnum n))
                             (the double-float (expt n 1.0d0)))))
                  (f #'always-one))))
         (warningp nil)
         (fun (handler-bind ((warning (lambda (c)
                                        (setf warningp t) (muffle-warning c))))
                (compile nil src))))
    (assert (not warningp))
    (assert (= 1.0d0 (funcall fun)))))

(with-test (:name :%array-data-vector-type-derivation)
  (let* ((f (compile nil
                     `(lambda (ary)
                        (declare (type (simple-array (unsigned-byte 32) (3 3)) ary))
                        (setf (aref ary 0 0) 0))))
         (text (with-output-to-string (s)
                 (disassemble f :stream s))))
    (assert (not (search "OBJECT-NOT-SIMPLE-ARRAY-UNSIGNED-BYTE-32-ERROR" text)))))

(with-test (:name :array-storage-vector-type-derivation)
  (let ((f (compile nil
                    `(lambda (ary)
                       (declare (type (simple-array (unsigned-byte 32) (3 3)) ary))
                       (ctu:compiler-derived-type (array-storage-vector ary))))))
    (assert (equal '(simple-array (unsigned-byte 32) (9))
                   (funcall f (make-array '(3 3) :element-type '(unsigned-byte 32)))))))

(with-test (:name :bug-523612)
  (let ((fun
         (compile nil
                  `(lambda (&key toff)
                     (make-array 3 :element-type 'double-float
                                 :initial-contents
                                 (if toff (list toff 0d0 0d0) (list 0d0 0d0 0d0)))))))
    (assert (equalp (vector 0.0d0 0.0d0 0.0d0) (funcall fun :toff nil)))
    (assert (equalp (vector 2.3d0 0.0d0 0.0d0) (funcall fun :toff 2.3d0)))))

(with-test (:name :bug-309788)
  (let ((fun
         (compile nil
                  `(lambda (x)
                     (declare (optimize speed))
                     (let ((env nil))
                       (typep x 'fixnum env))))))
    (assert (not (ctu:find-named-callees fun)))))

(with-test (:name :bug-309124)
  (let ((fun
         (compile nil
                  `(lambda (x)
                     (declare (integer x))
                     (declare (optimize speed))
                     (cond ((typep x 'fixnum)
                            "hala")
                           ((typep x 'fixnum)
                            "buba")
                           ((typep x 'bignum)
                            "hip")
                           (t
                            "zuz"))))))
    (assert (equal (list "hala" "hip")
                   (sort (ctu:find-code-constants fun :type 'string)
                         #'string<)))))

(with-test (:name :bug-316078)
  (let ((fun
         (compile nil
                  `(lambda (x)
                     (declare (type (and simple-bit-vector (satisfies bar)) x)
                              (optimize speed))
                     (elt x 5)))))
    (assert (not (ctu:find-named-callees fun)))
    (assert (= 1 (funcall fun #*000001)))
    (assert (= 0 (funcall fun #*000010)))))

(with-test (:name :mult-by-one-in-float-acc-zero)
  (assert (eql 1.0 (funcall (compile nil `(lambda (x)
                                            (declare (optimize (sb-c::float-accuracy 0)))
                                            (* x 1.0)))
                            1)))
  (assert (eql -1.0 (funcall (compile nil `(lambda (x)
                                             (declare (optimize (sb-c::float-accuracy 0)))
                                             (* x -1.0)))
                             1)))
  (assert (eql 1.0d0 (funcall (compile nil `(lambda (x)
                                              (declare (optimize (sb-c::float-accuracy 0)))
                                              (* x 1.0d0)))
                              1)))
  (assert (eql -1.0d0 (funcall (compile nil `(lambda (x)
                                               (declare (optimize (sb-c::float-accuracy 0)))
                                               (* x -1.0d0)))
                               1))))

(with-test (:name :dotimes-non-integer-counter-value)
  (assert (raises-error? (dotimes (i 8.6)) type-error)))

(with-test (:name :bug-454681)
  ;; This used to break due to reference to a dead lambda-var during
  ;; inline expansion.
  (assert (compile nil
                   `(lambda ()
                      (multiple-value-bind (iterator+977 getter+978)
                          (does-not-exist-but-does-not-matter)
                        (flet ((iterator+976 ()
                                 (funcall iterator+977)))
                          (declare (inline iterator+976))
                          (let ((iterator+976 #'iterator+976))
                            (funcall iterator+976))))))))

(with-test (:name :complex-float-local-fun-args)
  ;; As of 1.0.27.14, the lambda below failed to compile due to the
  ;; compiler attempting to pass unboxed complex floats to Z and the
  ;; MOVE-ARG method not expecting the register being used as a
  ;; temporary frame pointer.  Reported by sykopomp in #lispgames,
  ;; reduced test case provided by _3b`.
  (compile nil '(lambda (a)
                  (labels ((z (b c)
                              (declare ((complex double-float) b c))
                              (* b (z b c))))
                          (loop for i below 10 do
                                (setf a (z a a)))))))

(with-test (:name :bug-309130)
  (assert (eq :warning
              (handler-case
                  (compile nil `(lambda () (svref (make-array 8 :adjustable t) 1)))
                ((and warning (not style-warning)) ()
                  :warning))))
  (assert (eq :warning
              (handler-case
                  (compile nil `(lambda (x)
                                  (declare (optimize (debug 0)))
                                  (declare (type vector x))
                                  (list (fill-pointer x) (svref x 1))))
                ((and warning (not style-warning)) ()
                  :warning))))
  (assert (eq :warning
              (handler-case
                  (compile nil `(lambda (x)
                                  (list (vector-push (svref x 0) x))))
                ((and warning (not style-warning)) ()
                  :warning))))
  (assert (eq :warning
              (handler-case
                  (compile nil `(lambda (x)
                                  (list (vector-push-extend (svref x 0) x))))
                ((and warning (not style-warning)) ()
                  :warning)))))

(with-test (:name :bug-646796)
  (assert 42
          (funcall
           (compile nil
                    `(lambda ()
                       (load-time-value (the (values fixnum) 42)))))))

(with-test (:name :bug-654289)
  ;; Test that compile-times don't explode when quoted constants
  ;; get big.
  (labels ((time-n (n)
             (let* ((tree (make-tree (expt 10 n) nil))
                    (t0 (get-internal-run-time))
                    (f (compile nil `(lambda (x) (eq x (quote ,tree)))))
                    (t1 (get-internal-run-time)))
               (assert (funcall f tree))
               (- t1 t0)))
           (make-tree (n acc)
             (cond ((zerop n) acc)
                   (t (make-tree (1- n) (cons acc acc))))))
    (let* ((times (loop for i from 0 upto 4
                        collect (time-n i)))
           (max-small (reduce #'max times :end 3))
           (max-big (reduce #'max times :start 3)))
      ;; This way is hopefully fairly CPU-performance insensitive.
      (assert (> (* (+ 2 max-small) 2) max-big)))))

(with-test (:name :bug-309063)
  (let ((fun (compile nil `(lambda (x)
                             (declare (type (integer 0 0) x))
                             (ash x 100)))))
    (assert (zerop (funcall fun 0)))))

(with-test (:name :bug-655872)
  (let ((f (compile nil `(lambda (x)
                           (declare (optimize (safety 3)))
                           (aref (locally (declare (optimize (safety 0)))
                                   (coerce x '(simple-vector 128)))
                                 60))))
        (long (make-array 100 :element-type 'fixnum)))
    (dotimes (i 100)
      (setf (aref long i) i))
    ;; 1. COERCE doesn't check the length in unsafe code.
    (assert (eql 60 (funcall f long)))
    ;; 2. The compiler doesn't trust the length from COERCE
    (assert (eq :caught
                (handler-case
                    (funcall f (list 1 2 3))
                  (sb-int:invalid-array-index-error (e)
                    (assert (eql 60 (type-error-datum e)))
                    (assert (equal '(integer 0 (3)) (type-error-expected-type e)))
                    :caught))))))

(with-test (:name :bug-655203-regression)
  (let ((fun (compile nil
                      `(LAMBDA (VARIABLE)
                         (LET ((CONTINUATION
                                (LAMBDA
                                    (&OPTIONAL DUMMY &REST OTHER)
                                  (DECLARE (IGNORE OTHER))
                                  (PRIN1 DUMMY)
                                  (PRIN1 VARIABLE))))
                           (FUNCALL CONTINUATION (LIST 1 2)))))))
    ;; This used to signal a bogus type-error.
    (assert (equal (with-output-to-string (*standard-output*)
                     (funcall fun t))
                   "(1 2)T"))))

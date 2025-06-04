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

;; The tests in this file do not work under the legacy interpreter.
(when (and (eq sb-ext:*evaluator-mode* :interpret)
           (not (member :sb-fasteval *features*)))
  (invoke-restart 'run-tests::skip-file))

(enable-test-parallelism)

;;; Exercise a compiler bug (by crashing the compiler).
;;;
;;; This test code is from Douglas Crosher's simplified TICKLE-BUG
;;; (2000-09-06 on cmucl-imp).
;;;
;;; The bug was fixed by Douglas Crosher's patch, massaged for SBCL by
;;; Martin Atzmueller (2000-09-13 on sbcl-devel).
(with-test (:name (:compiler-bug labels tagbody))
  (checked-compile-and-assert ()
      `(lambda ()
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
           nil))
    (() nil)))

;;; Exercise a compiler bug (by crashing the compiler).
;;;
;;; Tim Moore gave a patch for this bug in CMU CL 2000-05-24 on
;;; cmucl-imp, and Martin Atzmueller applied it to SBCL.
(with-test (:name (:compiler-bug flet inline :undefined-function))
  (multiple-value-bind (fun failure-p warnings style-warnings)
      (checked-compile
       `(lambda (x)
          (or (integerp x)
              (block used-by-some-y?
                (flet ((frob (stk)
                         (dolist (y stk)
                           (unless (rejected? y)
                             (return-from used-by-some-y? t)))))
                  (declare (inline frob))
                  (frob (rstk x))
                  (frob (mrstk x)))
                nil)))
       :allow-style-warnings t)
    (declare (ignore failure-p warnings))
    (assert (= 3 (length style-warnings)))
    (funcall fun 13)))

;;; bug 112, reported by Martin Atzmueller 2001-06-25 (originally
;;; from Bruno Haible in CMU CL bugs collection), fixed by
;;; Alexey Dejneka 2002-01-27
(with-test (:name (let special :bug-112))
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
                   (declare (special x)) y))))))

;;; another LET-related bug fixed by Alexey Dejneka at the same
;;; time as bug 112
(with-test (:name (let :repeated-name :bug-112))
  ;; Should complain about duplicate variable names in LET binding
  (multiple-value-bind (fun failure-p)
      (checked-compile `(lambda ()
                          (let (x
                                (x 1))
                            (list x)))
                       :allow-failure t)
    (assert (functionp fun))
    (assert failure-p)))

;;; bug 169 (reported by Alexey Dejneka 2002-05-12, fixed by David
;;; Lichteblau 2002-05-21)
(with-test (:name (let :earmuffs))
  ;; Compiling this code should cause a STYLE-WARNING about *X*
  ;; looking like a special variable but not being one.
  (multiple-value-bind (fun failure-p warnings style-warnings)
      (checked-compile
       `(lambda (n)
          (let ((*x* n))
            (funcall (symbol-function 'x-getter))
            (print *x*)))
       :allow-style-warnings 'sb-kernel:asterisks-around-lexical-variable-name)
    (declare (ignore failure-p warnings))
    (assert (functionp fun))
    (assert (= 1 (length style-warnings))))
  ;; Compiling this code should not cause a warning (because the
  ;; DECLARE turns *X* into a special variable as its name suggests it
  ;; should be).
  (checked-compile `(lambda (n)
                      (let ((*x* n))
                        (declare (special *x*))
                        (funcall (symbol-function 'x-getter))
                        (print *x*)))))

;;; a bug in 0.7.4.11
(with-test (:name (compile typep satisfies))
  (let ((*standard-output* (make-broadcast-stream)))
    (funcall (checked-compile
              '(lambda ()
                 (dolist (i '(a b 1 2 "x" "y"))
                   ;; In sbcl-0.7.4.11, the compiler tried to source-transform the
                   ;; TYPEP here but got confused and died, doing
                   ;;   (ASSOC '(AND INTEGERP (SATISFIES PLUSP)))
                   ;;          *BACKEND-TYPE-PREDICATES*
                   ;;          :TEST #'TYPE=)
                   ;; and blowing up because TYPE= tried to call PLUSP on the
                   ;; characters of the MEMBER-TYPE representing STANDARD-CHAR.
                   (when (typep i '(and integer (satisfies oddp)))
                     (print i))))))
    (funcall (checked-compile
              '(lambda ()
                 (dotimes (i 14)
                   (when (typep i '(and integer (satisfies oddp)))
                     (print i))))))))

;;; bug 156 (reported by APD sbcl-devel 2002-04-12, fixed by CSR patch
;;; sbcl-devel 2002-07-02): FUNCTION-LAMBDA-EXPRESSION of
;;; interactively-compiled functions was broken by sleaziness and
;;; confusion in the assault on 0.7.0, so this expression used to
;;; signal TYPE-ERROR when it found NIL instead of a DEBUG-SOURCE.
(with-test (:name (function-lambda-expression :bug-156))
  (eval '(function-lambda-expression #'(lambda (x) x))))

;;; bug caught and fixed by Raymond Toy cmucl-imp 2002-07-10: &REST
;;; variable is not optional.
(with-test (:name (:lambda-list &rest :missing-name))
  (multiple-value-bind (fun failure-p)
      (checked-compile `(lambda (&rest) 12) :allow-failure t)
    (assert failure-p)
    (assert-error (funcall fun))))

;;; on the PPC, we got the magic numbers in undefined_tramp wrong for
;;; a while; fixed by CSR 2002-07-18
(with-test (:name :undefined-function-error
            :fails-on :ppc64)
  (multiple-value-bind (value error)
      (ignore-errors (funcall (checked-compile
                               `(lambda () (some-undefined-function))
                               :allow-style-warnings t)))
    (assert (null value))
    (assert (eq (cell-error-name error) 'some-undefined-function))))

(with-test (:name :unbound-variable-error)
  (let ((foo (gensym)))
    (assert (eq (handler-case (symbol-value foo)
                  (unbound-variable (c) (cell-error-name c)))
                foo))
    ;; on x86-64 the code for a literal symbol uses a slightly different path,
    ;; so test that too
    (assert (eq (handler-case xyzzy*%state
                  (unbound-variable (c) (cell-error-name c)))
                'xyzzy*%state))
    ;; And finally, also on x86-64, there was massive confusion about
    ;; variable names that looked like names of thread slots.
    (assert (eq (handler-case *state*
                  (unbound-variable (c) (cell-error-name c)))
                '*state*))))

;;; Non-symbols shouldn't be allowed as VARs in lambda lists. (Where VAR
;;; is a variable name, as in section 3.4.1 of the ANSI spec.)
(with-test (:name (:lambda-list :non-symbols))
  (mapc (lambda (case)
          (destructuring-bind (form wrongp) case
            (multiple-value-bind (fun failure-p)
                (checked-compile form :allow-failure wrongp)
              (assert (functionp fun))
              (when wrongp
                (assert failure-p)
                (assert-error (funcall fun))))))
        '(((lambda ("foo") 12)                     t)
          ((lambda (foo) foo)                      nil)

          ((lambda (&optional 12) "foo")           t)
          ((lambda (&optional twelve) twelve)      nil)

          ((lambda (&optional (12 12)) "foo")      t)
          ((lambda (&optional (twelve 12)) twelve) nil)

          ((lambda (&key #\c) "foo")               t)
          ((lambda (&key c) c)                     nil)

          ((lambda (&key (#\c #\c)) "foo")         t)
          ((lambda (&key (c #\c)) c)               nil)

          ((lambda (&key ((#\c #\c) #\c)) "foo")   t)
          ((lambda (&key ((:c c-var) #\c)) c-var)  nil))))

;;; As reported and fixed by Antonio Martinez-Shotton sbcl-devel
;;; 2002-09-12, this failed in sbcl-0.7.7.23. (with failed AVER
;;; "(LEAF-HAS-SOURCE-NAME-P LEAF)")
(with-test (:name (funcall eval lambda))
  (assert (= (funcall (eval `(lambda (x) (funcall ,(lambda (y) (+ y 3)) x))) 14)
             17)))

;;; bug 181: bad type specifier dropped compiler into debugger
(with-test (:name (compile declare :bad-type-specifier :bug-181))
  (multiple-value-bind (fun failure-p)
      (checked-compile `(lambda (x)
                          (declare (type (0) x))
                          x)
                       :allow-warnings t)
    (declare (ignore fun))
    (assert failure-p)))

(with-test (:name (compile make-array :bad-type-specifier :bug-181))
  (multiple-value-bind (fun failure-p warnings)
      (checked-compile `(lambda (x)
                          (declare (ignore x))
                          (make-array 1 :element-type '(0)))
                       :allow-warnings t)
    (declare (ignore failure-p))
    (assert (= 1 (length warnings)))
    (assert-error (funcall fun 1))))

;;; the following functions must not be flushable
(with-test (:name (make-sequence concatenate map merge :not-flushable))
  (dolist (form '((make-sequence 'fixnum 10)
                  (concatenate 'fixnum nil)
                  (map 'fixnum #'identity nil)
                  (merge 'fixnum nil nil #'<)))
    (checked-compile-and-assert (:allow-warnings t :allow-style-warnings t)
        `(lambda () (progn ,form t))
      (() (condition 'error)))))

(with-test (:name (eval safety))
  (dolist (form '((values-list (car (list '(1 . 2))))
                  (fboundp '(set bet))
                  (atan #c(1 1) (car (list #c(2 2))))
                  (nthcdr (car (list (floor (cos 3)))) '(1 2 3 4 5))
                  (nthcdr (car (list 5)) '(1 2 . 3))))
    (assert (not (eval `(locally (declare (optimize (safety 3)))
                          (ignore-errors (progn ,form t))))))))

;;; feature: we shall complain if functions which are only useful for
;;; their result are called and their result ignored.
(with-test (:name :discarded-result)
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
        for expected = (sb-int:ensure-list expected-des)
        do
       (multiple-value-bind (fun failure-p warnings style-warnings)
           (checked-compile `(lambda () ,form) :allow-style-warnings (when expected t))
         (declare (ignore failure-p warnings))
         (when expected
           (assert (= (length expected) (length style-warnings)))
           (dolist (warning style-warnings)
             (let ((expect-one (pop expected)))
               (assert (search expect-one
                               (with-standard-io-syntax
                                 (let ((*print-right-margin* nil))
                                   (princ-to-string warning))))
                       ()
                       "~S should have warned ~S, but instead warned: ~A"
                       form expect-one warning))))
         (assert (functionp fun)))))

;;; a bug in the MAP deftransform caused non-VECTOR array specifiers
;;; to cause errors in the compiler.  Fixed by CSR in 0.7.8.10
(with-test (:name (map :non-vector))
  (checked-compile `(lambda (x) (map 'simple-array 'identity x))))

;;; bug 129: insufficient syntax checking in MACROLET
(with-test (:name (compile macrolet :syntax :bug-129))
  (multiple-value-bind (fun failure-p)
      (checked-compile '(lambda () (macrolet ((foo x `',x)) (foo 1 2 3)))
                       :allow-failure t)
    (assert failure-p)
    (assert-error (funcall fun) program-error)))

;;; bug 124: environment of MACROLET-introduced macro expanders
(with-test (:name (macrolet :bug-124))
  (assert (equal
           (macrolet ((mext (x) `(cons :mext ,x)))
             (macrolet ((mint (y) `'(:mint ,(mext y))))
               (list (mext '(1 2))
                     (mint (1 2)))))
           '((:MEXT 1 2) (:MINT (:MEXT 1 2))))))

;;; bug 48c: SYMBOL-MACROLET should signal PROGRAM-ERROR if introduced
;;; symbol is declared to be SPECIAL
(with-test (:name (compile symbol-macrolet special program-error :bug-48c))
  (multiple-value-bind (fun failure-p)
      (checked-compile '(lambda ()
                          (symbol-macrolet ((s '(1 2)))
                            (declare (special s))
                            s))
                       :allow-failure t)
    (assert failure-p)
    (assert-error (funcall fun) program-error)))

;;; ECASE should treat a bare T as a literal key
(with-test (:name (ecase t 1))
  (assert-error (ecase 1 (t 0)) sb-kernel:case-failure))

(with-test (:name (ecase t 2))
  (assert (eql (ecase 1 (t 0) (1 2)) 2)))

;;; FTYPE should accept any functional type specifier
(with-test (:name (compile ftype function))
  (checked-compile '(lambda (x) (declare (ftype function f)) (f x))
                   :allow-style-warnings t))

;;; FUNCALL of special operators and macros should signal an
;;; UNDEFINED-FUNCTION error
;;; But note the subtle distinction between writing (FUNCALL 'QUOTE 1)
;;; and (FUNCALL #'QUOTE 1). In the latter, the error must be signaled
;;; by the FUNCTION special operator, but the error class is unspecified.
(with-test (:name (funcall undefined-function cell-error-name quote))
  (multiple-value-bind (result error)
      (ignore-errors (funcall 'quote 1))
    (assert (null result))
    (assert (typep error 'undefined-function))
    (assert (eq (cell-error-name error) 'quote))))

(with-test (:name (funcall undefined-function cell-error-name and))
  (multiple-value-bind (result error)
      (ignore-errors (funcall 'and 1))
    (assert (null result))
    (assert (typep error 'undefined-function))
    (assert (eq (cell-error-name error) 'and))))

;;; PSETQ should behave when given complex symbol-macro arguments
(with-test (:name (psetq symbol-macrolet))
  (multiple-value-bind (sequence index)
      (symbol-macrolet ((x (aref a (incf i)))
                        (y (aref a (incf i))))
        (let ((a (copy-seq #(0 1 2 3 4 5 6 7 8 9)))
              (i 0))
          (psetq x (aref a (incf i))
                 y (aref a (incf i)))
          (values a i)))
    (assert (equalp sequence #(0 2 2 4 4 5 6 7 8 9)))
    (assert (= index 4))))

(with-test (:name (psetq :ill-formed-variable))
  (multiple-value-bind (fun failure-p)
      (checked-compile `(lambda ()
                          (let ((x (list 1 2)))
                            (psetq (car x) 3)
                            x))
                       :allow-failure t)
    (assert failure-p)
    (assert-error (funcall fun) program-error)))

;;; COPY-SEQ should work on known-complex vectors:
(with-test (:name (copy-seq :complex vector))
  (assert (equalp #(1)
                  (let ((v (make-array 0 :fill-pointer 0)))
                    (vector-push-extend 1 v)
                    (copy-seq v)))))

;;; to support INLINE functions inside MACROLET, it is necessary for
;;; FUNCTION-LAMBDA-EXPRESSION to return a proper lambda expression in
;;; certain circumstances, one of which is when compile is called from
;;; top-level.
(with-test (:name (compile function-lambda-expression
                   :toplevel :must-return-lambda-expression))
  (let ((form '(lambda (x) (block nil (print x)))))
    (assert (equal form (function-lambda-expression
                         (checked-compile form))))))

;;; bug 62: too cautious type inference in a loop
(with-test (:name (compile loop :type-inference))
  (multiple-value-bind (fun failure-p warnings)
      (checked-compile `(lambda (a)
                          (declare (optimize speed (safety 0)))
                          (typecase a
                            (array (loop (print (car a))))))
                       :allow-failure t
                       :allow-warnings t)
    (declare (ignore fun))
    (assert failure-p)
    (assert (= 1 (length warnings)))))

;;; Bug reported by Robert E. Brown sbcl-devel 2003-02-02: compiler
;;; failure
(with-test (:name (:compiler-bug declare type loop))
  (checked-compile
   `(lambda (key tree collect-path-p)
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
                 (return (values node path t)))))))
   :allow-style-warnings t))

;;; CONSTANTLY should return a side-effect-free function (bug caught
;;; by Paul Dietz' test suite)
(with-test (:name (constantly :no-side-effects))
  (let ((i 0))
    (let ((fn (constantly (progn (incf i) 1))))
      (assert (= i 1))
      (assert (= (funcall fn) 1))
      (assert (= i 1))
      (assert (= (funcall fn) 1))
      (assert (= i 1)))))

;;; Bug 240 reported by tonyms on #lisp IRC 2003-02-25 (modified version)
(with-test (:name (:lambda-list &optional :earmuffs))
  (loop for (form warns-p) in
       '(((lambda (&optional *x*) *x*) t)
         ((lambda (&optional *x* &rest y) (values *x* y)) t)
         ((lambda (&optional *print-length*) (values *print-length*)) nil)
         ((lambda (&optional *print-length* &rest y) (values *print-length* y)) nil)
         ((lambda (&optional *x*) (declare (special *x*)) (values *x*)) nil)
         ((lambda (&optional *x* &rest y) (declare (special *x*)) (values *x* y)) nil))
     do (let ((style-warnings (nth-value
                               3 (checked-compile
                                  form :allow-style-warnings warns-p))))
          (assert (= (if warns-p 1 0) (length style-warnings))))))

;;; Bug reported by Gilbert Baumann on #lisp IRC 2003-03-26
(with-test (:name (eval lambda &optional))
  (assert (equal (funcall (eval '(lambda (x &optional (y (pop x))) (list x y)))
                          '(1 2))
                 '((2) 1))))

;;; Bug reported by Paul Dietz on cmucl-imp and fixed by Gerd
;;; Moellmann: CONVERT-MORE-CALL failed on the following call
(with-test (:name (eval &key :allow-other-keys))
  (assert (eq (eval '((lambda (&key) 'u) :allow-other-keys nil)) 'u)))

(with-test (:name (eval multiple-value-bind declare type-error))
 (assert-error (multiple-value-bind (a b c)
                   (eval '(truncate 3 4))
                 (declare (integer c))
                 (list a b c))
               type-error))

(with-test (:name (eval values the))
  (assert (equal (multiple-value-list (the (values &rest integer)
                                           (eval '(values 3))))
                 '(3))))

;;; Bug relating to confused representation for the wild function
;;; type:
(with-test (:name (eval lambda function type))
  (assert (null (funcall (eval '(lambda () (multiple-value-list (values))))))))

;;; &ENVIRONMENT parameter should be bound first (from Paul Dietz'
;;; test suite)
(with-test (:name (macrolet &environment))
  (assert (eql (macrolet ((foo () 1))
                 (macrolet ((%f (&optional (x (macroexpand '(foo) env))
                                 &environment env)
                              x))
                   (%f)))
               1)))

;;; MACROLET should check for duplicated names
(with-test (:name (macrolet :lambda-list :repeated-names))
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
                (&whole x &optional x)))
    (let ((style-warnings (nth-value
                           3 (checked-compile
                              `(lambda ()
                                 (macrolet ((foo ,ll nil)
                                            (bar (&environment env)
                                              `',(macro-function 'foo env)))
                                   (bar)))
                              :allow-style-warnings t))))
      (assert style-warnings))))

;; Uh, this test is semi-bogus - it's trying to test that you can't
;; repeat, but it's now actually testing that &WHOLE has to appear
;; first, per the formal spec.
(with-test (:name (macrolet :lambda-list &whole :must-be-first))
  (assert-error (checked-compile
                 `(lambda ()
                    (macrolet ((foo (&environment x &whole x) nil)
                               (bar (&environment env)
                                 `',(macro-function 'foo env)))
                      (bar))))))

(with-test (:name (typep arithmetic-error))
  (assert (typep (eval `(the arithmetic-error
                             ',(make-condition 'arithmetic-error)))
                 'arithmetic-error)))

(with-test (:name (compile make-array :dimensions nil))
  (checked-compile `(lambda ()
                      (make-array nil :initial-element 11))))

(with-test (:name (eval open load :invalid :external-format))
  (assert-error (funcall (eval #'open) "compiler.pure.lisp"
                         :external-format '#:nonsense))
  (assert-error (funcall (eval #'load) "compiler.pure.lisp"
                         :external-format '#:nonsense)))

(with-test (:name (the values))
  (assert (= (the (values integer symbol) (values 1 'foo 13)) 1)))

;;; the implicit block does not enclose lambda list
(with-test (:name (compile :implicit block :does-not-enclose :lambda-list))
  (let ((forms '((defmacro #1=#:foo (&optional (x (return-from #1#)))
                   (declare (ignore x)))
                 #+nil(macrolet ((#2=#:foo (&optional (x (return-from #2#))))))
                 (define-compiler-macro #3=#:foo (&optional (x (return-from #3#)))
                   (declare (ignore x)))
                 (deftype #4=#:foo (&optional (x (return-from #4#)))
                   (declare (ignore x)))
                 (define-setf-expander #5=#:foo (&optional (x (return-from #5#)))
                   (declare (ignore x)))
                 (defsetf #6=#:foo (&optional (x (return-from #6#))) ()
                   (declare (ignore x))))))
    (dolist (form forms)
      (assert (nth-value
               1 (checked-compile `(lambda () ,form) :allow-failure t))))))

(with-test (:name (compile make-array svref :derive-type))
  (multiple-value-bind (fun failurep warnings)
      (checked-compile `(lambda ()
                          (svref (make-array '(8 9) :adjustable t) 1))
                       :allow-warnings t)
    (declare (ignore fun))
    (assert failurep)
    (assert (= 1 (length warnings)))))

;;; CHAR= did not check types of its arguments (reported by Adam Warner)
(macrolet ((define-char=-test (function form)
             `(with-test (:name (compile ,function :argument-type-check))
                (checked-compile-and-assert (:optimize :safe)
                    ,form
                  ((#\a #\b nil) (condition 'type-error))))))
  (define-char=-test char= `(lambda (x y z) (char= x y z)))
  (define-char=-test char/= `(lambda (x y z)
                               (char/= x y z))))

;;; Compiler lost return type of MAPCAR and friends
(with-test (:name (compile mapcar mapc maplist mapl
                           :return-type :type-derivation))
  (dolist (fun '(mapcar mapc maplist mapl))
    (assert (= 1 (length (nth-value
                          2 (checked-compile
                             `(lambda (x)
                                (1+ (,fun #'print x)))
                             :allow-warnings t))))))

  (assert (= 1 (length (nth-value
                        2 (checked-compile
                           `(lambda ()
                              (declare (notinline mapcar))
                              (1+ (mapcar #'print '(1 2 3))))
                           :allow-warnings t))))))

;;; bug found by Paul Dietz: (SETF AREF) for bit vectors with constant
;;; index was effectless
(with-test (:name (compile setf aref bit-vector))
  (let ((f (checked-compile `(lambda (a v)
                               (declare (type simple-bit-vector a) (type bit v))
                               (declare (optimize (speed 3) (safety 0)))
                               (setf (aref a 0) v)
                               a))))
    (let ((y (make-array 2 :element-type 'bit :initial-element 0)))
      (assert (equal y #*00))
      (funcall f y 1)
      (assert (equal y #*10)))))

;;; use of declared array types
(with-test (:name (compile declare array type :no sb-ext:compiler-note))
  (dolist (form `((lambda (x)
                    (declare (type (simple-array (simple-string 3) (5)) x)
                             (optimize speed))
                    (aref (aref x 0) 0))
                  (lambda (x)
                    (declare (type (simple-array (simple-array bit (10)) (10)) x)
                             (optimize speed))
                    (1+ (aref (aref x 0) 0)))))
    (checked-compile form :allow-notes nil)))

;;; compiler failure
(with-test (:name (compile typep not member))
  (checked-compile-and-assert () `(lambda (x) (typep x '(not (member 0d0))))
    ((1d0) 't)))

(with-test (:name (compile double-float atan))
  (checked-compile `(lambda (x)
                      (declare (double-float x))
                      (let ((y (* x pi)))
                        (atan y y)))))

;;; bogus optimization of BIT-NOT
(with-test (:name (bit-not optimize speed))
  (multiple-value-bind (result x)
      (eval '(let ((x (eval #*1001)))
              (declare (optimize (speed 2) (space 3))
               (type (bit-vector) x))
              (values (bit-not x nil) x)))
    (assert (equal x #*1001))
    (assert (equal result #*0110))))

;;; the VECTOR type in CONCATENATE/MERGE/MAKE-SEQUENCE means (VECTOR T).
(with-test (:name (compile vector make-sequence sb-ext:compiler-note))
  (checked-compile-and-assert (:allow-notes nil)
      `(lambda ()
         (let ((x (make-sequence 'vector 10 :initial-element 'a)))
           (setf (aref x 4) 'b)
           x))
    (() #(a a a a b a a a a a) :test #'equalp)))

;;; this is not a check for a bug, but rather a test of compiler
;;; quality
(with-test (:name (compile integer :type-derivation))
  (dolist (type '((integer 0 *)         ; upper bound
                  (real (-1) *)
                  float                 ; class
                  (real * (-10))        ; lower bound
                  ))
    (assert (= 1 (length (nth-value
                          2 (checked-compile
                             `(lambda (n)
                                (declare (optimize (speed 3) (compilation-speed 0)))
                                (loop for i from 1 to (the (integer -17 10) n) by 2
                                   collect (when (> (random 10) 5)
                                             (the ,type (- i 11)))))
                             :allow-warnings t)))))))

;;; bug 278b
;;;
;;; We suppose that INTEGER arithmetic cannot be efficient, and the
;;; compiler has an optimized VOP for +; so this code should cause an
;;; efficiency note.
(with-test (:name (compile integer + sb-ext:compiler-note :bug-278b))
  (assert (= 1 (length (nth-value
                        4 (checked-compile
                           `(lambda (i)
                              (declare (optimize speed))
                              (declare (type integer i))
                              (+ i 2))))))))

;;; bug 277: IGNORE/IGNORABLE declarations should be acceptable for
;;; symbol macros
(with-test (:name (compile symbol-macrolet ignore ignorable :bug-277))
  (checked-compile `(lambda (u v)
                      (symbol-macrolet ((x u)
                                        (y v))
                        (declare (ignore x)
                                 (ignorable y))
                        (list u v)))))

;;; bug reported by Paul Dietz: wrong optimizer for (EXPT ... 0)
(with-test (:name (compile expt :optimizer))
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
                   (fun (checked-compile src))
                   (result (1+ (funcall (eval #'*) x real-zero))))
              (assert (eql result (funcall fun x))))))))

;;; (SIGNED-BYTE 1) [ returned from the logxor derive-type optimizer ]
;;; wasn't recognized as a good type specifier.
(with-test (:name (logxor :derive-type))
  (let ((fun (lambda (x y)
               (declare (type (integer -1 0) x y) (optimize speed))
               (logxor x y))))
    (assert (= (funcall fun 0 0) 0))
    (assert (= (funcall fun 0 -1) -1))
    (assert (= (funcall fun -1 -1) 0))))

;;; from PFD's torture test, triggering a bug in our effective address
;;; treatment.
(with-test (:name (compile declare type logandc1 logandc2))
  (checked-compile `(lambda (a b)
                      (declare (type (integer 8 22337) b))
                      (logandc2
                       (logandc2
                        (* (logandc1 (max -29303 b) 4) b)
                        (abs (logorc1 (+ (logandc1 -11 b) 2607688420) -31153924)))
                       (logeqv (max a 0) b)))))

;;; Alpha floating point modes weren't being reset after an exception,
;;; leading to an exception on the second compile, below.
(with-test (:name (compile :floating-point-mode))
  (let ((form `(lambda (x y) (declare (type (double-float 0.0d0) x y)) (/ x y))))
    (checked-compile form)
    (handler-case (/ 1.0 0.0)
      ;; provoke an exception
      (arithmetic-error ()))
    (checked-compile form)))

;;; bug reported by Paul Dietz: component last block does not have
;;; start ctran
(with-test (:name (compile block return-from))
  (checked-compile `(lambda ()
                      (declare (notinline + logand)
                               (optimize (speed 0)))
                      (logand
                       (block b5
                         (flet ((%f1 ()
                                  (return-from b5 -220)))
                           (let ((v7 (%f1)))
                             (+ 359749 35728422))))
                       -24076))))

(with-test (:name :ansi-misc.293a)
  (checked-compile-and-assert (:allow-style-warnings t
                               :optimize :quick/incomplete)
      '(lambda (a b c)
         (declare (optimize (space 3) (compilation-speed 2)))
         (block b6
           (multiple-value-prog1
               0 b 0
               (catch 'ct7
                 (return-from b6
                   (catch 'ct2
                     (complex (cl::handler-bind nil -254932942) 0)))))))
    ((1 2 3) -254932942)))

(with-test (:name :ansi-misc.293d)
  (checked-compile-and-assert (:optimize :quick/incomplete)
      `(lambda ()
         (declare (optimize (space 2) (compilation-speed 2)))
         (block b4
           (multiple-value-prog1
               0
             (catch 'ct8
               (return-from b4 (catch 'ct2 (progn (tagbody) 0)))))))
    (() 0)))

(with-test (:name :ansi-misc.618)
  (checked-compile-and-assert (:optimize :quick/incomplete)
      `(lambda (c)
         (declare (optimize (space 0) (compilation-speed 2)))
         (block b1
           (ignore-errors
             (multiple-value-prog1 0
               (apply (constantly 0)
                      c
                      (catch 'ct2 (return-from b1 0))
                      nil)))))
    ((-4951) 0)))

;;; bug 294 reported by Paul Dietz: miscompilation of REM and MOD
(with-test (:name (compile rem :bug-294))
  (checked-compile-and-assert ()
      `(lambda (b)
         (declare (type (integer 2 152044363) b))
         (rem b (min -16 0)))
    ((108251912) 8)))

(with-test (:name (compile mod :bug-294))
  (checked-compile-and-assert ()
      `(lambda (c)
         (declare (type (integer 23062188 149459656) c))
         (mod c (min -2 0)))
    ((95019853) -1)))

;;; bug reported by Paul Dietz: block splitting inside FLUSH-DEAD-CODE
(with-test (:name (compile logeqv rem :dead-code :block-splitting))
  (checked-compile `(lambda (a b c)
                      (block b6
                        (logeqv (rem c -6758)
                                (rem b (max 44 (return-from b6 a))))))))

(with-test (:name (compile block flet :dead-code :block-splitting))
  (checked-compile `(lambda ()
                      (block nil
                        (flet ((foo (x y) (if (> x y) (print x) (print y))))
                          (foo 1 2)
                          (bar)
                          (foo (return 14) 2))))
                   :allow-style-warnings t))

;;; bug in Alpha backend: not enough sanity checking of arguments to
;;; instructions
(with-test (:name (compile ash))
  (checked-compile-and-assert ()
      '(lambda (x)
        (declare (fixnum x))
        (ash x -257))
    ((1024) 0)))

;;; bug found by WHN and pfdietz: compiler failure while referencing
;;; an entry point inside a deleted lambda
(with-test (:name (compile :reference-entry-point-in-deleted lambda))
  (checked-compile
   `(lambda ()
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
        r3533))
   :allow-failure t :allow-style-warnings t))

(with-test (:name (compile flet unwind-protect :dead-code))
  (checked-compile `(lambda () (flet ((%f () (unwind-protect nil))) nil))))

;;; the strength reduction of constant multiplication used (before
;;; sbcl-0.8.4.x) to lie to the compiler.  This meant that, under
;;; certain circumstances, the compiler would derive that a perfectly
;;; reasonable multiplication never returned, causing chaos.  Fixed by
;;; explicitly doing modular arithmetic, and relying on the backends
;;; being smart.
(with-test (:name (compile * :constant))
  (checked-compile-and-assert ()
      '(lambda (x)
         (declare (type (integer 178956970 178956970) x))
         (* x 24))
    ((178956970) 4294967280)))

;;; bug in modular arithmetic and type specifiers
(with-test (:name (compile logand))
  (checked-compile-and-assert ()
      '(lambda (x) (logand x x 0))
    ((-1) 0)))

;;; MISC.99 from Paul Dietz' random tester: FAST-ASH-MOD32-C VOP
;;; produced wrong result for shift >=32 on X86
(with-test (:name (compile mask-field :fast-ash-mod32-c-vop 18))
  (checked-compile-and-assert ()
      '(lambda (a)
         (declare (type (integer 4303063 101130078) a))
         (mask-field (byte 18 2) (ash a 77)))
    ((57132532) 0)))

;;; rewrite the test case to get the unsigned-byte 32/64
;;; implementation even after implementing some modular arithmetic
;;; with signed-byte 30:
(with-test (:name (compile mask-field :fast-ash-mod32-c-vop 30))
  (checked-compile-and-assert ()
      '(lambda (a)
        (declare (type (integer 4303063 101130078) a))
        (mask-field (byte 30 2) (ash a 77)))
    ((57132532) 0)))
(with-test (:name (compile mask-field :fast-ash-mod32-c-vop 64))
  (checked-compile-and-assert ()
      '(lambda (a)
         (declare (type (integer 4303063 101130078) a))
         (mask-field (byte 64 2) (ash a 77)))
    ((57132532) 0)))
;;; and a similar test case for the signed masking extension (not the
;;; final interface, so change the call when necessary):
(with-test (:name (compile sb-c::mask-signed-field :fast-ash-mod32-c-vop 30))
  (checked-compile-and-assert ()
      '(lambda (a)
         (declare (type (integer 4303063 101130078) a))
         (sb-c::mask-signed-field 30 (ash a 77)))
    ((57132532) 0)))
(with-test (:name (compile sb-c::mask-signed-field :fast-ash-mod32-c-vop 61))
  (checked-compile-and-assert ()
      '(lambda (a)
         (declare (type (integer 4303063 101130078) a))
         (sb-c::mask-signed-field 61 (ash a 77)))
    ((57132532) 0)))

;;; MISC.101 and MISC.103: FLUSH-DEST did not mark the USE's block for
;;; type check regeneration
(with-test (:name (compile :flush-dest :use :regenerate-type-check :misc.101))
  (checked-compile-and-assert (:allow-style-warnings 'sb-int:type-style-warning)
      '(lambda (a c)
         (declare (type (integer 185501219873 303014665162) a))
         (declare (type (integer -160758 255724) c))
         (let ((v8
                (- -554046873252388011622614991634432
                   (ignore-errors c)
                   (unwind-protect 2791485))))
           (max (ignore-errors a)
                (let ((v6 (- v8 (restart-case 980))))
                  (min v8 v6)))))
    ((259448422916 173715) 259448422916)))
(with-test (:name (compile :flush-dest :use :regenerate-type-check :misc.103))
  (checked-compile-and-assert (:allow-style-warnings 'sb-int:type-style-warning)
      '(lambda (a b)
        (min -80
         (abs
          (ignore-errors
            (+
             (logeqv b
                     (block b6
                       (return-from b6
                         (load-time-value -6876935))))
             (if (logbitp 1 a) b (setq a -1522022182249)))))))
    ((-1802767029877 -12374959963) -80)))

;;; various MISC.*, related to NODEs/LVARs with derived type NIL
(with-test (:name (compile :node/lvar :derive-type :misc.1))
  (checked-compile-and-assert (:allow-style-warnings 'sb-int:type-style-warning)
      '(lambda (c)
         (declare (type (integer -3924 1001809828) c))
         (min 47 (if (ldb-test (byte 2 14) c)
                     -570344431
                     (ignore-errors -732893970))))
    ((705347625) -570344431)))
(with-test (:name (compile :node/lvar :derive-type :misc.2))
  (checked-compile-and-assert (:allow-style-warnings t
                               :allow-warnings t)
      '(lambda (b)
         (declare (type (integer -1598566306 2941) b))
         (max -148949 (ignore-errors b)))
    ((0) 0)))
(with-test (:name (compile :node/lvar :derive-type :misc.3))
  (checked-compile-and-assert (:allow-style-warnings t)
      '(lambda (b c)
         (declare (type (integer -4 -3) c))
         (block b7
           (flet ((%f1 (f1-1 f1-2 f1-3)
                    (if (logbitp 0 (return-from b7
                                     (- -815145138 f1-2)))
                        (return-from b7 -2611670)
                        99345)))
             (let ((v2 (%f1 -2464 (%f1 -1146 c c) -2)))
               b))))
    ((2950453607 -4) -815145134)))
(with-test (:name (compile :node/lvar :derive-type :misc.4))
  (checked-compile-and-assert (:allow-style-warnings 'sb-int:type-style-warning)
      '(lambda (b c)
         (declare (type (integer -29742055786 23602182204) b))
         (declare (type (integer -7409 -2075) c))
         (floor
          (labels ((%f2 ()
                     (block b6
                       (ignore-errors (return-from b6
                                        (if (= c 8) b 82674))))))
            (%f2))))
    ((22992834060 -5833) (values 82674 0))))
(with-test (:name (compile :node/lvar :derive-type :misc.5))
  (checked-compile-and-assert (:allow-style-warnings 'sb-int:type-style-warning)
      '(lambda (a)
         (declare (type (integer -944 -472) a))
         (round
          (block b3
            (return-from b3
              (if (= 55957 a) -117 (ignore-errors
                                     (return-from b3 a)))))))
    ((-589) (values -589 0))))

;;; MISC.158
(with-test (:name (compile :misc.158))
  (checked-compile-and-assert ()
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
                                   (ash a (min 36 22477))))))))
    ((100000 0 0) 0)))

;;; MISC.152, 153: deleted code and iteration var type inference
(with-test (:name (compile :deleted-code :iteration-variable :type-inference :misc.152))
  (checked-compile-and-assert (:allow-style-warnings t)
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
                               (%f6 -2 -108626545 (%f4)))))))))))
    ((-6) 87246015)))

(with-test (:name (compile :deleted-code :iteration-variable :type-inference :misc.153))
  (checked-compile-and-assert ()
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
             a))
    ((0) -20343)))

;;; MISC.165
(with-test (:name (compile :misc.165))
  (checked-compile-and-assert (:allow-style-warnings t)
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
             (multiple-value-call #'%f15 (values -519354 a 121 c -1905)))))
    ((0 0 -5) -16)))

;;; MISC.172
(with-test (:name (compile :misc.172))
  (checked-compile-and-assert (:allow-style-warnings t)
      '(lambda (a b c)
         (declare (notinline list apply))
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
           (apply #'%f12 (list 774 -4413))))
    ((0 1 2) 774)))

;;; MISC.173
(with-test (:name (compile :misc.173))
  (checked-compile-and-assert (:allow-style-warnings t)
      '(lambda (a b c)
        (declare (notinline values))
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
              -32326608)))
    ((1 2 3) -32326608)))

;;; MISC.177, 182: IR2 copy propagation missed a hidden write to a
;;; local lambda argument
(with-test (:name (compile :ir2-copy-propagation :misc.177 :misc.182))
  (checked-compile-and-assert (:allow-style-warnings t)
      '(lambda (a b c)
         (declare (type (integer 804561 7640697) a))
         (declare (type (integer -1 10441401) b))
         (declare (type (integer -864634669 55189745) c))
         (declare (ignorable a b c))
         (flet ((%f11
                    (f11-1 f11-2)
                  (labels ((%f4 () (round 200048 (max 99 c))))
                    (logand
                     f11-1
                     (labels ((%f3 (f3-1) -162967612))
                       (%f3 (let* ((v8 (%f4)))
                              (setq f11-1 (%f4)))))))))
           (%f11 -120429363 (%f11 62362 b))))
    ((6714367 9645616 -637681868) -264223548)))

;;; Bug reported by Paul F. Dietz caused by derive type loss in VALUE
;;; transform
(with-test (:name (compile :value-transform :derive-type))
  (checked-compile-and-assert (:optimize :quick/incomplete)
      '(lambda ()
         (declare (optimize (space 0) (compilation-speed 1)))
         (ceiling (ceiling (flet ((%f16 () 0)) (%f16)))))
    (() (values 0 0))))

;;; MISC.184
(with-test (:name (compile :misc.184))
  (checked-compile-and-assert (:allow-style-warnings t)
      '(lambda (a b c)
         (declare (type (integer 867934833 3293695878) a))
         (declare (type (integer -82111 1776797) b))
         (declare (type (integer -1432413516 54121964) c))
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
             0))
    ((3040851270 1664281 -1340106197) 0)))

;;; MISC.249
(with-test (:name (compile :misc.249))
  (checked-compile-and-assert (:optimize :quick/incomplete)
      '(lambda (a b)
         (declare (notinline <=))
         (declare (optimize (space 3) (compilation-speed 3)))
         (if (if (<= 0) nil nil)
             (labels ((%f9 (f9-1 f9-2 f9-3)
                        (ignore-errors 0)))
               (dotimes (iv4 5 a) (%f9 0 0 b)))
             0))
    ((1 2) 0)))

;;; MISC.259-264 (aka "CSR screwed up implementing *-MOD32")
(with-test (:name (compile :mod32 :misc.259))
  (checked-compile-and-assert (:optimize :quick/incomplete)
      '(lambda (a)
         (declare (type (integer 177547470 226026978) a))
         (declare (optimize (space 0) (compilation-speed 1)))
         (logand a (* a 438810)))
    ((215067723) 13739018)))


;;;; Bugs in stack analysis
;;; bug 299 (reported by PFD)
(with-test (:name (compile :stack-analysis :bug-299))
  (checked-compile-and-assert ()
      '(lambda ()
         (multiple-value-call #'list
           (if (eval t) (eval '(values :a :b :c)) nil)
           (catch 'foo (throw 'foo (values :x :y)))))
    (() '(:a :b :c :x :y))))
;;; bug 298 (= MISC.183)
(with-test (:name (compile :bug-298 :misc.183))
  (checked-compile-and-assert (:allow-style-warnings t)
      '(lambda (a b c)
        (declare (type (integer -368154 377964) a))
        (declare (type (integer 5044 14959) b))
        (declare (type (integer -184859815 -8066427) c))
        (declare (ignorable a b c))
        (block b7
          (flet ((%f3 (f3-1 f3-2 f3-3) 0))
            (apply #'%f3 0 (catch 'foo (return-from b7 (%f3 0 b c))) c nil))))
    ((0 6000 -9000000) 0)))
(assert (equal (eval '(let () (apply #'list 1 (list (catch 'a (throw 'a (block b 2)))))))
               '(1 2)))
(with-test (:name (compile multiple-value-call block return-from))
  (checked-compile-and-assert ()
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
                             (throw 'baz 2)))))))))))
    ((t)   '(:b 1))
    ((nil) '(:a 2))))

;;; MISC.185
(with-test (:name (compile :misc.185))
  (checked-compile-and-assert (:allow-style-warnings t)
      '(lambda (a b c)
         (declare (type (integer 5 155656586618) a))
         (declare (type (integer -15492 196529) b))
         (declare (type (integer 7 10) c))
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
           (%f3 (%f3 (%f3 b 0 0 0) a 0) a b b b c)))
    ((5 0 7) 0)))
;;; MISC.186
(with-test (:name (eval compile :misc.186))
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
           :good)))

;;; MISC.290
(with-test (:name (compile :misc.290))
  (assert (zerop
           (funcall
            (checked-compile
             '(lambda ()
               (declare
                (optimize (speed 3) (space 3) (safety 1)
                 (debug 2) (compilation-speed 0)))
               (apply (constantly 0) (catch 'ct2 0) 0 (catch 'ct2 0) nil)))))))

;;; MISC.292
(with-test (:name (compile :misc.292))
  (assert (zerop (funcall
                  (checked-compile
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
                      nil))
                   :allow-style-warnings t)
                  1 2))))

;;; misc.295
(with-test (:name (compile :misc.295))
  (assert (eql
           (funcall
            (checked-compile
             '(lambda ()
               (declare (optimize (speed 1) (space 0) (safety 0) (debug 0)))
               (multiple-value-prog1
                   (the integer (catch 'ct8 (catch 'ct7 15867134)))
                 (catch 'ct1 (throw 'ct1 0))))))
           15867134)))

;;; misc.361: replacing CAST with (m-v-call #'%compile-time-type-error)
;;; could transform known-values LVAR to UVL
(with-test (:name (compile :cast multiple-value-call :misc.361))
  (assert (zerop (funcall
                  (checked-compile
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
                            (progv nil nil (values (boole boole-and 0 v9))))))))
                   :allow-warnings 'sb-int:type-warning :allow-style-warnings t)
                  1 2 3))))

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
(with-test (:name (:non-constant-stack))
  (assert (equal (non-continuous-stack-test t) '(11 12 13 14 1 2 3 4 5 6 7 8 :ext)))
  (assert (equal (non-continuous-stack-test nil) '(:b1 :b2 :b3 :b4 :a1 :a2 :a3 :a4 :int))))

;;; MISC.362: environment of UNWIND-PROTECTor is different from that
;;; if ENTRY.
(with-test (:name (compile unwind-protect :misc.362))
  (assert (equal (multiple-value-list
                  (funcall
                   (checked-compile
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
                          (common-lisp:handler-case 0))))
                    :allow-style-warnings t)
                   1 2 3))
                 '(0))))


;;; MISC.275
(with-test (:name (compile :misc.275))
  (assert
   (zerop
    (funcall
     (checked-compile
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
     -33))))

;;; Discussion of a CMUCL PCL bug on Sparc with Raymond Toy revealed a
;;; potential problem: optimizers and type derivers for MAX and MIN
;;; were not consistent in treating EQUALP, but not EQL, arguments.
(with-test (:name (compile min max :derive-type equalp))
  (dolist (f '(min max))
    (loop for complex-arg-args in '((1d0 2d0) (0d0 1d0))
       for complex-arg = `(if x ,@complex-arg-args)
       do
         (loop for args in `((1 ,complex-arg)
                             (,complex-arg 1))
            for form = `(,f ,@args)
            for f1 = (checked-compile `(lambda (x) ,form))
            and f2 = (checked-compile `(lambda (x)
                                         (declare (notinline min max))
                                         ,form))
            do
              (dolist (x '(nil t))
                (assert (eql (funcall f1 x) (funcall f2 x))))))))

;;;
;; Ignore the note for the float -> pointer conversion of the
;; return value.
(defun float->pointer-conversion-note-p (condition)
  (string= (car (last (sb-c::simple-condition-format-arguments condition)))
           "<return value>"))
(with-test (:name (compile :trust-result-type-declaration))
  (checked-compile
   '(lambda (x)
     (declare (optimize (speed 3) (safety 0)))
     (the double-float (sqrt (the double-float x))))
   :allow-notes '(satisfies float->pointer-conversion-note-p)))

(with-test (:name (compile block the return))
  (let ((f (checked-compile
            '(lambda (x)
              (declare (optimize speed (safety 0)))
              (block nil
                (the double-float
                     (multiple-value-prog1
                         (sqrt (the double-float x))
                       (when (< x 0)
                         (return :minus)))))))))
    (assert (eql (funcall f -1d0) :minus))
    (assert (eql (funcall f 4d0) 2d0))))

;;; bug 304: SBCL produced something similar to (/ (ASH x 4) 8)
(defun %ash/right-note-p (condition)
  (search "%ASH/RIGHT" (first (simple-condition-format-arguments condition))))
(with-test (:name (compile deref :bug-304))
  (checked-compile
   '(lambda (a i)
     (locally
         (declare (optimize (speed 3) (safety 0) (space 0) (debug 0)
                            (inhibit-warnings 0)))
       (declare (type (alien (* (unsigned 8))) a)
                (type (unsigned-byte 32) i))
       (deref a i)))
   :allow-notes '(satisfies %ash/right-note-p)))

(with-test (:name (compile identity :derive-type))
  (checked-compile
   '(lambda (x)
     (declare (type (integer -100 100) x))
     (declare (optimize speed))
     (declare (notinline identity))
     (1+ (identity x)))
   :allow-notes nil))

(with-test (:name (compile funcall cddr))
 (assert (null (funcall (checked-compile '(lambda (x) (funcall #'cddr x))) nil))))

;;; MISC.293 = easy variant of bug 303: repeated write to the same
;;; LVAR; here the first write may be cleared before the second is
;;; made.
(with-test (:name (compile :repeated-write-to-lvar :misc.293 :easy))
  (assert
   (zerop
    (funcall
     (checked-compile
      '(lambda ()
        (declare (notinline complex))
        (declare (optimize (speed 1) (space 0) (safety 1)
                  (debug 3) (compilation-speed 3)))
        (flet ((%f () (multiple-value-prog1 0 (return-from %f 0))))
          (complex (%f) 0))))))))

(with-test (:name (compile :misc.293 :harder))
  ;; Similar to the above case, but can blow up in stack analysis if
  ;; the two blocks in the M-V-P1 are joined (due to both (FUNCALL Y)
  ;; forms producing multiple-value results into the same LVAR,
  ;; requiring a cleanup to be inserted between the two, yet not
  ;; possible due to the lack of a block boundary).
  (checked-compile
   '(lambda (x)
     (declare (notinline complex))
     (declare (optimize (speed 1) (space 0) (safety 1)
               (debug 3) (compilation-speed 3)))
     (flet ((%f (y) (multiple-value-prog1 (funcall y)
                      (return-from %f (funcall y)))))
       (complex (%f x) 0)))))

;;; MISC.110A: CAST optimizer forgot to flush LVAR derived type
(with-test (:name (compile :cast :flush-lvar-derived-type :misc.110a))
  (assert (zerop (funcall
                  (checked-compile
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
                        0 0)))
                   :allow-style-warnings 'sb-int:type-style-warning)
                  391833530 -32785211))))

;;; efficiency notes for ordinary code
(with-test (:name (compile :no-notes))
  (macrolet ((frob (arglist &body body)
               `(progn
                  (checked-compile '(lambda ,arglist ,@body) :allow-notes nil)
                  (unless (nth-value
                           4 (checked-compile
                              '(lambda ,arglist (declare (optimize speed))
                                ,@body)))
                    (error "missing compiler note for ~S" ',body)))))
    (frob (x) (funcall x))
    (frob (x y) (find x y))
    (frob (x y) (find-if x y))
    (frob (x y) (find-if-not x y))
    (frob (x y) (position x y))
    (frob (x y) (position-if x y))
    (frob (x y) (position-if-not x y))
    (frob (x) (aref x 0))))

(with-test (:name (compile style-warning :smoke))
  (macrolet ((frob (style-warn-p form)
               (unless (eq (car form) 'lambda)
                 (setq form `(lambda () ,form)))
               (if style-warn-p
                   `(unless (nth-value
                             3 (checked-compile ',form :allow-style-warnings t))
                      (error "missing style-warning for ~S" ',form))
                   `(checked-compile ',form))))
    (frob t (lambda (x &optional y &key z) (list x y z)))
    (frob nil (lambda (x &optional y z) (list x y z)))
    (frob nil (lambda (x &key y z) (list x y z)))
    (frob t (defgeneric #:foo (x &optional y &key z)))
    (frob nil (defgeneric #:foo (x &optional y z)))
    (frob nil (defgeneric #:foo (x &key y z)))
    (frob t (defun #:foo (x) (flet ((foo (x &optional y &key z) (list x y z))) (foo x x :z x))))))

;;; this was a bug in the LOGXOR type deriver.  The top form gave a
;;; note, because the system failed to derive the fact that the return
;;; from LOGXOR was small and negative, though the bottom one worked.
(with-test (:name (compile logxor :derive-type))
  (checked-compile
   '(lambda ()
     (declare (optimize speed (safety 0)))
     (lambda (x y)
       (declare (type (integer 3 6) x)
                (type (integer -6 -3) y))
       (+ (logxor x y) most-positive-fixnum)))
   :allow-notes nil)
  (checked-compile
   '(lambda ()
     (declare (optimize speed (safety 0)))
     (lambda (x y)
       (declare (type (integer 3 6) y)
                (type (integer -6 -3) x))
       (+ (logxor x y) most-positive-fixnum)))
   :allow-notes nil))

;;; check that modular ash gives the right answer, to protect against
;;; possible misunderstandings about the hardware shift instruction.
(with-test (:name (compile ash :smoke))
  (assert (zerop (funcall
                  (checked-compile '(lambda (x y)
                                     (declare (optimize speed)
                                      (type (unsigned-byte 32) x y))
                                     (logand #xffffffff (ash x y))))
                  1 257))))

;;; code instrumenting problems
(with-test (:name (compile :instrumentation-problems))
  (checked-compile
   '(lambda ()
     (declare (optimize (debug 3)))
     (list (the integer (if nil 14 t))))
   :allow-warnings t)

  (checked-compile
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
        B)))
   :allow-style-warnings t)

  (checked-compile
   '(lambda (buffer i end)
     (declare (optimize (debug 3)))
     (loop (when (not (eql 0 end)) (return)))
     (let ((s (make-string end)))
       (setf (schar s i) (schar buffer i))
       s))))

;;; check that constant string prefix and suffix don't cause the
;;; compiler to emit code deletion notes.
(with-test (:name (compile pprint-logical-block :prefix :suffix :no-notes))
  (checked-compile
   '(lambda (s x)
     (pprint-logical-block (s x :prefix "(")
       (print x s)))
   :allow-notes nil)
  (checked-compile
   '(lambda (s x)
     (pprint-logical-block (s x :per-line-prefix ";")
       (print x s)))
   :allow-notes nil)
  (checked-compile
   '(lambda (s x)
     (pprint-logical-block (s x :suffix ">")
       (print x s)))
   :allow-notes nil))

;;; MISC.427: loop analysis requires complete DFO structure
(with-test (:name (compile :loop-analysis :misc.427))
  (assert (eql 17 (funcall
                   (checked-compile
                    '(lambda (a)
                      (declare (notinline list reduce logior))
                      (declare (optimize (safety 2) (compilation-speed 1)
                                (speed 3) (space 2) (debug 2)))
                      (logior
                       (let* ((v5 (reduce #'+ (list 0 a))))
                         (declare (dynamic-extent v5))
                         v5))))
                   17))))

;;;  MISC.434
(with-test (:name (compile :misc.434))
  (checked-compile-and-assert (:allow-style-warnings t)
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
              0))))
    ((-7639589303599 -1368) 0))

  (checked-compile
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
                  iv1))))))

;;; MISC.435: lambda var substitution in a deleted code.
(with-test (:name (compile :misc.435))
  (checked-compile-and-assert (:allow-style-warnings t)
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
          0))
    ((3021871717588 -866608 -2 -17194) 0)))

;;; MISC.436, 438: lost reoptimization
(with-test (:name (compile :lost-reoptimization :misc.436 :misc.438))
  (checked-compile-and-assert (:optimize nil)
      '(lambda (a b)
        (declare (type (integer -2917822 2783884) a))
        (declare (type (integer 0 160159) b))
        (declare (ignorable a b))
        (declare
         (optimize (compilation-speed 1)
          (speed 3) (safety 3) (space 0) (debug 0)
          ;; #+sbcl (sb-c:insert-step-conditions 0)
          ))
        (if (oddp (loop for lv1 below 2
                        count (logbitp
                               0 (1- (ash b (min 8
                                                 (count 0
                                                        '(-10197561 486 430631291
                                                          9674068))))))))
            b
            0))
    ((1265797 110757) 0))

  (checked-compile-and-assert (:optimize nil)
      '(lambda (a)
        (declare (type (integer 0 1696) a))
                                        ; (declare (ignorable a))
        (declare (optimize (space 2) (debug 0) (safety 1)
                  (compilation-speed 0) (speed 1)))
        (if (logbitp 0 (ash (1- a) (min 11 a))) 0 0))
    ((805) 0)))

;;; bug #302
(with-test (:name (compile :bug-302))
  (checked-compile
   '(lambda (s ei x y)
     (declare (type (simple-array function (2)) s) (type ei ei))
     (funcall (aref s ei) x y))
   :allow-style-warnings t))

;;; MISC.320: ir1-transform can create an intercomponent reference to
;;; a DEFINED-FUN.
(with-test (:name (compile :misc.320))
  (checked-compile-and-assert (:optimize nil)
      '(lambda ()
        (declare (optimize (speed 3) (space 0) (safety 2)
                  (debug 2) (compilation-speed 0)))
        (catch 'ct2
          (elt '(102)
               (flet ((%f12 () (rem 0 -43)))
                 (multiple-value-call #'%f12 (values))))))
    (() 102)))

;;; MISC.437: lost reoptimization after FLUSH-DEST
(with-test (:name (compile :misc.437))
  (checked-compile-and-assert (:allow-style-warnings t)
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
          (complex (multiple-value-call #'%f10 (values a c b 0 0)) 0)))
    ((80043 74953652306 33658947 -63099937105 -27842393) 0)))

;;; bug #351 -- program-error for malformed LET and LET*, including those
;;; resulting from SETF of LET.
(with-test (:name (compile let let* :malformed program-error :bug-351))
  (dolist (fun (list (checked-compile '(lambda (x) (let :bogus-let :oops))
                                      :allow-failure t :allow-style-warnings t)
                     (checked-compile '(lambda (x) (let* :bogus-let* :oops))
                                      :allow-failure t :allow-style-warnings t)
                     (checked-compile '(lambda (x) (push x (let ((y 0)) y)))
                                      :allow-failure t :allow-warnings t)))
    (assert (functionp fun))
    (multiple-value-bind (res err) (ignore-errors (funcall fun t))
      (assert (not res))
      (assert (typep err 'program-error))
      (assert (not (sequence:emptyp (princ-to-string err)))))))

(with-test (:name (compile random :distribution))
  (let ((fun (checked-compile '(lambda (x) (random (if x 10 20))))))
    (dotimes (i 100 (error "bad RANDOM distribution"))
      (when (> (funcall fun nil) 9)
        (return t)))
    (dotimes (i 100)
      (when (> (funcall fun t) 9)
        (error "bad RANDOM event")))))

;;; 0.8.17.28-sma.1 lost derived type information.
(with-test (:name (compile :0.8.17.28-sma.1) :fails-on :sparc)
  (checked-compile
   '(lambda (x y v)
     (declare (optimize (speed 3) (safety 0)))
     (declare (type (integer 0 80) x)
      (type (integer 0 11) y)
      (type (simple-array (unsigned-byte 32) (*)) v))
     (setf (aref v 0) (* (* x #.(floor (ash 1 32) (* 11 80))) y))
     nil)
   :allow-notes nil))

;;; Bug reported by Robert J. Macomber: instrumenting of more-entry
;;; prevented open coding of %LISTIFY-REST-ARGS.
(with-test (:name (compile :more-entry :%listity-rest-args))
  (let ((f (checked-compile '(lambda ()
                              (declare (optimize (debug 3)))
                              (with-simple-restart (blah "blah") (error "blah"))))))
    (handler-bind ((error (lambda (c) (declare (ignore c)) (invoke-restart 'blah))))
      (assert (equal (multiple-value-list (funcall f)) '(nil t))))))

;;; Bug reported by Timmy Douglas: overflow in bit vector setter with
;;; constant index and value.
(with-test (:name (compile bit-vector setf aref :overflow))
  (loop for n-bits = 1 then (* n-bits 2)
     for type = `(unsigned-byte ,n-bits)
     and v-max = (1- (ash 1 n-bits))
     while (<= n-bits sb-vm:n-word-bits)
     do
       (let* ((n (* 2 (1+ (- sb-vm:n-word-bits n-bits))))
              (array1 (make-array n :element-type type))
              (array2 (make-array n :element-type type)))
         (dotimes (i n)
           (dolist (v (list 0 v-max))
             (let ((f (checked-compile
                       `(lambda (a)
                          (declare (type (simple-array ,type (,n)) a))
                          (setf (aref a ,i) ,v)))))
               (fill array1 (- v-max v))
               (fill array2 (- v-max v))
               (funcall f array1)
               (setf (aref array2 i) v)
               (assert (every #'= array1 array2))))))))

(with-test (:name (compile array bit count))
  (checked-compile-and-assert (:optimize nil)
      '(lambda (x)
        (declare (type bit x))
        (declare (optimize speed))
        (let ((b (make-array 64 :element-type 'bit
                                :initial-element 0)))
          (count x b)))
    ((0) 64)
    ((1) 0)))

(with-test (:name (compile simple-bit-vector equal))
  (let ((fn (checked-compile '(lambda (x y)
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
                b))))))

;;; MISC.535: compiler failure
(with-test (:name (compile :misc.535))
  (let ((c0 #c(4196.088977268509d0 -15943.3603515625d0)))
    (checked-compile-and-assert (:optimize nil)
        `(lambda (p1 p2)
           (declare (optimize speed (safety 1))
                    (type (eql ,c0) p1)
                    (type number p2))
           (eql (the (complex double-float) p1) p2))
      ((c0 #c(12 612/979)) nil))))

;;; reported by Lutz Euler: we shouldn't signal a compiler note for
;;; simple-bit-vector functions.
(with-test (:name (compile simple-bit-vector count :should-not-compiler-note))
  (checked-compile '(lambda (x)
                     (declare (type simple-bit-vector x))
                     (count 1 x))
                   :allow-notes nil))
(with-test (:name (compile simple-bit-vector equal :should-not-compiler-note))
  (checked-compile '(lambda (x y)
                     (declare (type simple-bit-vector x y))
                     (equal x y))
                   :allow-notes nil))

;;; MISC.550: CAST merging in IR1 finalization caused unexpected
;;; code transformations.
(with-test (:name (compile :misc.550))
  (checked-compile-and-assert (:optimize nil)
      '(lambda (p1 p2)
        (declare (optimize (speed 3) (safety 2) (debug 3) (space 3))
         (type atom p1)
         (type symbol p2))
        (or p1 (the (eql t) p2)))
    ((nil t) t)))

;;; MISC.548: type check weakening converts required type into
;;; optional
(with-test (:name (compile :misc.548))
  (checked-compile-and-assert (:optimize nil)
      '(lambda (p1)
        (declare (optimize (speed 2) (safety 1) (debug 3) (space 2)))
        (atom (the (member f assoc-if write-line t w) p1)))
    ((t) t)))

;;; Free special bindings only apply to the body of the binding form, not
;;; the initialization forms.
(with-test (:name (compile declare special))
  (checked-compile-and-assert ()
      '(lambda ()
        (let ((x :bad))
          (declare (special x))
          (let ((x :good))
            ((lambda (&optional (y x))
               (declare (special x)) y)))))
    (() :good)))

;;; Bug from pfdietz's random tester: the compiler knew that IMAGPART of
;;; a rational was zero, but didn't do the substitution, leading to a
;;; crash in the ASH vop (since a shift of 57 wouldn't fit in the
;;; machine's ASH instruction's immediate field) that the compiler
;;; thought was legitimate.
(with-test (:name :overlarge-immediate-in-ash-vop)
  (checked-compile `(lambda (b)
                      (declare (type (integer -2 14) b))
                      (declare (ignorable b))
                      (ash (imagpart b) 57))))

;;; bug reported by Eduardo Mu\~noz
(with-test (:name (compile vector loop))
  (checked-compile
   `(lambda (struct first)
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
        new))
   :allow-style-warnings t))

;;; bug #389: "0.0 can't be converted to type NIL."  (Brian Rowe
;;; sbcl-devel)
(with-test (:name (compile float :bug-389))
  (checked-compile `(lambda (x y a b c)
                      (- y (* (signum x) (sqrt (abs (- (* b x) c))))))
                   :allow-style-warnings t))

;;; Type inference from CHECK-TYPE
(with-test (:name (compile check-type :type-inference))
  (let ((notes (nth-value
                4 (checked-compile '(lambda (x)
                                     (declare (optimize (speed 3)))
                                     (+ x 2))))))
    ;; forced-to-do GENERIC-+, etc, possible word -> bignum conversion note
    (assert (> (length notes) 0)))

  (let ((notes (nth-value
                4 (checked-compile '(lambda (x)
                                     (declare (optimize (speed 3)))
                                     (check-type x fixnum)
                                     (+ x 2))))))
    ;; Only the posssible word -> bignum conversion note
    (assert (= (length notes) 1))))

;;; Up to 0.9.8.22 x86-64 had broken return value handling in the
;;; %SET-SAP-REF-DOUBLE/SINGLE VOPs.
(with-test (:name :sap-ref-float)
  (checked-compile '(lambda (sap)
                     (let ((x (setf (sb-vm::sap-ref-double sap 0) 1d0)))
                       (1+ x))))
  (checked-compile '(lambda (sap)
                     (let ((x (setf (sb-vm::sap-ref-single sap 0) 1f0)))
                       (1+ x)))))

;;; bug #399
(with-test (:name :string-union-types)
  (checked-compile '(lambda (x)
                     (declare (type (or (simple-array character (6))
                                        (simple-array character (5))) x))
                     (aref x 0))))

;;; MISC.623: missing functions for constant-folding
(with-test (:name (compile :constant-folding :missing-functions :misc.623))
  (checked-compile-and-assert (:optimize nil)
      '(lambda ()
        (declare (optimize (space 2) (speed 0) (debug 2)
                  (compilation-speed 3) (safety 0)))
        (loop for lv3 below 1
              count (minusp
                     (loop for lv2 below 2
                           count (logbitp 0
                                          (bit #*1001101001001
                                               (min 12 (max 0 lv3))))))))
    (() 0)))

;;; MISC.624: erroneous AVER in x86's %LOGBITP VOPs
(with-test (:name (compile :%logbitp-vops :misc.624))
  (checked-compile-and-assert
      (:optimize nil :allow-style-warnings t)
      '(lambda (a)
        (declare (type (integer 21 28) a))
        (declare (optimize (compilation-speed 1) (safety 2)
                  (speed 0) (debug 0) (space 1)))
        (let* ((v7 (flet ((%f3 (f3-1 f3-2)
                            (loop for lv2 below 1
                                  count
                                     (logbitp 29
                                              (sbit #*10101111
                                                    (min 7 (max 0 (eval '0))))))))
                     (%f3 0 a))))
          0))
    ((22) 0)))

;;; MISC.626: bandaged AVER was still wrong
(with-test (:name (compile logbitp :misc.626))
  (checked-compile-and-assert (:optimize nil)
      '(lambda (a)
        (declare (type (integer -902970 2) a))
        (declare (optimize (space 2) (debug 0) (compilation-speed 1)
                  (speed 0) (safety 3)))
        (prog2 (if (logbitp 30 a) 0 (block b3 0)) a))
    ((-829253) -829253)))

;; MISC.628: constant-folding %LOGBITP was buggy
(with-test (:name (compile logbitp :constant-folding :misc.628))
  (checked-compile-and-assert (:optimize nil)
      `(lambda ()
         (declare (optimize (safety 3) (space 3) (compilation-speed 3)
                            (speed 0) (debug 1)))
         (not (not (logbitp 0 (floor 2147483651 (min -23 0))))))
    (() t)))

;; mistyping found by random-tester
(with-test (:name (compile :type-derivation))
  (checked-compile-and-assert (:optimize nil)
      `(lambda ()
         (declare (optimize (speed 1) (debug 0)
                            (space 2) (safety 0) (compilation-speed 0)))
         (unwind-protect 0
           (* (/ (multiple-value-prog1 -29457482 -5602513511) 1))))
    (() 0)))

;; aggressive constant folding (bug #400)
(with-test (:name (compile :aggressive-constant-folding :bug-400))
  (checked-compile-and-assert ()
      `(lambda () (or t (the integer (/ 1 0))))
    (() t)))

(with-test (:name (:compiler :constraint-propagation :var-eql-to-non-var-1))
  (checked-compile `(lambda (x y)
                      (when (eql x (length y))
                        (locally
                            (declare (optimize (speed 3)))
                          (1+ x))))
                   :allow-notes '(not compiler-note)))

(with-test (:name (:compiler :constraint-propagation :var-eql-to-non-var-2))
  (checked-compile `(lambda (x y)
                      (when (eql (length y) x)
                        (locally
                            (declare (optimize (speed 3)))
                          (1+ x))))
                   :allow-notes '(not compiler-note)))

(with-test (:name (:compiler :constraint-propagation :float-bounds-1))
  (checked-compile `(lambda (x)
                      (declare (type (single-float * (3.0)) x))
                      (when (<= x 2.0)
                        (when (<= 2.0 x)
                          x)))
                   :allow-notes '(not compiler-note)))

(defun assert-code-deletion-note (lambda &optional (howmany 1))
  (let ((notes (nth-value
                4 (checked-compile lambda :allow-notes 'code-deletion-note))))
    (assert (= howmany (length notes)))))

(with-test (:name (:compiler :constraint-propagation :float-bounds-2))
  (assert-code-deletion-note
   `(lambda (x)
      (declare (type single-float x))
      (when (< 1.0 x)
        (when (<= x 1.0)
          (error "This is unreachable."))))))

(with-test (:name (:compiler :constraint-propagation :float-bounds-3
                             :LP-894498))
  (assert-code-deletion-note
   `(lambda (x)
      (declare (type (single-float 0.0) x))
      (when (> x 0.0)
        (when (zerop x)
          (error "This is unreachable."))))))

(with-test (:name (:compiler :constraint-propagation :float-bounds-4
                             :LP-894498))
  (assert-code-deletion-note
   `(lambda (x y)
      (declare (type (single-float 0.0) x)
               (type (single-float (0.0)) y))
      (when (> x y)
        (when (zerop x)
          (error "This is unreachable."))))))

(with-test (:name (:compiler :constraint-propagation :var-eql-to-var-1))
  (assert-code-deletion-note
   `(lambda (x y)
      (when (typep y 'fixnum)
        (when (eql x y)
          (unless (typep x 'fixnum)
            (error "This is unreachable"))
          (setq y nil))))))

(with-test (:name (:compiler :constraint-propagation :var-eql-to-var-2))
  (assert-code-deletion-note
   `(lambda (x y)
      (when (typep y 'fixnum)
        (when (eql y x)
          (unless (typep x 'fixnum)
            (error "This is unreachable"))
          (setq y nil))))))

;; Reported by John Wiseman, sbcl-devel
;; Subject: [Sbcl-devel] float type derivation bug?
;; Date: Tue, 4 Apr 2006 15:28:15 -0700
(with-test (:name (compile :type-derivation :float-bounds))
  (checked-compile
   `(lambda (bits)
      (let* ((s (if (= (ash bits -31) 0) 1 -1))
             (e (logand (ash bits -23) #xff))
             (m (if (= e 0)
                    (ash (logand bits #x7fffff) 1)
                    (logior (logand bits #x7fffff) #x800000))))
        (float (* s m (expt 2 (- e 150))))))))

;; Reported by James Knight
;; Subject: [Sbcl-devel] AVER: "(EQ (SB-NAME (SC-SB (TN-SC TN))) 'REGISTERS)"
;; Date: Fri, 24 Mar 2006 19:30:00 -0500
(with-test (:name (compile logbitp :vop))
  (checked-compile
   `(lambda (days shift)
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
                                         canonicalized-shift)
                                      7)))))))
        result))))

;;; MISC.637: incorrect delaying of conversion of optional entries
;;; with hairy constant defaults
(with-test (:name (compile :optional-entry :hairy-defaults :misc.637))
  (checked-compile-and-assert ()
      `(lambda ()
         (labels ((%f11 (f11-2 &key key1)
                    (labels ((%f8 (f8-2 &optional (f8-5 (if nil (return-from %f11 0) 0)))
                               :bad1))
                      (%f8 (%f8 0)))
                    :bad2))
           :good))
    (() :good)))

;;; MISC.555: new reference to an already-optimized local function
(with-test (:name (compile :already-optimized :local-function :misc.555))
  (checked-compile-and-assert (:optimize nil)
      `(lambda (p1)
         (declare (optimize (speed 1) (safety 2) (debug 2) (space 0))
                  (type keyword p1))
         (keywordp p1))
    ((:good) t)
    ((42)    (condition 'type-error))))

;;; Check that the compiler doesn't munge *RANDOM-STATE*.
(with-test (:name (compile *random-state*))
  (let* ((state (make-random-state))
         (*random-state* (make-random-state state))
         (a (random most-positive-fixnum)))
    (setf *random-state* state)
    (checked-compile `(lambda (x a)
                        (declare (single-float x)
                                 (type (simple-array double-float) a))
                        (+ (loop for i across a
                              summing i)
                           x)))
    (assert (= a (random most-positive-fixnum)))))

;;; MISC.641: LET-conversion after environment analysis lost NLX-INFOs
(with-test (:name (compile let :conversion :lost :nlx-infos :misc.641))
  (checked-compile-and-assert (:allow-style-warnings t :optimize nil)
      `(lambda ()
         (declare (optimize (speed 1) (space 0) (debug 2)
                            (compilation-speed 0) (safety 1)))
         (flet ((%f3 (f3-1 &key (key1 (count (floor 0 (min -74 0)) #())))
                  0))
           (apply #'%f3 0 nil)))
    (() 0)))

;;;  size mismatch: #<SB-VM::EA :DWORD base=#<SB-C:TN t1[RDX]> disp=1> is a :DWORD and #<SB-C:TN t2[RAX]> is a :QWORD. on x86-64
(with-test (:name (compile make-array aref :size-mismatch))
  (checked-compile `(lambda ()
                      (let ((x (make-array '(1) :element-type '(signed-byte 32))))
                        (setf (aref x 0) 1)))))

;;; step instrumentation confusing the compiler, reported by Faré
(with-test (:name (compile step))
  (checked-compile `(lambda ()
                      (declare (optimize (debug 2))) ; not debug 3!
                      (let ((val "foobar"))
                        (map-into (make-array (list (length val))
                                              :element-type '(unsigned-byte 8))
                                  #'char-code val)))))

;;; overconfident primitive type computation leading to bogus type
;;; checking.
(with-test (:name (compile :primitive-type standard-object condition function))
  (flet ((test-case/incompatible (type1 type2 object1 object2)
           (declare (ignore object1 object2))
           (multiple-value-bind (fun failure-p warnings)
               (checked-compile
                `(lambda (x)
                   (declare (type (and ,type1 ,type2) x))
                   x)
                :allow-failure t :allow-warnings t)
             (declare (ignore fun))
             (assert failure-p)
             (assert (= (length warnings) 1))
             ;; FIXME (declare (type <equivalent-to-empty-type> x)) is
             ;; currently dropped instead of compiled into a type
             ;; check.
             ;; (assert-error (funcall fun object1) type-error)
             ;; (assert-error (funcall fun object2) type-error)
             ))
         (test-case/compatible (type1 type2 object1 object2)
           (let ((fun (checked-compile
                       `(lambda (x)
                          (declare (type (and ,type1 ,type2) x))
                          x))))
             (when (typep object1 type2)
               (assert (typep (funcall fun object1) type1)))
             (when (typep object2 type1)
               (assert (typep (funcall fun object2) type2))))))
    ;; TODO Add structure classes, SEQUENCE and EXTENDED-SEQUENCE
    (let ((types `((condition                      . ,(make-condition 'error))
                   (sb-kernel:funcallable-instance . ,#'print-object)
                   (function                       . ,#'identity)
                   (sb-kernel:instance             . ,(find-class 'class))
                   (standard-object                . ,(find-class 'class))))
          (compatible '((sb-kernel:instance             . condition)
                        (sb-kernel:instance             . standard-object)
                        (sb-kernel:funcallable-instance . function)
                        (sb-kernel:funcallable-instance . standard-object)
                        (function                       . standard-object))))
      (loop :for (type1 . object1) :in types :do
         (loop :for (type2 . object2) :in types :do
            (funcall
             (if (or (eq type1 type2)
                     (find-if (lambda (cell)
                                (or (and (eq type1 (car cell))
                                         (eq type2 (cdr cell)))
                                    (and (eq type2 (car cell))
                                         (eq type1 (cdr cell)))))
                              compatible))
                 #'test-case/compatible
                 #'test-case/incompatible)
             type1 type2 object1 object2))))))

;;; VALUES declaration: while the declaration is a non-standard and
;;; possibly a non-conforming extension, as long as we do support it,
;;; we might as well get it right.
;;;
;;; The first of the following two tests originally asserted that the
;;; declaration _was_ allowed in a LET* form (The test was originally
;;; added in reaction to a bug reported by Kaersten Poeck on
;;; sbcl-devel 20061023). After commit
;;; 498e00334f2b1ba87c3edd557bbce4247977a11d, the declaration is
;;; accepted in LAMBDA but not LET*. The current form of the tests
;;; reflects this.
(with-test (:name (compile let* declare values))
  (multiple-value-bind (fun failure-p warnings)
      (checked-compile '(lambda () (let* () (declare (values list))))
                       :allow-warnings t)
    (declare (ignore fun failure-p))
    (assert (= (length warnings) 1))))

(with-test (:name (compile lambda declare values))
  (let ((fun (checked-compile '(lambda (x) (declare (values list)) x))))
    (assert (equal (sb-impl::%fun-ftype fun)
                   '(function (t) (values list &optional))))))

;;; test for some problems with too large immediates in x86-64 modular
;;; arithmetic vops
(with-test (:name (compile :modular-arithmetic-vops))
  (checked-compile
   '(lambda (x) (declare (fixnum x))
     (logand most-positive-fixnum (logxor x most-positive-fixnum))))

  (checked-compile
   '(lambda (x) (declare (fixnum x))
     (logand most-positive-fixnum (+ x most-positive-fixnum))))

  (checked-compile
   '(lambda (x) (declare (fixnum x))
     (logand most-positive-fixnum (* x most-positive-fixnum)))))

;;; bug 256.b
(with-test (:name :propagate-type-through-error-and-binding)
  (assert (nth-value
           2 (checked-compile '(lambda (x)
                                (list (let ((y (the real x)))
                                        (unless (floatp y) (error ""))
                                        y)
                                 (integer-length x)))
                              :allow-warnings 'sb-int:type-warning))))

;; Dead / in safe code
(with-test (:name (compile / :dead :safe))
  (checked-compile-and-assert (:optimize nil)
      '(lambda (x y)
        (declare (optimize (safety 3)))
        (/ x y)
        (+ x y))
    ((1 0) (condition 'division-by-zero))))

;;; Dead unbound variable (bug 412)
(with-test (:name (compile :dead-unbound :bug-412))
  (checked-compile-and-assert (:optimize '(:safety 3) :allow-warnings t)
      '(lambda ()
        #:unbound
        42)
    (() (condition 'unbound-variable))))

;;; No compiler notes from compiling SUBSEQ SIMPLE-VECTOR.
(with-test (:name (compile subseq simple-vector :no-notes))
  (checked-compile-and-assert (:optimize nil :allow-notes nil)
      `(lambda (s p e)
         (declare (optimize speed)
                  (simple-vector s))
         (subseq s p e))
    (((vector 1 2 3 4) 1 3) #(2 3) :test #'equalp)))

;;; No compiler notes from compiling COPY-SEQ SIMPLE-VECTOR.
(with-test (:name (compile copy-seq simple-bit-vector :if-input-does-not-exist))
  (checked-compile-and-assert (:optimize nil :allow-notes nil)
      `(lambda (s)
         (declare (optimize speed)
                  (simple-vector s))
         (copy-seq s))
    (((vector 1 2 3 4)) #(1 2 3 4) :test #'equalp)))

;;; bug in adding DATA-VECTOR-REF-WITH-OFFSET to x86-64
(with-test (:name (mismatch :data-vector-ref-with-offset))
  (assert (not (mismatch #(1.0f0 2.0f0)
                         (make-array 2 :element-type 'single-float
                                     :initial-contents (list 1.0f0 2.0f0))))))

;;; bug in interval-arithmetic used by the compiler: needless attempt to coerce too
;;; large bignums to floats
(with-test (:name (compile * / + - :interval-arithmetic))
  (dolist (op '(* / + -))
    (let ((fun (checked-compile
                `(lambda (x)
                   (declare (type (integer 0 #.(* 2 (truncate most-positive-double-float))) x))
                   (,op 0.0d0 x)))))
      (loop repeat 10
         do (let ((arg (random (truncate most-positive-double-float))))
              (assert (eql (funcall fun arg)
                           (funcall op 0.0d0 arg))))))))

(with-test (:name (compile :high-debug-known-function-inlining))
  (funcall (checked-compile
            '(lambda ()
              (declare (optimize (debug 3)) (inline append))
              (let ((fun (lambda (body)
                           (append
                            (first body)
                            nil))))
                (funcall fun '((foo (bar)))))))))

(with-test (:name (compile :high-debug-known-function-transform-with-optional-arguments))
  (checked-compile '(lambda (x y)
                     (declare (optimize sb-c::preserve-single-use-debug-variables))
                     (if (block nil
                           (some-unknown-function
                            (lambda ()
                              (return (member x y))))
                           t)
                         t
                         (error "~a" y)))
                   :allow-style-warnings t))

;;; Compiling W-P-O when the pinned objects are known to be fixnums
;;; or characters.
(with-test (:name (compile sb-sys:with-pinned-objects :known-type fixnum character))
  (checked-compile '(lambda (x y)
                     (declare (fixnum y) (character x))
                     (sb-sys:with-pinned-objects (x y)
                       (some-random-function)))
                   :allow-style-warnings t))

;;; *CHECK-CONSISTENCY* and TRULY-THE

(with-test (:name (compile :bug-423))
  (let ((sb-c::*check-consistency* t))
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
      (checked-compile (make-lambda 'list))
      (checked-compile (make-lambda 'vector)))))

;;; this caused a momentary regression when an ill-adviced fix to
;;; bug 427 made ANY-REG suitable for primitive-type T:
;;;
;;; no :MOVE-ARG VOP defined to move #<SB-C:TN t1> (SC SB-VM::SINGLE-REG) to #<SB-C:TN t2> (SC SB-VM::ANY-REG)
;;;    [Condition of type SIMPLE-ERROR]
(with-test (:name (compile :bug-427))
  (checked-compile
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
       (%zig)))
   :allow-style-warnings t
   :allow-warnings 'sb-int:local-argument-mismatch))

;;; non-required arguments in HANDLER-BIND
(with-test (:name (compile handler-bind :lambda-list))
  (checked-compile-and-assert ()
      '(lambda (x)
        (block nil
          (handler-bind ((error (lambda (&rest args) (return (cons :oops args)))))
            (/ 2 x))))
    ((0) :oops :test (lambda (x y)
                       (eq (car (first x)) (first y))))))

;;; NIL is a legal function name
(with-test (:name (nil :legal-function-name ))
  (assert (eq 'a (flet ((nil () 'a)) (nil)))))

;;; misc.528
(with-test (:name (compile :misc.528))
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
                  (funcall (checked-compile form) r y)
                  (let ((actual (aref r)))
                    (unless (eql expected actual)
                      (list expected actual)))))))
;;; misc.529
(with-test (:name (compile :misc.529))
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
                  (funcall (checked-compile form) r y)
                  (let ((actual (aref r)))
                    (unless (eql expected actual)
                      (list expected actual)))))))

;;; misc.556
(with-test (:name (compile :misc.556))
  (checked-compile-and-assert (:optimize nil)
      '(lambda (p1 p2)
        (declare
         (optimize (speed 1) (safety 0)
          (debug 0) (space 0))
         (type (member 8174.8604) p1)
         (type (member -95195347) p2))
        (floor p1 p2))
    ((8174.8604 -95195347) -1 :test (lambda (x y)
                                      (eql (first x) (first y))))))

;;; misc.557
(with-test (:name (compile :misc.557))
  (checked-compile-and-assert (:optimize nil)
      '(lambda (p1)
        (declare (optimize (speed 3) (safety 0) (debug 3) (space 1))
         (type (member -94430.086f0) p1))
        (floor (the single-float p1) 19311235))
    ((-94430.086f0) -1 :test (lambda (x y)
                               (eql (first x) (first y))))))

;;; misc.558
(with-test (:name (compile :misc.558))
  (checked-compile-and-assert (:optimize nil)
      '(lambda (p1)
        (declare (optimize (speed 1) (safety 2)
                  (debug 2) (space 3))
         (type (eql -39466.56f0) p1))
        (ffloor p1 305598613))
    ((-39466.56f0) -1.0f0 :test (lambda (x y)
                                  (eql (first x) (first y))))))

;;; misc.559
(with-test (:name (compile :misc.559))
  (checked-compile-and-assert (:optimize nil)
      '(lambda (p1)
        (declare (optimize (speed 1) (safety 1) (debug 1) (space 2))
         (type (eql -83232.09f0) p1))
        (ceiling p1 -83381228))
    ((-83232.09f0) 1 :test (lambda (x y)
                             (eql (first x) (first y))))))

;;; misc.560
(with-test (:name (compile :misc.560))
  (checked-compile-and-assert (:optimize nil)
      '(lambda (p1)
        (declare (optimize (speed 1) (safety 1)
                  (debug 1) (space 0))
         (type (member -66414.414f0) p1))
        (ceiling p1 -63019173f0))
    ((-66414.414f0) 1 :test (lambda (x y)
                              (eql (first x) (first y))))))

;;; misc.561
(with-test (:name (compile :misc.561))
  (checked-compile-and-assert (:optimize nil)
      '(lambda (p1)
        (declare (optimize (speed 0) (safety 1)
                  (debug 0) (space 1))
         (type (eql 20851.398f0) p1))
        (fceiling p1 80839863))
    ((20851.398f0) 1.0f0 :test (lambda (x y)
                                 (eql (first x) (first y))))))

;;; misc.581
(with-test (:name (compile :misc.581))
  (checked-compile-and-assert ()
      '(lambda (x)
        (declare (type (eql -5067.2056) x))
        (+ 213734822 x))
    ((-5067.2056) 'float :test (lambda (x y)
                                 (typep (first x) (first y))))))

;;; misc.581a
(with-test (:name (compile :misc.581a))
  (checked-compile-and-assert ()
      '(lambda (x) (declare (type (eql -1.0) x))
        (+ #x1000001 x))
    ((-1.0f0) 'single-float :test (lambda (x y)
                                    (typep (first x) (first y))))))

;;; misc.582
(with-test (:name (compile :misc.582))
  (checked-compile-and-assert (:optimize nil)
      '(lambda (p1)
        (declare (optimize (speed 0) (safety 1) (debug 1) (space 1))
         (type (eql -39887.645) p1))
        (mod p1 382352925))
    ((-39887.645) 0 :test (lambda (x y)
                            (> (first x) (first y))))))

;;; misc.587
(with-test (:name (compile :misc.587))
  (checked-compile-and-assert (:optimize nil)
      '(lambda (p2)
        (declare (optimize (speed 0) (safety 3) (debug 1) (space 0))
         (type (eql 33558541) p2))
        (- 92215.266 p2))
    ((33558541) 'single-float :test (lambda (x y)
                                      (typep (first x) (first y))))))

;;; misc.635
(with-test (:name (compile :misc.635))
  (checked-compile-and-assert (:optimize nil)
      '(lambda (p2)
        (declare (optimize (speed 0) (safety 1)
                  (debug 2) (space 2))
         (type (member -19261719) p2))
        (ceiling -46022.094 p2))
    ((-19261719) 1 :test (lambda (x y)
                           (eql (first x) (first y))))))

;;; misc.636
(with-test (:name (compile :misc.636))
  (assert (let* ((x 26899.875)
                 (form `(lambda (p2)
                          (declare (optimize (speed 3) (safety 1) (debug 3) (space 1))
                                   (type (member ,x #:g5437 char-code #:g5438) p2))
                          (* 104102267 p2))))
            (floatp (funcall (checked-compile form) x)))))

;;; misc.622
(with-test (:name (compile :misc.622))
  (checked-compile-and-assert (:optimize nil)
      '(lambda (p2)
        (declare (optimize (speed 3) (safety 2) (debug 3) (space 0))
         (type real p2))
        (+ 81535869 (the (member 17549.955 #:g35917) p2)))
    ((17549.955) (+ 81535869 17549.955))))

;;; misc.654
(with-test (:name (compile :misc.654))
  (checked-compile-and-assert (:optimize nil)
      '(lambda (p2)
        (declare (optimize (speed 0) (safety 2) (debug 0) (space 2))
         (type (member integer eql) p2))
        (coerce 2 p2))
    (('integer) 2)))

;;; misc.656
(with-test (:name (compile :misc.656))
  (checked-compile-and-assert (:optimize nil)
      '(lambda (p2)
        (declare (optimize (speed 0) (safety 2) (debug 0) (space 2))
         (type (member integer mod) p2))
        (coerce 2 p2))
    (('integer) 2)))

;;; misc.657
(with-test (:name (compile :misc.657))
  (checked-compile-and-assert (:optimize nil)
      '(lambda (p2)
        (declare (optimize (speed 0) (safety 2) (debug 0) (space 2))
         (type (member integer values) p2))
        (coerce 2 p2))
    (('integer) 2)))

(with-test (:name (compile aref string :derive-type))
  (checked-compile-and-assert ()
      '(lambda (s)
        (ctu:compiler-derived-type (aref (the string s) 0)))
    (("foo") (values 'character t))))

(with-test (:name (compile aref base-string :derive-type))
  (checked-compile-and-assert ()
      '(lambda (s)
        (ctu:compiler-derived-type (aref (the base-string s) 0)))
    (((coerce "foo" 'base-string)) (values #+sb-unicode 'base-char
                                           #-sb-unicode 'character
                                           t))))

(with-test (:name (compile dolist :constant :derive-type))
  (checked-compile-and-assert ()
      '(lambda (x)
        (dolist (y '(1 2 3))
          (when x
            (return (ctu:compiler-derived-type y)))))
    ((t) (values '(integer 1 3) t))))

(with-test (:name (compile dolist :simple list :derive-type))
  (checked-compile-and-assert ()
      '(lambda (x)
        (dolist (y (list 1 2 3))
          (when x
            (return (ctu:compiler-derived-type y)))))
    ((t) (values '(integer 1 3) t))))

(with-test (:name (compile dolist :dotted-constant-list :derive-type))
  (multiple-value-bind (fun failure-p warnings style-warnings)
      (checked-compile
       '(lambda (x)
         (dolist (y '(1 2 3 . 4) :foo)
           (when x
             (return (ctu:compiler-derived-type y)))))
       :allow-style-warnings t)
    (declare (ignore failure-p warnings))
    (assert (= 1 (length style-warnings)))
    (assert (equal '(integer 1 3) (funcall fun t)))
    (assert-error (funcall fun nil) type-error)))

(with-test (:name (compile destructuring-bind :constant list))
  (checked-compile-and-assert (:optimize nil :allow-notes nil)
      '(lambda ()
        (destructuring-bind (a (b c) d) '(1 (2 3) 4)
          (+ a b c d)))
    (() 10))
  (checked-compile-and-assert (:optimize nil :allow-notes nil)
      '(lambda (x)
        (or x
         (destructuring-bind (a (b c) d) '(1 "foo" 4)
           (+ a b c d))))
    ((:feh) :feh)))

;;; Functions with non-required arguments used to end up with
;;; (&OPTIONAL-DISPATCH ...) as their names.
(with-test (:name :hairy-function-name)
  (assert (eq 'read-line (nth-value 2 (function-lambda-expression #'read-line))))
  (assert (equal "#<FUNCTION READ-LINE>" (princ-to-string #'read-line))))

;;; PROGV + RESTRICT-COMPILER-POLICY
;; META: there's a test in compiler.impure.lisp that also tests
;; interaction of PROGV with (debug 3). These tests should be together.
(with-test (:name (compile progv sb-ext:restrict-compiler-policy))
  (let ((sb-c::*policy-min* sb-c::*policy-min*)
        (sb-c::*policy-max* sb-c::*policy-max*))
    (restrict-compiler-policy 'debug 3)
    (checked-compile-and-assert ()
        '(lambda (x)
          (let ((i x))
            (declare (special i))
            (list i
                  (progv '(i) (list (+ i 1))
                    i)
                  i)))
      ((1) '(1 2 1)))))

;;; It used to be possible to confuse the compiler into
;;; IR2-converting such a call to CONS
(with-test (:name (compile :late-bound-primitive))
  (checked-compile `(lambda ()
                      (funcall 'cons 1))
                   :allow-warnings t))

(with-test (:name (compile :hairy-array-element-type-derivation))
  (checked-compile
   '(lambda (x)
     (declare (type (and simple-string (satisfies eval)) x))
     (array-element-type x))))

(with-test (:name (compile &rest :derive-type 1))
  (checked-compile-and-assert ()
      `(lambda (&rest args)
         (ctu:compiler-derived-type args))
    ((nil) (values 'list t))))

(with-test (:name (compile &rest :derive-type 2))
  (multiple-value-bind (type derivedp)
      (funcall (funcall (checked-compile
                         `(lambda ()
                            (lambda (&rest args)
                              (ctu:compiler-derived-type args))))))
    (assert (eq 'list type))
    (assert derivedp)))

(with-test (:name (compile &rest :derive-type 3))
  (multiple-value-bind (type derivedp)
      (funcall (funcall (checked-compile
                         `(lambda ()
                            (lambda (&optional x &rest args)
                              (unless x (error "oops"))
                              (ctu:compiler-derived-type args)))))
               t)
    (assert (eq 'list type))
    (assert derivedp)))

(with-test (:name (compile &rest :derive-type 4))
  (multiple-value-bind (type derivedp)
      (funcall (funcall (checked-compile
                         `(lambda ()
                            (lambda (&optional x &rest args)
                              (declare (type (or null integer) x))
                              (when x (setf args x))
                              (ctu:compiler-derived-type args)))))
               42)
    (assert (equal '(or list integer) type))
    (assert derivedp)))

(with-test (:name (compile base-char typep :elimination))
  (checked-compile-and-assert (:optimize nil)
      `(lambda (ch)
         (declare (type base-char ch) (optimize (speed 3) (safety 0)))
         (typep ch 'base-char))
    ((t) t)))

(with-test (:name :regression-1.0.24.37)
  (checked-compile `(lambda (&key (test (constantly t)))
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
                     (eql (funcall (checked-compile ',lambda)
                                   ,@args)
                          (eval '(,lambda ,@args))))))))
  (sb-vm::with-float-traps-masked
      (:divide-by-zero :overflow :inexact :invalid)
    (let (#+sb-eval (sb-ext:*evaluator-mode* :interpret))
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
  (let ((f1 (checked-compile
             `(lambda (y)
                (declare (optimize speed))
                (let ((x (the (cons fixnum fixnum) (cons 1 2))))
                  (declare (type (cons t fixnum) x))
                  (rplaca x y)
                  (+ (car x) (cdr x))))))
        (f2 (checked-compile
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
  (checked-compile-and-assert (:optimize nil)
      `(lambda (x)
         (declare (optimize speed))
         (declare (type (array * (4 4)) x))
         (let ((y x))
           (setq x (make-array '(4 4)))
           (adjust-array y '(3 5))
           (array-dimension y 0)))
    (((make-array '(4 4) :initial-element nil :adjustable t)) 3))
  (checked-compile-and-assert (:optimize nil)
      `(lambda (x)
         (declare (optimize speed))
         (declare (type (array * (4 4)) x))
         (let ((y x))
           (setq x (make-array '(4 4)))
           (adjust-array y '(3 5))
           (array-dimension (the (array t) y) 0)))
    (((make-array '(4 4) :initial-element nil :adjustable t)) 3)))

(with-test (:name :with-timeout-code-deletion-note)
  (checked-compile `(lambda ()
                      (sb-ext:with-timeout 0
                        (sleep 1)))
                   :allow-notes nil))

;;; These next two tests rely on memoization and/or caching to d.t.r.t.
;;; which is absurd. The warning suppression mechanism should be based
;;; on something higher-level, and not whether an operation was memoized,
;;; because who's to say we didn't kick something out of a cache?
(with-test (:name :full-warning-for-undefined-type-in-cl
                  :broken-on :sbcl)
  (multiple-value-bind (fun failure-p warnings)
      (checked-compile `(lambda (x) (the replace x)) :allow-warnings t)
    (declare (ignore fun failure-p))
    (assert (= 1 (length warnings)))))

(with-test (:name :single-warning-for-single-undefined-type
                  :broken-on :sbcl)
  ;; STYLE-WARNING for symbol not in cl package.
  (multiple-value-bind (fun failure-p warnings style-warnings)
      (checked-compile `(lambda (x) (the #:no-type x))
                       :allow-style-warnings t)
    (declare (ignore fun failure-p warnings))
    (assert (= 1 (length style-warnings))))

  ;; Full WARNING for invalid type specifier starting with QUOTE.
  (multiple-value-bind (fun failure-p warnings)
      (checked-compile `(lambda (x) (the 'fixnum x)) :allow-warnings t)
    (declare (ignore fun failure-p))
    (assert (= 1 (length warnings)))))

(with-test (:name :complex-subtype-dumping-in-xc)
  (assert
   (= sb-vm:complex-single-float-widetag
      (sb-kernel:widetag-of
       (sb-vm:saetp-initial-element-default (sb-c::find-saetp '(complex single-float))))))
  (assert
   (= sb-vm:complex-double-float-widetag
      (sb-kernel:widetag-of
       (sb-vm:saetp-initial-element-default (sb-c::find-saetp '(complex double-float)))))))

(with-test (:name (compile complex single-float make-array :fill))
  (assert (every (lambda (x) (= #c(1.0 2.0) x))
                 (funcall
                  (checked-compile
                   `(lambda (n x)
                      (make-array (list n)
                                  :element-type '(complex single-float)
                                  :initial-element x)))
                  10
                  #c(1.0 2.0)))))

(with-test (:name :regression-1.0.28.21)
  (checked-compile-and-assert ()
      `(lambda (x) (typep x '(simple-array * 1)))
    (((vector 1 2 3))      t)
    (( "abc")              t)
    (((make-array '(2 2))) nil)))

(with-test (:name :no-silly-compiler-notes-from-character-function)
  (dolist (name '(char-code char-int character char-name standard-char-p
                  graphic-char-p alpha-char-p upper-case-p lower-case-p
                  both-case-p digit-char-p alphanumericp digit-char-p))
    (checked-compile `(lambda (x)
                        (declare (character x) (optimize (speed 3)))
                        (,name x))
                     :allow-notes nil))
  (dolist (name '(char= char/= char< char> char<= char>=
                  char-lessp char-greaterp char-not-greaterp
                  char-not-lessp))
    (checked-compile `(lambda (x y)
                        (declare (character x y) (optimize speed))
                        (,name x y))
                     :allow-notes nil)))

;;; optimizing make-array
(with-test (:name (make-array :open-code-initial-contents))
  (flet ((test (form) (assert (not (ctu:ir1-named-calls form)))))
    (test `(lambda (x y z)
             (make-array '(3) :initial-contents (list x y z))))
    (test `(lambda (x y z)
             (make-array '3 :initial-contents (vector x y z))))
    (test `(lambda (x y z)
             (make-array '3 :initial-contents `(,x ,y ,z))))
    (test `(lambda (x y z)
             ;; Single-use FLET is eliminated,
             ;; so MAKE-ARRAY's result is obviously a vector.
             (flet ((size () '(3)))
               (make-array (size) :initial-contents `(,x ,y ,z)))))
    (test `(lambda (x y z)
             (flet ((size () (list 3))) ; here too
               (make-array (size) :initial-contents `(,x ,y ,z)))))))

(with-test (:name :optimize-array-in-bounds-p)
  (macrolet ((must-optimize (&body exprs)
               `(progn
                  ,@(loop for (expr type) on exprs by #'cddr
                          collect `(assert-type (lambda () ,expr) (member ,type)))))
             (must-not-optimize (&body exprs)
               `(progn
                  ,@(loop for expr in exprs
                          collect `(assert-type (lambda () ,expr) boolean)))))
    (must-optimize
     ;; in bounds
     (let ((a (make-array '(1))))
       (array-in-bounds-p a 0))
     t
     ;; exceeds upper bound (constant)
     (let ((a (make-array '(1))))
       (array-in-bounds-p a 1))
     nil
     ;; exceeds upper bound (interval)
     (let ((a (make-array '(1))))
       (array-in-bounds-p a (+ 1 (random 2))))
     nil
     ;; negative lower bound (constant)
     (let ((a (make-array '(1))))
       (array-in-bounds-p a -1))
     nil
     ;; negative lower bound (interval)
     (let ((a (make-array 3))
           (i (- (random 1) 20)))
       (array-in-bounds-p a i))
     nil
     ;; multiple known dimensions
     (let ((a (make-array '(1 1))))
       (array-in-bounds-p a 0 0))
     t
     ;; union types
     (let ((s (the (simple-string 10) (eval "0123456789"))))
       (array-in-bounds-p s 9))
     t)
    (must-not-optimize
     ;; don't trust non-simple array length in safety=1
     (let ((a (the (array * (10 20)) (make-array '(10 20) :adjustable t))))
       (eval `(adjust-array ,a '(0 0)))
       (array-in-bounds-p a 9 0))
     ;; multiple unknown dimensions
     (let ((a (make-array (list (random 20) (random 5)))))
       (array-in-bounds-p a 5 2))
     ;; some other known dimensions
     (let ((a (make-array (list 1 (random 5)))))
       (array-in-bounds-p a 0 2))
     ;; subscript might be negative
     (let ((a (make-array '(5 10))))
       (array-in-bounds-p a 1 (- (random 3) 2)))
     ;; subscript might be too large
     (let ((a (make-array '(5 10))))
       (array-in-bounds-p a (random 6) 1))
     ;; unknown upper bound
     (let ((a (make-array '(5 10))))
       (array-in-bounds-p a (get-universal-time) 1))
     ;; unknown lower bound
     (let ((a (make-array '(5 30))))
       (array-in-bounds-p a 0 (- (get-universal-time))))
     ;; in theory we should be able to optimize
     ;; the following but the current implementation
     ;; doesn't cut it because the array type's
     ;; dimensions get reported as (* *).
     (let ((a (make-array (list (random 20) 1))))
       (array-in-bounds-p a 5 2)))))

;;; optimizing (EXPT -1 INTEGER)
(with-test (:name (expt -1 integer))
  (dolist (x '(-1 -1.0 -1.0d0))
    (multiple-value-bind (callees fun)
        (ctu:ir1-named-calls `(lambda (x) (expt ,x (the fixnum x))))
      (assert (not callees))
      (dotimes (i 12)
        (if (oddp i)
            (assert (eql x (funcall fun i)))
            (assert (eql (- x) (funcall fun i))))))))

(with-test (:name :float-division-using-exact-reciprocal)
  (flet ((test (lambda-form arg res &key (check-insts t))
           (declare (ignorable check-insts))
           (let ((fun (checked-compile lambda-form)))
             ;; Let's make sure there is no division at runtime: for x86 and
             ;; x86-64 that implies an FDIV, DIVSS, or DIVSD instruction, so
             ;; look for DIV in the disassembly. It's a terrible KLUDGE, but
             ;; it works.
             #+(or x86 x86-64)
             (when check-insts
               (assert (not (ctu:asm-search "DIV" fun))))
             ;; No generic arithmetic!
             (assert (not (ctu:asm-search "GENERIC" fun)))
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
           (let* ((fun1 (checked-compile lambda-form))
                  (fun2 (funcall (checked-compile
                                  `(lambda ()
                                     (declare (optimize (sb-c::float-accuracy 0)))
                                     ,lambda-form)))))
             ;; Not generic arithmetic, please!
             (assert (and (not (ctu:asm-search "GENERIC" fun1))
                          (not (ctu:asm-search "GENERIC" fun2))))
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
           (let* ((fun1 (checked-compile lambda-form))
                  (fun2 (funcall (checked-compile
                                  `(lambda ()
                                     (declare (optimize (sb-c::float-accuracy 0)))
                                     ,lambda-form)))))
             ;; Let's make sure there is no addition at runtime: for x86 and
             ;; x86-64 that implies an FADD, ADDSS, or ADDSD instruction, so
             ;; look for the ADDs in the disassembly. It's a terrible KLUDGE,
             ;; but it works. Unless FLOAT-ACCURACY is zero, we leave the
             ;; addition in to catch SNaNs.
             #+x86
             (progn
               (assert (or (ctu:asm-search "FADDD " fun1)
                           (ctu:asm-search "FADD-STI " fun1)))
               (assert (not (or (ctu:asm-search "FADDD " fun2)
                                (ctu:asm-search "FADD-STI " fun2)))))
             #+x86-64
             (let ((inst (if (typep result 'double-float)
                             "ADDSD" "ADDSS")))
               (assert (ctu:asm-search inst fun1))
               (assert (not (ctu:asm-search inst fun2))))
             (assert (eql result (funcall fun1 arg)))
             (assert (eql result (funcall fun2 arg))))))
    (test `(lambda (x) (declare (single-float x)) (+ x 0)) 123.45)
    (test `(lambda (x) (declare (single-float x)) (+ x 0.0)) 543.21)
    (test `(lambda (x) (declare (single-float x)) (+ x 0.0d0)) 42.00 42.d0)
    (test `(lambda (x) (declare (double-float x)) (+ x 0)) 123.45d0)
    (test `(lambda (x) (declare (double-float x)) (+ x 0.0)) 543.21d0)
    (test `(lambda (x) (declare (double-float x)) (+ x 0.0d0)) 42.d0)))

(with-test (:name :float-subtraction-of-zero)
  (flet ((test (lambda-form arg &optional (result arg))
           (let* ((fun1 (checked-compile lambda-form))
                  (fun2 (funcall (checked-compile
                                  `(lambda ()
                                     (declare (optimize (sb-c::float-accuracy 0)))
                                     ,lambda-form)))))
             ;; Let's make sure there is no substraction at runtime: for x86
             ;; and x86-64 that implies an FSUB, SUBSS, or SUBSD instruction,
             ;; so look for SUB in the disassembly. It's a terrible KLUDGE,
             ;; but it works. Unless FLOAT-ACCURACY is zero, we leave the
             ;; substraction in in to catch SNaNs.
             #+x86
             (assert (and (ctu:asm-search "FSUB" fun1)
                          (not (ctu:asm-search "FSUB" fun2))))
             #+x86-64
             (let ((inst (if (typep result 'double-float)
                             "SUBSD" "SUBSS")))
               (assert (and (ctu:asm-search inst fun1)
                            (not (ctu:asm-search inst fun2)))))
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
           (let* ((fun1 (checked-compile lambda-form))
                  (fun2 (funcall (checked-compile
                                  `(lambda ()
                                     (declare (optimize (sb-c::float-accuracy 0)))
                                     ,lambda-form)))))
             ;; Let's make sure there is no multiplication at runtime: for x86
             ;; and x86-64 that implies an FMUL, MULSS, or MULSD instruction,
             ;; so look for MUL in the disassembly. It's a terrible KLUDGE,
             ;; but it works.
             #+(or x86 x86-64)
             (assert (and (not (ctu:asm-search "MUL" fun1))
                          (not (ctu:asm-search "MUL" fun2))))
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
  (checked-compile-and-assert ()
      `(lambda ()
         (flet ((k (&rest x) (declare (ignore x)) 0))
           (multiple-value-call #'k #'k)))
    (() 0)))

(with-test (:name :allocate-closures-failing-aver)
  (checked-compile-and-assert (:allow-style-warnings t)
      `(lambda ()
         (labels ((k (&optional x) #'k))))
    (() nil)))

(with-test (:name :flush-vector-creation :skipped-on :interpreter
            :serial t)
  (let ((f (checked-compile `(lambda ()
                               (dotimes (i 1024)
                                 (vector i i i))
                               t))))
    (ctu:assert-no-consing (funcall f))))

(with-test (:name :truncate-float :fails-on :ppc64)
  (let ((s (checked-compile `(lambda (x)
                               (declare (single-float x))
                               (truncate x))))
        (d (checked-compile `(lambda (x)
                               (declare (double-float x))
                               (truncate x))))
        (s-inlined (checked-compile
                    `(lambda (x)
                       (declare (type (single-float 0.0s0 1.0s0) x))
                       (truncate x))))
        (d-inlined (checked-compile
                    `(lambda (x)
                       (declare (type (double-float 0.0d0 1.0d0) x))
                       (truncate x)))))
    ;; Check that there is no generic arithmetic
    (assert (not (ctu:asm-search "GENERIC" s)))
    (assert (not (ctu:asm-search "GENERIC" d)))
    ;; Check that we actually inlined the call when we were supposed to.
    (assert (not (ctu:asm-search "UNARY-TRUNCATE" s-inlined)))
    (assert (not (ctu:asm-search "UNARY-TRUNCATE" d-inlined)))))

(with-test (:name (make-array :unnamed-dimension-leaf))
  (checked-compile-and-assert ()
      `(lambda (stuff)
         (make-array (map 'list 'length stuff)))
    (('((1 2) (1 2 3))) #2A((0 0 0) (0 0 0)) :test #'equalp )))

(with-test (:name :fp-decoding-funs-not-flushable-in-safe-code)
  (dolist (name '(float-sign float-radix float-digits float-precision decode-float
                  integer-decode-float))
    (let ((fun (checked-compile `(lambda (x)
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
  (let* ((fun (checked-compile
               `(lambda (x y)
                  (declare (type sb-sys:system-area-pointer x)
                           (type (integer 0 100) y))
                  (sb-sys:sap-ref-16 x (+ 4 y)))))
         (vector (coerce '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
                         '(simple-array (unsigned-byte 8) (*))))
         (sap (sb-sys:vector-sap vector))
         (ret (funcall fun sap 0)))
    ;; test for either endianness
    (assert (or (= ret (+ (* 5 256) 4)) (= ret (+ (* 4 256) 5))))))

(with-test (:name (compile coerce :type-warning))
  (dolist (type '(t (unsigned-byte 8) (unsigned-byte 16) (unsigned-byte 32)
                  (signed-byte 8) (signed-byte 16) (signed-byte 32)))
    (let ((fun (checked-compile `(lambda (x)
                                   (declare (type simple-vector x))
                                   (coerce x '(vector ,type))))))
      (assert (typep (funcall fun #(1)) `(simple-array ,type (*)))))))

(with-test (:name (compile truncate double-float))
  (checked-compile-and-assert ()
      `(lambda (x)
         (multiple-value-bind (q r)
             (truncate (coerce x 'double-float))
           (declare (type unsigned-byte q)
                    (type double-float r))
           (list q r)))
    ((1.0d0) '(1 0.0d0))))

(with-test (:name :set-slot-value-no-warning)
  (let ((notes (nth-value
                4 (checked-compile `(lambda (x y)
                                      (declare (optimize speed safety))
                                      (setf (slot-value x 'bar) y))))))
    (assert (= 1 (length notes)))))

(with-test (:name (concatenate :string-opt))
  (flet ((test (type expect)
           (sb-int:binding*
                 (((calls fun)
                   (ctu:ir1-named-calls `(lambda (a b c d e)
                                           (concatenate ',type a b c d e))))
                  (args '("foo" #(#\.) "bar" (#\-) "quux"))
                  (res (apply fun args)))
             (assert (equal calls (list expect)))
             (assert (equal (apply #'concatenate type args)
                            res))
             (assert (typep res type)))))
    #+sb-unicode
    (test 'string 'sb-kernel:%concatenate-to-string)
    #+sb-unicode
    (test 'simple-string 'sb-kernel:%concatenate-to-string)
    (test 'base-string 'sb-kernel:%concatenate-to-base-string)
    (test 'simple-base-string 'sb-kernel:%concatenate-to-base-string)))

(with-test (:name (satisfies :no-local-fun)
            :fails-on :ppc64)
  (let ((fun (checked-compile
              `(lambda (arg)
                 (labels ((local-not-global-bug (x)
                            t)
                          (bar (x)
                            (typep x '(satisfies local-not-global-bug))))
                   (bar arg)))
              :allow-style-warnings t)))
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
  (checked-compile `(lambda (&optional (foo #p"foo/bar")))
                   :allow-style-warnings t))

(with-test (:name :bug-309129)
  (multiple-value-bind (fun failurep warnings)
      (checked-compile `(lambda (v) (values (svref v 0) (vector-pop v)))
                       :allow-failure t :allow-warnings t)
    (assert failurep)
    (assert (= 1 (length warnings)))
    (handler-case (funcall fun #(1))
      (type-error (c)
        ;; we used to put simply VECTOR into EXPECTED-TYPE, rather
        ;; than explicitly (AND VECTOR (NOT SIMPLE-ARRAY))
        (assert (not (typep (type-error-datum c) (type-error-expected-type c)))))
      (:no-error (&rest values)
        (declare (ignore values))
        (error "no error")))))

(with-test (:name (round :unary :type-derivation))
  (checked-compile-and-assert ()
      `(lambda (zone)
         (multiple-value-bind (h m) (truncate (abs zone) 1.0)
           (declare (ignore h))
           (round (* 60.0 m))))
    ((0.5) (values 30 0.0))))

(with-test (:name :bug-525949)
  (checked-compile-and-assert ()
      `(lambda ()
         (labels ((always-one () 1)
                  (f (z)
                    (let ((n (funcall z)))
                      (declare (fixnum n))
                      (the double-float (expt n 1.0d0)))))
           (f #'always-one)))
    (() 1.0d0)))

(with-test (:name :%array-data-type-derivation)
  (assert (not
           (ctu:asm-search "OBJECT-NOT-SIMPLE-ARRAY-UNSIGNED-BYTE-32-ERROR"
             `(lambda (ary)
                (declare (type (simple-array (unsigned-byte 32) (3 3)) ary))
                (setf (aref ary 0 0) 0))))))

(with-test (:name :array-storage-vector-type-derivation)
  (checked-compile-and-assert ()
      `(lambda (ary)
         (declare (type (simple-array (unsigned-byte 32) (3 3)) ary))
         (ctu:compiler-derived-type (array-storage-vector ary)))
    (((make-array '(3 3) :element-type '(unsigned-byte 32)))
     (values '(simple-array (unsigned-byte 32) (9)) t))))

(with-test (:name :bug-523612)
  (checked-compile-and-assert ()
      `(lambda (&key toff)
         (make-array 3 :element-type 'double-float
                       :initial-contents
                       (if toff (list toff 0d0 0d0) (list 0d0 0d0 0d0))))
    ((:toff nil)   (vector 0.0d0 0.0d0 0.0d0) :test #'equalp)
    ((:toff 2.3d0) (vector 2.3d0 0.0d0 0.0d0) :test #'equalp)))

(with-test (:name :bug-309788)
  (let ((calls (ctu:ir1-named-calls
                              `(lambda (x)
                                 (declare (optimize speed))
                                 (let ((env nil))
                                   (typep x 'fixnum env))))))
    (assert (not calls))))

(with-test (:name :bug-309124)
  (let ((fun (checked-compile `(lambda (x)
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
  (multiple-value-bind (calls fun)
      (ctu:ir1-named-calls
              `(lambda (x)
                 (declare (type (and simple-bit-vector (satisfies eval)) x)
                          (optimize speed))
                 (elt x 5)))
    (assert (equal calls '(eval)))
    (assert (= 1 (funcall fun #*000001)))
    (assert (= 0 (funcall fun #*000010)))))

(with-test (:name :mult-by-one-in-float-acc-zero)
  (checked-compile-and-assert ()
      `(lambda (x)
         (declare (optimize (sb-c::float-accuracy 0)))
         (* x 1.0))
    ((1) 1.0))
  (checked-compile-and-assert ()
      `(lambda (x)
         (declare (optimize (sb-c::float-accuracy 0)))
         (* x -1.0))
    ((1) -1.0))
  (checked-compile-and-assert ()
      `(lambda (x)
         (declare (optimize (sb-c::float-accuracy 0)))
         (* x 1.0d0))
    ((1) 1.0d0))
  (checked-compile-and-assert ()
      `(lambda (x)
         (declare (optimize (sb-c::float-accuracy 0)))
         (* x -1.0d0))
    ((1) -1.0d0)))

(with-test (:name :dotimes-non-integer-counter-value)
  (multiple-value-bind (fun failure-p warnings)
      (checked-compile `(lambda () (dotimes (i 8.6)))
                       :allow-warnings 'sb-int:type-warning)
    (declare (ignore failure-p))
    (assert warnings)
    (assert-error (funcall fun) type-error)))

(with-test (:name :bug-454681)
  ;; This used to break due to reference to a dead lambda-var during
  ;; inline expansion.
  (assert (checked-compile
           `(lambda ()
              (multiple-value-bind (iterator+977 getter+978)
                  (does-not-exist-but-does-not-matter)
                (flet ((iterator+976 ()
                         (funcall iterator+977)))
                  (declare (inline iterator+976))
                  (let ((iterator+976 #'iterator+976))
                    (funcall iterator+976)))))
           :allow-style-warnings t)))

(with-test (:name :complex-float-local-fun-args)
  ;; As of 1.0.27.14, the lambda below failed to compile due to the
  ;; compiler attempting to pass unboxed complex floats to Z and the
  ;; MOVE-ARG method not expecting the register being used as a
  ;; temporary frame pointer.  Reported by sykopomp in #lispgames,
  ;; reduced test case provided by _3b`.
  (checked-compile `(lambda (a)
                      (labels ((z (b c)
                                 (declare ((complex double-float) b c))
                                 (* b (z b c))))
                        (loop for i below 10 do
                             (setf a (z a a)))))))

(with-test (:name (compile :bug-309130))
  (flet ((test (form)
           (let ((warnings (nth-value
                            2 (checked-compile form :allow-warnings t))))
             (assert (= 1 (length warnings))))))
    (test `(lambda () (svref (make-array 8 :adjustable t) 1)))
    (test `(lambda (x)
             (declare (optimize (debug 0)))
             (declare (type vector x))
             (list (fill-pointer x) (svref x 1))))
    (test `(lambda (x)
             (list (vector-push (svref x 0) x))))
    (test `(lambda (x)
             (list (vector-push-extend (svref x 0) x))))))

(with-test (:name (compile :bug-646796))
  (checked-compile-and-assert ()
      `(lambda ()
         (load-time-value (the (values fixnum) 42)))
    (() 42)))

(with-test (:name (compile :bug-654289))
  ;; Test that compile-times don't explode when quoted constants
  ;; get big.
  (labels ((time-n (n)
             (gc :full t) ; Let's not confuse the issue with GC
             (let* ((tree (make-tree (expt 10 n) nil))
                    (t0 (get-internal-run-time))
                    (f (checked-compile `(lambda (x) (eq x (quote ,tree)))))
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
      (unless (> (+ (truncate internal-time-units-per-second 10)
                    (* 2 max-small))
                 max-big)
        (error "Bad scaling or test? ~S" times)))))

(with-test (:name (compile :bug-309063))
  (checked-compile-and-assert ()
      `(lambda (x)
         (declare (type (integer 0 0) x))
         (ash x 100))
    ((0) 0)))

(with-test (:name (compile :bug-655872))
  (let ((f (checked-compile
            `(lambda (x)
               (declare (optimize (safety 3)))
               (let ((coerced
                      (locally (declare (optimize (safety 0)))
                       (coerce x '(simple-vector 128)))))
                 (values coerced (aref coerced 60))))))
        (long (make-array 100 :element-type 'fixnum)))
    (dotimes (i 100)
      (setf (aref long i) i))
    ;; 1. COERCE doesn't check the length in unsafe code.
    (assert (eql 60 (nth-value 1 (funcall f long))))
    ;; 2. The compiler forces the result of COERCE to have the specified length
    (assert (= (length (funcall f long)) 128))
    (assert (= (length (funcall f #*1001)) 128))))

(with-test (:name (compile :bug-655203-regression))
  (let ((fun (checked-compile
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

(with-test (:name :constant-concatenate-compile-time)
  (flet ((make-lambda (n)
           `(lambda (x)
              (declare (optimize (speed 3) (space 0)))
              (concatenate 'string x ,(make-string n)))))
    (let* ((l0 (make-lambda 1))
           (l1 (make-lambda 10))
           (l2 (make-lambda 100))
           (l3 (make-lambda 1000))
           (t0 (get-internal-run-time))
           (f0 (checked-compile l0))
           (t1 (get-internal-run-time))
           (f1 (checked-compile l1))
           (t2 (get-internal-run-time))
           (f2 (checked-compile l2))
           (t3 (get-internal-run-time))
           (f3 (checked-compile l3))
           (t4 (get-internal-run-time))
           (d0 (- t1 t0))
           (d1 (- t2 t1))
           (d2 (- t3 t2))
           (d3 (- t4 t3))
           (short-avg (/ (+ d0 d1 d2) 3)))
      (assert (and f0 f1 f2 f3))
      (assert (< d3 (* 10 short-avg))))))

(with-test (:name :bug-384892)
  (assert (ctype=
           '(function (fixnum fixnum &key (:k1 boolean))
             (values (member t) &optional))
           (sb-kernel:%simple-fun-type
            (checked-compile `(lambda (x y &key k1)
                                (declare (fixnum x y))
                                (declare (boolean k1))
                                (declare (ignore x y k1))
                                t))))))

(with-test (:name :bug-309448
            :skipped-on :gc-stress)
  ;; Like all tests trying to verify that something doesn't blow up
  ;; compile-times this is bound to be a bit brittle, but at least
  ;; here we try to establish a decent baseline.
  (labels ((time-it (lambda want &optional times)
             (gc :full t) ; let's keep GCs coming from other code out...
             (let* ((start (get-internal-run-time))
                    (iterations 0)
                    (fun (if times
                             (loop for result = (checked-compile lambda)
                                   repeat times
                                   finally (return result))
                             (loop for result = (checked-compile lambda)
                                   do (incf iterations)
                                   until (> (get-internal-run-time) (+ start (* 10
                                                                                (/ internal-time-units-per-second
                                                                                   1000))))
                                   finally (return result))))
                    (end (get-internal-run-time))
                    (got (funcall fun)))
               (unless (eql want got)
                 (error "wanted ~S, got ~S" want got))
               (values (- end start) iterations)))
           (test-it (simple result1 complex result2)
             (multiple-value-bind (time-simple iterations)
                 (time-it simple result1)
               (assert (>= (* 10 (1+ time-simple))
                           (time-it complex result2 iterations))))))
    ;; This is mostly identical as the next one, but doesn't create
    ;; hairy unions of numeric types.
    (test-it `(lambda ()
                (labels ((bar (baz bim)
                           (let ((n (+ baz bim)))
                             (* n (+ n 1) bim))))
                  (let ((a (bar 1 1))
                        (b (bar 1 1))
                        (c (bar 1 1)))
                    (- (+ a b) c))))
             6
             `(lambda ()
                (labels ((bar (baz bim)
                           (let ((n (+ baz bim)))
                             (* n (+ n 1) bim))))
                  (let ((a (bar 1 1))
                        (b (bar 1 5))
                        (c (bar 1 15)))
                    (- (+ a b) c))))
             -3864)
    (test-it `(lambda ()
                (labels ((sum-d (n)
                           (let ((m (truncate 999 n)))
                             (/ (* n m (1+ m)) 2))))
                  (- (+ (sum-d 3)
                        (sum-d 3))
                     (sum-d 3))))
             166833
             `(lambda ()
                (labels ((sum-d (n)
                           (let ((m (truncate 999 n)))
                             (/ (* n m (1+ m)) 2))))
                  (- (+ (sum-d 3)
                        (sum-d 5))
                     (sum-d 15))))
             233168)))

(with-test (:name :regression-1.0.44.34)
  (checked-compile
   `(lambda (z &rest args)
      (declare (dynamic-extent args))
      (flet ((foo (w v) (list v w)))
        (setq z 0)
        (flet ((foo ()
                 (foo z args)))
          (declare (dynamic-extent #'foo))
          (call #'foo nil))))
   :allow-style-warnings t))

(with-test (:name :bug-713626)
  (let ((f (eval '(constantly 42))))
    (checked-compile-and-assert ()
        `(lambda () (funcall ,f 1 2 3))
      (() 42))))

(with-test (:name :known-fun-allows-other-keys)
  (funcall (checked-compile
            `(lambda () (directory "." :allow-other-keys t))))
  (funcall (checked-compile
            `(lambda () (directory "." :bar t :allow-other-keys t)))))

(with-test (:name :bug-551227)
  ;; This function causes constraint analysis to perform a
  ;; ref-substitution that alters the A referred to in (G A) at in the
  ;; consequent of the IF to refer to be NUMBER, from the
  ;; LET-converted inline-expansion of MOD.  This leads to attempting
  ;; to CLOSE-OVER a variable that simply isn't in scope when it is
  ;; referenced.
  (checked-compile
   `(lambda (a)
      (if (let ((s a))
            (block :block
              (map nil
                   (lambda (e)
                     (return-from :block
                       (f (mod a e))))
                   s)))
          (g a)))
   :allow-style-warnings t))

(with-test (:name :funcall-lambda-inlined)
  (assert (not
           (ctu:find-code-constants
            (checked-compile `(lambda (x y)
                                (+ x (funcall (lambda (z) z) y))))
            :type 'function))))

(with-test (:name :bug-720382)
  (multiple-value-bind (fun failurep warnings)
      (checked-compile `(lambda (b) ((lambda () b) 1)) :allow-warnings t)
    (assert failurep)
    (assert (= 1 (length warnings)))
    (assert-error (funcall fun 0))))

(with-test (:name :multiple-args-to-function)
  (let ((form `(flet ((foo (&optional (x 13)) x))
                 (funcall (function foo 42))))
        #+sb-eval (*evaluator-mode* :interpret))
    #+sb-eval
    (assert (eq :error
                (handler-case (eval form)
                  (error () :error))))
    (multiple-value-bind (fun failure-p)
        (checked-compile `(lambda () ,form) :allow-failure t)
      (assert failure-p)
      (assert-error (funcall fun)))))

;;; This doesn't test LVAR-FUN-IS directly, but captures it
;;; pretty accurately anyways.
(with-test (:name :lvar-fun-is)
  (dolist (fun (list
                `(lambda (x) (member x x :test #'eq))
                `(lambda (x) (member x x :test 'eq))
                `(lambda (x) (member x x :test #.#'eq))))
    (assert (equal '(sb-kernel:%member-eq) (ctu:ir1-named-calls fun))))
  (dolist (fun (list
                `(lambda (x)
                   (declare (notinline eq))
                   (member x x :test #'eq))
                `(lambda (x)
                   (declare (notinline eq))
                   (member x x :test 'eq))
                `(lambda (x)
                   (declare (notinline eq))
                   (member x x :test #.#'eq))))
    (assert (equal '(sb-kernel:%member-test) (ctu:ir1-named-calls fun)))))

(with-test (:name :delete-to-delq-opt :skipped-on :interpreter)
  (dolist (fun (list `(lambda (x y)
                        (declare (list y))
                        (delete x y :test #'eq))
                     `(lambda (x y)
                        (declare (fixnum x) (list y))
                        (delete x y))
                     `(lambda (x y)
                        (declare (symbol x) (list y))
                        (delete x y :test #'eql))))
    (assert (equal '(sb-int:delq)
                   (ctu:ir1-named-calls fun)))))

(with-test (:name (compile :bug-767959))
  ;; This used to signal an error.
  (checked-compile `(lambda ()
                      (declare (optimize sb-c:store-coverage-data))
                      (assoc
                       nil
                       '((:ordinary . ordinary-lambda-list))))))

;; This test failed formerly because the source transform of TYPEP would be
;; disabled when storing coverage data, thus giving no semantics to
;; expressions such as (TYPEP x 'INTEGER). The compiler could therefore not
;; prove that the else clause of the IF is unreachable - which it must be
;; since X is asserted to be fixnum. The conflicting requirement on X
;; that it be acceptable to LENGTH signaled a full warning.
;; Nobody on sbcl-devel could remember why the source transform was disabled,
;; but nobody disagreed with undoing the disabling.
(with-test (:name (compile :sb-cover-and-typep))
  (checked-compile
   '(lambda (x)
      (declare (fixnum x) (optimize sb-c:store-coverage-data))
      (if (typep x 'integer) x (length x)))))

(with-test (:name (compile :member :on-long-constant-list))
  ;; This used to blow stack with a sufficiently long list.
  (let ((cycle (list t)))
    (nconc cycle cycle)
    (assert (nth-value 1
                       (checked-compile `(lambda (x)
                                           (member x ',cycle))
                                        :allow-warnings t)))))

(with-test (:name (compile :bug-722734))
  (checked-compile-and-assert (:allow-warnings t :optimize '(:safety 3))
      '(lambda ()
         (eql (make-array 6)
              (list unbound-variable-1 unbound-variable-2)))
    (() (condition 'error))))

(with-test (:name (compile :bug-771673))
  (assert (equal `(the foo bar) (macroexpand `(truly-the foo bar))))
  ;; Make sure the compiler doesn't use THE, and check that setf-expansions
  ;; work.
  (let ((f (checked-compile `(lambda (x y)
                               (setf (truly-the fixnum (car x)) y)))))
    (let* ((cell (cons t t)))
      (funcall f cell :ok)
      (assert (equal '(:ok . t) cell)))))

(with-test (:name (compile :bug-793771 +))
  (let ((f (checked-compile `(lambda (x y)
                               (declare (type (single-float 2.0) x)
                                        (type (single-float (0.0)) y))
                               (+ x y)))))
    (assert (equal `(function ((single-float 2.0) (single-float (0.0)))
                              (values (single-float 2.0) &optional))
                   (sb-kernel:%simple-fun-type f)))))

(with-test (:name (compile :bug-793771 -))
  (let ((f (checked-compile `(lambda (x y)
                               (declare (type (single-float * 2.0) x)
                                        (type (single-float (0.0)) y))
                               (- x y)))))
    (assert (equal `(function ((single-float * 2.0) (single-float (0.0)))
                              (values (single-float * 2.0) &optional))
                   (sb-kernel:%simple-fun-type f)))))

(with-test (:name (:bug-793771 *))
  (let ((f (checked-compile
            `(lambda (x)
               (declare (type (single-float (0.0)) x))
               (* x 0.1)))))
    (assert (equal `(function ((single-float (0.0)))
                              (values (single-float 0.0) &optional))
                   (sb-kernel:%simple-fun-type f)))))

(with-test (:name (:bug-793771 /))
  (let ((f (checked-compile
            `(lambda (x)
               (declare (type (single-float (0.0)) x))
               (/ x 3.0)))))
    (assert (equal `(function ((single-float (0.0)))
                              (values (single-float 0.0) &optional))
                   (sb-kernel:%simple-fun-type f)))))

(with-test (:name (compile :bug-486812 single-float))
  (checked-compile `(lambda ()
                      (sb-kernel:make-single-float -1))))

(with-test (:name (compile :bug-486812 double-float))
  (checked-compile `(lambda ()
                      (sb-kernel:make-double-float -1 0))))

(with-test (:name (compile :bug-729765))
  (checked-compile `(lambda (a b)
                      (declare ((integer 1 1) a)
                               ((integer 0 1) b)
                               (optimize debug))
                      (lambda () (< b a)))))

;; Actually tests the assembly of RIP-relative operands to comparison
;; functions (one of the few x86 instructions that have extra bytes
;; *after* the mem operand's effective address, resulting in a wrong
;; offset).
(with-test (:name (compile :cmpps))
  (checked-compile-and-assert ()
      `(lambda (x)
         (= #C(2.0 3.0) (the (complex single-float) x)))
    ((#C(2.0 3.0)) t)
    ((#C(1.0 2.0)) nil)))

(with-test (:name (compile :cmppd))
  (checked-compile-and-assert ()
      `(lambda (x)
         (= #C(2d0 3d0) (the (complex double-float) x)))
    ((#C(2d0 3d0)) t)
    ((#C(1d0 2d0)) nil)))

(with-test (:name (compile :lvar-externally-checkable-type-nil))
  ;; Used to signal a BUG during compilation.
  (checked-compile-and-assert (:optimize :safe)
      `(lambda (a)
         (parse-integer "12321321" (the (member :start) a) 1))
    ((:start) (values 2321321 8))
    ((:end)   (condition 'type-error))))

(with-test (:name (compile simple-type-error :in-bound-propagation-a))
  (checked-compile `(lambda (i)
                      (declare (unsigned-byte i))
                      (expt 10 (expt 7 (- 2 i))))))

(with-test (:name (compile simple-type-error :in-bound-propagation-b))
  (assert (equal `(FUNCTION (UNSIGNED-BYTE)
                            (VALUES (SINGLE-FLOAT -1F0 1F0) &OPTIONAL))
                 (sb-kernel:%simple-fun-type
                  (checked-compile `(lambda (i)
                                      (declare (unsigned-byte i))
                                      (cos (expt 10 (+ 4096 i)))))))))

(with-test (:name (compile :fixed-%more-arg-values))
  (checked-compile-and-assert ()
      `(lambda (&rest rest)
         (apply #'cons rest))
    (('car 'cdr) '(car . cdr))))

(with-test (:name (compile :bug-826970))
  (checked-compile-and-assert ()
      `(lambda (a b c)
         (declare (type (member -2 1) b))
         (array-in-bounds-p a 4 b c))
    (((make-array '(5 2 2)) 1 1) t)))

(with-test (:name (compile :bug-826971))
  (let ((foo "foo"))
    (checked-compile-and-assert ()
        `(lambda (p1 p2)
           (schar (the (eql ,foo) p1) p2))
      ((foo 0) #\f))))

(with-test (:name (compile :bug-738464))
  (multiple-value-bind (fun failure-p warnings style-warnings)
      (checked-compile `(lambda ()
                          (flet ((foo () 42))
                            (declare (ftype non-function-type foo))
                            (foo)))
                       :allow-style-warnings t)
    (declare (ignore failure-p warnings))
    (assert style-warnings)
    (assert (eql 42 (funcall fun)))))

(with-test (:name (compile :bug-832005))
  (checked-compile-and-assert ()
      `(lambda (x)
         (declare (type (complex single-float) x))
         (+ #C(0.0 1.0) x))
    ((#C(1.0 2.0)) #C(1.0 3.0))))

;; A refactoring  1.0.12.18 caused lossy computation of primitive
;; types for member types.
(with-test (:name (compile member type :primitive-type))
  (checked-compile-and-assert ()
      `(lambda (p1 p2 p3)
         (if p1
             (the (member #c(1.2d0 1d0)) p2)
             (the (eql #c(1.0 1.0)) p3)))
    ((1 #c(1.2d0 1d0) #c(1.0 1.0)) #c(1.2d0 1.0d0))))

;; Fall-through jump elimination made control flow fall through to trampolines.
;; Reported by Eric Marsden on sbcl-devel@ 2011.10.26, with a test case
;; reproduced below (triggered a corruption warning and a memory fault).
(with-test (:name (compile :bug-883500))
  (checked-compile-and-assert ()
      `(lambda (a)
         (declare (type (integer -50 50) a))
         (mod (mod a (min -5 a)) 5))
    ((1) 1)))

;; Test for literals too large for the ISA (e.g. (SIGNED-BYTE 13) on SPARC).
#+sb-unicode
(with-test (:name (compile :bug-883519))
  (checked-compile `(lambda (x)
                      (declare (type character x))
                      (eql x #\U0010FFFF))))

;; Wide fixnum platforms had buggy address computation in atomic-incf/aref
(with-test (:name (compile :bug-887220))
  (checked-compile-and-assert ()
      `(lambda (vector index)
         (declare (type (simple-array sb-ext:word (4))
                        vector)
                  (type (mod 4) index))
         (sb-ext:atomic-incf (aref vector index) 1)
         vector)
    (((make-array 4 :element-type 'sb-ext:word
                  :initial-element 0)
      1)
     #(0 1 0 0) :test #'equalp)))

(with-test (:name (compile catch :interferes-with-debug-names))
  (let ((fun (funcall
              (checked-compile
               `(lambda ()
                  (catch 'out
                    (flet ((foo ()
                             (throw 'out (lambda () t))))
                      (foo))))))))
    (assert (equal '(lambda () :in foo) (sb-kernel:%fun-name fun)))))

(with-test (:name (compile :interval-div-signed-zero))
  (checked-compile-and-assert ()
      `(Lambda (a)
         (declare (type (member 0 -272413371076) a))
         (ffloor (the number a) -63243.127451934015d0))
    ((0) (values -0d0 0d0))))

(with-test (:name (compile :non-constant-keyword-typecheck))
  (checked-compile-and-assert ()
      `(lambda (p1 p3 p4)
         (declare (type keyword p3))
         (tree-equal p1 (cons 1 2) (the (member :test) p3) p4))
    (((cons 1.0 2.0) :test '=) t)))

(with-test (:name (compile truncate :wild-values))
  (checked-compile-and-assert ()
                              `(lambda (a)
                                 (declare (type (member 1d0 2d0) a))
                                 (block return-value-tag
                                   (funcall
                                    (the function
                                         (catch 'debug-catch-tag
                                           (return-from return-value-tag
                                             (progn (truncate a))))))))
                              ((2d0) (values 2 0d0))))

(with-test (:name (compile :boxed-fp-constant-for-full-call))
  (let ((fun (checked-compile
              `(lambda (x)
                 (declare (double-float x))
                 (unknown-fun 4.0d0 (+ 4.0d0 x)))
              :allow-style-warnings t)))
    (assert (equal '(4.0d0) (ctu:find-code-constants fun :type 'double-float)))))

(with-test (:name (compile :only-one-boxed-constant-for-multiple-uses))
  (let* ((big (1+ most-positive-fixnum))
         (fun (checked-compile
               `(lambda (x)
                  (unknown-fun ,big (+ ,big x)))
               :allow-style-warnings t)))
    (assert (= 1 (length (ctu:find-code-constants fun :type `(eql ,big)))))))

(with-test (:name (compile fixnum + float :coerces fixnum)
            :skipped-on :x86)
  (multiple-value-bind (calls fun)
      (ctu:ir1-named-calls
              `(lambda (x y)
                 (declare (fixnum x)
                          (single-float y))
                 (+ x y)))
    (assert (not calls))
    (assert (not (ctu:asm-search "GENERIC" fun)))))

(with-test (:name (compile :bug-803508))
  (checked-compile `(lambda ()
                      (print
                       (lambda (bar)
                         (declare (dynamic-extent bar))
                         (foo bar))))
                   :allow-style-warnings t))

(with-test (:name (compile :bug-803508-b))
  (checked-compile `(lambda ()
                      (list
                       (lambda (bar)
                         (declare (dynamic-extent bar))
                         (foo bar))))
                   :allow-style-warnings t))

(with-test (:name (compile :bug-803508-c))
  (checked-compile `(lambda ()
                      (list
                       (lambda (bar &optional quux)
                         (declare (dynamic-extent bar quux))
                         (foo bar quux))))
                   :allow-style-warnings t))

(with-test (:name (compile :cprop-with-constant-but-assigned-to-closure-variable))
  (checked-compile `(lambda (b c d)
                      (declare (type (integer -20545789 207590862) c))
                      (declare (type (integer -1 -1) d))
                      (let ((i (unwind-protect 32 (shiftf d -1))))
                        (or (if (= d c)  2 (= 3 b)) 4)))
                   :allow-style-warnings t))

(with-test (:name (compile :bug-913232))
  (checked-compile `(lambda (x)
                      (declare (optimize speed)
                               (type (or (and (or (integer -100 -50)
                                                  (integer 100 200))
                                              (satisfies foo))
                                         (and (or (integer 0 10) (integer 20 30)) a))
                                     x))
                      x)
                   :allow-style-warnings t)
  (checked-compile `(lambda (x)
                      (declare (optimize speed)
                               (type (and fixnum a) x))
                      x)
                   :allow-style-warnings t))

(with-test (:name (compile :bug-924276))
  (assert (nth-value
           3 (checked-compile `(lambda (a)
                                 (cons a (symbol-macrolet ((b 1))
                                           (declare (ignorable a))
                                           :c)))
                              :allow-style-warnings t))))

(with-test (:name (compile :bug-974406))
  (checked-compile-and-assert ()
      `(lambda (x)
         (declare (type (integer 53 86) x))
         (logand (+ x 1032791128) 11007078467))
    ((61) 268574721))
  (checked-compile-and-assert ()
      `(lambda (x)
         (declare (type (integer 53 86) x))
         (logand (+ x 1152921504606846975)
                 38046409652025950207))
    ((61) 60))

  (let (result)
    (do ((width 5 (1+ width)))
        ((= width 130))
      (dotimes (extra 4)
        (let ((fun (checked-compile
                    `(lambda (x)
                       (declare (optimize speed (safety 0)))
                       (declare (type (integer 1 16) x))
                       (logand
                        (+ x ,(1- (ash 1 width)))
                        ,(logior (ash 1 (+ width 1 extra))
                                 (1- (ash 1 width))))))))
          (unless (= (funcall fun 16) (logand 15 (1- (ash 1 width))))
            (push (cons width extra) result)))))
    (assert (null result))))

;; On x86-64 MOVE-IMMEDIATE of fixnum values into memory either directly
;; uses a MOV into memory or goes through a temporary register if the
;; value is larger than a certain number of bits. Check that it respects
;; the limits of immediate arguments to the MOV instruction (if not, the
;; assembler will fail an assertion) and doesn't have sign-extension
;; problems. (The test passes fixnum constants through the MOVE VOP
;; which calls MOVE-IMMEDIATE.)
(with-test (:name (compile :constant-fixnum-move))
  (let ((f (checked-compile `(lambda (g)
                               (funcall g
                                        ;; The first three args are
                                        ;; uninteresting as they are
                                        ;; passed in registers.
                                        1 2 3
                                        ,@(loop for i from 27 to 32
                                             collect (expt 2 i)))))))
    (assert (every #'plusp (funcall f #'list)))))

(with-test (:name (eval :malformed-ignore :lp-1000239)
                  :skipped-on :interpreter)
  (let ((*error-output* (make-broadcast-stream)))
    (assert-error
     (eval '(lambda () (declare (ignore (function . a)))))
     sb-int:simple-program-error)
    (assert-error
     (eval '(lambda () (declare (ignore (function a b)))))
     sb-int:simple-program-error)
    (assert-error
     (eval '(lambda () (declare (ignore (function)))))
     sb-int:simple-program-error)
    (assert-error
     (eval '(lambda () (declare (ignore (a)))))
     sb-int:simple-program-error)
    (assert-error
     (eval '(lambda () (declare (ignorable (a b)))))
     sb-int:simple-program-error)))

(with-test (:name (compile :malformed-type-declaraions))
  (assert (nth-value 1
                     (checked-compile
                      '(lambda (a) (declare (type (integer 1 2 . 3) a)) a)
                      :allow-warnings t))))

(with-test (:name :compiled-program-error-escaped-source)
  (assert
   (handler-case
       (funcall (checked-compile `(lambda () (lambda ("foo")))
                                 :allow-failure t))
     (sb-int:compiled-program-error (e)
       (let ((source (read-from-string (sb-kernel::program-error-source e))))
         (equal source '#'(lambda ("foo"))))))))

(defun cell-note-p (condition)
  (search "Allocating a value-cell at runtime for"
          (princ-to-string condition)))
(with-test (:name (compile :escape-analysis-for-nlxs)
            :serial t
            :skipped-on :interpreter)
  (flet ((test (check lambda &rest args)
           (multiple-value-bind (fun failure-p warnings style-warnings notes)
               (checked-compile lambda :allow-style-warnings t)
             (declare (ignore failure-p warnings style-warnings))
             (assert (eql check (when (some #'cell-note-p notes) t)))
             (if check
                 (assert
                  (eq :ok
                      (handler-case
                          (dolist (arg args nil)
                            (setf fun (funcall fun arg)))
                        (sb-int:simple-control-error (e)
                          (when (equal
                                 (simple-condition-format-control e)
                                 "attempt to RETURN-FROM a block or GO to a tag that no longer exists")
                            :ok)))))
                 (ctu:assert-no-consing (apply fun args))))))
    (test nil `(lambda (x)
                 (declare (optimize speed))
                 (block out
                   (flet ((ex () (return-from out 'out!)))
                     (typecase x
                       (cons (or (car x) (ex)))
                       (t (ex)))))) :foo)
    (test nil `(lambda (x)
                 (declare (optimize speed))
                 (block out
                   (flet ((ex () (return-from out 'out!)))
                     (typecase x
                       (cons (or (car x) (ex)))
                       (t (ex) 2))))) :foo)
    (test t   `(lambda (x)
                 (declare (optimize speed))
                 (funcall
                  (block nasty
                    (flet ((oops () (return-from nasty t)))
                      #'oops)))) t)
    (test t   `(lambda (r)
                 (declare (optimize speed))
                 (block out
                   (flet ((ex () (return-from out r)))
                     (lambda (x)
                       (typecase x
                         (cons (or (car x) (ex)))
                         (t (ex) 3)))))) t t)
    (test t   `(lambda (x)
                 (declare (optimize speed))
                 (flet ((eh (x)
                          (flet ((meh () (return-from eh 'meh)))
                            (lambda ()
                              (typecase x
                                (cons (or (car x) (meh)))
                                (t (meh) 3))))))
                   (funcall (eh x)))) t t)))

(with-test (:name (compile :bug-1050768 :symptom))
  ;; Used to signal an error.
  (checked-compile
   `(lambda (string position)
      (char string position)
      (array-in-bounds-p string (1+ position)))))

(with-test (:name (:bug-1050768 :cause))
  (let ((types `((string string)
                 ((or (simple-array character 9) (vector t 9))
                  (or (simple-array character 9) (vector t))))))
    (dolist (pair types)
      (destructuring-bind (orig conservative) pair
        (assert (sb-kernel:type= (sb-kernel:specifier-type conservative)
                                 (sb-c::conservative-type
                                  (sb-kernel:specifier-type orig))))))))

(with-test (:name (compile :smodular64 :wrong-width))
  (checked-compile-and-assert ()
      '(lambda (x)
         (declare (type (signed-byte 64) x))
         (sb-c::mask-signed-field 64 (- x 7033717698976965573)))
    ((10038) -7033717698976955535)))

(with-test (:name (compile :smodular32 :wrong-width))
  (checked-compile-and-assert ()
      '(lambda (x)
         (declare (type (signed-byte 31) x))
         (sb-c::mask-signed-field 31 (- x 1055131947)))
    ((10038) -1055121909)))

(with-test (:name (compile :first-open-coded))
  (assert (not (ctu:ir1-named-calls `(lambda (x) (first x))))))

(with-test (:name (compile :second-open-coded))
  (assert (not (ctu:ir1-named-calls `(lambda (x) (second x))))))

(with-test (:name (compile svref :symbol-macro))
  (checked-compile `(lambda (x)
                      (symbol-macrolet ((sv x))
                        (values (svref sv 0) (setf (svref sv 0) 99))))))

;; The compiler used to update the receiving LVAR's type too
;; aggressively when converting a large constant to a smaller
;; (potentially signed) one, causing other branches to be
;; inferred as dead.
(with-test (:name (compile :modular-cut-constant-to-width))
  (checked-compile-and-assert ()
      `(lambda (x)
         (logand 254
                 (case x
                   ((3) x)
                   ((2 2 0 -2 -1 2) 9223372036854775803)
                   (t 358458651))))
      ((-10470605025) 26)))

(with-test (:name (compile append :derive-type))
  (let ((test-cases
         '((lambda () (append 10)) (integer 10 10)
           (lambda () (append nil 10)) (integer 10 10)
           (lambda (x) (append x 10)) (or (integer 10 10) cons)
           (lambda (x) (append x (cons 1 2))) cons
           (lambda (x y) (append x (cons 1 2) y)) cons
           (lambda (x y) (nconc x (the list y) x)) t
           (lambda (x y) (nconc (the atom x) y)) t
           (lambda (x y) (nconc (the (or null (eql 10)) x) y)) t
           (lambda (x y) (nconc (the (or cons vector) x) y)) cons
           (lambda (x y) (nconc (the sequence x) y)) t
           (lambda (x y) (print (length y)) (append x y)) sequence
           (lambda (x y) (print (length y)) (append x y)) sequence
           (lambda (x y) (append (the (member (a) (b)) x) y)) cons
           (lambda (x y) (append (the (member (a) (b) c) x) y)) cons
           (lambda (x y) (append (the (member (a) (b) nil) x) y)) t)))
    (loop for (function result-type) on test-cases by #'cddr
       do (assert (sb-kernel:type= (sb-kernel:specifier-type
                                    (car (cdaddr (sb-kernel:%simple-fun-type
                                                  (checked-compile function)))))
                                   (sb-kernel:specifier-type result-type))))))

(with-test (:name (compile :bug-504121))
  (checked-compile `(lambda (s)
                      (let ((p1 #'upper-case-p))
                        (funcall
                         (lambda (g)
                           (funcall p1 g))))
                      (let ((p2 #'(lambda (char) (upper-case-p char))))
                        (funcall p2 s)))
                   :allow-warnings 'sb-int:local-argument-mismatch))

(with-test (:name (compile :bug-504121 :optional-missing))
  (checked-compile `(lambda (s)
                      (let ((p1 #'upper-case-p))
                        (funcall
                         (lambda (g &optional x)
                           (funcall p1 g))))
                      (let ((p2 #'(lambda (char) (upper-case-p char))))
                        (funcall p2 s)))
                   :allow-warnings 'sb-int:local-argument-mismatch))

(with-test (:name (compile :bug-504121 :optional-superfluous))
  (checked-compile `(lambda (s)
                      (let ((p1 #'upper-case-p))
                        (funcall
                         (lambda (g &optional x)
                           (funcall p1 g))
                         #\1 2 3))
                      (let ((p2 #'(lambda (char) (upper-case-p char))))
                        (funcall p2 s)))
                   :allow-warnings 'sb-int:local-argument-mismatch))

(with-test (:name (compile :bug-504121 :key-odd))
  (checked-compile `(lambda (s)
                      (let ((p1 #'upper-case-p))
                        (funcall
                         (lambda (g &key x)
                           (funcall p1 g))
                         #\1 :x))
                      (let ((p2 #'(lambda (char) (upper-case-p char))))
                        (funcall p2 s)))
                   :allow-warnings t))

(with-test (:name (compile :bug-504121 :key-unknown))
  (checked-compile `(lambda (s)
                      (let ((p1 #'upper-case-p))
                        (funcall
                         (lambda (g &key x)
                           (funcall p1 g))
                         #\1 :y 2))
                      (let ((p2 #'(lambda (char) (upper-case-p char))))
                        (funcall p2 s)))
                   :allow-warnings t))

(with-test (:name (compile :bug-1181684))
  (checked-compile `(lambda ()
                      (let ((hash #xD13CCD13))
                        (setf hash (logand most-positive-word
                                           (ash hash 5)))))))

(with-test (:name (compile :local-&optional-recursive-inline :bug-1180992))
  (checked-compile `(lambda ()
                      (labels ((called (&optional a))
                               (recursed (&optional b)
                                 (called)
                                 (recursed)))
                        (declare (inline recursed called))
                        (recursed)))
                   :allow-style-warnings t))

(with-test (:name (compile :constant-fold-logtest))
  (assert (equal (sb-kernel:%simple-fun-type
                  (checked-compile `(lambda (x)
                                      (declare (type (mod 1024) x)
                                               (optimize speed))
                                      (logtest x 2048))))
                 '(function ((unsigned-byte 10)) (values null &optional)))))

;; type mismatches on LVARs with multiple potential sources used to
;; be reported as mismatches with the value NIL.  Make sure we get
;; a warning, but that it doesn't complain about a constant NIL ...
;; of type FIXNUM.
(with-test (:name (compile :multiple-use-lvar-interpreted-as-NIL :cast))
  (let ((warnings (nth-value
                   2 (checked-compile `(lambda (x y z)
                                         (declare (type fixnum y z))
                                         (aref (if x y z) 0))
                                      :allow-warnings 'sb-int:type-warning))))
    (assert warnings)
    (assert (notany (lambda (condition)
                      (search "Constant "
                              (sb-format::unparse-fmt-control
                               (simple-condition-format-control
                                condition))))
                    warnings))))

(with-test (:name (compile :multiple-use-lvar-interpreted-as-NIL catch))
  (let ((warnings (nth-value
                   3 (checked-compile `(lambda (x y z f)
                                         (declare (type fixnum y z))
                                         (catch (if x y z) (funcall f)))
                                      :allow-style-warnings t))))
    (assert warnings)
    (assert (notany (lambda (condition)
                      (position nil
                                (simple-condition-format-arguments
                                 condition)))
                    warnings))))

;; Smoke test for rightward shifts
(with-test (:name (compile :ash/right-signed))
  (let* ((f (checked-compile `(lambda (x y)
                                (declare (type (mod ,(* 2 sb-vm:n-word-bits)) y)
                                         (type sb-vm:signed-word x)
                                         (optimize speed))
                                (ash x (- y)))))
         (max (ash most-positive-word -1))
         (min (- -1 max)))
    (flet ((test (x y)
             (assert (= (ash x (- y))
                        (funcall f x y)))))
      (dotimes (x 32)
        (dotimes (y (* 2 sb-vm:n-word-bits))
          (test x y)
          (test (- x) y)
          (test (- max x) y)
          (test (+ min x) y))))))

(with-test (:name (compile :ash/right-unsigned))
  (let ((f (checked-compile `(lambda (x y)
                               (declare (type (mod ,(* 2 sb-vm:n-word-bits)) y)
                                        (type word x)
                                        (optimize speed))
                               (ash x (- y)))))
        (max most-positive-word))
    (flet ((test (x y)
             (assert (= (ash x (- y))
                        (funcall f x y)))))
      (dotimes (x 32)
        (dotimes (y (* 2 sb-vm:n-word-bits))
          (test x y)
          (test (- max x) y))))))

(with-test (:name (compile :ash/right-fixnum))
  (let ((f (checked-compile `(lambda (x y)
                               (declare (type (mod ,(* 2 sb-vm:n-word-bits)) y)
                                        (type fixnum x)
                                        (optimize speed))
                               (ash x (- y))))))
    (flet ((test (x y)
             (assert (= (ash x (- y))
                        (funcall f x y)))))
      (dotimes (x 32)
        (dotimes (y (* 2 sb-vm:n-word-bits))
          (test x y)
          (test (- x) y)
          (test (- most-positive-fixnum x) y)
          (test (+ most-negative-fixnum x) y))))))

(with-test (:name (compile :fold-index-addressing-positive-offset))
  (checked-compile-and-assert ()
      `(lambda (i)
         (if (typep i '(integer -31 31))
             (aref #.(make-array 63 :initial-element 0) (+ i 31))
             (error "foo")))
    ((-31) 0)))

;; 5d3a728 broke something like this in CL-PPCRE
(with-test (:name (compile :fold-index-addressing-potentially-negative-index))
  (checked-compile `(lambda (index vector)
                      (declare (optimize speed (safety 0))
                               ((simple-array character (*)) vector)
                               ((unsigned-byte 24) index))
                      (aref vector (1+ (mod index (1- (length vector))))))))

(with-test (:name (compile :constant-fold-ash/right-fixnum))
  (checked-compile `(lambda (a b)
                      (declare (type fixnum a)
                               (type (integer * -84) b))
                      (ash a b))))

(with-test (:name (compile :constant-fold-ash/right-word))
  (checked-compile `(lambda (a b)
                      (declare (type word a)
                               (type (integer * -84) b))
                      (ash a b))))

(with-test (:name (compile :nconc-derive-type))
  (assert-type (lambda (x y)
                 (declare (type (or cons fixnum) x))
                 (nconc x y))
               (function ((or cons fixnum) t) (values cons &optional))))

;; make sure that all data-vector-ref-with-offset VOPs are either
;; specialised on a 0 offset or accept signed indices
(with-test (:name :data-vector-ref-with-offset-signed-index)
  (let ((dvr (find-symbol "DATA-VECTOR-REF-WITH-OFFSET" "SB-KERNEL")))
    (when dvr
      (assert
       (null
        (loop for info in (sb-c::fun-info-templates
                           (sb-c::fun-info-or-lose dvr))
              for (nil second-arg third-arg) = (sb-c::vop-info-arg-types info)
              unless (or (typep second-arg '(cons (eql :constant)))
                         (equal third-arg '(:constant . (integer 0 0)))
                         (equal second-arg
                                `(:or ,(sb-c::primitive-type-or-lose
                                        'sb-vm::positive-fixnum)
                                      ,(sb-c::primitive-type-or-lose
                                        'fixnum))))
                collect info))))))

(with-test (:name :data-vector-set-with-offset-signed-index)
  (let ((dvr (find-symbol "DATA-VECTOR-SET-WITH-OFFSET" "SB-KERNEL")))
    (when dvr
      ;; I have no idea what this test is trying to say, or what user-visible
      ;; behavior would change if this assertion didn't hold.
      ;; But it seems unhappy with my changes to SIMPLE-BIT-VECTOR.
      ;; But we never fold indexes for bit-vector. So ignore it.
      (assert
       (null
        (loop for info in (sb-c::fun-info-templates
                           (sb-c::fun-info-or-lose dvr))
              for (nil second-arg third-arg) = (sb-c::vop-info-arg-types info)
              unless (or (string= (sb-c::vop-info-name info)
                                  ;; don't violate package lock if symbol isn't there
                                  "DATA-VECTOR-SET-WITH-OFFSET/SIMPLE-BIT-VECTOR")
                         (typep second-arg '(cons (eql :constant)))
                         (equal third-arg '(:constant . (integer 0 0)))
                         (equal second-arg
                                `(:or ,(sb-c::primitive-type-or-lose
                                        'sb-vm::positive-fixnum)
                                      ,(sb-c::primitive-type-or-lose
                                        'fixnum))))
                collect info))))))

(with-test (:name (compile :maybe-inline-ref-to-dead-lambda))
  (checked-compile `(lambda (string)
                      (declare (optimize speed (space 0)))
                      (cond ((every #'digit-char-p string)
                             nil)
                            ((some (lambda (c)
                                     (digit-char-p c))
                                   string))))))

;; the x87 backend used to sometimes signal FP errors during boxing,
;; because converting between double and single float values was a
;; noop (fixed), and no doubt many remaining issues.  We now store
;; the value outside pseudo-atomic, so any SIGFPE should be handled
;; correctly.
;;
;; When it fails, this test lands into ldb.
(with-test (:name :no-overflow-during-allocation)
  (handler-case (eval '(cosh 90))
    (floating-point-overflow ()
      t)))

;; unbounded integer types could break integer arithmetic.
(with-test (:name (compile :bug-1199127))
  (checked-compile
   `(lambda (b)
      (declare (type (integer -1225923945345 -832450738898) b))
      (declare (optimize (speed 3) (space 3) (safety 2)
                         (debug 0) (compilation-speed 1)))
      (loop for lv1 below 3
         sum (logorc2
              (if (>= 0 lv1)
                  (ash b (min 25 lv1))
                  0)
              -2)))))

;; non-trivial modular arithmetic operations would evaluate to wider results
;; than expected, and never be cut to the right final bitwidth.
(with-test (:name (compile :bug-1199428-1))
  (checked-compile-and-assert (:optimize :quick/incomplete)
      `(lambda (a c)
         (declare (type (integer -2 1217810089) a))
         (declare (type (integer -6895591104928 -561736648588) c))
         (declare (optimize (space 0) (compilation-speed 3)))
         (logandc1 (gcd c)
                   (+ (- a c)
                      (loop for lv2 below 1 count t))))
    ((530436387 -4890629672277) 1338004008))
  (checked-compile-and-assert (:optimize :quick/incomplete)
      `(lambda (a c)
         (declare (notinline - + gcd logandc1))
         (declare (optimize (space 1) (compilation-speed 3)))
         (logandc1 (gcd c)
                   (+ (- a c)
                      (loop for lv2 below 1 count t))))
    ((530436387 -4890629672277) 1338004008)))

(with-test (:name (compile :bug-1199428-2))
  (checked-compile-and-assert (:optimize :quick/incomplete)
      `(lambda (a b)
         (declare (type (integer -1869232508 -6939151) a))
         (declare (type (integer -11466348357 -2645644006) b))
         (declare (optimize (space 0) (compilation-speed 2)))
         (logand (lognand a -6) (* b -502823994)))
    ((-1491588365 -3745511761) 1084329992))
  (checked-compile-and-assert ()
      `(lambda (a b)
         (logand (lognand a -6) (* b -502823994)))
    ((-1491588365 -3745511761) 1084329992)))

(with-test (:name (compile restart-case optimize speed compiler-note))
  (checked-compile '(lambda ()
                     (declare (optimize speed))
                     (restart-case () (c ()))))
  (checked-compile '(lambda ()
                     (declare (optimize speed))
                     (let (x)
                       (restart-case (setf x (car (compute-restarts)))
                         (c ()))
                       x))))

(with-test (:name (compile :copy-more-arg)
                  :fails-on (or :mips :sparc))
  ;; copy-more-arg might not copy in the right direction
  ;; when there are more fixed args than stack frame slots,
  ;; and thus end up splatting a single argument everywhere.
  ;; Failing platforms still start their stack frames at 8 slots, so
  ;; this is less likely to happen.
  (let ((limit 33))
    (labels ((iota (n)
               (loop for i below n collect i))
             (test-function (function skip)
               ;; function should just be (subseq x skip)
               (loop for i from skip below (+ skip limit) do
                 (let* ((values (iota i))
                        (f (apply function values))
                        (subseq (subseq values skip)))
                   (assert (equal f subseq)))))
             (make-function (n)
               (let ((gensyms (loop for i below n collect (gensym))))
                 (checked-compile `(lambda (,@gensyms &rest rest)
                                     (declare (ignore ,@gensyms))
                                     rest)))))
      (dotimes (i limit)
        (test-function (make-function i) i)))))

(with-test (:name (compile apply aref bit sbit))
  (checked-compile `(lambda (x y) (setf (apply #'aref x y) 21)))
  (checked-compile `(lambda (x y) (setf (apply #'bit x y) 1)))
  (checked-compile `(lambda (x y) (setf (apply #'sbit x y) 0))))

(with-test (:name (compile :warn-on-the-values-constant))
  (multiple-value-bind (fun failure-p)
      (checked-compile
       ;; The compiler used to elide this test without
       ;; noting that the type demands multiple values.
       '(lambda () (the (values fixnum fixnum) 1))
       :allow-failure t :allow-warnings t)
    (assert (functionp fun))
    (assert failure-p)))

;; quantifiers shouldn't cons themselves.
(with-test (:name :quantifiers-no-consing
            :serial t
            :skipped-on :interpreter)
  (let ((constantly-t (lambda (x) x t))
        (constantly-nil (lambda (x) x nil))
        (list (make-list 1000 :initial-element nil))
        (vector (make-array 1000 :initial-element nil)))
    (macrolet ((test (quantifier)
                 (let ((function (make-symbol (format nil "TEST-~A" quantifier))))
                   `(flet ((,function (function sequence)
                             (,quantifier function sequence)))
                      (ctu:assert-no-consing (,function constantly-t list))
                      (ctu:assert-no-consing (,function constantly-nil vector))))))
      (test some)
      (test every)
      (test notany)
      (test notevery))))

(with-test (:name :propagate-complex-type-tests)
  (flet ((test (type value)
           (let ((ftype (sb-kernel:%simple-fun-type
                         (checked-compile `(lambda (x)
                                             (if (typep x ',type)
                                                 x
                                                 ',value))))))
             (assert (typep ftype `(cons (eql function))))
             (assert (= 3 (length ftype)))
             (let* ((return (third ftype))
                    (rtype (second return)))
               (assert (typep return `(cons (eql values)
                                            (cons t
                                                  (cons (eql &optional)
                                                        null)))))
               (assert (and (subtypep rtype type)
                            (subtypep type rtype)))))))
    (mapc (lambda (params)
            (apply #'test params))
          `(((unsigned-byte 17) 0)
            ((member 1 3 5 7) 5)
            ((or symbol (eql 42)) t)))))

(with-test (:name :constant-fold-complex-type-tests)
  (assert (equal (sb-kernel:%simple-fun-type
                  (checked-compile `(lambda (x)
                                      (if (typep x '(member 1 3))
                                          (typep x '(member 1 3 15))
                                          t))))
                 `(function (t) (values (member t) &optional))))
  (assert (type-specifiers-equal
                 (sb-kernel:%simple-fun-type
                  (checked-compile `(lambda (x)
                                      (declare (type (member 1 3) x))
                                      (typep x '(member 1 3 15)))))
                 `(function ((or (integer 1 1) (integer 3 3)))
                            (values (member t) &optional)))))

(with-test (:name :quietly-row-major-index-no-dimensions)
  (checked-compile `(lambda (x) (array-row-major-index x))))

(with-test (:name :array-rank-transform)
  (checked-compile `(lambda (a) (array-rank (the an-imaginary-type a)))
                   :allow-style-warnings t))

(with-test (:name (:array-rank-fold :bug-1252108))
  (let ((notes (nth-value
                4 (checked-compile
                   `(lambda (a)
                      (typecase a
                        ((array t 2)
                         (when (= (array-rank a) 3)
                           (array-dimension a 2)))))))))
    (assert (= 1 (length notes)))))

(with-test (:name (upgraded-array-element-type :undefined-type))
  (checked-compile-and-assert (:allow-style-warnings t :optimize nil)
      '(lambda () (upgraded-array-element-type 'an-undefined-type))
    (() (condition '(or sb-kernel:parse-unknown-type error)))))

(with-test (:name :xchg-misencoding)
  (checked-compile-and-assert ()
      `(lambda (a b)
         (declare (type single-float a))
         (unless (eql b 1/2)
           (min a -1f0)))
    ((0f0 1) -1f0)))

(with-test (:name :malformed-declare)
  (assert (nth-value
           1 (checked-compile `(lambda (x)
                                 (declare (unsigned-byte (x)))
                                 x)
                              :allow-failure t))))

(with-test (:name :no-dubious-asterisk-warning)
  (checked-compile
   `(lambda (foo)
      (macrolet ((frob-some-stuff (&rest exprs)
                   (let ((temps
                          (mapcar
                           (lambda (x)
                             (if (symbolp x) (copy-symbol x) (gensym)))
                           exprs)))
                     `(let ,(mapcar #'list temps exprs)
                        (if (and ,@temps)
                            (format t "Got~@{ ~S~^ and~}~%" ,@temps))))))
        (frob-some-stuff *print-base* (car foo))))))

(with-test (:name :interr-type-specifier-hashing)
  (let ((specifiers
         (remove nil
          (map 'list
               (lambda (saetp &aux (et (sb-vm:saetp-specifier saetp)))
                 (unless (member et '(nil t))
                   (sb-c::type-specifier
                    (sb-c::specifier-type `(simple-array ,et (*))))))
               sb-vm:*specialized-array-element-type-properties*))))
    (assert (sb-c::%interr-symbol-for-type-spec `(or ,@specifiers)))
    (assert (sb-c::%interr-symbol-for-type-spec
             `(or ,@specifiers system-area-pointer)))))

(with-test (:name :simple-rank-1-array-*-p-works)
  (checked-compile-and-assert ()
      `(lambda () (typep #() '(simple-array * (*))))
    (() t))
  (loop for saetp across sb-vm:*specialized-array-element-type-properties*
     do
     (dotimes (n-dimensions 3) ; test ranks 0, 1, and 2.
       (let ((dims (make-list n-dimensions :initial-element 2)))
         (dolist (adjustable-p '(nil t))
           (let ((a (make-array dims :element-type (sb-vm:saetp-specifier saetp)
                                     :adjustable adjustable-p)))
             (assert (eq (and (= n-dimensions 1) (not adjustable-p))
                         (typep a '(simple-array * (*)))))))))))

(with-test (:name :typep-empty-vector-or-string-simple-vector)
  (checked-compile-and-assert ()
      `(lambda ()
         (typep #() '(or simple-vector simple-string)))
    (() t)))
(with-test (:name :array-subtype-tests
            :skipped-on (:not (:or :x86 :x86-64)))
  (flet ((approx-lines-of-assembly-code (type-expr)
           (count #\Newline
                  (let ((sb-ext:*disassemble-annotate* nil))
                    (with-output-to-string (s)
                      (disassemble
                       `(lambda (x)
                          (declare (optimize (sb-c:verify-arg-count 0)))
                          (typep x ',type-expr))
                       :stream s))))))
    ;; These are fragile, but less bad than the possibility of messing up
    ;; any vops, especially since the generic code in 'vm-type' checks for
    ;; a vop by its name in a place that would otherwise be agnostic of the
    ;; backend were it not for my inability to test all platforms.
    (assert (< (approx-lines-of-assembly-code
                '(simple-array * (*))) (+ 25 #+sb-safepoint 2)))
    ;; this tested all possible widetags one at a time, e.g. in VECTOR-SAP
    (assert (< (approx-lines-of-assembly-code
                '(sb-kernel:simple-unboxed-array (*))) (+ 25 #+sb-safepoint 2)))
    ;; This is actually a strange type but it's what ANSI-STREAM-READ-N-BYTES
    ;; declares as its buffer, which would choke in %BYTE-BLT if you gave it
    ;; (simple-array t (*)). But that's a different problem.
    (assert (< (approx-lines-of-assembly-code
                '(or system-area-pointer (simple-array * (*)))) (+ 29 #+sb-safepoint 2)))
    ;; And this was used by %BYTE-BLT which tested widetags one-at-a-time.
    (assert (< (approx-lines-of-assembly-code
                '(or system-area-pointer (sb-kernel:simple-unboxed-array (*))))
               (+ 29 #+sb-safepoint 2)))))

(with-test (:name :local-argument-mismatch-error-string)
  (multiple-value-bind (fun failurep warnings)
      (checked-compile `(lambda (x)
                          (flet ((foo ()))
                            (foo x)))
                       :allow-warnings t)
    (declare (ignore failurep))
    (assert (= 1 (length warnings)))
    (multiple-value-bind (ok err) (ignore-errors (funcall fun 42))
      (assert (not ok))
      (assert (search "FLET FOO" (princ-to-string err))))))

(with-test (:name :bug-1310574-0)
  (checked-compile `(lambda (a)
                      (typecase a
                        ((or (array * (* * 3)) (array * (* * 4)))
                         (case (array-rank a)
                           (2 (aref a 1 2))))))))

(with-test (:name :bug-1310574-1)
  (checked-compile `(lambda (a)
                      (typecase a
                        ((or (array * ()) (array * (1)) (array * (1 2)))
                         (case (array-rank a)
                           (3 (aref a 1 2 3))))))))

(with-test (:name :bug-573747)
  (assert (nth-value
           1 (checked-compile `(lambda (x) (progn (declare (integer x)) (* x 6)))
                              :allow-failure t))))

;; Something in this function used to confuse lifetime analysis into
;; recording multiple conflicts for a single TNs in the dolist block.
(with-test (:name :bug-1327008)
  (checked-compile
   `(lambda (scheduler-spec
             schedule-generation-method
             utc-earliest-time utc-latest-time
             utc-other-earliest-time utc-other-latest-time
             &rest keys
             &key queue
               maximum-mileage
               maximum-extra-legs
               maximum-connection-time
               slice-number
               scheduler-hints
               permitted-route-locations prohibited-route-locations
               preferred-connection-locations disfavored-connection-locations
               origins destinations
               permitted-carriers prohibited-carriers
               permitted-operating-carriers prohibited-operating-carriers
               start-airports end-airports
               circuity-limit
               specified-circuity-limit-extra-miles
               (preferred-carriers :unspecified)
               &allow-other-keys)
      (declare (optimize speed))
      (let  ((table1 (list nil))
             (table2 (list nil))
             (skip-flifo-checks (getf scheduler-spec :skip-flifo-checks))
             (construct-gaps-p (getf scheduler-spec :construct-gaps-p))
             (gap-locations (getf scheduler-spec :gap-locations))
             (result-array (make-array 100))
             (number-dequeued 0)
             (n-new 0)
             (n-calcs 0)
             (exit-reason 0)
             (prev-start-airports origins)
             (prev-end-airports destinations)
             (prev-permitted-carriers permitted-carriers))
        (flet ((run-with-hint (hint random-magic other-randomness
                                    maximum-extra-legs
                                    preferred-origins
                                    preferred-destinations
                                    same-pass-p)
                 (let* ((hint-permitted-carriers (first hint))
                        (preferred-end-airports
                         (ecase schedule-generation-method
                           (:DEPARTURE preferred-destinations)
                           (:ARRIVAL preferred-origins)))
                        (revised-permitted-carriers
                         (cond ((and hint-permitted-carriers
                                     (not (eq permitted-carriers :ANY)))
                                (intersection permitted-carriers
                                              hint-permitted-carriers))
                               (hint-permitted-carriers)
                               (permitted-carriers)))
                        (revised-maximum-mileage
                         (min (let ((maximum-mileage 0))
                                (dolist (o start-airports)
                                  (dolist (d end-airports)
                                    (setf maximum-mileage
                                          (max maximum-mileage (mileage o d)))))
                                (round (+ (* circuity-limit maximum-mileage)
                                          (or specified-circuity-limit-extra-miles
                                              (hairy-calculation slice-number)))))
                              maximum-mileage)))
                   (when (or (not (equal start-airports prev-start-airports))
                             (not (equal end-airports prev-end-airports))
                             (and (not (equal revised-permitted-carriers
                                              prev-permitted-carriers))))
                     (incf n-calcs)
                     (calculate-vectors
                      prohibited-carriers
                      permitted-operating-carriers
                      prohibited-operating-carriers
                      permitted-route-locations
                      prohibited-route-locations
                      construct-gaps-p
                      gap-locations
                      preferred-carriers)
                     (setf prev-permitted-carriers revised-permitted-carriers))
                   (multiple-value-bind (this-number-dequeued
                                         this-exit-reason
                                         this-n-new)
                       (apply #'schedule-loop
                              utc-earliest-time  utc-other-earliest-time
                              utc-latest-time    utc-other-latest-time
                              scheduler-spec     schedule-generation-method
                              queue
                              :maximum-mileage revised-maximum-mileage
                              :maximum-extra-legs maximum-extra-legs
                              :maximum-connection-time maximum-connection-time
                              :same-pass-p same-pass-p
                              :preferred-end-airports preferred-end-airports
                              :maximum-blah random-magic
                              :skip-flifo-checks skip-flifo-checks
                              :magic1 table1
                              :magic2 table2
                              :preferred-connection-locations preferred-connection-locations
                              :disfavored-connection-locations disfavored-connection-locations
                              keys)
                     (when other-randomness
                       (loop for i fixnum from n-new to (+ n-new (1- this-n-new))
                          do (hairy-calculation i result-array)))
                     (incf number-dequeued this-number-dequeued)
                     (incf n-new this-n-new)
                     (setq exit-reason (logior exit-reason this-exit-reason))))))
          (let ((n-hints-processed 0))
            (dolist (hint scheduler-hints)
              (run-with-hint hint n-hints-processed t 0
                             nil nil nil)
              (incf n-hints-processed)))
          (run-with-hint nil 42 nil maximum-extra-legs
                         '(yyy) '(xxx) t))
        exit-reason))
   :allow-style-warnings t))

(with-test (:name :dead-code-in-optional-dispatch)
  ;; the translation of each optional entry is
  ;;   (let ((#:g (error "nope"))) (funcall #<clambda> ...))
  ;; but the funcall is unreachable. Since this is an artifact of how the
  ;; lambda is converted, it should not generate a note as if in user code.
  (checked-compile
   `(lambda (a &optional (b (error "nope")) (c (error "nope")))
      (values c b a))))

(with-test (:name :nth-value-of-non-constant-N :skipped-on :interpreter
            :serial t)
  (labels ((foo (n f) (nth-value n (funcall f)))
           (bar () (values 0 1 2 3 4 5 6 7 8 9)))
    (assert (= (foo 5 #'bar) 5)) ; basic correctness
    (assert (eq (foo 12 #'bar) nil))
    (ctu:assert-no-consing (eql (foo 953 #'bar) 953))))

(with-test (:name :position-derive-type-optimizer)
  (assert-code-deletion-note
   '(lambda (x) ; the call to POSITION can't return 4
     (let ((i (position x #(a b c d) :test 'eq)))
       (case i (4 'nope) (t 'okeydokey))))))

;; lp# 310267
(with-test (:name (optimize :quality-multiply-specified :bug-310267))
  (let ((sb-c::*policy* sb-c::*policy*)) ; to keep this test pure
    (assert-signal (proclaim '(optimize space debug (space 0)))
                   style-warning)
    (flet ((test (form)
             (assert (= 1 (length (nth-value
                                   3 (checked-compile
                                      form :allow-style-warnings t)))))))
      (test `(lambda () (declare (optimize speed (speed 0))) 5))
      (test `(lambda () (declare (optimize speed) (optimize (speed 0))) 5))
      (test `(lambda ()
               (declare (optimize speed)) (declare (optimize (speed 0)))
               5)))

    ;; these are OK
    (assert-no-signal (proclaim '(optimize (space 3) space)))
    (checked-compile `(lambda () (declare (optimize speed (speed 3))) 5))
    (checked-compile `(lambda () (declare (optimize speed) (optimize (speed 3))) 5))
    (checked-compile `(lambda ()
                        (declare (optimize speed)) (declare (optimize (speed 3)))
                        5))))

(with-test (:name (truncate :type-derivation))
  (checked-compile-and-assert ()
      `(lambda (a b)
         (truncate a (the (rational (1) (3)) b)))
    ((10 5/2) (values 4 0))))

(with-test (:name :constantp-on-a-literal-function-works)
  (assert (constantp `(the (function (list) t) ,#'car))))

(with-test (:name :arg-count-error)
  (assert (eq :win (handler-case (funcall (intern "CONS") 1 2 3)
                     (sb-int:simple-program-error () :win)
                     (condition () :lose)))))

(with-test (:name :mv-conversion)
  (checked-compile `(lambda (a)
                      (tagbody (go 0)
                         (list (unwind-protect a))
                         (multiple-value-call #'list
                           (values (catch 'ct5 (go 0))))
                       0))))

(with-test (:name (:null-cleanups-1 :bug-1416704 :bug-404441))
  (let ((x (funcall
            (checked-compile
             `(lambda ()
                (lambda (x)
                  (declare (optimize speed))
                  (if x
                      (funcall (flet ((bar () 10)) #'bar))
                      (funcall (flet ((fez ()
                                        (funcall (flet ((foo () 20)) #'foo))))
                                 #'fez)))))))))
    (assert (= (funcall x t) 10))
    (assert (= (funcall x nil) 20))))

(with-test (:name (:null-cleanups-2 :bug-1416704 :bug-404441))
  (let ((fun (funcall
              (checked-compile
               `(lambda ()
                  (lambda (x)
                    (declare (optimize speed))
                    (let* ((a2 (lambda () 20))
                           (a4 (lambda ()))
                           (a0 (flet ((f () (funcall a2)))
                                 #'f))
                           (a3 (lambda ()
                                 (if x
                                     (if x
                                         (throw 'x 10)
                                         (let ((a5 (lambda () (funcall a4))))
                                           (funcall a5)))
                                     (funcall a0)))))
                      (funcall a3))))))))
    (assert (= (catch 'x (funcall fun t)) 10))
    (assert (= (catch 'x (funcall fun nil)) 20))))


(with-test (:name :locall-already-let-converted)
  (assert (eq (funcall
               (funcall
                (checked-compile
                 `(lambda ()
                    (flet ((call (ff)
                             (flet ((f () (return-from f ff)))
                               (declare (inline f))
                               (f)
                               (f))))
                      (declare (inline call))
                      (call 1)
                      (call (lambda () 'result)))))))
              'result)))

(with-test (:name :debug-dump-elsewhere)
  (assert (eql (catch 'x
                 (funcall
                  (checked-compile
                   `(lambda ()
                      (declare (optimize debug))
                      (throw 'x *)))))
               *)))

(with-test (:name (typep :quasiquoted-constant))
  (assert (not (ctu:ir1-named-calls
                  `(lambda (x)
                     (typep x `(signed-byte ,sb-vm:n-word-bits)))))))

(with-test (:name (logior :transform))
  (multiple-value-bind (fun failurep warnings)
      (checked-compile `(lambda (c)
                          (flet ((f (x)
                                   (the integer x)))
                            (logior c (f nil))))
                       :allow-warnings t)
    (assert failurep)
    (assert (= 1 (length warnings)))
    (assert-error (funcall fun 10) type-error)))

(with-test (:name :eql/integer-folding)
  (checked-compile
   `(lambda (a)
      (fceiling (the (member 2.3 21672589639883401935) a)))))

(with-test (:name (position :derive-type))
  (let ((f (checked-compile
            `(lambda (x)
               (declare (type (simple-string 90) x))
               (declare (muffle-conditions code-deletion-note))
               (let ((b (position #\i x)))
                 (if (and (integerp b) (> b 100))
                     'yikes 'okey-dokey))))))
    ;; The function can not return YIKES
    (assert (not (ctu:find-code-constants f :type '(eql yikes))))))

(with-test (:name (coerce :derive-type))
  (macrolet ((check (type ll form &rest values)
               `(assert (equal (funcall (checked-compile
                                         `(lambda ,',ll
                                            (ctu:compiler-derived-type ,',form)))
                                        ,@values)
                               ',type))))
    (check list
           (a)
           (coerce a 'list)
           nil)
    (check (unsigned-byte 32)
           (a)
           (coerce a '(unsigned-byte 32))
           10)
    (check character
           (a x)
           (coerce a (array-element-type (the (array character) x)))
           #\a
           "abc")
    (check (unsigned-byte 32)
           (a x)
           (coerce a (array-element-type (the (array (unsigned-byte 32)) x)))
           10
           (make-array 10 :element-type '(unsigned-byte 32)))))

(with-test (:name :associate-args)
  (flet ((test (form argument)
           (multiple-value-bind (fun failurep warnings)
               (checked-compile form :allow-warnings t)
             (assert failurep)
             (assert (= 1 (length warnings)))
             (assert-error (funcall fun argument)))))
    (test `(lambda (x) (+ 1 x nil)) 2)
    (test `(lambda (x) (/ 1 x nil)) 4)))

(with-test (:name :eager-substitute-single-use-lvar)
  (checked-compile-and-assert ()
      `(lambda (a)
         (let ((a (the fixnum a))
               (x 1)
               z)
           (tagbody
              (flet ((jump () (go loop)))
                (jump))
            loop
              (setf z (the fixnum (if (= x 1) #xFFF a)))
              (unless (= x 0)
                (setf x 0)
                (go loop)))
           z))
      ((0)   0)
      ((2)   2)
      ((255) 255)))

(with-test (:name :vop-on-eql-type)
  (assert (= (funcall
              (funcall (checked-compile
                        `(lambda (b)
                           (declare ((eql -7) b)
                                    (optimize debug))
                           (lambda (x)
                             (logior x b))))
                       -7)
              3)
             -5)))

(with-test (:name :vop-on-eql-type.2)
  (assert (integerp
           (funcall
            (funcall (checked-compile
                      `(lambda (b)
                         (declare ((eql -7) b)
                                  (optimize debug))
                         (lambda ()
                           (+ (random 10) b))))
                     -7)))))

(with-test (:name :vop-on-eql-type.3)
  (assert (integerp
           (funcall
            (funcall (checked-compile
                      `(lambda (b)
                         (declare ((eql -7) b)
                                  (optimize (compilation-speed 0) debug))
                         (lambda ()
                           (+ (random 10) b))))
                     -7)))))

(flet ((test (form)
         (multiple-value-bind (fun failurep)
             (checked-compile `(lambda () ,form)
                              :allow-failure t)
           (assert failurep)
           (assert-error (funcall fun) sb-int:compiled-program-error))))

  (with-test (:name (compile macrolet :malformed) :serial t)
    (test '(macrolet (foo () 'bar)))
    (test '(macrolet x))
    (test '(symbol-macrolet x))
    (test '(symbol-macrolet (x))))

  (with-test (:name (compile flet :malformed) :serial t)
    (test '(flet (foo () 'bar)))
    (test '(flet x))
    (test '(labels (foo () 'bar)))
    (test '(labels x))))

(with-test (:name :compile-load-time-value-interpreted-mode)
  ;; This test exercises the same pattern as HANDLER-BIND (to a
  ;; degree).  In particular a HANDLER-BIND that was compiled when the
  ;; *EVALUATOR-MODE* was :INTERPRET would not compile its class
  ;; predicates, because LOAD-TIME-VALUE just called EVAL, and you
  ;; would get back a list with an interpreted function in it.
  ;;
  ;; In the code below, this function when called would generate a new
  ;; symbol each time. But if the compiler processes the guts as it
  ;; should, you get back a compiled lambda which returns a constant
  ;; symbol.
  (let ((f (let (#+(or sb-eval sb-fasteval) (sb-ext:*evaluator-mode* :interpret))
             (checked-compile
              `(lambda ()
                 (load-time-value
                  (list (lambda ()
                          (macrolet ((foo ()
                                       (sb-int:keywordicate (gensym))))
                            (foo))))))))))
    (eq (funcall (car (funcall f)))
        (funcall (car (funcall f))))))

(with-test (:name :constant-fold-%eql/integer)
  (checked-compile-and-assert ()
      `(lambda (x)
         (declare (type (complex single-float) x))
         (member (the (eql #c(0.0 0.0)) x)
                 '(1 2 3 9912477572127105188)))
    ((#C(0.0 0.0)) nil)))

(with-test (:name (compile svref :constant))
  (checked-compile-and-assert ()
      `(lambda () (svref #(1 2 3) 1))
    (() 2)))

(with-test (:name (compile char-equal :type-intersection))
  (checked-compile-and-assert ()
      `(lambda (x y)
         (char-equal (the (member #\a #\B) x)
                     (the (eql #\A) y)))
    ((#\a #\A) t)))

(with-test (:name (oddp fixnum :no-consing)
            :serial t :skipped-on :interpreter)
  (let ((f (checked-compile '(lambda (x) (oddp x)))))
    (ctu:assert-no-consing (funcall f most-positive-fixnum))))
(with-test (:name (oddp bignum :no-consing)
            :serial t :skipped-on :interpreter :fails-on (or :arm :ppc))
  (let ((f (checked-compile '(lambda (x) (oddp x))))
        (x (* most-positive-fixnum most-positive-fixnum 3)))
    (ctu:assert-no-consing (funcall f x))))
(with-test (:name (logtest fixnum :no-consing :bug-1277690)
            :serial t :skipped-on :interpreter)
  (let ((f (checked-compile '(lambda (x) (logtest x most-positive-fixnum)))))
    (ctu:assert-no-consing (funcall f 1))))
(with-test (:name (logtest bignum :no-consing)
            :serial t :skipped-on :interpreter :fails-on (or :arm :ppc))
  (let ((f (checked-compile '(lambda (x) (logtest x 1))))
        (x (* most-positive-fixnum most-positive-fixnum 3)))
    (ctu:assert-no-consing (funcall f x))))

(with-test (:name (array-dimension *))
  (checked-compile-and-assert ()
      `(lambda  (array)
         (declare ((or (vector t) (array character)) array))
         (array-dimension array 0))
    ((#(1 2 3)) 3)))

(with-test (:name :generate-type-checks-on-dead-blocks)
  (checked-compile-and-assert ()
      `(lambda (a b)
         (declare (type (member vector 42) a))
         (map a 'list (the vector b) #*))
    (('vector #()) #() :test #'equalp)))

(with-test (:name (make-list :large)
            :skipped-on (or :ubsan (not :64-bit)))
  (checked-compile `(lambda ()
                      (make-list (expt 2 28) :initial-element 0)))
  (checked-compile `(lambda ()
                      (make-list (expt 2 30) :initial-element 0))))

(with-test (:name :bad-cond)
  (assert-error
   (checked-compile
    '(lambda () (cond (t 10) 20)))))

(with-test (:name :removed-dx-cast)
  (checked-compile-and-assert ()
      `(lambda ()
         (loop
            (let ((x (the integer (return 0))))
              (declare (dynamic-extent x))
              (unwind-protect x 1))))
    (() 0)))

(with-test (:name (isqrt :derivation))
  (checked-compile-and-assert ()
      `(lambda (i)
         (isqrt (count (the bit i) #*11101)))
    ((1) 2)))

(with-test (:name :vector-zero-initialization)
  (assert (equalp (funcall (funcall (checked-compile
                              `(lambda (x b)
                                 (declare ((eql 0) x)
                                          (optimize (debug 2)))
                                 (lambda ()
                                   (vector x (isqrt b)))))
                                    0 4))
                  #(0 2))))

(with-test (:name :cons-zero-initialization)
  (assert (equalp (funcall (funcall (checked-compile
                              `(lambda (x b)
                                 (declare ((eql 0) x)
                                          (optimize (debug 2)))
                                 (lambda ()
                                   (cons x (isqrt b)))))
                                    0 4))
                  '(0 . 2))))

(with-test (:name :check-important-result-warning)
  (multiple-value-bind (fun failure warnings style-warnings)
      (checked-compile '(lambda (x z)
                         (declare (notinline nintersection))
                         (nintersection x z) x)
                       :allow-style-warnings t)
    (declare (ignore fun failure warnings))
    (loop for c in style-warnings
          do
          (assert (search "NINTERSECTION"
                          (princ-to-string c))))))

(with-test (:name :adjust-array-semi-important-result)
  (macrolet ((try (type assert-what)
               `(multiple-value-bind (fun failure warnings style-warnings)
                    (checked-compile '(lambda (v)
                                        (declare (,type v))
                                        (adjust-array v (* (length v) 2))
                                        (bit v 0))
                                     :allow-style-warnings t)
                  (declare (ignore fun failure warnings))
                  (assert ,assert-what))))
    (try simple-bit-vector style-warnings)
    (try bit-vector (not style-warnings))))

(with-test (:name :destroyed-constant-warning)
  (multiple-value-bind (fun failure warnings)
      (checked-compile '(lambda ()
                         (declare (notinline nunion))
                         (nunion '(1 2 3) '(1 2 4)))
                       :allow-warnings t)
    (declare (ignore fun failure))
    (loop for c in warnings
          do
          (assert (search "NUNION"
                          (princ-to-string c))))))

(with-test (:name :%array-data-complex-type-derivation)
  (let ((type (funcall (checked-compile
                        `(lambda (x)
                           (ctu:compiler-derived-type (sb-kernel:%array-data (the array x)))))
                       #2A())))
    (assert (eq type 'array))))

(with-test (:name :equalp-transofrm)
  (assert
   (funcall (checked-compile
             `(lambda (x y)
                (equalp (the (simple-array single-float (*)) x)
                        (the (simple-array double-float (*)) y))))
            (coerce '(1f0) '(simple-array single-float (*)))
            (coerce '(1d0) '(simple-array double-float (*))))))

(with-test (:name :array-hairy-type-derivation)
  (assert
   (equal (funcall (checked-compile
                    `(lambda (x)
                       (subseq (the (and (satisfies sb-impl::vector-with-fill-pointer-p)
                                         (string 3)) x)
                               1)))
                   (make-array 3 :element-type 'character
                                 :fill-pointer t
                                 :initial-contents "abc"))
          "bc")))

(with-test (:name :nreverse-derive-type)
  (assert
   (not (funcall (checked-compile
                  '(lambda (x)
                    (eql (car (nreverse (the (cons (eql 10)) x))) 10)))
                 (list 10 20)))))

(with-test (:name :subseq-derive-type)
  (assert
   (equalp (funcall (checked-compile
                    '(lambda (x)
                      (subseq (the (simple-vector 3) x) 1)))
                   #(1 2 3))
           #(2 3))))

(with-test (:name :sequence-derive-type)
  (assert
   (equalp (funcall (checked-compile
                     '(lambda (x)
                       (copy-seq (the (and string (not (simple-array nil))) x))))
                    (make-array 3 :element-type 'character
                                  :fill-pointer 2
                                  :initial-contents "123"))
           "12")))

(with-test (:name :sequence-derive-type.2)
  (assert
   (funcall (checked-compile
             '(lambda (x y)
               (equal (the (and string (not (simple-array nil))) x) y)))
            (make-array 3 :element-type 'character
                          :fill-pointer 2
                          :initial-contents "123")
            "12")))

(with-test (:name :sequence-derive-type.3)
  (assert
   (equalp (funcall (checked-compile
                     '(lambda (x)
                       (subseq (the (or (simple-array * (*)) string) x)  0 2)))
                    #(1 2 3))
           #(1 2))))

(with-test (:name :not-enough-values-cast)
  (assert
   (not (funcall (checked-compile
                  `(lambda ()
                     (car (describe 1 (make-broadcast-stream)))))))))

;; Vestigial exit deletion was a bit too aggressive, causing stack
;; analysis to decide that the value of (BAR 10) in both cases below
;; needed to be nipped out from under the dynamic-extent allocation of
;; Y (or #'Y), which %NIP-VALUES refused to do (DX values must not be
;; moved once allocated).
(with-test (:name (compile :exit-deletion :bug-1563127 :variable))
  (checked-compile '(lambda (x)
                     (block test
                       (multiple-value-prog1 (bar 10)
                         (let ((y (list x)))
                           (declare (dynamic-extent y))
                           (bar y)
                           (if x
                               (return-from test))))))
                   :allow-style-warnings t))

(with-test (:name (compile :exit-deletion :bug-1563127 :function))
  (checked-compile '(lambda (x)
                     (block test
                       (multiple-value-prog1 (bar 10)
                         (flet ((y () (list x)))
                           (declare (dynamic-extent #'y))
                           (bar #'y)
                           (if x
                               (return-from test))))))
                   :allow-style-warnings t))

(with-test (:name (compile :exit-deletion :bug-533930))
  (checked-compile '(lambda ()
                     (block a
                       (multiple-value-prog1 42
                         (catch 'ct
                           (let ((x (cons t t)))
                             (declare (dynamic-extent x))
                             (return-from a (catch 'ct (foo x))))))))
                   :allow-style-warnings t))

(with-test (:name (compile :exit-deletion :bug-518099))
  (checked-compile '(lambda (a)
                     (block b1
                       (multiple-value-prog1 42
                         (catch 'ct
                           (return-from b1
                             (catch 'ct2
                               (min 1 a)))))))))

(with-test (:name (compile :exit-deletion :bug-1655011))
  (checked-compile '(lambda (x)
                     (block nil
                       (multiple-value-prog1 (catch 'ct)
                         (let ((* (list 10)))
                           (declare (dynamic-extent *))
                           (if x
                               (return))))))))

(with-test (:name (compile :exit-deletion :2017-01-30))
  (checked-compile '(lambda (b c)
                     (block b5
                       (multiple-value-prog1 42
                         (restart-bind nil
                           (if b (return-from b5
                                   (catch 'foo c)))))))))

(with-test (:name (compile :exit-deletion :2017-01-30 :even-more-subtle))
  (checked-compile '(lambda (b c)
                     (block b5
                       (multiple-value-prog1 42
                         (restart-bind nil
                           (if b (return-from b5
                                   (catch 'foo
                                     (error "even more subtle"))))))))
                   :allow-style-warnings t))

(with-test (:name (compile :exit-deletion :2017-01-31 :tagbody))
  (let ((test-closure
         (checked-compile '(lambda (b c)
                            (declare (notinline funcall))
                            (block b5
                              (multiple-value-prog1 42
                                (if b (tagbody
                                         (return-from b5
                                           (funcall c (lambda () (go away))))
                                       away))))))))
    ;; Be careful here: The return value can be garbage.  Our saving
    ;; grace is that it's stack garbage, thus by definition GC-safe to
    ;; hold in a register long enough to take its address, but we
    ;; really don't want to try and externalize it in any way.
    (assert (= (sb-kernel:get-lisp-obj-address
                (funcall test-closure t #'funcall))
               (sb-kernel:get-lisp-obj-address
                42)))))

(with-test (:name (compile :exit-deletion :2017-03-29 :tagbody))
  (checked-compile '(lambda ()
                     (block foo
                       (multiple-value-prog1 42
                         (tagbody
                            (return-from foo
                              (catch 'ct (go tag6)))
                          tag6))))))

(with-test (:name (compile :exit-deletion :bug-1723993 :ignoring-block-value))
  (checked-compile '(lambda (x)
                     (block b1
                       (multiple-value-prog1 0
                         (block b2
                           (return-from b1
                             (catch 'c
                               (return-from b2
                                 x)))))))))

(with-test (:name :mv-call-no-let-conversion)
  (assert (equal
           (funcall
            (checked-compile
             '(lambda ()
               (locally (declare (optimize (sb-c::let-conversion 0)))
                 (multiple-value-call #'lisp-implementation-version (values))))))
           (lisp-implementation-version)))
  (assert (equal
           (funcall
            (checked-compile
             '(lambda ()
               (locally (declare (optimize (sb-c::let-conversion 0)))
                 (multiple-value-call #'lisp-implementation-type (values))))))
           (lisp-implementation-type)))
  (assert (equal
           (funcall
            (checked-compile
             '(lambda ()
               (locally (declare (optimize (sb-c::let-conversion 0)))
                 (multiple-value-call #'princ-to-string 1)))))
           "1")))

(with-test (:name :mv-call-argument-mismatch)
  (assert
   (nth-value 2
              (checked-compile
               '(lambda () (multiple-value-call #'cons 1 2 3))
               :allow-warnings t))))

(with-test (:name :valid-callable-argument-cast)
  (assert (equal (funcall (checked-compile '(lambda (x)
                                             (find-if (the function #'oddp)
                                              x)))
                          '(2 4 3))
                 3)))

(with-test (:name :unsigned-word-float-conversion)
  (assert (= (rational (funcall (checked-compile `(lambda (x)
                                                    (float (the sb-ext:word x) 1d0)))
                                sb-ext:most-positive-word))
             #+32-bit 4294967295
             #+64-bit 18446744073709551616)))

(with-test (:name :callable-argument-mismatch-on-xep)
  (checked-compile-and-assert ()
      `(lambda (s x)
         (locally (declare (notinline reduce))
           (reduce (lambda (a b)
                     (+ a b x))
                   s)))
    (('(1 2) 3) 6))
  (checked-compile-and-assert ()
      `(lambda (s x)
         (locally (declare (notinline reduce))
           (reduce (lambda (&optional a b z)
                     (declare (ignore z))
                     (+ a b x))
                   s)))
    (('(1 2) 3) 6)))

(with-test (:name :bug-1951889)
  (checked-compile-and-assert
   (:optimize '(:debug 2))
   `(lambda ()
      (block nil
        (flet ((%f6 (x &key)
                 (declare (ignore x))
                 (return 0)))
          (loop for lv3 below 1 count
                                (if (%f6 0)
                                    (%f6 0)
                                    (eval (%f6 0)))))))
   (() 0)))

(with-test (:name (yes-or-no-p type))
  (checked-compile `(lambda ()
                      (yes-or-no-p nil)))
  (checked-compile `(lambda ()
                      (y-or-n-p nil)))
  (checked-compile `(lambda ()
                      (yes-or-no-p #'list)))
  (checked-compile `(lambda ()
                      (y-or-n-p #'list))))

(with-test (:name :callable-delayed-mismatch)
  (multiple-value-bind (fun failure-p warnings)
      (checked-compile '(lambda () (let ((f 'cons)) (find-if f '(10))))
                       :allow-warnings 'simple-warning)
    (declare (ignore fun))
    (assert failure-p)
    (assert (= (length warnings) 1))
    (search "The function CONS is called by"
            (princ-to-string (first warnings)))))

(with-test (:name :set-type-conflict)
  (assert (nth-value 1
                     (checked-compile
                      '(lambda () (set '// 10))
                      :allow-warnings t))))

(with-test (:name :two-arg-funs-check)
  (loop for (nil x) in sb-c::*two-arg-functions*
        do
        (assert (fboundp x))
        (assert (sb-int:info :function :info x))))

(with-test (:name :two-arg-with-two-arguments-only)
  (checked-compile-and-assert ()
      `(lambda (x y) (string-lessp x y :start1 0))
    (("a" "b") 0)))

(with-test (:name :optimize-functional-arguments-casts)
  (checked-compile-and-assert (:optimize :safe)
      '(lambda (list key)
         (declare (type atom key))
         (find 1 list :key (the (member car) key)))
    (('((a b) (1 a)) 'car) '(1 a))
    (('((a b) (1 a)) 'cdr) (condition 'type-error))))

(with-test (:name (:two-arg-rewriting find-if))
  (checked-compile-and-assert ()
      `(lambda (x)
         (declare (type vector x))
         (find-if #'oddp x :key '-))
    ((#(1)) 1)))

(with-test (:name :transforms-check-policy-first)
  (checked-compile-and-assert (:optimize '(:safety t :debug t :speed 3 :space 3)
                               :allow-notes nil)
      `(lambda (x)
         (find x "a b c" :test #'char-equal))
    ((#\B) #\b)))

(with-test (:name (ignorable macrolet))
  (checked-compile-and-assert ()
      `(lambda ()
         (macrolet ((f () 10))
           (declare (ignorable #'f))
           (f)))
    (() 10))
  (assert
   (eql (assert-no-signal
         (eval `(macrolet ((f () 10))
                  (declare (ignorable #'f))
                  (f))))
        10)))

(with-test (:name (:cast values &rest))
  (checked-compile-and-assert ()
      `(lambda ()
         (values (the (values &rest integer) (eval '(values)))))
    (() nil)))


(with-test (:name (:cast values &optional))
  (checked-compile-and-assert ()
      `(lambda ()
         (let ((x (the (values &optional integer) (eval '(values)))))
           (when x
             (setf x 10))))
    (() nil)))

#+sb-unicode
(with-test (:name (setf schar :type-mismatch))
  (let ((fun (checked-compile
              `(lambda (a) (setf (schar a 0) #\HIRAGANA_LETTER_SMALL_TU)))))
    (let ((string (string #\a))
          (base-string (coerce "a" 'simple-base-string)))
      (assert (eq (funcall fun string) #\HIRAGANA_LETTER_SMALL_TU))
      (assert (equal string (string #\HIRAGANA_LETTER_SMALL_TU)))
      (assert-error (funcall fun base-string) type-error)
      (assert (equal base-string "a")))))

(with-test (:name :can-escape-functionals-escape)
  (checked-compile
   `(lambda ()
      (declare (optimize (speed 3)))
      (lambda () (unwind-protect :x :z)))
   :allow-notes nil)
  (checked-compile
   `(lambda ()
      (declare (optimize (speed 3)))
      (lambda () (catch :x)))
   :allow-notes nil))

(with-test (:name (:combination-implementation-style :constants))
  (checked-compile-and-assert ()
      `(lambda (p1 p2)
         (logbitp (the (eql 1) p1) (the fixnum p2)))
    ((1 2) t)))

(with-test (:name :reducing-constants)
  (checked-compile-and-assert ()
      `(lambda (x) (* 4.457268f31 4 x -46253801283659))
    ((5.0f-9) -4.123312f37))
  (checked-compile-and-assert ()
      `(lambda (x) (* #C(4.457268f31 0.0) 4 x -46253801283659))
    ((5.0f-9) #C(-4.123312f37 -0.0))))

(with-test (:name :reducing-constants.2)
  (let* ((style-warning-p nil)
         (fun (checked-compile `(lambda () (* 1.0 2 (expt 2 127)))
                               :allow-style-warnings t
                               :condition-transform (lambda (x)
                                                      (when (typep x 'style-warning)
                                                        (setf style-warning-p t))
                                                      x))))
    (handler-case (funcall fun)
      (floating-point-overflow () (assert style-warning-p))
      (:no-error (x) (assert (not style-warning-p)) (assert (eql x sb-ext:single-float-positive-infinity))))))

(with-test (:name (logbitp :past fixnum))
  (checked-compile-and-assert ()
      `(lambda (x) (logbitp sb-vm:n-fixnum-bits (the fixnum x)))
    ((1)  nil)
    ((-1) t)))

(with-test (:name (dpb :implementation-style 1))
  (checked-compile-and-assert ()
      `(lambda ()
         (let ((res 126))
           (declare (type sb-vm:signed-word res))
           (setf res (dpb res (byte 1 2) res))
           res))
    (() 122)))

(with-test (:name (dpb :implementation-style 2))
  (checked-compile-and-assert ()
      `(lambda (x i)
         (declare (fixnum x i))
         (dpb x (byte 0 0) i))
    (( 0 1) 1)
    ((-1 1) 1)))

(with-test (:name (fixnum :mod-p :immediate))
  (checked-compile-and-assert ()
      `(lambda (x)
         (declare (type fixnum x))
         (typep x '(integer 0 2049)))
      ((2050) nil)
      ((-1)   nil)
      ((1)    t)
      ((0)    t)))

(with-test (:name (make-array :initial-contents :element-type :mismatch))
  (checked-compile-and-assert ()
      `(lambda (x)
         (make-array '(1 2) :element-type 'list :initial-contents x))
      (('(((1 2) (3 4)))) #2A(((1 2) (3 4))) :test #'equalp)))

(with-test (:name :unknown-values-receiver-register-clobber)
  (checked-compile-and-assert ()
      `(lambda ()
         (let ((x (list 1)))
           (declare (dynamic-extent x))
           (progv '(*) x
             (catch 'ct (the integer (eval (dotimes (i 1 42) 42)))))))
    (() 42)))


(with-test (:name :single-float-bits-to-signed-stack)
  (checked-compile-and-assert ()
      `(lambda (p1)
         (declare (type single-float p1))
         (scale-float p1 27))
    ((1.0) 1.3421773e8)))

(with-test (:name (compile :call :dotted-list))
  (flet ((test (form)
           (multiple-value-bind (fun failure-p warnings style-warnings notes
                                 compiler-errors)
               (checked-compile `(lambda () ,form)
                                :allow-failure t :allow-warnings t)
             (declare (ignore fun warnings style-warnings notes))
             (assert failure-p)
             (assert (= 1 (length compiler-errors)))
             (assert (search "is not a proper list."
                             (princ-to-string (first compiler-errors)))))))
    (test '(cons 1 . 2))
    (test '((lambda (x) x) . 3))
    (test '(let () . 4))))
(with-test (:name (compile :macro-dotted-list))
  (checked-compile-and-assert ()
      `(lambda (i j)
         (macrolet ((k (a . b)
                      `(+ ,a ,b)))
           (k i . j)))
    ((1 2) 3)))

(with-test (:name (ldb :rlwinm))
  (checked-compile-and-assert ()
      `(lambda (x)
         (declare (fixnum x))
         (ldb (byte 13 19) x))
    ((-3560597) 8185)))

(with-test (:name (logand :transform))
  (checked-compile-and-assert ()
      `(lambda (b)
         (catch 'ct1
           (flet ((%f (&key (y (throw 'ct1 1)))
                    (return-from %f y)))
             (logand (%f) b))))
    ((1) 1)))

(with-test (:name (:left ash :cut-amount))
  (checked-compile-and-assert ()
      `(lambda (b z)
         (declare (type fixnum b)
                  (type (and fixnum unsigned-byte) z))
         (ldb (byte 64 0) (ash b z)))
    ((-1 70) 0)))

(with-test (:name :stp-load-fp-tn)
  (checked-compile-and-assert ()
      `(sb-int:named-lambda ,(gensym) (a)
         (max
          (handler-case
              (locally (declare (notinline values))
                (values a a a a a a a a a a a)))
          1))
    ((1) 1)))

#+sb-unicode
(with-test (:name :immediate-char-=)
  (checked-compile-and-assert ()
      `(lambda (x)
         (declare (type (member #\U9925 #\UBC19E) x))
         (the (member #\UBC19E) x))
    ((#\UBC19E) #\UBC19E)))

(with-test (:name (compile * :constant-behind-cast :lp-1717971]))
  (checked-compile-and-assert (:optimize :quick/incomplete)
      `(lambda (x)
         (declare (type integer x))
         (declare (optimize (space 3) (compilation-speed 0)))
         (catch 'ct5
           (* (flet ((%f (&key (x (throw 'ct5 123)))
                       (the integer x)))
                (%f))
              x)))
    ((45) 123)))

(with-test (:name (if :refs-to-the-same-var-but-different-types))
  (checked-compile-and-assert ()
      `(lambda (a b)
         (declare (type fixnum b a))
         (setf a -1)
         (plusp (if (<= a b)
                    b
                    (if nil
                        b
                        b))))
    ((1 2) t))
  (checked-compile-and-assert ()
      `(lambda (a b)
         (declare (type integer b a))
         (setf a -1)
         (plusp (if (<= a b)
                    b
                    (+ b 0))))
    ((1 2) t)))

(with-test (:name :unequal-n-values-type-intersection)
  (checked-compile-and-assert ()
      `(lambda ()
         (let ((b 0))
           (unwind-protect
                (if (and
                     (random (setf b 2))
                     (eql b 0))
                    (case (dpb b (byte 0 0) -25)
                      ((3) (values 1 2 3)))
                    (values 0 0)))))
    (() (values 0 0))))

#+sb-unicode
(with-test (:name :base-char-weakening)
  (checked-compile-and-assert (:optimize :safe)
      `(lambda (x) (the base-char x))
    (((code-char 252)) (condition 'type-error))))

(with-test (:name (logandc1 :constant-second-arg))
  (checked-compile-and-assert ()
    `(lambda (p1 p2)
       (declare (type (integer ,(- (expt 2 31)) ,(1- (expt 2 31))) p1)
                (type (eql -1) p2))
       (logandc1 p1 p2))
    ((-3 -1) 2)))

;;; A user reported a potential compiler bug when SBCL consumed all its memory
;;; while trying to compile a "trivial" wrapper macro similar to this one.
;;; (because MACROLET is not FLET)
(with-test (:name :macrolet-infinite-loop-detection)
  (multiple-value-bind (fun warningsp errorp)
      (compile nil
               '(lambda (x)
                  (macrolet ((complicated-fun (&rest keys)
                               `(complicated-fun :a 1 ,@keys)))
                    (complicated-fun :x 9))))
    (assert (and fun warningsp errorp))))

;;; This SAP+ call overflowed the size of an immediate on MIPS.
;;; 'bit-vector.impure.lisp' exposed this bug where it computes
;;;   (sb-sys:sap+ first sb-c:+backend-page-bytes+)
;;; which is not the ideal place to fail, considering that
;;; pointer arithmetic is not what's being tested.
(with-test (:name :sap+-immediate)
  (compile nil '(lambda (x) (sb-sys:sap+ x 65536))))

(with-test (:name (compile handler-bind :no-note))
  (checked-compile
   `(lambda (x)
      (handler-bind ((error (constantly nil)))
        (pathname-type x)))
   :allow-notes nil))

(with-test (:name (propagate-let-args :cross-component-constant-substition))
  (checked-compile
   `(lambda (a)
      (labels ((%f2 (x)
                 (flet ((%f6 (y)
                          (prog1 y
                            (catch 'tag y)
                            (return-from %f2
                              (unwind-protect (reduce #'(lambda (b c) x)
                                                      (list))
                                a))))))))
        (%f2 (loop for i below 1 sum (%f2 1)))))
   :allow-warnings t))

(with-test (:name :single-use-complement-inlines)
  (let ((fun (checked-compile
              `(lambda (z)
                 (funcall (complement z))))))
    (assert (not (ctu:find-named-callees #'complement)))))

(with-test (:name :single-use-constantly-inlines)
  (let ((fun (checked-compile
              `(lambda (z)
                 (funcall (constantly z))))))
    (assert (not (ctu:find-named-callees #'constantly)))))

(with-test (:name :note-argument-not-returning)
  (let ((notes
          (nth-value
           4 (checked-compile `(lambda ()
                                 (block nil
                                   (let ()
                                     (cons
                                      (eval 'nil)
                                      (return
                                        :good)))))))))
    (assert (dolist (note notes nil)
              (when (string= (format nil "~a" note)
                             "The second argument never returns a value.")
                (return t))))))

(with-test (:name :check-consistency-mv-call-substitute-single-use-lvar)
  (let ((sb-c::*check-consistency* t))
    (checked-compile
     `(lambda (spec)
       (multiple-value-call #'list
         (first spec)
         (values
          5
          6))))))

(with-test (:name :check-consistency-call-symbol)
  (let ((sb-c::*check-consistency* t))
    (checked-compile
     `(lambda ()
        (print (lambda (x) (apply (read) x)))))))

(with-test (:name :check-consistency-info-arg-count)
  (let ((sb-c::*check-consistency* t))
    (checked-compile
     `(lambda (symbol expr eqx)
        (declare (type function eqx))
        (if (boundp symbol)
            (let ((oldval (symbol-value symbol)))
              (if (funcall eqx oldval expr) oldval expr))
            expr)))))

(with-test (:name :check-consistency-deleted-let)
  (let ((sb-c::*check-consistency* t))
    (checked-compile
     `(lambda ()
        (let ((x (error "fail")))
          x)))))

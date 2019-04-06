;;;; tests of backquote readmacro

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

(with-test (:name :backq-smoke-test)
  (assert (equalp (macroexpand '`#(() a #(#() nil x) #()))
                  ''#(NIL A #(#() NIL X) #()))))

(defparameter *qq* '(*rr* *ss*))
(defparameter *rr* '(3 5))
(defparameter *ss* '(4 6))

(defun *rr* (x)
  (reduce #'* x))

(defparameter *x* '(a b))
(defparameter *y* '(c))
(defparameter *p* '(append *x* *y*))
(defparameter *q* '((append *x* *y*) (list 'sqrt 9)))
(defparameter *r* '(append *x* *y*))
(defparameter *s* '((append *x* *y*)))

(defun test-double-backquote (expression value)
  #+nil(format t "~&Testing: ~A... " expression)
  (assert (equal (eval (eval (read-from-string expression)))
                 value))
  #+nil(progn (format t "Ok. Look at PPRINTed version: ")
              (pprint (read-from-string expression))))

(defparameter *backquote-tests*
  '(("``(,,*QQ*)" . (24))
    ("``(,@,*QQ*)" . 24)
    ("``(,,@*QQ*)" . ((3 5) (4 6)))
    ("``(FOO ,,*P*)" . (foo (a b c)))
    ("``(FOO ,,@*Q*)" . (foo (a b c) (sqrt 9)))
    ("``(FOO ,',*R*)" . (foo (append *x* *y*)))
    ("``(FOO ,',@*S*)" . (foo (append *x* *y*)))
    ("``(FOO ,@,*P*)" . (foo a b c))
    ("``(FOO ,@',*R*)" . (foo append *x* *y*))
    ;; The following expression produces different result under LW.
    ("``(FOO . ,,@*Q*)" . (foo a b c sqrt 9))
    ;; These three did not work.
    ("``(FOO ,@',@*S*)" . (foo append *x* *y*))
    ("``(FOO ,@,@*Q*)" . (foo a b c sqrt 9))
    ("``(,@,@*QQ*)" . (3 5 4 6))))

(mapc (lambda (test)
        (test-double-backquote (car test) (cdr test)))
      *backquote-tests*)

(let ((string "`(foobar a b ,c ,'(e f g) d ,@'(e f g) (h i j) ,@foo)"))
  (assert (equalp (print (read-from-string string) (make-broadcast-stream))
                  (read-from-string string))))

(let ((a '`(1 ,@a ,@b ,.c ,.d)))
  (let ((*print-circle* t))
    (assert (equalp (read-from-string (write-to-string a)) a))))

(let ((s '``(,,@(list 1 2 3) 10)))
  (assert (equal (eval (eval s)) '(1 2 3 10))))

(with-test (:name :sharp-dot-resets-backquote-depth)
  (assert (equalp `#.(write-to-string (read-from-string "#(1 2 3)"))
                  "#(1 2 3)"))
  (assert (eq :error
              (handler-case (read-from-string "`(foo bar #.(max 5 ,*print-base*))")
                (reader-error () :error)))))

(with-test (:name :triple-backquote)
  (flet  ((expect (expect val)
            (assert (string= (write-to-string val) expect))))
    (let ((plet/fast 'val1)
          (expr '```(,',',plet/fast ,',kernel ,@body)))
      (declare (special plet/fast))
      (expect "```(,',',PLET/FAST ,',KERNEL ,@BODY)" expr)
      (expect "``(,','VAL1 ,',KERNEL ,@BODY)" (eval expr))
      (let ((kernel 'val2))
        (declare (special kernel))
        (expect "`(,'VAL1 ,'VAL2 ,@BODY)" (eval (eval expr)))
        (let ((body '((fn) (otherfn))))
          (declare (special body))
          (expect "(VAL1 VAL2 (FN) (OTHERFN))" (eval (eval (eval expr)))))))))

(defmacro broken-macro (more-bindings)
  `(macrolet ((with-bindings (&body body)
                `(let ((thing1 :something) ,',@more-bindings) ,@body)))
     (with-bindings (thing))))

;; In the above macro (WITH-BINDINGS (THING)) can be rendered unevaluable
;; due to syntax error via improper format of MORE-BINDINGS.
;; Regardless, the pprinter should faithfully indicate how BROKEN-MACRO expands.
;; All of these tests except for the baseline "accidentally working" case
;; either crashed the pprinter or displayed incorrectly.
(with-test (:name :bug-1063414-unprintable-nested-backquote)
  (flet  ((expect (expect form)
            (assert (string= (write-to-string (macroexpand-1 form))
                             expect))))

    ;; this example's expansion is correct but only by accident
    (expect "(MACROLET ((WITH-BINDINGS (&BODY BODY)
             `(LET ((THING1 :SOMETHING) ,'(VAR VAL))
                ,@BODY)))
  (WITH-BINDINGS (THING)))" '(broken-macro ((var val))))

    ;; this example should correctly print QUOTE with no operand
    (expect "(MACROLET ((WITH-BINDINGS (&BODY BODY)
             `(LET ((THING1 :SOMETHING) ,(QUOTE))
                ,@BODY)))
  (WITH-BINDINGS (THING)))" '(broken-macro nil))

    ;; ... or two operands
    (expect "(MACROLET ((WITH-BINDINGS (&BODY BODY)
             `(LET ((THING1 :SOMETHING) ,(QUOTE (VAR :SOME-FORM) (VAR2 2)))
                ,@BODY)))
  (WITH-BINDINGS (THING)))" '(broken-macro ((var :some-form) (var2 2))))

    ;; ... or an attempt to bind the symbol NIL
    (expect "(MACROLET ((WITH-BINDINGS (&BODY BODY)
             `(LET ((THING1 :SOMETHING) ,'NIL)
                ,@BODY)))
  (WITH-BINDINGS (THING)))" '(broken-macro (nil)))

    ;; ... or even a meaningless dotted-list QUOTE form
    (expect "(MACROLET ((WITH-BINDINGS (&BODY BODY)
             `(LET ((THING1 :SOMETHING) ,(QUOTE . FROB))
                ,@BODY)))
  (WITH-BINDINGS (THING)))" '(broken-macro frob))))

(with-test (:name :preserving-inner-backquotes)
  (flet  ((expect (expect val)
            (assert (string= (write-to-string val) expect))))

    ;; Continuing with *BACKQUOTE-TESTS*, instead of checking for the value
    ;; after twice evaluating, check for expected printed form after one eval.
    (expect "`(,(*RR* *SS*))" ``(,,*QQ*))
    (expect "`(,@(*RR* *SS*))" ``(,@,*QQ*))
    (expect "`(,*RR* ,*SS*)" ``(,,@*QQ*))

    ;; Three tests inspired by tests from CLISP, but our expected answers are,
    ;; I think, better because inner backquotes are preserved.
    (expect "(FOO `(BAR ,@'((BAZ 'A A) (BAZ 'B B) (BAZ 'C C) (BAZ 'D D))))"
            (let ((list '(a b c d)))
              `(foo `(bar ,@',(mapcar (lambda (sym) `(baz ',sym ,sym))
                                      list)))))

    (expect "```,,,X" ````,,,,'x)

    ;; In this one the leftmost backquote's comma is the second from the left.
    ;; That subform is "`,3" which is just 3. The inner quasiquote remains.
    (expect "`,3" ``,,`,3)))

(with-test (:name :preserving-backquotes-difficult)
  (assert (string= (write-to-string
                    (let ((c 'cee) (d 'dee) (g 'gee) (h 'hooray))
                      `(`(a ,b ,',c ,,d) . `(e ,f ,',g ,,h))))
                   "(`(A ,B ,'CEE ,DEE) . `(E ,F ,'GEE ,HOORAY))"))
  (assert (string= (write-to-string
                    (let ((c 'cee) (d 'dee) (g 'gee) (h 'hooray))
                      `(foo `(a ,b ,',c ,,d) . `(e ,f ,',g ,,h))))
                   "(FOO `(A ,B ,'CEE ,DEE) . `(E ,F ,'GEE ,HOORAY))")))

(with-test (:name :backquote-permissible-circularity)
  (flet  ((expect (expect val)
            (assert (string= (write-to-string val) expect))))
    (let ((*print-circle* t))
      ;; this should be agnostic of the circular form after the comma
      (expect "`(FOO BAR ,(HI '#1=(BAR FOO #1# . #1#)))"
              '`(FOO BAR ,(HI '#1=(BAR FOO #1# . #1#)))))))

(with-test (:name :read-backq-missing-expression)
  (assert (string= (handler-case (read-from-string "`(foo ,@)")
                     (sb-int:simple-reader-error (c)
                       (simple-condition-format-control c)))
                   "Trailing ~A in backquoted expression.")))
(with-test (:name :read-backq-vector-illegal)
  (assert (eql (search "Improper list"
                       (handler-case
                           (read-from-string "`((a  #(foo bar . ,(cons 1 2))))")
                         (sb-int:simple-reader-error (c)
                           (simple-condition-format-control c))))
               0)))

(with-test (:name :backq-vector)
  (assert-error (eval (read-from-string "`#(,@#())")))
  (assert-error (eval (read-from-string "`#(,@`#())")))
  (assert (equalp `#(,@(list 1 2 3)) #(1 2 3)))
  (assert (equalp `#(0 ,@(list 1 2 3)) #(0 1 2 3)))
  (assert (equalp `#(,@(list 1 2 3) ,4) #(1 2 3 4))))

(with-test (:name :backq-standard-list-constructors)
  (assert (equal (macroexpand '`(,.(list 1 2 3) 4))
                 '(nconc (list 1 2 3) '(4))))
  (assert (equal (funcall (compiler-macro-function 'sb-int:quasiquote)
                          '`(,.(list 1 2 3) 4) nil)
                 '(nconc (list 1 2 3) '(4))))
  (assert (equal (macroexpand '`(,@(list 1 2 3) 4))
                 '(append (list 1 2 3) '(4))))
  (assert (equal (funcall (compiler-macro-function 'sb-int:quasiquote)
                          '`(,@(list 1 2 3) 4) nil)
                 '(sb-impl::|Append| (list 1 2 3) '(4)))))

(import 'sb-int:quasiquote)

(test-util:with-test (:name :backquote-more-weirdness)
  ;; No expectation on any other Lisp.
  (flet  ((expect (expect val)
            (assert (string= (write-to-string val) expect))))
    ;; There is one quasiquote and one comma
    (expect "`(QUASIQUOTE QUASIQUOTE CADR ,FOO)"
            '`(quasiquote quasiquote cadr ,foo))
    ;; There are three quasiquotes
    (expect "```(CADR ,FOO)"
            '`(quasiquote (quasiquote (cadr ,foo))))))

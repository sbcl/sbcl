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

(in-package "CL-USER")

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
  (format t "~&Testing: ~A... " expression)
  (assert (equal (eval (eval (read-from-string expression)))
                 value))
  (format t "Ok. Look at PPRINTed version: ")
  (pprint (read-from-string expression)))

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
  (assert (equal (print (read-from-string string)) (read-from-string string))))

(let ((a '`(1 ,@a ,@b ,.c ,.d)))
  (let ((*print-circle* t))
    (assert (equal (read-from-string (write-to-string a)) a))))

(let ((s '``(,,@(list 1 2 3) 10)))
  (assert (equal (eval (eval s)) '(1 2 3 10))))

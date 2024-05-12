(load "compiler-test-util.lisp")

(with-test (:name :block-inline-then-notinline)
  (ctu:file-compile
   `((in-package :cl-user)
     (declaim (inline block-inline-foo))
     (defun block-inline-foo (a b)
       (+ a b))
     (defun bar-with-foo-inline (a b c)
       (* c (block-inline-foo a b)))

     (declaim (notinline block-inline-foo))
     (defun bar-with-foo-call (a b c)
       (* c (block-inline-foo a b))))
   :block-compile t
   :load t)
  (assert (not (ctu:find-named-callees (symbol-function 'bar-with-foo-inline))))
  (assert (ctu:find-named-callees (symbol-function 'bar-with-foo-call))))

(with-test (:name :block-defpackage-then-load-fasl)
  (ctu:file-compile
   `((defpackage block-defpackage (:use :cl :cl-user))

     (in-package :block-defpackage)

     (defstruct (struct-foo (:conc-name "FOO-"))
       (bar 0 :type number)
       (baz nil :type list)))
   :block-compile t
   :before-load (lambda () (delete-package :block-defpackage))
   :load t))

(defvar *x*)
(defvar *y*)

(with-test (:name :block-defpackage-top-level-form-order)
  (ctu:file-compile
   `((setq *x* (find-package "BLOCK-DEFPACKAGE"))

     (defpackage block-defpackage (:use :cl :cl-user))

     (setq *y* (find-package "BLOCK-DEFPACKAGE")))
   :block-compile t
   :before-load (lambda () (delete-package :block-defpackage))
   :load t)
  (assert (eq *x* nil))
  (assert *y*))

(with-test (:name :block-defpackage-symbol-inheritance-load-fasl)
  (defpackage block-defpackage2 (:use :cl))
  (ctu:file-compile
   `((in-package :block-defpackage2)

     (eval-when (:compile-toplevel :load-toplevel :execute)
       (shadow "+"))

     (defun + (x) (print x)))
   :block-compile t
   :before-load (lambda ()
                  (delete-package :block-defpackage2)
                  (defpackage block-defpackage2 (:use :cl)))
   :load t)
  (assert (not (eq (fdefinition (find-symbol "+" "BLOCK-DEFPACKAGE2"))
                   #'cl:+))))

(defpackage block-defpackage3
  (:use :cl))

(with-test (:name :block-defpackage-delete-package-redefpackage)
  (ctu:file-compile
   `((eval-when (:compile-toplevel :load-toplevel :execute)
       (when (find-package '#:block-defpackage3)
         (delete-package '#:block-defpackage3)))
     (defpackage block-defpackage3
       (:use :cl))
     (in-package block-defpackage3)
     ;; This can barf if, for example, F loses its package because
     ;; DELETE-PACKAGE and never gets it back when the package gets
     ;; defined again.
     (export '(block-defpackage3::f)))
   :block-compile t
   :load t)
  (assert (eq (nth-value 1 (find-symbol "F" "BLOCK-DEFPACKAGE3"))
              :external)))

;;; Similar to the above test case, but with RENAME-PACKAGE. This is
;;; probably, strictly speaking, non-conforming code according to ANSI
;;; 3.2.4.4 under item 1 for symbol, taking package "same"ness to mean
;;; EQness.
(with-test (:name :block-defpackage-rename-package-redefpackage)
  (ctu:file-compile
   `((eval-when (:compile-toplevel :load-toplevel :execute)
       (when (find-package "BLOCK-DEFPACKAGE4")
         (rename-package "BLOCK-DEFPACKAGE4" "OLD-BLOCK-DEFPACKAGE4")))
     (defpackage "BLOCK-DEFPACKAGE4"
       (:use :cl))
     (in-package "BLOCK-DEFPACKAGE4")
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (export '(f))))
   :block-compile t
   :load t)
  (assert (eq (nth-value 1 (find-symbol "F" "BLOCK-DEFPACKAGE4"))
              :external)))

(with-test (:name :block-defpackage-rename-package)
  (ctu:file-compile
   `((eval-when (:compile-toplevel :load-toplevel :execute)
       (cond
         ((find-package "BLOCK-DEFPACKAGE-FOO")
          (rename-package "BLOCK-DEFPACKAGE-FOO"
                          "BLOCK-DEFPACKAGE-BAR"))
         ((not (find-package "BLOCK-DEFPACKAGE-BAR"))
          (make-package "BLOCK-DEFPACKAGE-BAR" :use '("CL")))))

     (in-package "BLOCK-DEFPACKAGE-BAR")

     (defun stable-union (bar) bar))
   :block-compile t
   :before-load (lambda ()
                  (delete-package "BLOCK-DEFPACKAGE-BAR")
                  (defpackage block-defpackage-foo (:use :cl)))
   :load t)
  (assert (find-symbol "STABLE-UNION" "BLOCK-DEFPACKAGE-BAR")))

(with-test (:name :block-defpackage-rename-package-symbol-conflict)
  (with-scratch-file (fasl2 "fasl")
    (compile-file "package-test-2.lisp" :output-file fasl2
                                        :block-compile t)
    (delete-package "BAR")
    (with-scratch-file (fasl1 "fasl")
      (compile-file "package-test-1.lisp" :output-file fasl1
                                          :block-compile t)
      (load fasl2)))
  (assert (eq (symbol-package (find-symbol "BAZ" "BAR"))
              (find-package "BAR")))
  (assert (eq (funcall (find-symbol "BAZ" "BAR"))
              :good))
  (delete-package "BAR"))

(with-test (:name :block-defpackage-rename-package-preserve-externals)
  (with-scratch-file (fasl4 "fasl")
    (compile-file "package-test-4.lisp" :output-file fasl4
                                        :block-compile t)
    (delete-package "FOO-NEW")
    (with-scratch-file (fasl3 "fasl")
      (compile-file "package-test-3.lisp" :output-file fasl3
                                          :block-compile t)
      (load fasl4)))
  (assert (eq (nth-value 1 (find-symbol "BAR" "FOO-NEW"))
              :external))
  (delete-package "FOO-NEW"))

(with-test (:name :block-defconstant-then-load-fasl)
  (ctu:file-compile
   ;; test a non-EQL-comparable constant.
   `((defconstant testconstant '(1 2 3 4 5))
     (defun foo ()
       (loop for i in testconstant collect i)))
   :block-compile t
   :before-load (lambda () (unintern (find-symbol "TESTCONSTANT")))
   :load t))

(with-test (:name :block-defconstant-hairy-then-load-fasl)
  (ctu:file-compile
   `((sb-int:defconstant-eqx testconstant2
         (let (list)
           (dotimes (i 5 list) (push i list)))
       #'equal)
     (defun bar ()
       (loop for i in testconstant2 collect i)))
   :block-compile t
   :before-load (lambda () (unintern (find-symbol "TESTCONSTANT2")))
   :load t))

(with-test (:name :block-defconstant-same-component)
  (ctu:file-compile
   `((defun foo-before-defconstant (x) x)
     (defconstant +testconstant3+ '(1 2 3 4 5 6))
     (defun bar-after-defconstant () (foo-before-defconstant +testconstant3+)))
   :block-compile t
   :before-load (lambda () (unintern (find-symbol "+TESTCONSTANT3+")))
   :load t)
  (assert (eq (sb-kernel::fun-code-header #'foo-before-defconstant)
              (sb-kernel::fun-code-header #'bar-after-defconstant))))

(with-test (:name :block-defconstant-hairy-eqness-test)
  (ctu:file-compile
   `((sb-int:defconstant-eqx testconstant4
       (let (list)
         (dotimes (i 5 list) (push i list)))
       #'equal)
     (defun bar () testconstant4))
   :block-compile t
   :load t)
  (assert (eq (bar) (symbol-value 'testconstant4))))

(with-test (:name :block-defconstant-hairy-backq-dumping-test)
  (ctu:file-compile
   `((defconstant +stuff+
       (if (boundp '+stuff+)
           (symbol-value '+stuff+)
           '(0 0)))

     (defvar *backq-stuff*
       `((0 ,(random 10))
         (,@+stuff+ 1 2 (3 . 4))
         (5 6 ,+stuff+))))
   :block-compile t
   :before-load (lambda () (unintern (find-symbol "+STUFF+")))
   :load t)
  (assert (= 0 (first (third (third (symbol-value '*backq-stuff*)))))))

(with-test (:name :block-compile-ftype-proclamation)
  (ctu:file-compile
   `((declaim (ftype function zoo))
     (defun bar1 (x) (zoo x))

     (declaim (ftype (function (t) (values integer &optional)) bar1))

     (defun foo1 (z) (bar1 z)))
   :block-compile t
   :load t)
  (assert (ctype= (caddr (sb-kernel::%simple-fun-type (fdefinition 'foo1)))
                  '(values integer &optional))))

(with-test (:name :block-compile-variable-type-proclamation)
  (ctu:file-compile
   `((defvar *foo*)
     (declaim (type integer *foo*))
     (defun integer-type () *foo*)
     (declaim (type character *foo*))
     (defun character-type () *foo*))
   :block-compile t
   :load t)
  (assert (ctype= (caddr (sb-kernel::%simple-fun-type (fdefinition 'integer-type)))
                  '(values integer &optional)))
  (assert (ctype= (caddr (sb-kernel::%simple-fun-type (fdefinition 'character-type)))
                  '(values character &optional))))

(with-test (:name :block-compile-same-block-references-functional)
  (ctu:file-compile
   `((declaim (inline bar))
     (defun bar (x) (cons x #'bar))

     (defun foo (x) (cons (bar 9) #'bar)))
   :block-compile t
   :load t)
  (let ((value (funcall (fdefinition 'foo) 9)))
    (assert (eq (cdr value) (fdefinition 'bar)))
    (assert (eq (cdar value) (fdefinition 'bar))))
  (assert (eq (sb-kernel::fun-code-header (fdefinition 'foo))
              (sb-kernel::fun-code-header (fdefinition 'bar))))
  ;; Should not have to reference any FDEFN objects, as we are
  ;; compiling in the same block, allowing us to directly reference
  ;; the (simple) function objects directly.
  (assert (null (ctu:find-named-callees #'foo))))

(with-test (:name :block-compile-top-level-closures)
  (ctu:file-compile
   `(;; test forward reference
     (let ((y 0))
       (defun foo2 (x)
         (+ (bar2 x) (incf y))))

     (let ((y 8))
       (defun bar2 (x)
         (+ (incf y) (baz2 (+ 3 x)))))

     ;; test backward reference
     (defun baz2 (x)
       (if (zerop x)
           (foo2 x)
           x)))
   :block-compile t
   :load t)
  ;; Make sure BAZ2 and BAR2 get a component together. FOO2 should not
  ;; share a component with anyone, since it doesn't local call anyone
  ;; with a compatible environment.
  (assert (and (eq (sb-kernel::fun-code-header #'baz2)
                   (sb-kernel::fun-code-header (sb-kernel::%closure-fun #'bar2)))
               (not (eq (sb-kernel::fun-code-header #'baz2)
                        (sb-kernel::fun-code-header (sb-kernel::%closure-fun #'foo2))))))
  (assert (= (baz2 2) 2))
  ;; Test calling the closures behave as expected.
  (assert (= (baz2 0) 13))
  (assert (= (baz2 0) 15))
  (assert (= (baz2 0) 17))
  (assert (= (baz2 0) 19)))

;;; FLET should pose no problem.
(with-test (:name :block-compile-top-level-closures.flet)
  (ctu:file-compile
   `(;; test forward reference
     (flet ((blargh (x)
              (+ x x)))
       (defun boo (x)
         (+ (blargh x) (blargh x))))
     (defun zoo (x)
       (boo x)))
   :block-compile t
   :load t)
  ;; Make sure the defuns all get compiled into the same code
  ;; component.
  (assert (eq (sb-kernel::fun-code-header #'boo)
              (sb-kernel::fun-code-header #'zoo)))
  (assert (null (ctu:find-named-callees #'zoo)))
  (assert (= (zoo 3) 12)))

(with-test (:name :block-compile-top-level-closures.self-call)
  (ctu:file-compile
   `((let ((y 9))
       (defun self-call (x)
         (if (zerop x)
             y
             (+ (decf y) (self-call (1- x)))))))
   :block-compile t
   :load t)
  ;; Test that we can call ourselves.
  (assert (= (self-call 9) 36)))

(with-test (:name :block-compile-top-level-closures.self-call.local-calls)
  ;; Test that we can local call ourselves in the same environment despite
  ;; being a top level closure.
  (assert (not (member 'self-call (ctu:find-named-callees #'self-call)))))

(with-test (:name :block-compile-top-level-closures.same-environment)
  (ctu:file-compile
   `(;; test forward reference
     (let ((y 0))
       (defun foo3 (x)
         (+ (bar3 x) (incf y)))

       (defun bar3 (x)
         (+ (incf y) (baz3 (+ 3 x))))

       ;; test backward reference
       (defun baz3 (x)
         (if (zerop x)
             (foo3 x)
             x))))
   :block-compile t
   :load t)
  ;; Test that calls in the same environment work.
  (assert (= (baz3 0) 6))
  (assert (= (baz3 0) 10))
  (assert (= (baz3 0) 14))
  (assert (= (baz3 0) 18)))

(with-test (:name :block-compile-top-level-closures.same-environment.local-calls
                  :fails-on :sbcl)
  ;; FOO3, BAR3, and BAZ3 are all in the same lexical environment, so
  ;; therefore have compatible runtime environments. Thus they should
  ;; be able to all local call each other. (Not implemented yet.)
  (assert (and (eq (sb-kernel::fun-code-header (sb-kernel::%closure-fun #'baz3))
                   (sb-kernel::fun-code-header (sb-kernel::%closure-fun #'bar3)))
               (eq (sb-kernel::fun-code-header (sb-kernel::%closure-fun #'baz3))
                   (sb-kernel::fun-code-header (sb-kernel::%closure-fun #'foo3)))))
  (assert (null (ctu:find-named-callees #'baz3))))

(with-test (:name :block-compile-top-level-closures.simple-fun-reference)
  (ctu:file-compile
   `((defun simple (chr)
       (char<= #\0 chr #\9))

     (let ((scanner 3))
       (defun closure ()
         (values #'simple (incf scanner)))))
   :block-compile t
   :load t)
  ;; Closure can directly reference the simple fun for SIMPLE.
  (assert (not (member 'simple (ctu:find-named-callees #'closure))))
  (multiple-value-bind (val counter)
      (closure)
    (assert (eq val #'simple))
    (assert (eq counter 4)))
  (multiple-value-bind (val counter)
      (closure)
    (assert (eq val #'simple))
    (assert (eq counter 5))))

(with-test (:name :block-compile-top-level-closures.closure-fun-reference)
  (ctu:file-compile
   `((defun simple1 ()
       #'closure1)

     (let ((scanner 3))
       (defun closure1 (char)
         (values (char<= #\0 char #\9) (incf scanner)))))
   :block-compile t
   :load t)
  ;; SIMPLE1 cannot directly reference the simple fun for CLOSURE1. It
  ;; must go through the closure function object instead through the
  ;; FDEFN. TODO: It might be worthwhile in the future to treat
  ;; top-level closures as load time constants, so that we can
  ;; reference the closure object directly, rather than go through the
  ;; FDEFN.
  (multiple-value-bind (val counter)
      (funcall (simple1) #\a)
    (assert (eq val nil))
    (assert (eq counter 4)))
  (multiple-value-bind (val counter)
      (funcall (simple1) #\1)
    (assert (eq val t))
    (assert (eq counter 5))))

;;;; various tests of EVAL with side effects

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

;;;; Note: this stuff gets loaded in (by LOAD) and is therefore
;;;; evaluated by EVAL, rather than compiled and then loaded; this is
;;;; why this idiom (a sequence of top-level forms) works as a test of
;;;; EVAL.

(cl:in-package :cl-user)

(load "assertoid.lisp")
(use-package "ASSERTOID")

;;; Until sbcl-0.7.9.x, EVAL was not correctly treating LOCALLY,
;;; MACROLET and SYMBOL-MACROLET, which should preserve top-levelness
;;; of their body forms:

;;; LOCALLY
(locally (defstruct locally-struct a (b t)))

(let ((x (make-locally-struct :a 1)))
  (assert (eql (locally-struct-a x) 1))
  (assert (eql (locally-struct-b x) t)))

(locally
  (defmacro locally-macro (x) `(+ ,x 1))
  (assert (= (locally-macro 3) 4)))

(locally (declare (special x))
  (defun locally-special-test ()
    x)
  (defun locally-special-test-aux ()
    (let ((x 1))
      (declare (special x))
      (locally-special-test)))
  (assert (= (locally-special-test-aux) 1)))

;;; MACROLET
(macrolet ()
  (defstruct macrolet-struct a (b t)))

(let ((x (make-macrolet-struct :a 1)))
  (assert (eql (macrolet-struct-a x) 1))
  (assert (eql (macrolet-struct-b x) t)))

(macrolet ()
  (defmacro macrolet-macro (x) `(+ ,x 1))
  (assert (= (macrolet-macro 3) 4)))

(locally (declare (special x))
  (defun macrolet-special-test ()
    x)
  (defun macrolet-special-test-aux ()
    (let ((x 1))
      (declare (special x))
      (macrolet-special-test)))
  (assert (= (macrolet-special-test-aux) 1)))

(macrolet ((foo (x) `(macrolet-bar ,x)))
  (defmacro macrolet-bar (x) `(+ ,x 1))
  (assert (= (foo 1) 2)))

;;; SYMBOL-MACROLET
(symbol-macrolet ()
  (defstruct symbol-macrolet-struct a (b t)))

(let ((x (make-symbol-macrolet-struct :a 1)))
  (assert (eql (symbol-macrolet-struct-a x) 1))
  (assert (eql (symbol-macrolet-struct-b x) t)))

(symbol-macrolet ()
  (defmacro symbol-macrolet-macro (x) `(+ ,x 1))
  (assert (= (symbol-macrolet-macro 3) 4)))

(locally (declare (special x))
  (defun symbol-macrolet-special-test ()
    x)
  (defun symbol-macrolet-special-test-aux ()
    (let ((x 1))
      (declare (special x))
      (symbol-macrolet-special-test)))
  (assert (= (symbol-macrolet-special-test-aux) 1)))

(symbol-macrolet ((foo (symbol-macrolet-bar 1)))
  (defmacro symbol-macrolet-bar (x) `(+ ,x 1))
  (assert (= foo 2)))

;;; Bug reported by Paul Dietz: CONSTANTP on a self-evaluating object
;;; must return T
(assert (constantp (find-class 'symbol)))
(assert (constantp #p""))

;;; DEFPARAMETER must assign a dynamic variable
(let ((var (gensym)))
  (assert (equal (eval `(list (let ((,var 1))
                                (defparameter ,var 2)
                                ,var)
                              ,var))
                 '(1 2))))

;;; Bug 264: SYMBOL-MACROLET did not check for a bound SPECIAL
;;; declaration
(assert (raises-error? (progv '(foo) '(1)
                         (eval '(symbol-macrolet ((foo 3))
                                 (declare (special foo))
                                 foo)))
                       error))

;;; MAKE-PACKAGE (and other &key functions) should signal an error
;;; when given a NIL key.  This is kind of a compiler test really, but
;;; this'll do as a resting place.
(handler-case
    (eval '(make-package "FOO" nil nil))
  (error () :ok)
  (:no-error (c) (error "MAKE-PACKAGE succeeded: ~S" c)))

;;; FUNCTION
(defun function-eq-test ()
  'ok)
(trace function-eq-test)
(assert (eq (eval '(function function-eq-test))
            (funcall (compile nil '(lambda () (function function-eq-test))))))

;;; No extra output, please
(assert (equal ".."
               (with-output-to-string (*standard-output*)
                 (eval '(progn (princ ".") (let ((x 42)) t) (princ "."))))))

;;; success

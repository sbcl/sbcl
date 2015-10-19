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

;;; More CONSTANTP tests
;;;              form                   constantp sb-int:constant-form-value
(dolist (test '((t                      t         t)
                (x                      nil)
                ('x                     t         x)
                (:keyword               t         :keyword)
                (42                     t         42)
                ((if t :ok x)           t         :ok)
                ((if t x :no)           nil)
                ((progn
                   (error "oops")
                   t)                   nil)
                ((progn 1 2 3)          t         3)
                ((block foo :good)      t         :good)
                ((block foo
                   (return-from foo t)) nil)
                ((progv
                     (list (gensym))
                     '(1)
                   (+ 1))               nil)
                ((progv
                     '(x)
                     (list (random 2))
                   x)                   nil)
                ((progv
                     '(x)
                     '(1)
                   (1+ x))              t         2)
                ((progv '(x) '(t)
                   (if x 1 2))          t         1)
                ((unwind-protect 1 nil) t         1)
                ((unwind-protect 1
                   (xxx))               nil)
                ((the integer 1)        t         1)
                ((the integer (+ 1 1))  t         2)
                ((the integer (foo))    nil)
                ((the symbol 1)         nil)
                ((the "bad type" 1)     nil)
                ((multiple-value-prog1
                     (+ 1 1)
                   :nada)               t         2)
                ((multiple-value-prog1
                     :nada
                   (/ 1 0))             nil)
                ((/ 1 0)                nil)
                ((/ 1 1)                t         1)
                ((+ 1 2)                t         3)))
  (destructuring-bind (form c &optional v) test
    (assert (eql (constantp form) c))
    (when c
      (assert (eql v (sb-int:constant-form-value form))))))

;;; DEFPARAMETER must assign a dynamic variable
(let ((var (gensym)))
  (assert (equal (eval `(list (let ((,var 1))
                                (defparameter ,var 2)
                                ,var)
                              ,var))
                 '(1 2))))

;;; Bug 264: SYMBOL-MACROLET did not check for a bound SPECIAL
;;; declaration
(assert-error (progv '(foo) '(1)
                (eval '(symbol-macrolet ((foo 3))
                        (declare (special foo))
                        foo)))
              error)

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

;;; IF
(defun true () t)
(defun false () nil)
(defmacro oops () (throw :oops (list)))
(defun test-eval (ok form) (assert (eq ok (catch :oops (eval form)))))
(test-eval t '(if (false) (oops) t))
(test-eval t '(if (true) t (oops)))
(test-eval nil '(if (not (if (false) t)) (oops)))

;;; TAGBODY

;;; As of SBCL 1.0.1.8, TAGBODY should not accept duplicate go tags,
;;; yet choked on two duplicate tags.  Note that this test asserts a
;;; failure.
(with-test (:name :tagbody-dual-go-tags)
  (progn
    (defun tagbody-dual-go-tags ()
      (restart-case
          (handler-bind ((error (lambda (c)
                                  (declare (ignore c))
                                  (invoke-restart 'NOT-AN-ERROR))))
            (tagbody :A :A) nil)
        (NOT-AN-ERROR () t)))
    (assert (tagbody-dual-go-tags))))

;;; Ensure that NIL is a valid go tag.
(with-test (:name :tagbody-nil-is-valid-tag)
  (progn
    (defun tagbody-nil-is-valid-tag ()
      (tagbody (go NIL) NIL) t)
    (assert (tagbody-nil-is-valid-tag))))

;;; top-level DECLARE is formally undefined, but we want it to raise
;;; an error rather than silently return NIL.
(defvar *scratch*)
(with-test (:name :toplevel-declare)
  (assert-error (eval '(declare (type pathname *scratch*)))))

(with-test (:name (eval :no-compiler-notes))
  (handler-bind ((sb-ext:compiler-note #'error))
    (let ((sb-ext:*evaluator-mode* :compile))
      (eval '(let ((x 42))
              (if nil x)))
      (eval '(let ((* 13))
              (let ((x 42)
                    (y *))
                (declare (optimize speed))
                (+ x y)))))))

(with-test (:name :bug-238)
  (let ((sb-ext:*evaluator-mode* :compile))
    (handler-bind ((sb-ext:compiler-note #'error))
      (eval '(defclass bug-238 () ()))
      (eval '(defmethod bug-238 ((x bug-238) (bug-238 bug-238))
              (call-next-method)))
      (eval '(handler-case
              (with-input-from-string (*query-io* "    no")
                (yes-or-no-p))
              (simple-type-error () 'error)))
      t)))

(with-test (:name :bug-524707 :skipped-on '(not :sb-eval))
  (let ((*evaluator-mode* :interpret)
        (lambda-form '(lambda (x) (declare (fixnum x)) (1+ x))))
    (let ((fun (eval lambda-form)))
      (assert (equal lambda-form (function-lambda-expression fun))))))

(with-test (:name (eval :source-context-in-compiler))
  (let ((noise (with-output-to-string (*error-output*)
                 (let ((*evaluator-mode* :compile))
                   (eval `(defun source-context-test (x) y))))))
    (with-input-from-string (s noise)
      (assert (equal "; in: DEFUN SOURCE-CONTEXT-TEST" (read-line s))))))

(with-test (:name (eval :empty-let-is-not-toplevel))
  (let ((sb-ext:*evaluator-mode* :compile))
    (eval `(let ()
             (defmacro empty-let-is-not-toplevel-x () :macro)
             (defun empty-let-is-not-toplevel-fun ()
               (empty-let-is-not-toplevel-x))))
    (eval `(defun empty-let-is-not-toplevel-x () :fun))
    (assert (eq :fun (empty-let-is-not-toplevel-fun))))
  ;; While at it, test that we get the late binding under
  ;; interpreter mode.
  #+sb-eval
  (let ((sb-ext:*evaluator-mode* :interpret))
    (eval `(let ()
             (defmacro empty-let-is-not-toplevel-x () :macro)
             (defun empty-let-is-not-toplevel-fun ()
               (empty-let-is-not-toplevel-x))))
    (assert (eq :macro (empty-let-is-not-toplevel-fun)))
    (eval `(defun empty-let-is-not-toplevel-x () :fun))
    (assert (eq :fun (empty-let-is-not-toplevel-fun)))))

(with-test (:name (eval function-lambda-expression))
  (assert (equal `(sb-int:named-lambda eval-fle-1 (x)
                    (block eval-fle-1
                      (+ x 1)))
                 (function-lambda-expression
                  (eval `(progn
                           (defun eval-fle-1 (x) (+ x 1))
                           #'eval-fle-1)))))
  (assert (equal `(lambda (x y z) (+ x 1 y z))
                 (function-lambda-expression
                  (eval `(lambda (x y z) (+ x 1 y z)))))))

(with-test (:name (:bug-573747 eval :compile))
  (let ((*out* (make-string-output-stream))
        (sb-ext:*evaluator-mode* :compile))
    (declare (special *out*))
    (assert-error (eval '(declare (print "foo" *out*))))
    (assert (string= (get-output-stream-string *out*) ""))))

(with-test (:name (:bug-573747 eval :interpret))
  (let ((*out* (make-string-output-stream))
        (sb-ext:*evaluator-mode* :interpret))
    (declare (special *out*))
    (assert-error (eval '(declare (print "foo" *out*))))
    (assert (string= (get-output-stream-string *out*) ""))))

;; If the DEFUN macro produces a style-warning, it needs to perform the
;; effect of defun no matter what. The style-warning comes from an EVAL-WHEN,
;; not as part of the execution-time behavior of %DEFUN because it is neither
;; polite nor useful to issue a warning about the co-existence of a DEFSETF
;; and DEFUN SETF after compile-time. The other viable alternative would have
;; been to remove the :EXECUTE situation from the expansion of DEFUN
;; where it signal to warning.
(with-test (:name :handler-case-does-not-bork-defun)
  (defsetf bar set-bar)
  (handler-case (defun (setf bar) (newval x) (declare (ignore newval x)))
    (style-warning () 'drat))
  (assert (fboundp '(setf bar))))
;;; success

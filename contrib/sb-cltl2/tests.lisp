(defpackage :sb-cltl2-tests
  (:use :sb-cltl2 :cl :sb-rt))
(in-package :sb-cltl2-tests)

(rem-all-tests)

(defmacro *x*-value ()
  (declare (special *x*))
  *x*)

(deftest compiler-let.1
    (let ((*x* :outer))
      (compiler-let ((*x* :inner))
        (list *x* (*x*-value))))
  (:outer :inner))

(defvar *expansions* nil)
(defmacro macroexpand-macro (arg)
  (push arg *expansions*)
  arg)

(deftest macroexpand-all.1
    (progn
      (macroexpand-all '(defmethod foo ((x fixnum)) (1+ x)))
      t)
  t)

(deftest macroexpand-all.2
    (let ((*expansions* nil))
      (macroexpand-all '(list (macroexpand-macro 1)
                         (let (macroexpand-macro :no)
                           (macroexpand-macro 2))))
      (remove-duplicates (sort *expansions* #'<)))
  (1 2))

(deftest macroexpand-all.3
    (let ((*expansions* nil))
      (compile nil '(lambda ()
                     (macrolet ((foo (key &environment env)
                                  (macroexpand-all `(bar ,key) env)))
                       (foo
                        (macrolet ((bar (key)
                                     (push key *expansions*)
                                     key))
                          (foo 1))))))
      (remove-duplicates *expansions*))
  (1))

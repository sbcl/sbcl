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

(defun smv (env)
  (multiple-value-bind (expansion macro-p)
      (macroexpand 'srlt env)
    (when macro-p (eval expansion))))
(defmacro testr (&environment env)
  `',(getf (smv env) nil))

(deftest macroexpand-all.4
    (macroexpand-all '(symbol-macrolet ((srlt '(nil zool))) (testr)))
  (symbol-macrolet ((srlt '(nil zool))) 'zool))

(defmacro dinfo (thing &environment env)
  `',(declaration-information thing env))

(macrolet ((def (x)
               `(macrolet ((frob (suffix answer &optional declaration)
                            `(deftest ,(intern (concatenate 'string
                                                            "DECLARATION-INFORMATION."
                                                            (symbol-name ',x)
                                                            suffix))
                               (locally (declare ,@(when declaration
                                                         (list declaration)))
                                 (cadr (assoc ',',x (dinfo optimize))))
                              ,answer)))
                 (frob ".DEFAULT" 1)
                 (frob ".0" 0 (optimize (,x 0)))
                 (frob ".1" 1 (optimize (,x 1)))
                 (frob ".2" 2 (optimize (,x 2)))
                 (frob ".3" 3 (optimize (,x 3)))
                 (frob ".IMPLICIT" 3 (optimize ,x)))))
  (def speed)
  (def safety)
  (def debug)
  (def compilation-speed)
  (def space))

(deftest declaration-information.muffle-conditions.default
  (dinfo sb-ext:muffle-conditions)
  nil)
(deftest declaration-information.muffle-conditions.1
  (locally (declare (sb-ext:muffle-conditions warning))
    (dinfo sb-ext:muffle-conditions))
  warning)
(deftest declaration-information.muffle-conditions.2
  (locally (declare (sb-ext:muffle-conditions warning))
    (locally (declare (sb-ext:unmuffle-conditions style-warning))
      (let ((dinfo (dinfo sb-ext:muffle-conditions)))
        (not
         (not
          (and (subtypep dinfo '(and warning (not style-warning)))
               (subtypep '(and warning (not style-warning)) dinfo)))))))
  t)

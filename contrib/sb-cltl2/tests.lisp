;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; The software is in the public domain and is provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for
;;;; more information.

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
  (let ((junk (dinfo sb-ext:muffle-conditions)))
    (declare (sb-ext:muffle-conditions warning))
    (locally (declare (sb-ext:unmuffle-conditions style-warning))
      (let ((dinfo (dinfo sb-ext:muffle-conditions)))
        (not
         (not
          (and (subtypep dinfo `(or (and warning (not style-warning))
                                    (and ,junk (not style-warning))))
               (subtypep '(and warning (not style-warning)) dinfo)))))))
  t)

;;;; VARIABLE-INFORMATION

(defvar *foo*)

(defmacro var-info (var &environment env)
  (list 'quote (multiple-value-list (variable-information var env))))

(deftest variable-info.global-special/unbound
    (var-info *foo*)
  (:special nil nil))

(deftest variable-info.global-special/unbound/extra-decl
    (locally (declare (special *foo*))
      (var-info *foo*))
  (:special nil nil))

(deftest variable-info.global-special/bound
    (let ((*foo* t))
      (var-info *foo*))
  (:special nil nil))

(deftest variable-info.global-special/bound/extra-decl
    (let ((*foo* t))
      (declare (special *foo*))
      (var-info *foo*))
  (:special nil nil))

(deftest variable-info.local-special/unbound
    (locally (declare (special x))
      (var-info x))
  (:special nil nil))

(deftest variable-info.local-special/bound
    (let ((x 13))
      (declare (special x))
      (var-info x))
  (:special nil nil))

(deftest variable-info.local-special/shadowed
    (let ((x 3))
      (declare (special x))
      x
      (let ((x 3))
        x
        (var-info x)))
  (:lexical t nil))

(deftest variable-info.local-special/shadows-lexical
    (let ((x 3))
      (let ((x 3))
        (declare (special x))
        (var-info x)))
  (:special nil nil))

(deftest variable-info.lexical
    (let ((x 8))
      (var-info x))
  (:lexical t nil))

(deftest variable-info.lexical.type
    (let ((x 42))
      (declare (fixnum x))
      (var-info x))
  (:lexical t ((type . fixnum))))

(deftest variable-info.lexical.type.2
    (let ((x 42))
      (prog1
          (var-info x)
        (locally (declare (fixnum x))
          (assert (plusp x)))))
  (:lexical t nil))

(deftest variable-info.lexical.type.2
    (let ((x 42))
      (locally (declare (fixnum x))
        (var-info x)))
  (:lexical t ((type . fixnum))))

(deftest variable-info.ignore
    (let ((x 8))
      (declare (ignore x))
      (var-info x))
  (:lexical t ((ignore . t))))

(deftest variable-info.symbol-macro/local
    (symbol-macrolet ((x 8))
      (var-info x))
  (:symbol-macro t nil))

(define-symbol-macro my-symbol-macro t)

(deftest variable-info.symbol-macro/global
    (var-info my-symbol-macro)
  (:symbol-macro nil nil))

(deftest variable-info.undefined
    (var-info #:undefined)
  (nil nil nil))

;;;; FUNCTION-INFORMATION

(defmacro fun-info (var &environment env)
  (list 'quote (multiple-value-list (function-information var env))))

(defun my-global-fun (x) x)

(deftest function-info.global/no-ftype
    (fun-info my-global-fun)
  (:function nil nil))

(declaim (ftype (function (cons) (values t &optional)) my-global-fun-2))

(defun my-global-fun-2 (x) x)

(deftest function-info.global/ftype
    (fun-info my-global-fun-2)
  (:function nil ((ftype function (cons) (values t &optional)))))

(defmacro my-macro (x) x)

(deftest function-info.macro
    (fun-info my-macro)
  (:macro nil nil))

(deftest function-info.macrolet
    (macrolet ((thingy () nil))
      (fun-info thingy))
  (:macro t nil))

(deftest function-info.special-form
    (fun-info progn)
  (:special-form  nil nil))

(deftest function-info.notinline/local
    (flet ((x (y) y))
      (declare (notinline x))
      (x 1)
      (fun-info x))
  (:function t ((inline . notinline))))

(declaim (notinline my-notinline))
(defun my-notinline (x) x)

(deftest function-info.notinline/global
    (fun-info my-notinline)
  (:function nil ((inline . notinline))))

(declaim (inline my-inline))
(defun my-inline (x) x)

(deftest function-info.inline/global
    (fun-info my-inline)
  (:function nil ((inline . inline))))

(deftest function-information.known-inline
    (locally (declare (inline identity))
      (fun-info identity))
  (:function nil ((inline . inline)
                  (ftype function (t) (values t &optional)))))


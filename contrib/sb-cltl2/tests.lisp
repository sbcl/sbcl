;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; The software is in the public domain and is provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for
;;;; more information.

(defpackage :sb-cltl2-tests
  (:use :sb-cltl2 :cl :sb-rt :sb-ext :sb-kernel :sb-int))

(in-package :sb-cltl2-tests)

(rem-all-tests)

(defmacro *x*-value ()
  (declare (special *x*))
  *x*)

(deftest compiler-let.1
    (let ((*x* :outer))
      (compiler-let ((*x* :inner))
        (list *x* (*x*-value))))
  ;; See the X3J13 writeup for why the interpreter
  ;; might return (and does return) a different answer.
  #.(if (eq sb-ext:*evaluator-mode* :compile)
        '(:outer :inner)
        '(:inner :inner)))

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
                     (declare (muffle-conditions style-warning))
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

;; Quasiquotation
(deftest macroexpand-all.5
    ;; The second use of (W) is expanded to X, the first is untouched.
    ;; Use EQUALP explicitly because the RT tester's EQUALP-WITH-CASE
    ;; is not quite EQUALP with regard to structures.
    (equalp (macroexpand-all '(macrolet ((w () 'x))
               `(let ((y `(z ,(w) ,,(w)))) (g))))
            '(macrolet ((w () 'x)) `(let ((y `(z ,(w) ,,x))) (g))))
  t)

(deftest macroexpand-all.6
    ;; The subform (AND Z) in (PROGN `(F ,(WHEN X Y) . `(,B ,,(AND Z))))
    ;; is evaluable though unlikely to appear in real code. Unless F is a
    ;; macro, this form when evaluated does not comprise a well-formed sexpr.
    (equalp (macroexpand-all '(progn `(f ,(when x y) . `(,b ,,(and z)))))
            '(progn `(f ,(if x y) . `(,b ,,(the t z)))))
  t)

;;; Symbol macros
(define-symbol-macro global-symbol-macro xxx)

(deftest macroexpand-all.7
    (equalp (macroexpand-all 'global-symbol-macro) 'xxx)
  t)
(deftest macroexpand-all.8
    (symbol-macrolet ((global-symbol-macro yyy))
      (macrolet ((frob (&environment env form)
                   `',(macroexpand-all form env)))
        (equalp (frob global-symbol-macro) 'yyy)))
  t)
(deftest macroexpand-all.9
    (let ((global-symbol-macro 3))
      (macrolet ((frob (&environment env form)
                   `',(macroexpand-all form env)))
        (equalp (frob global-symbol-macro) 'global-symbol-macro)))
  t)
(deftest macroexpand-all.10
    (macrolet ((frob (&environment env form)
                 `',(macroexpand-all form env)))
      (equalp (frob (let ((anything 1)) global-symbol-macro))
              '(let ((anything 1)) xxx)))
  t)
(deftest macroexpand-all.11
    (macrolet ((frob (&environment env form)
                 `',(macroexpand-all form env)))
      (equalp (frob (let ((global-symbol-macro global-symbol-macro))
                      global-symbol-macro))
              '(let ((global-symbol-macro xxx)) global-symbol-macro)))
  t)
(deftest macroexpand-all.12
    (macrolet ((frob (&environment env form)
                 `',(macroexpand-all form env)))
      (equalp (frob (symbol-macrolet ((global-symbol-macro 3))
                      global-symbol-macro))
              '(symbol-macrolet ((global-symbol-macro 3)) 3)))
  t)
(deftest macroexpand-all.13
    (symbol-macrolet ((x y))
      (macrolet ((frob (&environment env form)
                   `',(macroexpand-all form env)))
        (equalp (frob (+ x x))
                '(+ y y))))
  t)
;;;; DECLARATION-INFORMATION

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


(deftest declaration-information.restrict-compiler-policy.1
    (with-compilation-unit (:policy '(optimize) :override t)
      (restrict-compiler-policy 'speed 3)
      (eval '(cadr (assoc 'speed (dinfo optimize)))))
  3)

;; This usage is esoteric, and the expected answer differs based on whether the
;; code is interpreted or compiled. Compiling RESTRICT-COMPILER-POLICY doesn't
;; actually do anything to affect the compiler since it is not a toplevel form
;; in an eval-when. (I suspect that it wouldn't normally be used this way)
;; But the interpreter calls it, which has an immediate visible effect.
(deftest declaration-information.restrict-compiler-policy.2
    (with-compilation-unit (:policy '(optimize) :override t)
      (restrict-compiler-policy 'speed 3)
      (locally (declare (optimize (speed 2)))
        (cadr (assoc 'speed (dinfo optimize)))))
  ;; sb-rt doesn't eval the "expected result" form.
  #.(if (eq sb-ext:*evaluator-mode* :compile) 2 3))

(deftest declaration-information.restrict-compiler-policy.3
    (locally (declare (optimize (speed 2)))
      (with-compilation-unit (:policy '(optimize) :override t)
        (restrict-compiler-policy 'speed 3)
        (cadr (assoc 'speed (dinfo optimize)))))
  #.(if (eq sb-ext:*evaluator-mode* :compile) 2 3))

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


(declaim (declaration fubar))

(deftest declaration-information.declaration
    (if (member 'fubar (declaration-information 'declaration)) 'yay)
  yay)

;;;; VARIABLE-INFORMATION

(defvar *foo*)

(defmacro var-info (var &environment env)
  (list 'quote (multiple-value-list (variable-information var env))))

(deftest variable-info.global-special/unbound
    (var-info *foo*)
  (:special nil nil))

(defvar *variable-info.global-special/unbound.deprecation*)
(declaim (sb-ext:deprecated :early ("SBCL" "1.2.3")
          (variable *variable-info.global-special/unbound.deprecation* :replacement foo)))
(deftest variable-info.global-special/unbound.deprecation
    (var-info *variable-info.global-special/unbound.deprecation*)
  (:special nil ((sb-ext:deprecated . (:state        :early
                                       :since        ("SBCL" "1.2.3")
                                       :replacements (foo))))))

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

(deftest variable-info.lexical.type.3
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

(declaim (sb-ext:deprecated :early ("SBCL" "1.2.3")
          (variable *variable-info.undefined.deprecation* :replacement foo)))
(deftest variable-info.undefined.deprecation
    (var-info *variable-info.undefined.deprecation*)
  (nil nil ((sb-ext:deprecated . (:state        :early
                                  :since        ("SBCL" "1.2.3")
                                  :replacements (foo))))))

(declaim (global this-is-global))
(deftest global-variable
    (var-info this-is-global)
  (:global nil nil))

(defglobal this-is-global-too 42)
(deftest global-variable.2
    (var-info this-is-global-too)
  (:global nil ((always-bound . t))))

(sb-alien:define-alien-variable "errno" sb-alien:int)
(deftest alien-variable
    (var-info errno)
  (:alien nil nil))

(defglobal *variable-info.global.deprecation* 1)
(declaim (sb-ext:deprecated :early ("SBCL" "1.2.3")
          (variable *variable-info.global.deprecation* :replacement foo)))
(deftest variable-info.global.deprecation
    (var-info *variable-info.global.deprecation*)
  (:global nil ((always-bound . t)
                (sb-ext:deprecated . (:state        :early
                                      :since        ("SBCL" "1.2.3")
                                      :replacements (foo))))))

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
  (:function nil ((ftype . (function (cons) (values t &optional))))))

(defun function-info.global.deprecation ())
(declaim (sb-ext:deprecated :early "1.2.3"
          (function function-info.global.deprecation :replacement foo)))
(deftest function-info.global.deprecation
    (fun-info function-info.global.deprecation)
  (:function nil ((sb-ext:deprecated . (:state        :early
                                        :since        (nil "1.2.3")
                                        :replacements (foo))))))

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

(deftest function-information.ftype
    (flet ((foo (x) x))
      (declare (ftype (sfunction (integer) integer) foo))
      (fun-info foo))
  (:function
   t
   ((ftype function (integer) (values integer &optional)))))

;;;;; AUGMENT-ENVIRONMENT

(defmacro ct (form &environment env)
  (let ((toeval `(let ((lexenv (quote ,env)))
                   ,form)))
    `(quote ,(eval toeval))))


(deftest augment-environment.variable1
    (multiple-value-bind (kind local alist)
        (variable-information
         'x
         (augment-environment nil :variable (list 'x) :declare '((type integer x))))
      (list kind local (cdr (assoc 'type alist))))
  (:lexical t integer))

(defvar *foo*)

(deftest augment-environment.variable2
    (identity (variable-information '*foo* (augment-environment nil :variable '(*foo*))))
  :lexical)

(deftest augment-environment.variable3
    (identity (variable-information 'foo (augment-environment nil :variable '(foo))))
  :lexical)

(deftest augment-environment.variable.special1
    (identity (variable-information 'x (augment-environment nil :variable '(x) :declare '((special x)))))
  :special)

(deftest augment-environment.variable.special12
    (locally (declare (special x))
      (ct
       (variable-information
        'x
        (identity (augment-environment lexenv :variable '(x))))))
  :lexical)

(deftest augment-environment.variable.special13
    (let* ((e1 (augment-environment nil :variable '(x) :declare '((special x))))
           (e2 (augment-environment e1  :variable '(x))))
      (identity (variable-information 'x e2)))
  :lexical)

(deftest augment-environment.variable.special.mask
    (let* ((e1 (augment-environment nil :variable '(x) :declare '((ignore x))))
           (e2 (augment-environment e1  :variable '(x))))
      (assoc 'ignore
             (nth 2 (multiple-value-list
                     (variable-information 'x e2)))))
  nil)

(deftest augment-environment.variable.ignore
    (variable-information
     'x
     (augment-environment nil
                          :variable '(x)
                          :declare  '((ignore x))))
  :lexical
  t
  ((ignore . t)))

(deftest augment-environment.function
    (function-information
     'foo
     (augment-environment nil
                          :function '(foo)
                          :declare  '((ftype (sfunction (integer) integer) foo))))
  :function
  t
  ((ftype function (integer) (values integer &optional))))


(deftest augment-environment.macro
    (macroexpand '(mac feh)
                 (augment-environment
                  nil
                  :macro (list (list 'mac #'(lambda (form benv)
                                              (declare (ignore env))
                                              `(quote ,form ,form ,form))))))
  (quote (mac feh) (mac feh) (mac feh))
  t)

(deftest augment-environment.symbol-macro
    (macroexpand 'sym
                 (augment-environment
                  nil
                  :symbol-macro (list (list 'sym '(foo bar baz)))))
  (foo bar baz)
  t)

(deftest augment-environment.macro2
    (eval (macroexpand '(newcond
                         ((= 1 2) 'foo)
                         ((= 1 1) 'bar))
                       (augment-environment nil :macro (list (list 'newcond (macro-function 'cond))))))
  bar)


(deftest augment-environment.nest
    (let ((x 1))
      (ct
       (let* ((e (augment-environment lexenv :variable '(y))))
         (list
          (variable-information 'x e)
          (variable-information 'y e)))))
  (:lexical :lexical))

(deftest augment-environment.nest2
    (symbol-macrolet ((x "x"))
      (ct
       (let* ((e (augment-environment lexenv :variable '(y))))
         (list
          (macroexpand 'x e)
          (variable-information 'y e)))))
  ("x" :lexical))

(deftest augment-environment.symbol-macro-var
    (let ((e (augment-environment
              nil
              :symbol-macro (list (list 'sym '(foo bar baz)))
              :variable '(x))))
      (list (macroexpand 'sym e)
            (variable-information 'x e)))
  ((foo bar baz)
   :lexical))

;;;;; DEFINE-DECLARATION

(defmacro third-value (form)
  (sb-int::with-unique-names (a b c)
    `(multiple-value-bind (,a ,b ,c) ,form
       (declare (ignore ,a ,b))
       ,c)))

(deftest define-declaration.declare
    (progn
      (define-declaration zaphod (spec env)
        (declare (ignore env))
        (values :declare (cons 'zaphod spec)))
      (locally (declare (zaphod beblebrox))
         (locally (declare (zaphod and ford))
           (ct (declaration-information 'zaphod lexenv)))))
  (zaphod and ford))


(deftest define-declaration.declare2
    (progn
      (define-declaration zaphod (spec env)
        (declare (ignore env))
        (values :declare (cons 'zaphod spec)))
      (locally
           (declare (zaphod beblebrox)
                    (special x))
         (ct (declaration-information 'zaphod lexenv))))
  (zaphod beblebrox))

(deftest define-declaration.variable
    (progn
      (define-declaration vogon (spec env)
        (declare (ignore env))
        (values :variable `((,(cadr spec) vogon-key vogon-value))))
      (locally (declare (vogon poetry))
        (ct
         (assoc 'vogon-key
                (third-value
                 (variable-information
                  'poetry
                  lexenv))))))
  (vogon-key . vogon-value))


(deftest define-declaration.variable.special
    (progn
      (define-declaration vogon (spec env)
        (declare (ignore env))
        (values :variable `((,(cadr spec) vogon-key vogon-value))))
      (let (x)
        (declare (vogon x))
        (declare (special x))
        (ct
         (assoc 'vogon-key
                (third-value
                 (variable-information 'x lexenv))))))
  (vogon-key . vogon-value))

(deftest define-declaration.variable.special2
    (progn
      (define-declaration vogon (spec env)
        (declare (ignore env))
        (values :variable `((,(cadr spec) vogon-key vogon-value))))
      (let (x)
        (declare (special x))
        (declare (vogon x))
        (ct
         (assoc 'vogon-key
                (third-value
                 (variable-information 'x lexenv))))))
  (vogon-key . vogon-value))

(deftest define-declaration.variable.mask
    (progn
      (define-declaration vogon (spec env)
        (declare (ignore env))
        (values :variable `((,(cadr spec) vogon-key vogon-value))))
      (let (x)
        (declare (vogon x))
        (let (x)
          (ct
           (assoc
            'vogon-key
            (third (multiple-value-list (variable-information 'x lexenv))))))))
  nil)

(deftest define-declaration.variable.macromask
    (progn
      (define-declaration vogon (spec env)
        (declare (ignore env))
        (values :variable `((,(cadr spec) vogon-key vogon-value))))
      (let (x)
        (declare (vogon x))
        (symbol-macrolet ((x 42))
          (ct
           (assoc
            'vogon-key
            (third (multiple-value-list (variable-information 'x lexenv))))))))
  nil)

(deftest define-declaration.variable.macromask2
    (progn
      (define-declaration vogon (spec env)
        (declare (ignore env))
        (values :variable `((,(cadr spec) vogon-key vogon-value))))
      (symbol-macrolet ((x 42))
        (declare (vogon x))
        (list
         (let (x)
           (ct
            (assoc
             'vogon-key
             (third (multiple-value-list (variable-information 'x lexenv))))))
         (ct
          (assoc
           'vogon-key
           (third (multiple-value-list (variable-information 'x lexenv))))))))
  (nil (vogon-key . vogon-value)))

(deftest define-declaration.variable.mask2
    (progn
      (define-declaration vogon-a (spec env)
        (declare (ignore env))
        (values :variable `((,(cadr spec) vogon-key a))))
      (define-declaration vogon-b (spec env)
        (declare (ignore env))
        (values :variable `((,(cadr spec) vogon-key b))))
      (let (x)
        (declare (vogon-a x))
        (let (x)
          (declare (vogon-b x)))
        (ct
         (assoc
          'vogon-key
          (third (multiple-value-list (variable-information 'x lexenv)))))))
  (vogon-key . a))

(deftest define-declaration.variable.specialmask
    (progn
      (define-declaration vogon (spec env)
        (declare (ignore env))
        (values :variable `((,(cadr spec) vogon-key vogon-value))))
      (locally
          (declare (vogon *foo*))
        (let (*foo*)
          (ct
           (assoc
            'vogon-key
            (third (multiple-value-list (variable-information '*foo* lexenv))))))))
  (vogon-key . vogon-value))



(deftest define-declaration.function
    (progn
      (define-declaration sad (spec env)
        (declare (ignore env))
        (values :function `((,(cadr spec) emotional-state sad))))
      (locally (declare (zaphod beblebrox))
        (locally (declare (sad robot))
          (ct
           (assoc 'emotional-state
                  (third-value (function-information
                                'robot
                                lexenv)))))))
  (emotional-state . sad))

(deftest define-declaration.function.lexical
    (progn
      (define-declaration sad (spec env)
        (declare (ignore env))
        (values :function `((,(cadr spec) emotional-state sad))))
      (flet ((robot nil))
        (locally (declare (sad robot))
          (ct
           (assoc 'emotional-state
                  (third-value (function-information
                                'robot
                                lexenv)))))))
  (emotional-state . sad))


(deftest define-declaration.function.lexical2
    (progn
      (define-declaration sad (spec env)
        (declare (ignore env))
        (values :function `((,(cadr spec) emotional-state sad))))
      (labels ((robot nil))
        (declare (sad robot))
        (ct
         (assoc 'emotional-state
                (third-value (function-information
                              'robot
                              lexenv))))))
  (emotional-state . sad))

(deftest define-declaration.function.mask
    (progn
      (define-declaration sad (spec env)
        (declare (ignore env))
        (values :function `((,(cadr spec) emotional-state sad))))
      (labels ((robot nil))
        (declare (sad robot))
        (labels ((robot nil))
          (ct
           (assoc 'emotional-state
                  (third-value (function-information
                                'robot
                                lexenv)))))))
  nil)


(deftest define-declaration.function.mask2
    (progn
      (define-declaration sad (spec env)
        (declare (ignore env))
        (values :function `((,(cadr spec) emotional-state sad))))
      (locally
          (declare (sad robot))
        (labels ((robot nil))
          (ct
           (assoc 'emotional-state
                  (third-value (function-information
                                'robot
                                lexenv)))))))
  nil)

(deftest define-declaration.function2
    (progn
      (define-declaration happy (spec env)
        (declare (ignore env))
        (values :function `((,(cadr spec) emotional-state happy))))
      (locally (declare (zaphod beblebrox))
        (locally (declare (sad robot))
          (locally (declare (happy robot))
            (ct
             (assoc 'emotional-state
                    (third-value (function-information
                                  'robot
                                  lexenv))))))))
  (emotional-state . happy))

(deftest macroexpand-all.special-binding
    (let ((form '(macrolet ((v (x &environment env)
                             (sb-cltl2:variable-information x env)))
                  (let* ((x :foo)
                         (y (v x)))
                    (declare (special x))
                    (list y (v x))))))
      (list (eval form)
            (eval (sb-cltl2:macroexpand-all form))))
  ((:special :special) (:special :special)))

(deftest macroexpand-all.symbol-macro-shadowed
    (let ((form '(macrolet ((v (x &environment env)
                             (macroexpand x env)))
                  (symbol-macrolet ((x :bad))
                    (let* ((x :good)
                           (y (v x)))
                      y)))))
      (list (eval form)
            (eval (sb-cltl2:macroexpand-all form))))
  (:good :good))

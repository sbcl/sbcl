(in-package :sb-cltl2)

(defun macroexpand-all (form &optional environment)
  (let ((sb-walker::*walk-form-expand-macros-p* t))
    (sb-walker:walk-form
     form environment
     (lambda (subform context env)
       (acond ((and (eq context :eval)
                    (listp subform)
                    (symbolp (car subform))
                    (get (car subform) :partial-macroexpander))
               ;; The partial expander must return T as its second value
               ;; if it wants to stop the walk.
               (funcall it subform env))
              (t
               subform))))))

;; Given EXPR, the argument to an invocation of Quasiquote macro, macroexpand
;; evaluable subforms of EXPR using ENV. A subform is evaluable if all
;; preceding occurrences of #\` have been "canceled" by a comma.
;; DEPTH counts the nesting and should not be supplied by external callers.
(defun %quasiquoted-macroexpand-all (expr env &optional (depth 0))
  (flet ((quasiquote-p (x)
           (and (listp x) (eq (car x) 'quasiquote) (singleton-p (cdr x))))
         (recurse (x)
           (%quasiquoted-macroexpand-all x env depth)))
    (if (atom expr)
        (cond ((simple-vector-p expr) (map 'vector #'recurse expr))
              ((comma-p expr)
               (unquote (if (> depth 1)
                            (%quasiquoted-macroexpand-all
                             (comma-expr expr) env (1- depth))
                            (macroexpand-all (comma-expr expr) env))
                        (comma-kind expr)))
              (t expr))
        (if (quasiquote-p expr)
            (list 'quasiquote
                  (%quasiquoted-macroexpand-all (second expr) env (1+ depth)))
            (let (result)
              (loop
               (push (recurse (pop expr)) result)
               (when (or (atom expr) (quasiquote-p expr))
                 (return (nreconc result (recurse expr))))))))))

(setf (get 'quasiquote :partial-macroexpander)
      (lambda (form env)
        (destructuring-bind (arg) (cdr form) ; sanity-check the shape
          (declare (ignore arg))
          (values (%quasiquoted-macroexpand-all form env) t))))

#|

;; Another example that some people might find useful.

(defun macroexpand-decls+forms (body env) ; a bit of a kludge, but it works
  (mapcar (lambda (x)
            (if (and (listp x) (eq (car x) 'declare))
                x
                (macroexpand-all x env)))
          body))

(setf (get 'dotimes :partial-macroexpander)
      (lambda (form env)
        (destructuring-bind ((var count &optional (result nil result-p))
                             &body body) (cdr form)
            (values `(dotimes (,var ,(macroexpand-all count env)
                               ,@(if result-p
                                     (list (macroexpand-all result env))))
                       ,@(macroexpand-decls+forms body env))
                    t))))

(macroexpand-all '(macrolet ((hair (x) `(car ,x)))
                   (dotimes (i (bar)) (foo i (hair baz)) l))))
=>
(MACROLET ((HAIR (X)
             `(CAR ,X)))
  (DOTIMES (I (BAR)) (FOO I (CAR BAZ)) L))

instead of

(MACROLET ((HAIR (X)
             `(CAR ,X)))
  (BLOCK NIL
    (LET ((I 0) (#:COUNT699 (BAR)))
      (DECLARE (TYPE UNSIGNED-BYTE I)
               (TYPE INTEGER #:COUNT699))
      (TAGBODY
        (GO #:G701)
       #:G700
        (TAGBODY (FOO I (CAR BAZ)) L)
        (LET* ()
          (MULTIPLE-VALUE-BIND (#:NEW702) (1+ I) (PROGN (SETQ I #:NEW702) NIL)))
       #:G701
        (IF (>= I #:COUNT699)
            NIL
            (PROGN (GO #:G700)))
        (RETURN-FROM NIL (PROGN NIL))))))
|#

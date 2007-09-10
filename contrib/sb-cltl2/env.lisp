;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; The software is in the public domain and is provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for
;;;; more information.

(in-package :sb-cltl2)

#| TODO:
declaration-information
augment-environment
define-declaration
(map-environment)
|#

(declaim (ftype (sfunction (symbol &optional (or null lexenv))
                           (values (member nil :function :macro :special-form)
                                   boolean
                                   list))
                function-information))
(defun function-information (name &optional env)
  "Return information about the function NAME in the lexical environment ENV.
Note that the global function binding may differ from the local one.

This function returns three values. The first indicates the type of
function definition or binding:

  NIL
    There is no apparent definition for NAME.

  :FUNCTION
    NAME refers to a function.

  :MACRO
    NAME refers to a macro.

  :SPECIAL-FORM
    NAME refers to a special operator. If the name refers to both a
    macro and a special operator, the macro takes precedence.

The second value is true if NAME is bound locally.

The third value is an alist describing the declarations that apply to
the function NAME. Standard declaration specifiers that may appear in
CARS of the alist include:

  DYNAMIC-EXTENT
    If the CDR is T, NAME has been declared DYNAMIC-EXTENT. If the CDR
    is NIL, the alist element may be omitted.

  INLINE
    The CDR is one of the symbols INLINE, NOTINLINE, or NIL, to
    indicate if the function has been declared INLINE or NOTINLINE. If
    the CDR is NIL the alist element may be omitted.

  FTYPE
    The CDR is the type specifier associated with NAME, or the symbol
    FUNCTION if there is functional type declaration or proclamation
    associated with NAME. If the CDR is FUNCTION the alist element may
    be omitted."
  (let* ((*lexenv* (or env (make-null-lexenv)))
         (fun (lexenv-find name funs))
         binding localp ftype dx inlinep)
    (etypecase fun
      (sb-c::leaf
       (let ((env-type (or (lexenv-find fun type-restrictions)
                           *universal-fun-type*)))
         (setf binding :function
               ftype (if (eq :declared (sb-c::leaf-where-from fun))
                         (type-intersection (sb-c::leaf-type fun)
                                            env-type)
                         env-type)
               dx (sb-c::leaf-dynamic-extent fun))
         (etypecase fun
           (sb-c::functional
            (setf localp t
                  inlinep (sb-c::functional-inlinep fun)))
           (sb-c::defined-fun
            ;; Inlined known functions.
            (setf localp nil
                  inlinep (sb-c::defined-fun-inlinep fun))))))
      (cons
       (setf binding :macro
             localp t))
      (null
       (case (info :function :kind name)
         (:macro
          (setf binding :macro
                localp nil))
         (:special-form
          (setf binding :special-form
                localp nil))
         (:function
          (setf binding :function
                localp nil
                ftype (when (eq :declared (info :function :where-from name))
                        (info :function :type name))
                inlinep (info :function :inlinep name))))))
    (values binding
            localp
            (let (alist)
              (when (and ftype (neq *universal-fun-type* ftype))
                (push (cons 'ftype (type-specifier ftype)) alist))
              (ecase inlinep
                ((:inline :maybe-inline) (push (cons 'inline 'inline) alist))
                (:notinline (push (cons 'inline 'notinline) alist))
                ((nil)))
              (when dx (push (cons 'dynamic-extent t) alist))
              alist))))

(declaim (ftype (sfunction
                 (symbol &optional (or null lexenv))
                 (values (member nil :special :lexical :symbol-macro :constant)
                         boolean
                         list))
                variable-information))
(defun variable-information (name &optional env)
  "Return information about the variable name VAR in the lexical environment ENV.
Note that the global binding may differ from the local one.

This function returns three values. The first indicated the type of the variable
binding:

  NIL
    There is no apparent binding for NAME.

  :SPECIAL
    NAME refers to a special variable.

  :LEXICAL
    NAME refers to a lexical variable.

  :SYMBOL-MACRO
    NAME refers to a symbol macro.

  :CONSTANT
    NAME refers to a named constant defined using DEFCONSTANT, or NAME
    is a keyword.

The second value is true if NAME is bound locally. This is currently
always NIL for special variables, although arguably it should be T
when there is a lexically apparent binding for the special variable.

The third value is an alist describind the declarations that apply to
the function NAME. Standard declaration specifiers that may appear in
CARS of the alist include:

  DYNAMIC-EXTENT
    If the CDR is T, NAME has been declared DYNAMIC-EXTENT. If the CDR
    is NIL, the alist element may be omitted.

  IGNORE
    If the CDR is T, NAME has been declared IGNORE. If the CDR is NIL,
    the alist element may be omitted.

  TYPE
    The CDR is the type specifier associated with NAME, or the symbol
    T if there is explicit type declaration or proclamation associated
    with NAME. The type specifier may be equivalent to or a supertype
    of the original declaration. If the CDR is T the alist element may
    be omitted."
  (let* ((*lexenv* (or env (make-null-lexenv)))
         (var (lexenv-find name vars))
         binding localp dx ignorep type)
    (etypecase var
      (sb-c::leaf
       (let ((env-type (or (lexenv-find var type-restrictions)
                           *universal-type*)))
         (setf type (if (eq :declared (sb-c::leaf-where-from var))
                        (type-intersection (sb-c::leaf-type var)
                                           env-type)
                        env-type)
               dx (sb-c::leaf-dynamic-extent var)))
       (etypecase var
         (sb-c::lambda-var
          (setf binding :lexical
                localp t
                ignorep (sb-c::lambda-var-ignorep var)))
         ;; FIXME: IGNORE doesn't make sense for specials or constants
         ;; -- though it is _possible_ to declare them ignored, but
         ;; we don't keep the information around.
         (sb-c::global-var
          (setf binding :special
                ;; FIXME: Lexically apparent binding or not?
                localp nil))
         (sb-c::constant
          (setf binding :constant
                localp nil))))
      (cons
       (setf binding :symbol-macro
             localp t))
       (null
        (let ((global-type (info :variable :type name))
              (kind (info :variable :kind name)))
          (setf binding (case kind
                          (:macro :symbol-macro)
                          (:global nil)
                          (t kind))
                type (if (eq *universal-type* global-type)
                         nil
                         global-type)
                localp nil))))
    (values binding
            localp
            (let (alist)
              (when ignorep (push (cons 'ignore t) alist))
              (when (and type (neq *universal-type* type))
                (push (cons 'type (type-specifier type)) alist))
              (when dx (push (cons 'dynamic-extent t) alist))
              alist))))

(declaim (ftype (sfunction (symbol &optional (or null lexenv)) t)
                declaration-information))
(defun declaration-information (declaration-name &optional env)
  (let ((env (or env (make-null-lexenv))))
    (case declaration-name
      (optimize
       (let ((policy (sb-c::lexenv-policy env)))
         (collect ((res))
           (dolist (name sb-c::*policy-qualities*)
             (res (list name (cdr (assoc name policy)))))
           (loop for (name . nil) in sb-c::*policy-dependent-qualities*
                 do (res (list name (sb-c::policy-quality policy name))))
           (res))))
      (sb-ext:muffle-conditions
       (car (rassoc 'muffle-warning
                    (sb-c::lexenv-handled-conditions env))))
      (t (error "Unsupported declaration ~S." declaration-name)))))

(defun parse-macro (name lambda-list body &optional env)
  (declare (ignore env))
  (with-unique-names (whole environment)
    (multiple-value-bind (body decls)
        (parse-defmacro lambda-list whole body name
                        'parse-macro
                        :environment environment)
      `(lambda (,whole ,environment)
         ,@decls
         ,body))))

(defun enclose (lambda-expression &optional env)
  (let ((env (if env
                 (sb-c::make-restricted-lexenv env)
                 (make-null-lexenv))))
    (compile-in-lexenv nil lambda-expression env)))

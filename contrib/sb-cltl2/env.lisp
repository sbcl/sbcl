;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; The software is in the public domain and is provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for
;;;; more information.

(in-package :sb-cltl2)

#| TODO:
(map-environment)
|#


(defvar *null-lexenv* (make-null-lexenv))

(defun augment-environment
    (env &key variable symbol-macro function macro declare)
  "Create a new lexical environment by augmenting ENV with new information.

   VARIABLE
     is a list of symbols to introduce as new variable bindings.

   SYMBOL-MACRO
     is a list symbol macro bindings of the form (name definition).

   MACRO
     is a list of macro definitions of the form (name definition), where
     definition is a function of two arguments (a form and an environment).

   FUNCTION
     is a list of symbols to introduce as new local function bindings.

   DECLARE
     is a list of declaration specifiers. Declaration specifiers attach to the
     new variable or function bindings as if they appeared in let, let*, flet
     or labels form. For example:

      (augment-environment env :variable '(x) :declare '((special x)))

     is like

      (let (x) (declare (special x)) ....)

     but

      (augment-environment (augment-environment env :variable '(x))
                           :declare '((special x)))

     is like

       (let (x) (locally (declare (special x))) ...)
"
  (setq env (copy-structure (sb-c::coerce-to-lexenv env)))
  (collect ((lvars)
            (clambdas))
    (unless (or variable symbol-macro function macro declare)
      (return-from augment-environment env))

    ;; a null policy is used to identify a null lexenv
    (when (sb-c::null-lexenv-p env)
      (setf (sb-c::lexenv-%policy env) sb-c::*policy*))

    (when macro
      (setf (sb-c::lexenv-funs env)
            (nconc
             (loop for (name def) in macro
                collect (cons name (cons 'sb-sys::macro def)))
             (sb-c::lexenv-funs env))))

    (when symbol-macro
      (setf (sb-c::lexenv-vars env)
            (nconc
             (loop for (name def) in symbol-macro
                collect (cons name (cons 'sb-sys::macro def)))
             (sb-c::lexenv-vars env))))

    (dolist (name variable)
      (lvars (sb-c::make-lambda-var :%source-name name)))

    (dolist (name function)
      (clambdas
       (sb-c::make-lambda
        :lexenv *null-lexenv*
        :%source-name name
        :allow-instrumenting nil)))

    (when declare
      ;; process-decls looks in *lexenv* policy to decide what warnings to print
      (let ((*lexenv* *null-lexenv*))
        (setq env (sb-c::process-decls
                   (list `(declare ,@declare))
                   (lvars) (clambdas) :lexenv env :context nil))))

    (when function
      (setf (sb-c::lexenv-funs env)
            (nconc
             (loop for name in function for lambda in (clambdas)
                  collect (cons name lambda))
             (sb-c::lexenv-funs env))))

    (when variable
      (setf (sb-c::lexenv-vars env)
            (nconc
             (loop for name in variable for lvar in (lvars)
                collect
                (cons name
                      ;; If one of the lvars is declared special then
                      ;; process-decls will set it's specvar.
                      (if (sb-c::lambda-var-specvar lvar)
                          (sb-c::lambda-var-specvar lvar)
                          lvar)))
             (sb-c::lexenv-vars env))))

    env))

;;; Retrieve the user-supplied (from define-declaration) pairs for a
;;; function or a variable from a lexical environment.
;;;
;;; KEYWORD should be :function or :variable, VAR should be a
;;; function or variable name, respectively.
(defun extra-pairs (keyword var binding env)
  (when env
    (let ((ret nil))
      (dolist (entry (sb-c::lexenv-user-data env))
        (destructuring-bind
              (entry-keyword entry-var entry-binding &rest entry-cons)
            entry
          (when (and (eq keyword entry-keyword)
                     (typecase binding
                       (sb-c::global-var
                        (and (eq var entry-var)
                             (typecase entry-binding
                               (sb-c::global-var t)
                               (sb-c::lambda-var
                                (sb-c::lambda-var-specvar entry-binding))
                               (null t)
                               (t nil))))
                       (t
                        (eq binding entry-binding))))
            (push entry-cons ret))))
      (nreverse ret))))

(defun maybe-deprecation-entry (info)
  (when info
    (with-accessors ((state sb-int:deprecation-info-state)
                     (software sb-int:deprecation-info-software)
                     (version sb-int:deprecation-info-version)
                     (replacements sb-int:deprecation-info-replacements))
        info
      (list (cons 'sb-ext:deprecated
                  (list :state state
                        :since (list software version)
                        :replacements replacements))))))

;;; Retrieve the user-supplied (from define-declaration) value for
;;; the declaration with the given NAME
(defun extra-decl-info (name env)
  (when env
    (dolist (entry (sb-c::lexenv-user-data env))
      (when (and (eq :declare (car entry))
                 (eq name (cadr entry)))
        (return-from extra-decl-info (cddr entry))))
    nil))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro list-cons-when (test car cdr)
    `(when ,test
       (list (cons ,car ,cdr)))))

(declaim (ftype (sfunction ((or symbol cons) &optional lexenv-designator)
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
    be omitted.

  SB-EXT:DEPRECATED
    \(SBCL specific)
    The CDR is a plist containing the following properties

      :STATE ( :EARLY | :LATE | :FINAL )
        Use of :EARLY deprecated functions signals a STYLE-WARNING at
        compile-time.

        Use of :LATE deprecated functions signals a full WARNING at
        compile-time.

        Use of :FINAL deprecated functions signals a full WARNING at
        compile-time and an error at runtime.

      :SINCE (SOFTWARE VERSION)
        VERSION is a string designating the version since which the
        function has been deprecated. SOFTWARE is NIL or the name of
        the software to which VERSION refers, e.g. \"SBCL\" for
        deprecated functions in SBCL.

      :REPLACEMENTS REPLACEMENTS
        When this property is present, REPLACEMENTS is a list of
        symbols naming functions that should be used instead of the
        deprecated function.

In addition to these declarations defined using DEFINE-DECLARATION may
appear."
  (let* ((*lexenv*
           (typecase env
             #+sb-fasteval
             (sb-interpreter:basic-env (sb-interpreter::lexenv-from-env env))
             (null (make-null-lexenv))
             (t env)))
         (fun (lexenv-find name funs))
         binding localp ftype dx inlinep)
    (etypecase fun
      (sb-c::leaf
       (let ((env-type (or (lexenv-find fun type-restrictions)
                           *universal-fun-type*)))
         (setf binding :function
               ftype (type-intersection (sb-c::leaf-type fun) env-type)
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
       ;; FIXME: we document above that :MACRO trumps :SPECIAL-FORM
       ;; but that is clearly untrue.
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
                        (proclaimed-ftype name))
                inlinep (info :function :inlinep name))))))
    (values binding
            localp
            (nconc (ecase inlinep
                     ((:inline :maybe-inline)
                      (list '(inline . inline)))
                     (:notinline
                      (list '(inline . notinline)))
                     ((nil)))
                   (list-cons-when (and ftype (neq *universal-fun-type* ftype))
                     'ftype (type-specifier ftype))
                   (list-cons-when dx 'dynamic-extent t)
                   ;; FIXME: a local name shadowing a deprecated global
                   ;; wrongly returns deprecation info.
                   (maybe-deprecation-entry
                    (info :function :deprecated name))
                   (extra-pairs :function name fun *lexenv*)))))

(declaim (ftype (sfunction
                 (symbol &optional lexenv-designator)
                 (values (member nil :special :lexical :symbol-macro :constant :global :alien)
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

  :GLOBAL
    NAME refers to a global variable. (SBCL specific extension.)

  :ALIEN
    NAME refers to an alien variable. (SBCL specific extension.)

The second value is true if NAME is bound locally. This is currently
always NIL for special variables, although arguably it should be T
when there is a lexically apparent binding for the special variable.

The third value is an alist describing the declarations that apply to
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
    be omitted.

  SB-EXT:ALWAYS-BOUND
    \(SBCL specific)
    If CDR is T, NAME has been declared as SB-EXT:ALWAYS-BOUND

  SB-EXT:DEPRECATED
    \(SBCL specific)
    The CDR is a plist containing the following properties

      :STATE ( :EARLY | :LATE | :FINAL )
        Use of :EARLY deprecated variables signals a STYLE-WARNING at
        compile-time.

        Use of :LATE deprecated variables signals a full WARNING at
        compile-time.

        Use of :FINAL deprecated variables signals a full WARNING at
        compile-time and an error at runtime.

      :SINCE (SOFTWARE VERSION)
        VERSION is a string designating the version since which the
        variable has been deprecated. SOFTWARE is NIL or the name of
        the software to which VERSION refers, e.g. \"SBCL\" for
        deprecated variables in SBCL.

      :REPLACEMENTS REPLACEMENTS
        When this property is present, REPLACEMENTS is a list of
        symbols naming variables that should be used instead of the
        deprecated variable.

In addition to these declarations defined using DEFINE-DECLARATION may
appear."
  (let* ((*lexenv* (sb-c::coerce-to-lexenv env))
         (kind (info :variable :kind name))
         (var (lexenv-find name vars))
         binding localp dx ignorep type)
    (etypecase var
      (sb-c::leaf
       (let ((env-type (or (lexenv-find var type-restrictions)
                           *universal-type*)))
         (setf type (type-intersection (sb-c::leaf-type var) env-type)
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
          (setf binding (if (eq :global kind)
                            :global
                            :special)
                ;; FIXME: Lexically apparent binding or not for specials?
                localp nil))
         (sb-c::constant
          (setf binding :constant
                localp nil))))
      (cons
       (setf binding :symbol-macro
             localp t))
       (null
        (let ((global-type (info :variable :type name)))
          (setf binding (case kind
                          (:macro :symbol-macro)
                          (:unknown nil)
                          (t kind))
                type (if (eq *universal-type* global-type)
                         nil
                         global-type)
                localp nil))))
    (values binding
            localp
            (nconc (list-cons-when ignorep 'ignore t)
                   (list-cons-when (and type (neq *universal-type* type))
                     'type (type-specifier type))
                   (list-cons-when dx 'dynamic-extent t)
                   (list-cons-when (info :variable :always-bound name)
                     'sb-ext:always-bound t)
                   (maybe-deprecation-entry
                    (info :variable :deprecated name))
                   (extra-pairs :variable name var *lexenv*)))))

;;; Unlike policy-related declarations which the interpeter itself needs
;;; for correct operation of some macros, muffled conditions are irrelevant,
;;; since warnings are not signaled much, if at all.
;;; This is even more useless than env-package-locks.
;;; It's only for SB-CLTL2, and not tested in the least.
#+sb-fasteval
(defun compute-handled-conditions (env)
  (named-let recurse ((env env))
    (let ((result (acond ((sb-interpreter::env-parent env)
                          (compute-handled-conditions it))
                         (t sb-c::*handled-conditions*))))
      (sb-interpreter::do-decl-spec
          (declaration (sb-interpreter::env-declarations env) result)
        (let ((f (case (car declaration)
                  (sb-ext:muffle-conditions
                    #'sb-c::process-muffle-conditions-decl)
                  (sb-ext:unmuffle-conditions
                   #'sb-c::process-unmuffle-conditions-decl))))
          (when f
            (setq result (funcall f declaration result))))))))

(declaim (ftype (sfunction (symbol &optional lexenv-designator) t)
                declaration-information))
(defun declaration-information (declaration-name &optional env)
  "Return information about declarations named by DECLARATION-NAME.

If DECLARATION-NAME is OPTIMIZE return a list who's entries are of the
form \(QUALITY VALUE).

If DECLARATION-NAME is DECLARATION return a list of declaration names that
have been proclaimed as valid.

If DECLARATION-NAME is a name that has defined via DEFINE-DECLARATION return a
user defined value.

If DECLARATION-NAME is SB-EXT:MUFFLE-CONDITIONS return a type specifier for
the condition types that have been muffled."
  (let ((env (or env (make-null-lexenv))))
    (case declaration-name
      (optimize
       ;; CLtL2-mandated behavior:
       ;; "The returned list always contains an entry for each of the standard
       ;; qualities and for each of the implementation-specific qualities"
       (sb-c::policy-to-decl-spec
        (typecase env
          #+sb-fasteval
          (sb-interpreter:basic-env (sb-interpreter:env-policy env))
          (t (sb-c::lexenv-policy env)))
        nil t))
      (sb-ext:muffle-conditions
       (let ((handled-conditions
              (typecase env
                #+sb-fasteval
                (sb-interpreter:basic-env (compute-handled-conditions env))
                (t (sb-c::lexenv-handled-conditions env)))))
         (sb-int:awhen (car (rassoc 'muffle-warning handled-conditions))
           (sb-kernel:type-specifier it))))
      (declaration
       (copy-list sb-c::*recognized-declarations*))
      (t (if (info :declaration :handler declaration-name)
             (extra-decl-info
              declaration-name
              (typecase env
                #+sb-fasteval
                (sb-interpreter:basic-env (sb-interpreter::lexenv-from-env env))
                (t env)))
             (error "Unsupported declaration ~S." declaration-name))))))


(defun parse-macro (name lambda-list body &optional env)
  "Process a macro definition of the kind that might appear in a DEFMACRO form
into a lambda expression of two variables: a form and an environment. The
lambda expression will parse its form argument, binding the variables in
LAMBDA-LIST appropriately, and then execute BODY with those bindings in
effect."
  (declare (ignore env))
  (make-macro-lambda (if (and name (symbolp name)) (string name) "PARSE-MACRO")
                     lambda-list body 'parse-macro name))

(defun enclose (lambda-expression &optional environment)
  "Return a function consistent with LAMBDA-EXPRESSION in ENVIRONMENT: the
lambda expression is allowed to reference the declarations and macro
definitions in ENVIRONMENT, but consequences are undefined if lexical
variables, functions, tags or any other run-time entity defined in ENVIRONMENT
is referred to by the expression."
  (let ((env (if environment
                 (sb-c::make-restricted-lexenv environment)
                 (make-null-lexenv))))
    (compile-in-lexenv nil lambda-expression env)))

;;; Add a bit of user-data to a lexenv.
;;;
;;; If KIND is :declare then DATA should be of the form
;;;    (declaration-name . value)
;;; If KIND is :variable then DATA should be of the form
;;;     (variable-name key value)
;;; If KIND is :function then DATA should be of the form
;;;     (function-name key value)
;;;
;;; PD-VARS and PD-FVARS are are the vars and fvars arguments
;;; of the process-decls call that called this function.
(defun update-lexenv-user-data (env kind data pd-vars pd-fvars)
  (let ((user-data (sb-c::lexenv-user-data env)))
    ;; user-data looks like this:
    ;; ((:declare d . value)
    ;;  (:variable var binding key . value)
    ;;  (:function var binding key . value))
    (let ((*lexenv* env))
      (ecase kind
        (:variable
         (loop
            for (name key value) in data
            for binding1 = (sb-c::find-in-bindings pd-vars name)
            for binding  =  (if binding1 binding1 (lexenv-find name vars))
            do (push (list* :variable name binding key value) user-data)))
        (:function
         (loop
            for (name key value) in data
            for binding1 = (find name pd-fvars :key #'sb-c::leaf-source-name :test #'equal)
            for binding = (if binding1 binding1 (lexenv-find name funs))
            do (push (list* :function name binding key value) user-data)))
        (:declare
         (destructuring-bind (decl-name . value) data
           (push (list* :declare decl-name value) user-data)))))
    (sb-c::make-lexenv :default env :user-data user-data)))

(defmacro define-declaration (decl-name lambda-list &body body)
  "Define a handler for declaration specifiers starting with DECL-NAME.

The function defined by this macro is called with two arguments: a declaration
specifier and a environment. It must return two values. The first value must
be :VARIABLE, :FUNCTION, or :DECLARE.

If the first value is :VARIABLE or :FUNCTION then the second value should be a
list of elements of the form (BINDING-NAME KEY VALUE). conses (KEY . VALUE)
will be added to the alist returned by:

   (function-information binding-name env)

 or

   (variable-information binding-name env)

If the first value is :DECLARE then the second value should be a
cons (DECL-NAME . VALUE). VALUE will be returned by:

   (declaration-information decl-name env)
"
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (proclaim '(declaration ,decl-name))
     (flet ((func ,lambda-list
              ,@body))
       (setf
        (info :declaration :handler ',decl-name)
        (lambda (lexenv spec pd-vars pd-fvars)
          (multiple-value-bind (kind data) (func spec lexenv)
            (update-lexenv-user-data lexenv kind data pd-vars pd-fvars)))))))

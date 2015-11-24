;;;; An interpreting EVAL

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!EVAL")

;; (declaim (optimize (speed 3) (debug 1) (safety 1)))

;;; Values used for marking specials/macros/etc in environments.
(defvar *special* (gensym "SPECIAL"))
(defvar *macro* (gensym "MACRO"))
(defvar *symbol-macro* (gensym "SYMBOL-MACRO"))
(defvar *not-present* (gensym "NOT-PRESENT"))

(define-condition interpreted-program-error (program-error simple-condition sb!impl::encapsulated-condition)
  ()
  (:report (lambda (condition stream)
             (if (slot-boundp condition 'condition)
                 (progn
                   (format stream "Error evaluating a form:~% ~A"
                           (sb!impl::encapsulated-condition condition)))
                 (format stream "Error evaluating a form:~% ~?"
                         (simple-condition-format-control condition)
                         (simple-condition-format-arguments condition))))))

;;; ANSI defines that program syntax errors should be of type
;;; PROGRAM-ERROR.  Therefore...
(define-condition arg-count-program-error (sb!kernel::arg-count-error
                                           program-error)
  ())

;;; FIXME: This macro is not clearly better than plain destructuring-bind.
;;;
;;; First of all, it's ridiculous that the error message says
;;;   "error while parsing arguments to PROGRAM-DESTRUCTURING-BIND".
;;; The user doesn't care what the macro was that parsed the arguments
;;; to the special operator. It should instead say
;;;   "... while parsing arguments to special operator <foo>"
;;;
;;; Second, it is naive to think that existence of this macro suffices
;;; to always signal an INTEPRETED-PROGRAM-ERROR and not just ERROR.
;;; e.g. (LET ((X 1)) . JUNK) binds the &BODY variable to the non-list JUNK.
;;; To fix the general problem, every use of DOLIST and other things
;;; would have to be replaced by something like SB-PCL::DOLIST-CAREFULLY.
;;; Similarly for ((&REST BINDINGS) &BODY BODY) wherein it's not even
;;; obvious that BINDINGS is enforced by the macro to be a list. [lp#1469275]

;; OAOOM? (see destructuring-bind.lisp)
(defmacro program-destructuring-bind (lambda-list arg-list &body body)
  ;; (:EVAL) is a dummy context. We don't have enough information to
  ;; show the operator name without using debugger internals to get the stack frame.
  ;; It would be easier to make the name an argument to this macro.
  `(sb!int:binding* ,(sb!c::expand-ds-bind lambda-list arg-list t nil '(:eval))
     ,@body))

(defun ip-error (format-control &rest format-arguments)
  (error 'interpreted-program-error
         :format-control format-control
         :format-arguments format-arguments))

(defmacro nconc-2 (a b)
  (let ((tmp (gensym))
        (tmp2 (gensym)))
    `(let ((,tmp ,a)
           (,tmp2 ,b))
       (if ,tmp
           (progn (setf (cdr (last ,tmp)) ,tmp2) ,tmp)
           ,tmp2))))

;;; Construct a compiler LEXENV from the same data that's used for
;;; creating an interpreter ENV. This is needed for example when
;;; passing the environment to macroexpanders or when compiling an
;;; interpreted function.
(defun fabricate-new-native-environment (old-lexenv new-funs new-expanders
                                         new-vars new-symbol-expansions
                                         declarations)
  (labels ((to-native-funs (binding)
             ;; Non-macroexpander function entries are irrelevant for
             ;; the LEXENV. If we're using the LEXENV for
             ;; macro-expansion any references to local non-macro
             ;; function bindings are undefined behaviour. If we're
             ;; compiling an interpreted function, a lexical environment
             ;; with non-macro functions will be too hairy to compile.
             (if (eq (cdr binding) *macro*)
                 (cons (car binding)
                       (cons 'sb!sys:macro
                             (cdr (assoc (car binding) new-expanders))))
                 (cons (car binding)
                       :bogus)))
           (to-native-vars (binding)
             ;; And likewise for symbol macros.
             (if (eq (cdr binding) *symbol-macro*)
                 (cons (car binding)
                       (cons 'sb!sys:macro
                             (cdr (assoc (car binding) new-symbol-expansions))))
                 (cons (car binding)
                       :bogus))))
    (let ((lexenv (sb!c::internal-make-lexenv
                   (nconc-2 (mapcar #'to-native-funs new-funs)
                            (sb!c::lexenv-funs old-lexenv))
                   (nconc-2 (mapcar #'to-native-vars new-vars)
                            (sb!c::lexenv-vars old-lexenv))
                   nil nil nil nil nil
                   (sb!c::lexenv-handled-conditions old-lexenv)
                   (sb!c::lexenv-disabled-package-locks old-lexenv)
                   (sb!c::lexenv-policy old-lexenv) ; = (OR %POLICY *POLICY*)
                   (sb!c::lexenv-user-data old-lexenv)
                   old-lexenv)))
      (dolist (declaration declarations)
        (unless (consp declaration)
          (ip-error "malformed declaration specifier ~S in ~S"
                    declaration (cons 'declare declarations)))
        (case (car declaration)
          ((optimize)
           (setf (sb!c::lexenv-%policy lexenv)
                 (copy-structure (sb!c::lexenv-%policy lexenv)))
           (dolist (element (cdr declaration))
             (multiple-value-bind (quality value)
                 (if (not (consp element)) ; FIXME: OAOOM w/'proclaim'
                     (values element 3)
                     (program-destructuring-bind (quality value)
                         element
                       (values quality value)))
               (sb!int:acond
                ((sb!c::policy-quality-name-p quality)
                 (sb!c::alter-policy (sb!c::lexenv-%policy lexenv)
                                     sb!int:it value))
                (t (warn "ignoring unknown optimization quality ~S in ~S"
                         quality (cons 'declare declarations)))))))
          (muffle-conditions
           (setf (sb!c::lexenv-handled-conditions lexenv)
                 (sb!c::process-muffle-conditions-decl
                  declaration
                  (sb!c::lexenv-handled-conditions lexenv))))
          (unmuffle-conditions
           (setf (sb!c::lexenv-handled-conditions lexenv)
                 (sb!c::process-unmuffle-conditions-decl
                  declaration
                  (sb!c::lexenv-handled-conditions lexenv))))
          ((disable-package-locks sb!ext:enable-package-locks)
           (setf (sb!c::lexenv-disabled-package-locks lexenv)
                 (sb!c::process-package-lock-decl
                  declaration
                  (sb!c::lexenv-disabled-package-locks lexenv))))))
      lexenv)))

(defstruct (env
             (:constructor %make-env
                           (parent vars funs expanders symbol-expansions
                            tags blocks declarations native-lexenv)))
  parent
  vars
  funs
  expanders
  symbol-expansions
  tags
  blocks
  declarations
  native-lexenv)

(defun make-env (&key parent vars funs expanders
                 symbol-expansions tags blocks declarations)
  (%make-env parent
             (append vars (env-vars parent))
             (append funs (env-funs parent))
             (append expanders (env-expanders parent))
             (append symbol-expansions (env-symbol-expansions parent))
             (nconc-2 tags (env-tags parent))
             (nconc-2 blocks (env-blocks parent))
             declarations
             (fabricate-new-native-environment (env-native-lexenv parent)
                                               funs expanders
                                               vars symbol-expansions
                                               declarations)))

(defun make-null-environment ()
  (%make-env nil nil nil nil nil nil nil nil
             (sb!c::internal-make-lexenv
              nil nil
              nil nil nil nil nil nil nil
              sb!c::*policy*
              nil nil)))

;;; Augment ENV with a special or lexical variable binding
(declaim (inline push-var))
(defun push-var (name value env)
  (push (cons name value) (env-vars env))
  (push (cons name :bogus) (sb!c::lexenv-vars (env-native-lexenv env))))

;;; Augment ENV with a local function binding
(declaim (inline push-fun))
(defun push-fun (name value calling-env body-env)
  (when (fboundp name)
    (let ((sb!c:*lexenv* (env-native-lexenv calling-env)))
      (program-assert-symbol-home-package-unlocked
       :eval name "binding ~A as a local function")))
  (push (cons name value) (env-funs body-env))
  (push (cons name :bogus) (sb!c::lexenv-funs (env-native-lexenv body-env))))

(sb!int:def!method print-object ((env env) stream)
  (print-unreadable-object (env stream :type t :identity t)))

(macrolet ((define-get-binding (name accessor &key (test '#'eq))
             ;; A macro, sadly, because an inline function here is
             ;; "too hairy"
             `(defmacro ,name (symbol env)
                `(assoc ,symbol (,',accessor ,env) :test ,',test))))
  (define-get-binding get-binding env-vars)
  (define-get-binding get-fbinding env-funs :test #'equal)
  (define-get-binding get-expander-binding env-expanders)
  (define-get-binding get-symbol-expansion-binding env-symbol-expansions)
  (define-get-binding get-tag-binding env-tags :test #'eql)
  (define-get-binding get-block-binding env-blocks))

;;; Return a list of all symbols that are declared special in the
;;; declarations listen in DECLS.
(defun declared-specials (decls)
  (let ((specials nil))
    (dolist (decl decls)
      (when (eql (car decl) 'special)
        (dolist (var (cdr decl))
          (push var specials))))
    specials))

;;; Given a list of variables that should be marked as special in an
;;; environment, return the appropriate binding forms to be given
;;; to MAKE-ENV.
(defun special-bindings (specials env)
  (mapcar #'(lambda (var)
              (let ((sb!c:*lexenv* (env-native-lexenv env)))
                (program-assert-symbol-home-package-unlocked
                 :eval var "declaring ~A special"))
              (cons var *special*))
          specials))

;;; Return true if SYMBOL has been declared special either globally
;;; or is in the DECLARED-SPECIALS list.
(defun specialp (symbol declared-specials)
  (let ((type (sb!int:info :variable :kind symbol)))
    (cond
      ((eq type :constant)
       ;; Horrible place for this, but it works.
       (ip-error "Can't bind constant symbol: ~S" symbol))
      ((eq type :global)
       ;; Ditto...
       (ip-error "Can't bind a global variable: ~S" symbol))
      ((eq type :special) t)
      ((member symbol declared-specials :test #'eq)
       t)
      (t nil))))

(defun binding-name (binding)
  (if (consp binding) (first binding) binding))
(defun binding-value (binding)
  (if (consp binding) (second binding) nil))
(defun supplied-p-parameter (spec)
  (if (consp spec) (third spec) nil))
(defun keyword-name (spec)
  (if (consp spec)
      (if (consp (first spec))
          (second (first spec))
          (first spec))
      spec))
(defun keyword-key (spec)
  (if (consp spec)
      (if (consp (first spec))
          (first (first spec))
          (intern (symbol-name (first spec)) "KEYWORD"))
      (intern (symbol-name spec) "KEYWORD")))
(defun keyword-default-value (spec)
  (if (consp spec) (second spec) nil))

;;; Given a list of ARGUMENTS and a LAMBDA-LIST, return two values:
;;;   * An alist[*] mapping the required parameters of the function to
;;;     the corresponding argument values
;;;   * An alist mapping the keyword, optional and rest parameters of
;;;     the function to the corresponding argument values (if supplied)
;;;     or to the parameter's default expression (if not). Supplied-p
;;;     parameters and aux variables are handled in a similar manner.
;;;
;;; For example given the argument list of (1 2) and the lambda-list of
;;; (A &OPTIONAL (B A) (C (1+ A))), we'd return the values
;;; (A . '1) and ((B . '2) (C . (1+ A))).
;;;
;;; Used only for implementing calls to interpreted functions.
(defun parse-arguments (arguments lambda-list)
  (multiple-value-bind (llks required optional rest keyword aux)
      ;; FIXME: shouldn't this just pass ":silent t" ?
      (handler-bind ((style-warning #'muffle-warning))
        (sb!int:parse-lambda-list lambda-list))
    (let* ((original-arguments arguments)
           (rest-p (not (null rest)))
           (rest (car rest))
           (keyword-p (sb!int:ll-kwds-keyp llks))
           (allow-other-keys-p (sb!int:ll-kwds-allowp llks))
           (arguments-present (length arguments))
           (required-length (length required))
           (optional-length (length optional))
           (non-keyword-arguments (+ required-length optional-length))
           (optionals-present (- (min non-keyword-arguments arguments-present)
                                 required-length))
           (keywords-present-p (> arguments-present non-keyword-arguments))
           (let-like-bindings nil)
           (let*-like-bindings nil))
      (cond
        ((< arguments-present required-length)
         (ip-error "~@<Too few arguments in ~S to satisfy lambda list ~S.~:@>"
                   arguments lambda-list))
        ((and (not (or rest-p keyword-p)) keywords-present-p)
         (ip-error "~@<Too many arguments in ~S to satisfy lambda list ~S.~:@>"
                   arguments lambda-list))
        ((and keyword-p keywords-present-p
              (oddp (- arguments-present non-keyword-arguments)))
         (ip-error "~@<Odd number of &KEY arguments in ~S for ~S.~:@>"
                   arguments lambda-list)))
      (dotimes (i required-length)
        (push (cons (pop required) (pop arguments)) let-like-bindings))
      (do ((optionals-parsed 0 (1+ optionals-parsed)))
          ((null optional))
        (let ((this-optional (pop optional))
              (supplied-p (< optionals-parsed optionals-present)))
          (push (cons (binding-name this-optional)
                      (if supplied-p
                          (list 'quote (pop arguments))
                          (binding-value this-optional)))
                let*-like-bindings)
          (when (supplied-p-parameter this-optional)
            (push (cons (supplied-p-parameter this-optional)
                        (list 'quote supplied-p))
                  let*-like-bindings))))
      (let ((keyword-plist arguments))
        (when rest-p
          (push (cons rest (list 'quote keyword-plist)) let*-like-bindings))
        (when keyword-p
          (unless (or allow-other-keys-p
                      (getf keyword-plist :allow-other-keys))
            (loop for (key value) on keyword-plist by #'cddr doing
                  (when (and (not (eq key :allow-other-keys))
                             (not (member key keyword :key #'keyword-key)))
                    (ip-error "~@<Unknown &KEY argument ~S in ~S for ~S.~:@>"
                              key original-arguments lambda-list))))
          (dolist (keyword-spec keyword)
            (let ((supplied (getf keyword-plist (keyword-key keyword-spec)
                                  *not-present*)))
              (push (cons (keyword-name keyword-spec)
                          (if (eq supplied *not-present*)
                              (keyword-default-value keyword-spec)
                              (list 'quote supplied)))
                    let*-like-bindings)
              (when (supplied-p-parameter keyword-spec)
                (push (cons (supplied-p-parameter keyword-spec)
                            (list 'quote (not (eq supplied *not-present*))))
                      let*-like-bindings))))))
      (when aux
        (do ()
            ((null aux))
          (let ((this-aux (pop aux)))
            (push (cons (binding-name this-aux)
                        (binding-value this-aux))
                  let*-like-bindings))))
      (values (nreverse let-like-bindings) (nreverse let*-like-bindings)))))

;;; Evaluate LET*-like (sequential) bindings.
;;;
;;; Given an alist of BINDINGS, evaluate the value form of the first
;;; binding in ENV, generate an augmented environment with a binding
;;; of the variable to the value in ENV, and then evaluate the next
;;; binding form. Once all binding forms have been handled, END-ACTION
;;; is funcalled with the final environment.
;;;
;;; SPECIALS is a list of variables that have a bound special declaration.
;;; These variables (and those that have been declaimed as special) are
;;; bound as special variables.
(defun eval-next-let*-binding (bindings specials env end-action)
  (flet ((maybe-eval (exp)
           ;; Pick off the easy (QUOTE x) case which is very common
           ;; due to function calls.  (see PARSE-ARGUMENTS)
           (if (and (consp exp) (eq (car exp) 'quote))
               (second exp)
               (%eval exp env))))
    (if bindings
        (let* ((binding-name (car (car bindings)))
               (binding-value (cdr (car bindings)))
               (new-env (make-env :parent env)))
          (if (specialp binding-name specials)
              (progv
                  (list binding-name)
                  (list (maybe-eval binding-value))
                ;; Mark the variable as special in this environment
                (push-var binding-name *special* new-env)
                (eval-next-let*-binding
                 (cdr bindings) specials new-env end-action))
              (progn
                (push-var binding-name (maybe-eval binding-value) new-env)
                (eval-next-let*-binding
                 (cdr bindings) specials new-env end-action))))
        (funcall end-action env))))

;;; Create a new environment based on OLD-ENV by adding the variable
;;; bindings in BINDINGS to it, and call FUNCTION with the new environment
;;; as the only parameter. DECLARATIONS are the declarations that were
;;; in a source position where bound declarations for the bindings could
;;; be introduced.
;;;
;;; FREE-SPECIALS-P controls whether all special declarations should
;;; end cause the variables to be marked as special in the environment
;;; (when true), or only bound declarations (when false). Basically
;;; it'll be T when handling a LET, and NIL when handling a call to an
;;; interpreted function.
(defun call-with-new-env (old-env bindings declarations
                          free-specials-p function)
  (let* ((specials (declared-specials declarations))
         (dynamic-vars nil)
         (dynamic-values nil))
    ;; To check for package-lock violations
    (special-bindings specials old-env)
    (flet ((generate-binding (binding)
             (if (specialp (car binding) specials)
                 ;; If the variable being bound is globally special or
                 ;; there's a bound special declaration for it, record it
                 ;; in DYNAMIC-VARS / -VALUES separately:
                 ;;   * To handle the case of FREE-SPECIALS-P == T more
                 ;;     cleanly.
                 ;;   * The dynamic variables will be bound with PROGV just
                 ;;     before funcalling
                 (progn
                   (push (car binding) dynamic-vars)
                   (push (cdr binding) dynamic-values)
                   nil)
                 ;; Otherwise it's a lexical binding, and the value
                 ;; will be recorded in the environment.
                 (list binding))))
      (let ((new-env (make-env
                      :parent old-env
                      :vars (mapcan #'generate-binding bindings)
                      :declarations declarations)))
        (dolist (special (if free-specials-p specials dynamic-vars))
          (push-var special *special* new-env))
        (if dynamic-vars
            (progv dynamic-vars dynamic-values
              (funcall function new-env))
            ;; When there are no specials, the PROGV would be a no-op,
            ;; but it's better to elide it completely, since the
            ;; funcall is then in tail position.
            (funcall function new-env))))))

;;; Create a new environment based on OLD-ENV by binding the argument
;;; list ARGUMENTS to LAMBDA-LIST, and call FUNCTION with the new
;;; environment as argument. DECLARATIONS are the declarations that
;;; were in a source position where bound declarations for the
;;; bindings could be introduced.
(defun call-with-new-env-full-parsing
    (old-env lambda-list arguments declarations function)
  (multiple-value-bind (let-like-bindings let*-like-binding)
      (parse-arguments arguments lambda-list)
    (let ((specials (declared-specials declarations))
          var-specials free-specials)
      ;; Separate the bound and free special declarations
      (dolist (special specials)
        (if (or (member special let-like-bindings :key #'car)
                (member special let*-like-binding :key #'car))
            (push special var-specials)
            (push special free-specials)))
      ;; First introduce the required parameters into the environment
      ;; with CALL-WITH-NEW-ENV
      (call-with-new-env
       old-env let-like-bindings declarations nil
       #'(lambda (env)
           ;; Then deal with optionals / keywords / etc.
           (eval-next-let*-binding
            let*-like-binding var-specials env
            #'(lambda (env)
                ;; And now that we have evaluated all the
                ;; initialization forms for the bindings, add the free
                ;; special declarations to the environment. To see why
                ;; this is the right thing to do (instead of passing
                ;; FREE-SPECIALS-P == T to CALL-WITH-NEW-ENV),
                ;; consider:
                ;;
                ;;   (eval '(let ((*a* 1))
                ;;     (declare (special *a*))
                ;;     (let ((*a* 2))
                ;;       (funcall (lambda (&optional (b *a*))
                ;;                  (declare (special *a*))
                ;;                  (values b *a*))))))
                ;;
                ;; *A* should be special in the body of the lambda, but
                ;; not when evaluating the default value of B.
                (dolist (special free-specials)
                  (push-var special *special* env))
                (funcall function env))))))))

;;; Set the VALUE of the binding (either lexical or special) of the
;;; variable named by SYMBOL in the environment ENV.
(defun set-variable (symbol value env)
  (let ((binding (get-binding symbol env)))
    (if binding
        (cond
          ((eq (cdr binding) *special*)
           (setf (symbol-value symbol) value))
          ((eq (cdr binding) *symbol-macro*)
           (error "Tried to set a symbol-macrolet!"))
          (t (setf (cdr binding) value)))
        (case (sb!int:info :variable :kind symbol)
          (:macro (error "Tried to set a symbol-macrolet!"))
          (:alien (let ((type (sb!int:info :variable :alien-info symbol)))
                    (setf (sb!alien::%heap-alien type) value)))
          (t
           (let ((type (sb!c::info :variable :type symbol)))
             (when type
               (let ((type-specifier (type-specifier type)))
                 (unless (typep value type-specifier)
                   (error 'type-error
                          :datum value
                          :expected-type type-specifier))))
             (setf (symbol-value symbol) value)))))))

;;; Retrieve the value of the binding (either lexical or special) of
;;; the variable named by SYMBOL in the environment ENV. For symbol
;;; macros the expansion is returned instead.
(defun get-variable (symbol env)
  (let ((binding (get-binding symbol env)))
    (if binding
        (cond
          ((eq (cdr binding) *special*)
           (values (symbol-value symbol) :variable))
          ((eq (cdr binding) *symbol-macro*)
           (values (cdr (get-symbol-expansion-binding symbol env))
                   :expansion))
          (t (values (cdr binding) :variable)))
        (case (sb!int:info :variable :kind symbol)
          (:macro (values (macroexpand-1 symbol) :expansion))
          (:alien (values (sb!alien-internals:alien-value symbol) :variable))
          (t (values (symbol-value symbol) :variable))))))

;;; Retrieve the function/macro binding of the symbol NAME in
;;; environment ENV. The second return value will be :MACRO for macro
;;; bindings, :FUNCTION for function bindings.
(defun get-function (name env)
  (let ((binding (get-fbinding name env)))
    (if binding
        (cond
          ((eq (cdr binding) *macro*)
           (values (cdr (get-expander-binding name env)) :macro))
          (t (values (cdr binding) :function)))
        (cond
          ((and (symbolp name) (macro-function name))
           (values (macro-function name) :macro))
          (t (values (%coerce-name-to-fun name) :function))))))

;;; Return true if EXP is a lambda form.
(defun lambdap (exp)
  (case (car exp)
    ((lambda sb!int:named-lambda) t)))

;;; Split off the declarations (and the docstring, if
;;; DOC-STRING-ALLOWED is true) from the actual forms of BODY.
;;; Returns three values: the cons in BODY containing the first
;;; non-header subform, the docstring, and a list of the declarations.
;;;
;;; FIXME: The name of this function is somewhat misleading. It's not
;;; used just for parsing the headers from lambda bodies, but for all
;;; special forms that have attached declarations.
(defun parse-lambda-headers (body &key doc-string-allowed)
  (loop with documentation = nil
        with declarations = nil
        with lambda-list = :unspecified
        for form on body do
        (cond
          ((and doc-string-allowed (stringp (car form)))
           (if (cdr form)               ; CLHS 3.4.11
               (if documentation
                   (ip-error "~@<Duplicate doc string ~S.~:@>" (car form))
                   (setf documentation (car form)))
               (return (values form documentation declarations))))
          ((and (consp (car form)) (eql (caar form) 'declare))
           (when (eq lambda-list :unspecified)
             (dolist (item (cdar form))
               (when (and (consp item) (eq (car item) 'sb!c::lambda-list))
                 (setq lambda-list (second item)))))
           (setf declarations (append declarations (cdar form))))
          (t (return (values form documentation declarations lambda-list))))
        finally (return (values nil documentation declarations lambda-list))))

;;; Create an interpreted function from the lambda-form EXP evaluated
;;; in the environment ENV.
(defun eval-lambda (exp env)
  (sb!int:binding* (((name rest)
                     (case (car exp)
                      ((lambda) (values nil (cdr exp)))
                      ((sb!int:named-lambda) (values (second exp) (cddr exp)))))
                    (lambda-list (car rest))
                    ((forms documentation declarations debug-lambda-list)
                     (parse-lambda-headers (cdr rest) :doc-string-allowed t)))
       (make-interpreted-function :name name
                                  :lambda-list lambda-list
                                  :debug-lambda-list
                                  (if (eq debug-lambda-list :unspecified)
                                      lambda-list debug-lambda-list)
                                  :env env :body forms
                                  :documentation documentation
                                  :source-location (sb!c::make-definition-source-location)
                                  :declarations declarations)))

(defun eval-progn (body env)
  (let ((previous-exp nil))
    (dolist (exp body)
      (if previous-exp
          (%eval previous-exp env))
      (setf previous-exp exp))
    ;; Preserve tail call
    (%eval previous-exp env)))

(defun eval-if (body env)
  (program-destructuring-bind (test if-true &optional if-false) body
    (if (%eval test env)
        (%eval if-true env)
        (%eval if-false env))))

(defun eval-let (body env)
  (program-destructuring-bind (bindings &body body) body
    ;; First evaluate the bindings in parallel
    (let ((bindings (mapcar
                     #'(lambda (binding)
                         (cons (binding-name binding)
                               (%eval (binding-value binding) env)))
                     bindings)))
      (multiple-value-bind (body documentation declarations)
          (parse-lambda-headers body :doc-string-allowed nil)
        (declare (ignore documentation))
        ;; Then establish them into the environment, and evaluate the
        ;; body.
        (call-with-new-env env bindings declarations t
                           #'(lambda (env)
                               (eval-progn body env)))))))

(defun eval-let* (body old-env)
  (program-destructuring-bind (bindings &body body) body
    (multiple-value-bind (body documentation declarations)
        (parse-lambda-headers body :doc-string-allowed nil)
      (declare (ignore documentation))
      ;; First we separate the special declarations into bound and
      ;; free declarations.
      (let ((specials (declared-specials declarations))
            var-specials free-specials)
        (dolist (special specials)
          (if (member special bindings :key #'binding-name)
              (push special var-specials)
              (push special free-specials)))
        (let ((env (make-env :parent old-env
                             :declarations declarations)))
          ;; Then we establish the bindings into the environment
          ;; sequentially.
          (eval-next-let*-binding
           (mapcar #'(lambda (binding)
                       (cons (binding-name binding)
                             (binding-value binding)))
                   bindings)
           var-specials env
           #'(lambda (env)
               ;; Now that we're done evaluating the bindings, add the
               ;; free special declarations. See also
               ;; CALL-WITH-NEW-ENV-FULL-PARSING.
               (dolist (special free-specials)
                 (push-var special *special* env))
               (eval-progn body env))))))))

;; Return a named local function in the environment ENV, made from the
;; definition form FUNCTION-DEF.
(defun eval-local-function-def (function-def env)
  (program-destructuring-bind (name lambda-list &body local-body) function-def
    (multiple-value-bind (local-body documentation declarations)
        (parse-lambda-headers local-body :doc-string-allowed t)
      (%eval `#'(sb!int:named-lambda ,name ,lambda-list
                  ,@(if documentation
                        (list documentation)
                        nil)
                  (declare ,@declarations)
                  (block ,(cond ((consp name) (second name))
                                (t name))
                    ,@local-body))
             env))))

(defun eval-flet (body env)
  (program-destructuring-bind ((&rest local-functions) &body body) body
    (multiple-value-bind (body documentation declarations)
        (parse-lambda-headers body :doc-string-allowed nil)
      (declare (ignore documentation))
      (let* ((specials (declared-specials declarations))
             (new-env (make-env :parent env
                                :vars (special-bindings specials env)
                                :declarations declarations)))
        (dolist (function-def local-functions)
          (push-fun (car function-def)
                    ;; Evaluate the function definitions in ENV.
                    (eval-local-function-def function-def env)
                    ;; Do package-lock checks in ENV.
                    env
                    ;; But add the bindings to the child environment.
                    new-env))
        (eval-progn body new-env)))))

(defun eval-labels (body old-env)
  (program-destructuring-bind ((&rest local-functions) &body body) body
    (multiple-value-bind (body documentation declarations)
        (parse-lambda-headers body :doc-string-allowed nil)
      (declare (ignore documentation))
      ;; Create a child environment, evaluate the function definitions
      ;; in it, and add them into the same environment.
      (let ((env (make-env :parent old-env
                           :declarations declarations)))
        (dolist (function-def local-functions)
          (push-fun (car function-def)
                    (eval-local-function-def function-def env)
                    old-env
                    env))
        ;; And then add an environment for the body of the LABELS.  A
        ;; separate environment from the one where we added the
        ;; functions to is needed, since any special variable
        ;; declarations need to be in effect in the body, but not in
        ;; the bodies of the local functions.
        (let* ((specials (declared-specials declarations))
               (new-env (make-env :parent env
                                  :vars (special-bindings specials env))))
          (eval-progn body new-env))))))

;; Return a local macro-expander in the environment ENV, made from the
;; definition form FUNCTION-DEF.
(defun eval-local-macro-def (function-def env)
  (program-destructuring-bind (name lambda-list &body local-body) function-def
    (%eval (sb!int:make-macro-lambda nil ; the lambda is anonymous.
                                     lambda-list local-body
                                     'macrolet name)
           env)))

(defun eval-macrolet (body env)
  (program-destructuring-bind ((&rest local-functions) &body body) body
    (flet ((generate-fbinding (macro-def)
             (cons (car macro-def) *macro*))
           (generate-mbinding (macro-def)
             (let ((name (car macro-def))
                   (sb!c:*lexenv* (env-native-lexenv env)))
               (when (fboundp name)
                 (program-assert-symbol-home-package-unlocked
                  :eval name "binding ~A as a local macro"))
               (cons name (eval-local-macro-def macro-def env)))))
      (multiple-value-bind (body documentation declarations)
          (parse-lambda-headers body :doc-string-allowed nil)
        (declare (ignore documentation))
        (let* ((specials (declared-specials declarations))
               (new-env (make-env :parent env
                                  :vars (special-bindings specials env)
                                  :funs (mapcar #'generate-fbinding
                                                local-functions)
                                  :expanders (mapcar #'generate-mbinding
                                                     local-functions)
                                  :declarations declarations)))
          (eval-progn body new-env))))))

(defun eval-symbol-macrolet (body env)
  (program-destructuring-bind ((&rest bindings) &body body) body
    (flet ((generate-binding (binding)
             (cons (car binding) *symbol-macro*))
           (generate-sm-binding (binding)
             (let ((name (car binding))
                   (sb!c:*lexenv* (env-native-lexenv env)))
               (when (or (boundp name)
                         (eq (sb!int:info :variable :kind name) :macro))
                 (program-assert-symbol-home-package-unlocked
                  :eval name "binding ~A as a local symbol-macro"))
               (cons name (second binding)))))
      (multiple-value-bind (body documentation declarations)
          (parse-lambda-headers body :doc-string-allowed nil)
        (declare (ignore documentation))
        (let ((specials (declared-specials declarations)))
          (dolist (binding bindings)
            (when (specialp (binding-name binding) specials)
              (ip-error "~@<Can't bind SYMBOL-MACROLET of special ~
                         variable ~S.~:@>"
                        (binding-name binding)))))
        (let* ((specials (declared-specials declarations))
               (new-env (make-env :parent env
                                  :vars (nconc-2 (mapcar #'generate-binding
                                                         bindings)
                                                 (special-bindings specials env))
                                  :symbol-expansions (mapcar
                                                      #'generate-sm-binding
                                                      bindings)
                                  :declarations declarations)))
          (eval-progn body new-env))))))

(defun eval-progv (body env)
  (program-destructuring-bind (vars vals &body body) body
    (progv (%eval vars env) (%eval vals env)
      (eval-progn body env))))

(defun eval-function (body env)
  (program-destructuring-bind (name) body
    (cond
      ;; LAMBDAP assumes that the argument is a cons, so we need the
      ;; initial symbol case, instead of relying on the fall-through
      ;; case that has the same function body.
      ((symbolp name) (nth-value 0 (get-function name env)))
      ((lambdap name) (eval-lambda name env))
      (t (nth-value 0 (get-function name env))))))

(defun eval-eval-when (body env)
  (program-destructuring-bind ((&rest situation) &body body) body
    ;; FIXME: check that SITUATION only contains valid situations
    (if (or (member :execute situation)
            (member 'eval situation))
        (eval-progn body env))))

(defun eval-quote (body env)
  (declare (ignore env))
  (program-destructuring-bind (object) body
    object))

(defun eval-setq (pairs env)
  (when (oddp (length pairs))
    (ip-error "~@<Odd number of args to SETQ: ~S~:@>" (cons 'setq pairs)))
  (let ((last nil))
    (loop for (var new-val) on pairs by #'cddr do
          (handler-case
              (multiple-value-bind (expansion type) (get-variable var env)
                (ecase type
                  (:expansion
                   (setf last
                         (%eval (list 'setf expansion new-val) env)))
                  (:variable
                   (setf last (set-variable var (%eval new-val env)
                                            env)))))
            (unbound-variable (c)
              (declare (ignore c))
              (setf last (setf (symbol-value var)
                               (%eval new-val env))))))
    last))

(defun eval-multiple-value-call (body env)
  (program-destructuring-bind (function-form &body forms) body
    (%apply (%eval function-form env)
            (loop for form in forms
                  nconc (multiple-value-list (%eval form env))))))

(defun eval-multiple-value-prog1 (body env)
  (program-destructuring-bind (first-form &body forms) body
    (multiple-value-prog1 (%eval first-form env)
      (eval-progn forms env))))

(defun eval-catch (body env)
  (program-destructuring-bind (tag &body forms) body
    (catch (%eval tag env)
      (eval-progn forms env))))

(defun eval-tagbody (body old-env)
  (let ((env (make-env :parent old-env))
        (tags nil)
        (start body)
        (target-tag nil))
    (tagbody
       (flet ((go-to-tag (tag)
                (setf target-tag tag)
                (go go-to-tag)))
         ;; For each tag, store a trampoline function into the environment
         ;; and the location in the body into the TAGS alist.
         (do ((form body (cdr form)))
             ((null form) nil)
           (when (atom (car form))
             (when (assoc (car form) tags)
               (ip-error "The tag :A appears more than once in a tagbody."))
             (push (cons (car form) (cdr form)) tags)
             (push (cons (car form) #'go-to-tag) (env-tags env)))))
       ;; And then evaluate the forms in the body, starting from the
       ;; first one.
       (go execute)
     go-to-tag
       ;; The trampoline has set the TARGET-TAG. Restart evaluation of
       ;; the body from the location in body that matches the tag.
       (setf start (cdr (assoc target-tag tags)))
     execute
       (dolist (form start)
         (when (not (atom form))
           (%eval form env))))))

(defun eval-go (body env)
  (program-destructuring-bind (tag) body
    (let ((target (get-tag-binding tag env)))
      (if target
          ;; Call the GO-TO-TAG trampoline
          (funcall (cdr target) tag)
          (ip-error "~@<Attempt to GO to nonexistent tag: ~S~:@>" tag)))))

(defun eval-block (body old-env)
  (flet ((return-from-eval-block (&rest values)
           (return-from eval-block (values-list values))))
    (program-destructuring-bind (name &body body) body
      (unless (symbolp name)
        (ip-error "~@<The block name ~S is not a symbol.~:@>" name))
      (let ((env (make-env
                  :blocks (list (cons name #'return-from-eval-block))
                  :parent old-env)))
        (eval-progn body env)))))

(defun eval-return-from (body env)
  (program-destructuring-bind (name &optional result) body
    (let ((target (get-block-binding name env)))
      (if target
          (multiple-value-call (cdr target) (%eval result env))
          (ip-error "~@<Return for unknown block: ~S~:@>" name)))))

(defun eval-the (body env)
  (program-destructuring-bind (value-type form) body
    (let ((values (multiple-value-list (%eval form env)))
          (vtype (if (ctype-p value-type) value-type (values-specifier-type value-type))))
      ;; FIXME: we should probably do this only if SAFETY>SPEED
      (cond
        ((eq vtype *wild-type*) (values-list values))
        ((values-type-p vtype)
         (do ((vs values (cdr vs))
              (ts (values-type-required vtype) (cdr ts)))
             ((null ts)
              (do ((vs vs (cdr vs))
                   (ts (values-type-optional vtype) (cdr ts)))
                  ((null ts)
                   (do ((vs vs (cdr vs))
                        (rest (values-type-rest vtype)))
                       ((null vs) (values-list values))
                     (if rest
                         (unless (ctypep (car vs) rest)
                           (error 'type-error :datum (car vs) :expected-type (type-specifier rest)))
                         (error 'type-error :datum vs :expected-type nil))))
                (let ((v (car vs))
                      (type (car ts)))
                  (when vs
                    (unless (ctypep v type)
                      (error 'type-error :datum v :expected-type (type-specifier type)))))))
           (let ((v (car vs))
                 (type (car ts)))
             (unless (ctypep v type)
               (error 'type-error :datum v :expected-type (type-specifier type))))))

        ((ctypep (car values) vtype) (values-list values))
        (t (error 'type-error :datum (car values) :expected-type (type-specifier vtype)))))))

(defun eval-unwind-protect (body env)
  (program-destructuring-bind (protected-form &body cleanup-forms) body
    (unwind-protect (%eval protected-form env)
      (eval-progn cleanup-forms env))))

(defun eval-throw (body env)
  (program-destructuring-bind (tag result-form) body
    (throw (%eval tag env)
      (%eval result-form env))))

(defun eval-load-time-value (body env)
  (program-destructuring-bind (form &optional read-only-p) body
    (declare (ignore read-only-p))
    (%eval form env)))

(defun eval-locally (body env)
  (multiple-value-bind (body documentation declarations)
      (parse-lambda-headers body :doc-string-allowed nil)
    (declare (ignore documentation))
    (let* ((specials (declared-specials declarations))
           (new-env (if (or specials declarations)
                        (make-env :parent env
                                  :vars (special-bindings specials env)
                                  :declarations declarations)
                        env)))
      (eval-progn body new-env))))

(defun eval-args (args env)
  (mapcar #'(lambda (arg) (%eval arg env)) args))

;;; The expansion of SB-SYS:WITH-PINNED-OBJECTS on GENCGC uses some
;;; VOPs which can't be reasonably implemented in the interpreter. So
;;; we special-case the macro.
(defun eval-with-pinned-objects (args env)
  (program-destructuring-bind (values &body body) args
    (if (null values)
        (eval-progn body env)
        (sb!sys:with-pinned-objects ((car values))
          (eval-with-pinned-objects (cons (cdr values) body) env)))))

(defvar *eval-dispatch-functions* nil)

;;; Dispatch to the appropriate EVAL-FOO function based on the contents of EXP.
(declaim (inline %%eval))
(defun %%eval (exp env)
  (cond
    ((symbolp exp)
     ;; CLHS 3.1.2.1.1 Symbols as Forms
     (multiple-value-bind (value kind) (get-variable exp env)
       (ecase kind
         (:variable value)
         (:expansion (%eval value env)))))
    ;; CLHS 3.1.2.1.3 Self-Evaluating Objects
    ((atom exp) exp)
    ;; CLHS 3.1.2.1.2 Conses as Forms
    ((consp exp)
     (case (car exp)
       ;; CLHS 3.1.2.1.2.1 Special Forms
       ((block)                (eval-block (cdr exp) env))
       ((catch)                (eval-catch (cdr exp) env))
       ((eval-when)            (eval-eval-when (cdr exp) env))
       ((flet)                 (eval-flet (cdr exp) env))
       ((function)             (eval-function (cdr exp) env))
       ((go)                   (eval-go (cdr exp) env))
       ((if)                   (eval-if (cdr exp) env))
       ((labels)               (eval-labels (cdr exp) env))
       ((let)                  (eval-let (cdr exp) env))
       ((let*)                 (eval-let* (cdr exp) env))
       ((load-time-value)      (eval-load-time-value (cdr exp) env))
       ((locally)              (eval-locally (cdr exp) env))
       ((macrolet)             (eval-macrolet (cdr exp) env))
       ((multiple-value-call)  (eval-multiple-value-call (cdr exp) env))
       ((multiple-value-prog1) (eval-multiple-value-prog1 (cdr exp) env))
       ((progn)                (eval-progn (cdr exp) env))
       ((progv)                (eval-progv (cdr exp) env))
       ((quote)                (eval-quote (cdr exp) env))
       ((return-from)          (eval-return-from (cdr exp) env))
       ((setq)                 (eval-setq (cdr exp) env))
       ((symbol-macrolet)      (eval-symbol-macrolet (cdr exp) env))
       ((tagbody)              (eval-tagbody (cdr exp) env))
       ((the)                  (eval-the (cdr exp) env))
       ((throw)                (eval-throw (cdr exp) env))
       ((unwind-protect)       (eval-unwind-protect (cdr exp) env))
       ;; SBCL-specific:
       ((truly-the)            (eval-the (cdr exp) env))
       ;; Not a special form, but a macro whose expansion wouldn't be
       ;; handled correctly by the evaluator.
       ((sb!sys:with-pinned-objects) (eval-with-pinned-objects (cdr exp) env))
       (t
        (let ((dispatcher (getf *eval-dispatch-functions* (car exp))))
          (cond
            (dispatcher
             (funcall dispatcher exp env))
            ;; CLHS 3.1.2.1.2.4 Lambda Forms
            ((and (consp (car exp)) (eq (caar exp) 'lambda))
             (interpreted-apply (eval-function (list (car exp)) env)
                                (eval-args (cdr exp) env)))
            (t
             (multiple-value-bind (function kind) (get-function (car exp) env)
               (ecase kind
                 ;; CLHS 3.1.2.1.2.3 Function Forms
                 (:function (%apply function (eval-args (cdr exp) env)))
                 ;; CLHS 3.1.2.1.2.2 Macro Forms
                 (:macro
                  (let ((hook (valid-macroexpand-hook)))
                    (%eval (funcall (truly-the function hook)
                                    function
                                    exp
                                    (env-native-lexenv env))
                           env)))))))))))))

(defun %eval (exp env)
  (incf *eval-calls*)
  (if *eval-verbose*
      ;; Dynamically binding *EVAL-LEVEL* will prevent tail call
      ;; optimization. So only do it when its value will be used for
      ;; printing debug output.
      (let ((*eval-level* (1+ *eval-level*)))
        (let ((*print-circle* t))
          (format t "~&~vA~S~%" *eval-level* "" `(%eval ,exp)))
        (%%eval exp env))
      (%%eval exp env)))

(defun %apply (fun args)
  (etypecase fun
    (interpreted-function (interpreted-apply fun args))
    (function (apply fun args))
    (symbol (apply fun args))))

(defun interpreted-apply (fun args)
  (let ((lambda-list (interpreted-function-lambda-list fun))
        (env (interpreted-function-env fun))
        (body (interpreted-function-body fun))
        (declarations (interpreted-function-declarations fun)))
    (call-with-new-env-full-parsing
     env lambda-list args declarations
     #'(lambda (env)
         (eval-progn body env)))))

;;; We need separate conditions for the different *-TOO-COMPLEX-ERRORs to
;;; avoid spuriously triggering the handler in EVAL-IN-NATIVE-ENVIRONMENT
;;; on code like:
;;;
;;;   (let ((sb-ext:*evaluator-mode* :interpret))
;;;     (let ((fun (eval '(let ((a 1)) (lambda () a)))))
;;;         (eval `(compile nil ,fun))))
;;;
;;; FIXME: should these be exported?
(define-condition interpreter-environment-too-complex-error (simple-error)
  ())
(define-condition compiler-environment-too-complex-error (simple-error)
  ())

;;; Try to compile an interpreted function. If the environment
;;; contains local functions or lexical variables we'll punt on
;;; compiling it.
(defun prepare-for-compile (function)
  (let ((env (interpreted-function-env function)))
    (when (or (env-tags env)
              (env-blocks env)
              (find-if-not #'(lambda (x) (eq x *macro*))
                           (env-funs env) :key #'cdr)
              (find-if-not #'(lambda (x) (eq x *symbol-macro*))
                           (env-vars env)
                           :key #'cdr))
      (error 'interpreter-environment-too-complex-error
             :format-control
             "~@<Lexical environment of ~S is too complex to compile.~:@>"
             :format-arguments
             (list function)))
    (values
     `(sb!int:named-lambda ,(interpreted-function-name function)
          ,(interpreted-function-lambda-list function)
        (declare ,@(interpreted-function-declarations function))
        ,@(interpreted-function-body function))
     (env-native-lexenv env))))

;;; Convert a compiler LEXENV to an interpreter ENV. This is needed
;;; for EVAL-IN-LEXENV.
(defun make-env-from-native-environment (lexenv)
  (let ((native-funs (sb!c::lexenv-funs lexenv))
        (native-vars (sb!c::lexenv-vars lexenv)))
    (flet ((is-macro (thing)
             (and (consp thing) (eq (car thing) 'sb!sys:macro))))
      (when (or (sb!c::lexenv-blocks lexenv)
                (sb!c::lexenv-cleanup lexenv)
                (sb!c::lexenv-lambda lexenv)
                (sb!c::lexenv-tags lexenv)
                (sb!c::lexenv-type-restrictions lexenv)
                (find-if-not #'is-macro native-funs :key #'cdr)
                (find-if-not #'is-macro native-vars :key #'cdr))
        (error 'compiler-environment-too-complex-error
               :format-control
               "~@<Lexical environment is too complex to evaluate in: ~S~:@>"
               :format-arguments
               (list lexenv))))
    (flet ((make-binding (native)
             (cons (car native) *symbol-macro*))
           (make-sm-binding (native)
             (cons (car native) (cddr native)))
           (make-fbinding (native)
             (cons (car native) *macro*))
           (make-mbinding (native)
             (cons (car native) (cddr native))))
      (%make-env nil
                 (mapcar #'make-binding native-vars)
                 (mapcar #'make-fbinding native-funs)
                 (mapcar #'make-mbinding native-funs)
                 (mapcar #'make-sm-binding native-vars)
                 nil
                 nil
                 nil
                 lexenv))))

(defun eval-in-environment (form env)
  (%eval form env))

(defun eval-in-native-environment (form lexenv)
  (handler-bind
      ((sb!impl::eval-error
         (lambda (condition)
           (error 'interpreted-program-error
                  :condition (sb!int:encapsulated-condition condition)
                  :form form))))
    (sb!c:with-compiler-error-resignalling
      (handler-case
          (let ((env (make-env-from-native-environment lexenv)))
            (%eval form env))
        (compiler-environment-too-complex-error (condition)
          (declare (ignore condition))
          (sb!int:style-warn 'lexical-environment-too-complex
                             :form form :lexenv lexenv)
          (sb!int:simple-eval-in-lexenv form lexenv))))))

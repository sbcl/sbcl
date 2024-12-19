;;;; a simple code walker
;;;;
;;;; The code which implements the macroexpansion environment
;;;; manipulation mechanisms is in the first part of the file, the
;;;; real walker follows it.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.

;;;; This software is derived from software originally released by Xerox
;;;; Corporation. Copyright and release statements follow. Later modifications
;;;; to the software are in the public domain and are provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for more
;;;; information.

;;;; copyright information from original PCL sources:
;;;;
;;;; Copyright (c) 1985, 1986, 1987, 1988, 1989, 1990 Xerox Corporation.
;;;; All rights reserved.
;;;;
;;;; Use and copying of this software and preparation of derivative works based
;;;; upon this software are permitted. Any distribution of this software or
;;;; derivative works must comply with all applicable United States export
;;;; control laws.
;;;;
;;;; This software is made available AS IS, and Xerox Corporation makes no
;;;; warranty about the software, its performance or its conformity to any
;;;; specification.

(in-package "SB-WALKER")

;;;; forward references

(defvar *key-to-walker-environment*)

;;;; environment hacking stuff, necessarily SBCL-specific

;;; Here in the original PCL were implementations of the
;;; implementation-specific environment hacking functions for each of
;;; the implementations this walker had been ported to. This
;;; functionality was originally factored out in order to make PCL
;;; portable from one Common Lisp to another. As of 19981107, that
;;; portability was fairly stale and (because of the scarcity of CLTL1
;;; implementations and the strong interdependence of the rest of ANSI
;;; Common Lisp on the CLOS system) fairly irrelevant. It was fairly
;;; thoroughly put out of its misery by WHN in his quest to clean up
;;; the system enough that it can be built from scratch using any ANSI
;;; Common Lisp.
;;;
;;; This code just hacks 'macroexpansion environments'. That is, it is
;;; only concerned with the function binding of symbols in the
;;; environment. The walker needs to be able to tell if the symbol
;;; names a lexical macro or function, and it needs to be able to
;;; build environments which contain lexical macro or function
;;; bindings. It must be able, when walking a MACROLET, FLET or LABELS
;;; form to construct an environment which reflects the bindings
;;; created by that form. Note that the environment created does NOT
;;; have to be sufficient to evaluate the body, merely to walk its
;;; body. This means that definitions do not have to be supplied for
;;; lexical functions, only the fact that that function is bound is
;;; important. For macros, the macroexpansion function must be
;;; supplied.
;;;
;;; This code is organized in a way that lets it work in
;;; implementations that stack cons their environments. That is
;;; reflected in the fact that the only operation that lets a user
;;; build a new environment is a WITH-BODY macro which executes its
;;; body with the specified symbol bound to the new environment. No
;;; code in this walker or in PCL will hold a pointer to these
;;; environments after the body returns. Other user code is free to do
;;; so in implementations where it works, but that code is not
;;; considered portable.
;;;
;;; There are 3 environment hacking tools. One macro,
;;; WITH-AUGMENTED-ENVIRONMENT, which is used to create new
;;; environments, and two functions, ENVIRONMENT-FUNCTION and
;;; ENVIRONMENT-MACRO, which are used to access the bindings of
;;; existing environments

;;; In SBCL, as in CMU CL before it, the environment is represented
;;; with a structure that holds alists for the functional things,
;;; variables, blocks, etc. Except for SYMBOL-MACROLET, only the
;;; SB-C::LEXENV-FUNS slot is relevant. It holds: Alist (Name . What),
;;; where What is either a functional (a local function) or a list
;;; (MACRO . <function>) (a local macro, with the specifier expander.)
;;; Note that Name may be a (SETF <name>) function. Accessors are
;;; defined below, eg (ENV-WALK-FUNCTION ENV).
;;;
;;; If WITH-AUGMENTED-ENVIRONMENT is called from WALKER-ENVIRONMENT-BIND
;;; this code hides the WALKER version of an environment
;;; inside the SB-C::LEXENV structure.
;;;
;;; In CMUCL (and former SBCL), This used to be a list of lists of form
;;; (<gensym-name> MACRO . #<interpreted-function>) in the :functions slot
;;; in a C::LEXENV.
;;; This form was accepted by the compiler, but this was a crude hack,
;;; because the <interpreted-function> was used as a structure to hold the
;;; bits of interest, {function, form, declarations, lexical-variables},
;;; a list, which was not really an interpreted function.
;;; Instead this list was COERCEd to a #<FUNCTION ...>!
;;;
;;; Instead, we now use a special sort of "function"-type for that
;;; information, because the functions slot in SB-C::LEXENV is
;;; supposed to have a list of <Name MACRO . #<function> elements.
;;; So, now we hide our bits of interest in the walker-info slot in
;;; our new BOGO-FUN.
;;;
;;; MACROEXPAND-1 and SB-INT:EVAL-IN-LEXENV are the only SBCL
;;; functions that get called with the constructed environment
;;; argument.

(defmacro with-augmented-environment
    ((new-env old-env &key functions macros) &body body)
  `(let ((,new-env (with-augmented-environment-internal ,old-env
                                                        ,functions
                                                        ,macros)))
     ,@body))

;;; a unique tag to show that we're the intended caller of BOGO-FUN
(defvar *bogo-fun-magic-tag*
  '(:bogo-fun-magic-tag))

;;; The interface of BOGO-FUNs (previously implemented as
;;; FUNCALLABLE-INSTANCEs) is just these two operations, so we can do
;;; them with ordinary closures.
;;;
;;; KLUDGE: BOGO-FUNs are sorta weird, and MNA and I have both hacked
;;; on this code without quite figuring out what they're for. (He
;;; changed them to work after some changes in the IR1 interpreter
;;; made functions not be built lazily, and I changed them so that
;;; they don't need FUNCALLABLE-INSTANCE stuff, so that the F-I stuff
;;; can become less general.) There may be further simplifications or
;;; clarifications which could be done. -- WHN 2001-10-19
(defun walker-info-to-bogo-fun (walker-info)
  (lambda (magic-tag &rest rest)
    (aver (not rest)) ; else someone is using me in an unexpected way
    (aver (eql magic-tag *bogo-fun-magic-tag*)) ; else ditto
    walker-info))
(defun bogo-fun-to-walker-info (bogo-fun)
  (declare (type function bogo-fun))
  (funcall bogo-fun *bogo-fun-magic-tag*))

(defun with-augmented-environment-internal (env funs macros)
  ;; Note: In order to record the correct function definition, we
  ;; would have to create an interpreted closure, but the
  ;; WITH-NEW-DEFINITION macro down below makes no distinction between
  ;; FLET and LABELS, so we have no idea what to use for the
  ;; environment. So we just blow it off, 'cause anything real we do
  ;; would be wrong. But we still have to make an entry so we can tell
  ;; functions from macros -- same for telling variables apart from
  ;; symbol macros.
  (let ((lexenv (sb-kernel:coerce-to-lexenv env)))
    (sb-c::make-lexenv
     :default lexenv
     :vars (when (eql (caar macros) *key-to-walker-environment*)
             (copy-tree (mapcar (lambda (b)
                                  (let ((name (car b))
                                        (info (cadr b)))
                                    (if (eq info :lexical-var)
                                        (cons name
                                              (if (var-special-p name env)
                                                  (sb-c::make-global-var :special name)
                                                  (sb-c::make-lambda-var name)))
                                        b)))
                                (fourth (cadar macros)))))
     :funs (append (mapcar (lambda (f)
                             (cons (car f)
                                   (sb-c::make-functional :lexenv lexenv)))
                           funs)
                   (mapcar (lambda (m)
                             (list* (car m)
                                    'sb-sys:macro
                                    (if (eq (car m)
                                            *key-to-walker-environment*)
                                        (walker-info-to-bogo-fun (cadr m))
                                        (coerce (cadr m) 'function))))
                           macros)))))

(defun environment-function (env fn)
  (when env
    (let ((entry (assoc fn (sb-c::lexenv-funs env) :test #'equal)))
      (and entry
           (sb-c::functional-p (cdr entry))
           (cdr entry)))))

(defun environment-macro (env macro)
  (when env
    (let ((entry (assoc macro (sb-c::lexenv-funs env) :test #'eq)))
      (and entry
           (eq (cadr entry) 'sb-sys:macro)
           (if (eq macro *key-to-walker-environment*)
               (values (bogo-fun-to-walker-info (cddr entry)))
               (values (function-lambda-expression (cddr entry))))))))

;;;; other environment hacking, not so SBCL-specific as the
;;;; environment hacking in the previous section

(defmacro with-new-definition-in-environment
          ((new-env old-env macrolet/flet/labels-form) &body body)
  (let ((functions (make-symbol "Functions"))
        (macros (make-symbol "Macros")))
    `(let ((,functions ())
           (,macros ()))
       (ecase (car ,macrolet/flet/labels-form)
         ((flet labels)
          (dolist (fn (cadr ,macrolet/flet/labels-form))
            (push fn ,functions)))
         ((macrolet)
          (dolist (mac (cadr ,macrolet/flet/labels-form))
            (push (list (car mac)
                        (convert-macro-to-lambda (cadr mac)
                                                 (cddr mac)
                                                 ,old-env
                                                 (string (car mac))))
                  ,macros))))
       (with-augmented-environment
              (,new-env ,old-env :functions ,functions :macros ,macros)
         ,@body))))

(defun convert-macro-to-lambda (llist body env &optional (name "dummy macro"))
  (declare (ignorable llist body env name))
  (let ((gensym (make-symbol name)))
    #+sb-xc-host
    (eval `(sb-xc:defmacro ,gensym ,llist ,@body))
    #-sb-xc-host
    (eval-in-lexenv `(defmacro ,gensym ,llist ,@body)
                    (sb-c::make-restricted-lexenv env))
    (macro-function gensym)))

;;;; the actual walker

;;; As the walker walks over the code, it communicates information to
;;; itself about the walk. This information includes the walk
;;; function, variable bindings, declarations in effect etc. This
;;; information is inherently lexical, so the walker passes it around
;;; in the actual environment the walker passes to macroexpansion
;;; functions.
(defmacro walker-environment-bind ((var env &rest key-args)
                                      &body body)
  `(with-augmented-environment
     (,var ,env :macros (walker-environment-bind-1 ,env ,.key-args))
     .,body))

(defvar *key-to-walker-environment* (gensym))

(defun env-lock (env)
  (environment-macro env *key-to-walker-environment*))

(defun walker-environment-bind-1 (env &key (walk-function nil wfnp)
                                           (walk-form nil wfop)
                                           (declarations nil decp)
                                           (lexical-vars nil lexp))
  (let ((lock (env-lock env)))
    (list
      (list *key-to-walker-environment*
            (list (if wfnp walk-function (car lock))
                  (if wfop walk-form     (cadr lock))
                  (if decp declarations  (caddr lock))
                  (if lexp lexical-vars  (cadddr lock)))))))

(defun env-walk-function (env)
  (car (env-lock env)))

(defun env-walk-form (env)
  (cadr (env-lock env)))

(defun env-declarations (env)
  (caddr (env-lock env)))

(defun env-var-type (var env)
  (dolist (decl (env-declarations env) t)
    (when (and (eq 'type (car decl)) (member var (cddr decl) :test 'eq))
      (return (cadr decl)))))

(defun env-lexical-variables (env)
  (cadddr (env-lock env)))

(defun note-declaration (declaration env)
  (push declaration (caddr (env-lock env))))

(defun note-var-binding (thing env)
  (push (list thing :lexical-var) (cadddr (env-lock env))))

(defun var-lexical-p (var env)
  (let ((entry (member var (env-lexical-variables env) :key #'car :test #'eq)))
    (when (eq (cadar entry) :lexical-var)
      (return-from var-lexical-p entry)))
  ;; if we're finding something in the real lexenv, we don't have a
  ;; bound declaration and so we specifically don't want to return
  ;; a special object that declarations can attach to, just the name.
  (and env (find var (sb-c::lexenv-vars env) :key #'car)))

(defun variable-symbol-macro-p (var env)
  ;; FIXME: crufty return convention
  (declare (symbol var))
  (let ((entry (or (member var (env-lexical-variables env) :key #'car :test #'eq)
                   (and env (member var (sb-c::lexenv-vars env) :key #'car :test #'eq)))))
    (when (and (consp (cdar entry)) (eq (cadar entry) 'sb-sys:macro))
      (return-from variable-symbol-macro-p entry))
    (unless entry
      (when (eq (info :variable :kind var) :macro)
        (list (list* var 'sb-sys:macro (info :variable :macro-expansion var)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun walked-var-declaration-p (declaration)
  (member declaration '(sb-pcl::%class sb-pcl::%parameter
                        sb-pcl::%variable-rebinding special))))

(defun %var-declaration (declaration var env)
  (let ((id (or (var-lexical-p var env) var)))
    (if (eq 'special declaration)
        (dolist (decl (env-declarations env))
          (when (and (eq (car decl) declaration)
                     (or (member var (cdr decl))
                         (and id (member id (cdr decl)))))
            (return decl)))
        (dolist (decl (env-declarations env))
          (when (and (eq (car decl) declaration)
                     (eq (cadr decl) id))
            (return decl))))))

(defun var-declaration (declaration var env)
  (if (walked-var-declaration-p declaration)
      (%var-declaration declaration var env)
      (error "Not a variable declaration the walker cares about: ~S" declaration)))

(define-compiler-macro var-declaration (&whole form declaration var env
                                        &environment lexenv)
  (if (constantp declaration lexenv)
      (let ((decl (constant-form-value declaration lexenv)))
        (if (walked-var-declaration-p decl)
            `(%var-declaration ,declaration ,var ,env)
            form))
      form))

(defun var-special-p (var env)
  (and (or (var-declaration 'special var env)
           (var-globally-special-p var))
       t))

(defun var-globally-special-p (symbol)
  (eq (info :variable :kind symbol) :special))


;;;; handling of special forms

;;; Here are some comments from the original PCL on the difficulty of
;;; doing this portably across different CLTL1 implementations. This
;;; is no longer directly relevant because this code now only runs on
;;; SBCL, but the comments are retained for culture: they might help
;;; explain some of the design decisions which were made in the code.
;;;
;;; and I quote...
;;;
;;;     The set of special forms is purposely kept very small because
;;;     any program analyzing program (read code walker) must have
;;;     special knowledge about every type of special form. Such a
;;;     program needs no special knowledge about macros...
;;;
;;; So all we have to do here is a define a way to store and retrieve
;;; templates which describe how to walk the 24 special forms and we
;;; are all set...
;;;
;;; Well, its a nice concept, and I have to admit to being naive
;;; enough that I believed it for a while, but not everyone takes
;;; having only 24 special forms as seriously as might be nice. There
;;; are (at least) 3 ways to lose:
;;
;;;   1 - Implementation x implements a Common Lisp special form as
;;;       a macro which expands into a special form which:
;;;      - Is a common lisp special form (not likely)
;;;      - Is not a common lisp special form (on the 3600 IF --> COND).
;;;
;;;     * We can save ourselves from this case (second subcase really)
;;;       by checking to see whether there is a template defined for
;;;       something before we check to see whether we can macroexpand it.
;;;
;;;   2 - Implementation x implements a Common Lisp macro as a special form.
;;;
;;;     * This is a screw, but not so bad, we save ourselves from it by
;;;       defining extra templates for the macros which are *likely* to
;;;       be implemented as special forms. [Note: As of sbcl-0.6.9, these
;;;       extra templates have been deleted, since this is not a problem
;;;       in SBCL and we no longer try to make this walker portable
;;;       across other possibly-broken CL implementations.]
;;;
;;;   3 - Implementation x has a special form which is not on the list of
;;;       Common Lisp special forms.
;;;
;;;     * This is a bad sort of a screw and happens more than I would
;;;       like to think, especially in the implementations which provide
;;;       more than just Common Lisp (3600, Xerox etc.).
;;;       The fix is not terribly satisfactory, but will have to do for
;;;       now. There is a hook in get walker-template which can get a
;;;       template from the implementation's own walker. That template
;;;       has to be converted, and so it may be that the right way to do
;;;       this would actually be for that implementation to provide an
;;;       interface to its walker which looks like the interface to this
;;;       walker.

(defmacro define-walker-template (name
                                  &optional (template '(nil repeat (eval))))
  `(setf (info :function :walker-template ',name) ',template))

(defun get-walker-template (x context)
  (cond ((symbolp x)
         (info :function :walker-template x))
        ((and (listp x) (eq (car x) 'lambda))
         '(lambda repeat (eval)))
        (t
         ;; FIXME: In an ideal world we would do something similar to
         ;; COMPILER-ERROR here, replacing the form within the walker
         ;; with an error-signalling form. This is slightly less
         ;; pretty, but informative non the less. Best is the enemy of
         ;; good, etc.
         (error "Illegal function call in method body:~%  ~S"
                context))))

;;;; the actual templates

;;; ANSI special forms
(define-walker-template block                (nil nil repeat (eval)))
(define-walker-template catch                (nil eval repeat (eval)))
(define-walker-template declare              walk-unexpected-declare)
(define-walker-template eval-when            (nil quote repeat (eval)))
(define-walker-template flet                 walk-flet)
(define-walker-template function             (nil call))
(define-walker-template go                   (nil quote))
(define-walker-template if                   walk-if)
(define-walker-template labels               walk-labels)
(define-walker-template lambda               walk-lambda)
(define-walker-template let                  walk-let)
(define-walker-template let*                 walk-let*)
(define-walker-template load-time-value      walk-load-time-value)
(define-walker-template locally              walk-locally)
(define-walker-template macrolet             walk-macrolet)
(define-walker-template multiple-value-call  (nil eval repeat (eval)))
(define-walker-template multiple-value-prog1 (nil return repeat (eval)))
(define-walker-template progn                (nil repeat (eval)))
(define-walker-template progv                (nil eval eval repeat (eval)))
(define-walker-template quote                (nil quote))
(define-walker-template return-from          (nil quote repeat (return)))
(define-walker-template setq                 walk-setq)
(define-walker-template symbol-macrolet      walk-symbol-macrolet)
(define-walker-template tagbody              walk-tagbody)
(define-walker-template the                  (nil quote eval))
(define-walker-template throw                (nil eval eval))
(define-walker-template unwind-protect       (nil return repeat (eval)))
(define-walker-template defun                walk-defun)

;;; SBCL-only special forms
(define-walker-template truly-the (nil quote eval))
(define-walker-template sb-kernel:the* (nil quote eval))
(define-walker-template sb-c::with-source-form (nil nil eval))
;;; FIXME: maybe we don't need this one any more, given that
;;; NAMED-LAMBDA now expands into (FUNCTION (NAMED-LAMBDA ...))?
(define-walker-template named-lambda walk-named-lambda)
(define-walker-template sb-c::jump-table (nil eval repeat ((quote repeat (eval)))))
#|
;;; To find templateized symbols that aren't special operators:
(do-all-symbols (s)
  (let ((template
         (sb-int:info :function :walker-template s)))
    (when (and template (not (special-operator-p s)))
      (format t "Why? ~S~%" s))))
|#

(defvar *walk-form-expand-macros-p* nil)
(defvar *walk-form-preserve-source* nil)

(defun macroexpand-all (form &optional environment)
  (let ((*walk-form-expand-macros-p* t))
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
    (declare (dynamic-extent #'recurse))
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

#+sb-fasteval
(declaim (ftype (sfunction (sb-interpreter:basic-env &optional t) sb-kernel:lexenv)
                sb-interpreter:lexenv-from-env environment))

(defun walk-form (form
                  &optional environment
                            (walk-function
                             (lambda (subform context env)
                               (declare (ignore context env))
                               subform)))
  #+sb-fasteval
  (when (typep environment 'sb-interpreter:basic-env)
    (setq environment (sb-interpreter:lexenv-from-env environment)))
  (walker-environment-bind (new-env environment :walk-function walk-function)
    (walk-form-internal form :eval new-env)))

;;; WALK-FORM-INTERNAL is the main driving function for the code
;;; walker. It takes a form and the current context and walks the form
;;; calling itself or the appropriate template recursively.
;;;
;;;   "It is recommended that a program-analyzing-program process a form
;;;    that is a list whose car is a symbol as follows:
;;;
;;;     1. If the program has particular knowledge about the symbol,
;;;        process the form using special-purpose code. All of the
;;;        standard special forms should fall into this category.
;;;     2. Otherwise, if MACRO-FUNCTION is true of the symbol apply
;;;        either MACROEXPAND or MACROEXPAND-1 and start over.
;;;     3. Otherwise, assume it is a function call. "
(defun walk-form-internal (form context env)
  (declare (type (member :eval :set) context))
  ;; First apply the walk-function to perform whatever translation
  ;; the user wants to this form. If the second value returned
  ;; by walk-function is T then we don't recurse...
  (catch form
    (with-current-source-form (form)
      (let ((sb-c::*compiler-error-bailout*
              (lambda (c)
                (return-from walk-form-internal
                  (sb-c::make-compiler-error-form c form)))))
        (multiple-value-bind (newform walk-no-more-p)
            (funcall (env-walk-function env) form context env)
          (catch newform
            (cond
              (walk-no-more-p newform)
              ((not (eq form newform))
               (record-new-source-path form newform)
               (walk-form-internal newform context env))
              ((and (not (consp newform))
                    (or (eql context :eval)
                        (eql context :set)))
               (let ((symmac (and (symbolp newform)
                                  (car (variable-symbol-macro-p newform env)))))
                 (if symmac
                     (let* ((newnewform (walk-form-internal (cddr symmac)
                                                            context
                                                            env))
                            (resultform
                              (if (eq newnewform (cddr symmac))
                                  (if *walk-form-expand-macros-p* newnewform newform)
                                  newnewform))
                            (type (env-var-type newform env)))
                       (if (eq t type)
                           resultform
                           `(the ,type ,resultform)))
                     newform)))
              ((eql context :set) newform)
              (t
               (let* ((fn (car newform))
                      (template (get-walker-template fn newform)))
                 (if template
                     (if (symbolp template)
                         (funcall template newform context env)
                         (walk-template newform template context env))
                     (multiple-value-bind (newnewform macrop)
                         (walker-environment-bind
                             (new-env env :walk-form newform)
                           (%macroexpand-1 newform new-env))
                       (cond
                         (macrop
                          (let ((newnewnewform (walk-form-internal newnewform
                                                                   context
                                                                   env)))
                            (cond ((eq newnewnewform newnewform)
                                   (if *walk-form-expand-macros-p* newnewform newform))
                                  (t
                                   (record-new-source-path newform newnewnewform )))))
                         ((and (symbolp fn)
                               (special-operator-p fn))
                          ;; This shouldn't happen, since this walker is now
                          ;; maintained as part of SBCL, so it should know
                          ;; about all the special forms that SBCL knows
                          ;; about.
                          (bug "unexpected special form ~S" fn))
                         (t
                          ;; Otherwise, walk the form as if it's just a
                          ;; standard function call using a template for
                          ;; standard function call.
                          (walk-template
                           newnewform '(call repeat (eval)) context env))))))))))))))

(defun record-new-source-path (old-form new-form)
  (when (and *walk-form-preserve-source*
             (boundp 'sb-c::*source-paths*))
    (let ((path (gethash old-form sb-c::*source-paths*)))
      (when path
        (setf (gethash new-form sb-c::*source-paths*) path))))
  new-form)

(defun recons (old-cons car cdr)
  (if (and (eq car (car old-cons)) (eq cdr (cdr old-cons)))
      old-cons
      (record-new-source-path old-cons (cons car cdr))))

(defun walk-template (form template context env)
  (if (atom template)
      (ecase template
        ((eval function test effect return)
         (walk-form-internal form :eval env))
        ((quote nil) form)
        (set
          (walk-form-internal form :set env))
        ((lambda call)
         (cond ((legal-fun-name-p form)
                form)
               (t (walk-form-internal form context env)))))
      (case (car template)
        (repeat
          (walk-template-handle-repeat form
                                       (cdr template)
                                       ;; For the case where nothing
                                       ;; happens after the repeat
                                       ;; optimize away the call to
                                       ;; LENGTH.
                                       (if (null (cddr template))
                                           ()
                                           (nthcdr (- (length form)
                                                      (length
                                                        (cddr template)))
                                                   form))
                                       context
                                       env))
        (if
          (walk-template form
                         (if (if (listp (cadr template))
                                 (eval (cadr template))
                                 (funcall (cadr template) form))
                             (caddr template)
                             (cadddr template))
                         context
                         env))
        (remote
          (walk-template form (cadr template) context env))
        (otherwise
          (cond ((atom form) form)
                (t (recons form
                           (walk-template
                             (car form) (car template) context env)
                           (walk-template
                             (cdr form) (cdr template) context env))))))))

(defun walk-template-handle-repeat (form template stop-form context env)
  (if (eq form stop-form)
      (walk-template form (cdr template) context env)
      (walk-template-handle-repeat-1
       form template (car template) stop-form context env)))

(defun walk-template-handle-repeat-1 (form template repeat-template
                                           stop-form context env)
  (cond ((null form) ())
        ((eq form stop-form)
         (if (null repeat-template)
             (walk-template stop-form (cdr template) context env)
             (error "while handling code walker REPEAT:
                     ~%ran into STOP while still in REPEAT template")))
        ((null repeat-template)
         (walk-template-handle-repeat-1
           form template (car template) stop-form context env))
        (t
         (recons form
                 (walk-template (car form) (car repeat-template) context env)
                 (walk-template-handle-repeat-1 (cdr form)
                                                template
                                                (cdr repeat-template)
                                                stop-form
                                                context
                                                env)))))

(defun walk-repeat-eval (form env)
  (and form
       (recons form
               (walk-form-internal (car form) :eval env)
               (walk-repeat-eval (cdr form) env))))

(defun relist (x &rest args)
  (if (null args)
      nil
      (relist-internal x args nil)))

(defun relist* (x &rest args)
  (relist-internal x args t))

(defun relist-internal (x args *p)
  (if (null (cdr args))
      (if *p
          (car args)
          (recons x (car args) nil))
      (recons x
              (car args)
              (relist-internal (cdr x) (cdr args) *p))))

;;;; special walkers

(defun walk-declarations (body fn env
                               &optional doc-string-p declarations old-body
                               &aux (form (car body)))
  (cond ((and (stringp form)                    ;might be a doc string
              (cdr body)                        ;isn't the returned value
              (null doc-string-p)               ;no doc string yet
              ;; FIXME: {decl}+ doc {decl}+ is supposed to be valid.
              (null declarations))              ;no declarations yet
         (recons body
                 form
                 (walk-declarations (cdr body) fn env t)))
        ((and (listp form) (eq (car form) 'declare))
         ;; We got ourselves a real live declaration. Record it, look
         ;; for more.
         (dolist (declaration (cdr form))
           (let ((type (car declaration))
                 (name (cadr declaration))
                 (args (cddr declaration)))
             (cond ((eq type 'special)
                    (loop for name in (cdr declaration)
                          for var = (or (var-lexical-p name env)
                                        name)
                          do
                          (note-declaration `(special ,(or var name)) env)
                          ;; Shadow
                          (when (variable-symbol-macro-p name env)
                            (note-var-binding name env))))
                   ((walked-var-declaration-p type)
                    (note-declaration `(,type
                                        ,(or (var-lexical-p name env) name)
                                        ,.args)
                                      env))
                   (t
                    (let ((canonical (sb-c::canonized-decl-spec declaration)))
                      (typecase canonical
                        ((cons (eql type) cons)
                         (destructuring-bind (type &rest vars) (cdr canonical)
                           (loop for name in vars
                                 for symbol-macro = (and (symbolp name)
                                                         (car (variable-symbol-macro-p name env)))
                                 do (if symbol-macro
                                        (push (list* name 'sb-sys:macro
                                                     `(the ,type ,(cddr symbol-macro)))
                                              (cadddr (env-lock env)))
                                        (note-declaration canonical env)))))
                        (t
                         (note-declaration canonical env))))))
             (push declaration declarations)))
         (recons body
                 form
                 (walk-declarations
                  (cdr body) fn env doc-string-p declarations)))
        (t
         ;; Now that we have walked and recorded the declarations,
         ;; call the function our caller provided to expand the body.
         ;; We call that function rather than passing the real-body
         ;; back, because we are RECONSING up the new body.
         (funcall fn (or old-body body) env))))

(defun walk-unexpected-declare (form context env)
  (declare (ignore context env))
  (warn "encountered ~S ~_in a place where a DECLARE was not expected"
        form)
  form)

(defun walk-arglist (arglist context env &optional (destructuringp nil)
                                         &aux arg)
  (cond ((null arglist) ())
        ((symbolp (setq arg (car arglist)))
         (or (member arg lambda-list-keywords :test #'eq)
             (note-var-binding arg env))
         (recons arglist
                 arg
                 (walk-arglist (cdr arglist)
                               context
                               env
                               (and destructuringp
                                    (not (member arg lambda-list-keywords))))))
        ((consp arg)
         (prog1 (recons arglist
                        (if destructuringp
                            (walk-arglist arg context env destructuringp)
                            (relist* arg
                                     (car arg)
                                     (walk-form-internal (cadr arg) :eval env)
                                     (cddr arg)))
                        (walk-arglist (cdr arglist) context env nil))
                (if (symbolp (car arg))
                    (note-var-binding (car arg) env)
                    (note-var-binding (cadar arg) env))
                (or (null (cddr arg))
                    (not (symbolp (caddr arg)))
                    (note-var-binding (caddr arg) env))))
          (t
           (error "can't understand something in the arglist ~S" arglist))))

(defun walk-let (form context env)
  (walker-environment-bind (new-env env)
    (let* ((let (car form))
           (bindings (cadr form))
           (body (cddr form))
           walked-bindings
           (walked-body
             (walk-declarations
              body
              (lambda (real-body real-env)
                (setf walked-bindings
                      (walk-bindings-1 bindings env new-env context))
                (walk-repeat-eval real-body real-env))
              new-env)))
      (relist* form let walked-bindings walked-body))))

(defun let*-bindings (bindings &aux names inits (seen (alloc-xset)))
  (dolist (binding (reverse bindings) (values names inits))
    (multiple-value-bind (name init)
        (cond ((atom binding) (values binding 'no-init))
              ((not (cdr binding)) (values (car binding) 'no-init))
              (t (values (car binding) (cdr binding))))
      (push (cons name (xset-member-p name seen)) names)
      (add-to-xset name seen)
      (push init inits))))

(defun walk-let* (form context env)
  (walker-environment-bind (new-env env)
    (let ((let* (car form))
          (bindings (cadr form))
          (body (cddr form)))
      (multiple-value-bind (names inits) (let*-bindings bindings)
        (multiple-value-bind (newbody decls doc) (parse-body body nil)
          (declare (ignore newbody))
          (aver (null doc))
          (labels ((maybe-process-and-munge-declaration (name declaration env)
                     (if (walked-var-declaration-p (car declaration))
                         (case (car declaration)
                           (special (if (find name (cdr declaration))
                                        (progn
                                          (note-declaration `(special ,(var-lexical-p name env)) env)
                                          (cons 'special (remove name (cdr declaration))))
                                        declaration))
                           (t (if (eql name (cadr declaration))
                                  (progn
                                    (note-declaration `(,(car declaration)
                                                         ,(var-lexical-p name env)
                                                         ,@(cddr declaration))
                                                      env)
                                    nil)
                                  declaration)))
                         declaration))
                   (walk-let*-bindings (bindings names inits)
                     (when bindings
                       (recons bindings
                               (let ((name (car names))
                                     (init (car inits)))
                                 (prog1
                                     (if (eql init 'no-init)
                                         (prog1 (car bindings)
                                           (note-var-binding (car name) new-env))
                                         (prog1
                                             (recons (car bindings)
                                                     (caar bindings)
                                                     (recons init
                                                             (walk-form-internal (car init) context new-env)
                                                             (cdr init)))
                                           (note-var-binding (car name) new-env)))
                                   (unless (cdr name)
                                     (setf decls (mapcar (lambda (d)
                                                           (cons 'declare
                                                                 (mapcar (lambda (dd) (maybe-process-and-munge-declaration (car name) dd new-env))
                                                                         (cdr d))))
                                                         decls)))))
                               (walk-let*-bindings (cdr bindings) (cdr names) (cdr inits))))))
            (relist* form let*
                     (walk-let*-bindings bindings names inits)
                     (walk-declarations body (lambda (form env) (walk-repeat-eval form env)) new-env))))))))

(defun walk-load-time-value (form context env)
  (destructuring-bind (ltv val &optional read-only-p) form
    (declare (ignore ltv))
    (relist form
            'load-time-value
            ;; this is wrong: VAL is handled differently depending on
            ;; whether we're in EVAL, COMPILE or COMPILE-FILE, and
            ;; (after macroexpansion) is evaluated in the null lexical
            ;; environment.  The macro semantics are the same in each
            ;; case, but VAR-LEXICAL-P can be tricked into giving the
            ;; wrong answer.
            (walk-form-internal val context env)
            (walk-form-internal read-only-p context env))))

(defun walk-locally (form context env)
  (declare (ignore context))
  (walker-environment-bind (new-env env)
    (let* ((locally (car form))
           (body (cdr form))
           (walked-body
            (walk-declarations body #'walk-repeat-eval new-env)))
      (relist*
       form locally walked-body))))

(defun walk-bindings-1 (bindings old-env new-env context)
  (and bindings
       (let ((binding (car bindings)))
         (recons bindings
                 (typecase binding
                   (symbol
                     (prog1 binding
                       (note-var-binding binding new-env)))
                   ((cons symbol list)
                     (prog1 (relist* binding
                                     (car binding)
                                     (walk-form-internal
                                      (cadr binding) context old-env)
                                     ;; Preserve "trailing junk"
                                     (cddr binding))
                       (note-var-binding (car binding) new-env)))
                   (t ; illegal syntax, (let (#(foo))) or (let (a . #*1)) etc
                    binding))
                 (walk-bindings-1 (cdr bindings) old-env new-env context)))))

(defun walk-lambda (form context old-env)
  (walker-environment-bind (new-env old-env)
    (let* ((arglist (cadr form))
           (body (cddr form))
           (walked-arglist (walk-arglist arglist context new-env))
           (walked-body
             (walk-declarations body #'walk-repeat-eval new-env)))
      (relist* form
               (car form)
               walked-arglist
               walked-body))))

(defun walk-named-lambda (form context old-env)
  (walker-environment-bind (new-env old-env)
    (let* ((name (second form))
           (arglist (third form))
           (body (cdddr form))
           (walked-arglist (walk-arglist arglist context new-env))
           (walked-body
             (walk-declarations body #'walk-repeat-eval new-env)))
      (relist* form
               (car form)
               name
               walked-arglist
               walked-body))))

;;; The DEFUN macro does some stuff with the environment, handle it here.
(defun walk-defun (form context env)
  (recons form (car form) (walk-lambda (cdr form) context env)))

(defun walk-setq (form context env)
  (if (cdddr form)
      (let* ((expanded (let ((rforms nil)
                             (tail (cdr form)))
                         (loop (when (null tail) (return (nreverse rforms)))
                               (let ((var (pop tail)) (val (pop tail)))
                                 (push `(setq ,var ,val) rforms)))))
             (walked (walk-repeat-eval expanded env)))
        (if (eq expanded walked)
            form
            `(progn ,@walked)))
      (let* ((var (cadr form))
             (val (caddr form))
             (symmac (and (symbolp var) (car (variable-symbol-macro-p var env)))))
        (if symmac
            (let* ((expanded `(setf ,(cddr symmac) ,val))
                   (walked (walk-form-internal expanded context env)))
              (if (eq expanded walked)
                  form
                  walked))
            (relist form 'setq
                    (walk-form-internal var :set env)
                    (walk-form-internal val :eval env))))))

(defun walk-symbol-macrolet (form context old-env)
  (declare (ignore context))
  (let* ((bindings (cadr form))
         (body (cddr form)))
    (walker-environment-bind
        (new-env old-env
                 :lexical-vars
                 (append (mapcar (lambda (binding)
                                   `(,(car binding)
                                     sb-sys:macro . ,(cadr binding)))
                                 bindings)
                         (env-lexical-variables old-env)))
      (relist* form 'symbol-macrolet bindings
               (walk-declarations body #'walk-repeat-eval new-env)))))

(defun walk-tagbody (form context env)
  (recons form (car form) (walk-tagbody-1 (cdr form) context env)))

(defun walk-tagbody-1 (form context env)
  (and form
       (recons form
               (if (or (symbolp (car form)) (integerp (car form)))
                   (walk-template (car form) 'quote context env)
                   (walk-form-internal (car form) context env))
               (walk-tagbody-1 (cdr form) context env))))

(defun walk-macrolet (form context old-env)
  (walker-environment-bind (old-env old-env)
    (walker-environment-bind (macro-env
                              nil
                              :walk-function (env-walk-function old-env))
      (labels ((walk-definitions (definitions)
                 (and definitions
                      (let ((definition (car definitions)))
                        (recons definitions
                                (relist* definition
                                         (car definition)
                                         (walk-arglist (cadr definition)
                                                       context
                                                       macro-env
                                                       t)
                                         (walk-declarations (cddr definition)
                                                            #'walk-repeat-eval
                                                            macro-env))
                                (walk-definitions (cdr definitions)))))))
        (with-new-definition-in-environment (new-env old-env form)
          (relist* form
                   (car form)
                   (walk-definitions (cadr form))
                   (walk-declarations (cddr form)
                                      #'walk-repeat-eval
                                      new-env)))))))

(defun walk-flet (form context old-env)
  (walker-environment-bind (old-env old-env)
    (labels ((walk-definitions (definitions)
               (if (null definitions)
                   ()
                   (recons definitions
                           (walk-lambda (car definitions) context old-env)
                           (walk-definitions (cdr definitions))))))
      (recons form
              (car form)
              (recons (cdr form)
                      (walk-definitions (cadr form))
                      (with-new-definition-in-environment (new-env old-env form)
                        (walk-declarations (cddr form)
                                           #'walk-repeat-eval
                                           new-env)))))))

(defun walk-labels (form context old-env)
  (walker-environment-bind (old-env old-env)
    (with-new-definition-in-environment (new-env old-env form)
      (labels ((walk-definitions (definitions)
                 (if (null definitions)
                     ()
                     (recons definitions
                             (walk-lambda (car definitions) context new-env)
                             (walk-definitions (cdr definitions))))))
        (recons form
                (car form)
                (recons (cdr form)
                        (walk-definitions (cadr form))
                        (walk-declarations (cddr form)
                                           #'walk-repeat-eval
                                           new-env)))))))

(defun walk-if (form context env)
  (destructuring-bind (if predicate arm1 &optional arm2) form
    (declare (ignore if)) ; should be 'IF
    (relist form
            'if
            (walk-form-internal predicate context env)
            (walk-form-internal arm1 context env)
            (walk-form-internal arm2 context env))))

;;;; examples

#|
;;; Here are some examples of the kinds of things you should be able
;;; to do with your implementation of the macroexpansion environment
;;; hacking mechanism.
;;;
;;; WITH-LEXICAL-MACROS is kind of like MACROLET, but it only takes
;;; names of the macros and actual macroexpansion functions to use to
;;; macroexpand them. The win about that is that for macros which want
;;; to wrap several MACROLETs around their body, they can do this but
;;; have the macroexpansion functions be compiled. See the WITH-RPUSH
;;; example.
;;;
;;; If the implementation had a special way of communicating the
;;; augmented environment back to the evaluator that would be totally
;;; great. It would mean that we could just augment the environment
;;; then pass control back to the implementations own compiler or
;;; interpreter. We wouldn't have to call the actual walker. That
;;; would make this much faster. Since the principal client of this is
;;; defmethod it would make compiling defmethods faster and that would
;;; certainly be a win.

(defmacro with-lexical-macros (macros &body body &environment old-env)
  (with-augmented-environment (new-env old-env :macros macros)
    (walk-form (cons 'progn body) :environment new-env)))

(defun expand-rpush (form env)
  (declare (ignore env))
  `(push ,(caddr form) ,(cadr form)))

(defmacro with-rpush (&body body)
  `(with-lexical-macros ,(list (list 'rpush #'expand-rpush)) ,@body))
|#

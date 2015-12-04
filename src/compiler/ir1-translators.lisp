;;;; the usual place for DEF-IR1-TRANSLATOR forms (and their
;;;; close personal friends)

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

;;;; special forms for control

(def-ir1-translator progn ((&rest forms) start next result)
  #!+sb-doc
  "PROGN form*

Evaluates each FORM in order, returning the values of the last form. With no
forms, returns NIL."
  (ir1-convert-progn-body start next result forms))

(def-ir1-translator if ((test then &optional else) start next result)
  #!+sb-doc
  "IF predicate then [else]

If PREDICATE evaluates to true, evaluate THEN and return its values,
otherwise evaluate ELSE and return its values. ELSE defaults to NIL."
  (let* ((pred-ctran (make-ctran))
         (pred-lvar (make-lvar))
         (then-ctran (make-ctran))
         (then-block (ctran-starts-block then-ctran))
         (else-ctran (make-ctran))
         (else-block (ctran-starts-block else-ctran))
         (maybe-instrument *instrument-if-for-code-coverage*)
         (*instrument-if-for-code-coverage* t)
         (node (make-if :test pred-lvar
                        :consequent then-block
                        :alternative else-block)))
    ;; IR1-CONVERT-MAYBE-PREDICATE requires DEST to be CIF, so the
    ;; order of the following two forms is important
    (setf (lvar-dest pred-lvar) node)
    (multiple-value-bind (context count) (possible-rest-arg-context test)
      (if context
          (ir1-convert start pred-ctran pred-lvar `(%rest-true ,test ,context ,count))
          (ir1-convert start pred-ctran pred-lvar test)))
    (link-node-to-previous-ctran node pred-ctran)

    (let ((start-block (ctran-block pred-ctran)))
      (setf (block-last start-block) node)
      (ctran-starts-block next)

      (link-blocks start-block then-block)
      (link-blocks start-block else-block))

    (let ((path (best-sub-source-path test)))
      (ir1-convert (if (and path maybe-instrument)
                       (let ((*current-path* path))
                         (instrument-coverage then-ctran :then test))
                       then-ctran)
                   next result then)
      (ir1-convert (if (and path maybe-instrument)
                       (let ((*current-path* path))
                         (instrument-coverage else-ctran :else test))
                       else-ctran)
                   next result else))))

;;; To get even remotely sensible results for branch coverage
;;; tracking, we need good source paths. If the macroexpansions
;;; interfere enough the TEST of the conditional doesn't actually have
;;; an original source location (e.g. (UNLESS FOO ...) -> (IF (NOT
;;; FOO) ...). Look through the form, and try to find some subform
;;; that has one.
(defun best-sub-source-path (form)
  (if (policy *lexenv* (= store-coverage-data 0))
      nil
      (labels ((sub (form)
                 (or (get-source-path form)
                     (when (consp form)
                       (unless (eq 'quote (car form))
                         (somesub form)))))
               (somesub (forms)
                 (when (consp forms)
                   (or (sub (car forms))
                       (somesub (cdr forms))))))
        (sub form))))

;;;; BLOCK and TAGBODY

;;;; We make an ENTRY node to mark the start and a :ENTRY cleanup to
;;;; mark its extent. When doing GO or RETURN-FROM, we emit an EXIT
;;;; node.

;;; Make a :ENTRY cleanup and emit an ENTRY node, then convert the
;;; body in the modified environment. We make NEXT start a block now,
;;; since if it was done later, the block would be in the wrong
;;; environment.
(def-ir1-translator block ((name &rest forms) start next result)
  #!+sb-doc
  "BLOCK name form*

Evaluate the FORMS as a PROGN. Within the lexical scope of the body,
RETURN-FROM can be used to exit the form."
  (unless (symbolp name)
    (compiler-error "The block name ~S is not a symbol." name))
  (start-block start)
  (ctran-starts-block next)
  (let* ((dummy (make-ctran))
         (entry (make-entry))
         (cleanup (make-cleanup :kind :block
                                :mess-up entry)))
    (push entry (lambda-entries (lexenv-lambda *lexenv*)))
    (setf (entry-cleanup entry) cleanup)
    (link-node-to-previous-ctran entry start)
    (use-ctran entry dummy)

    (let* ((env-entry (list entry next result))
           (*lexenv* (make-lexenv :blocks (list (cons name env-entry))
                                  :cleanup cleanup)))
      (ir1-convert-progn-body dummy next result forms))))

(def-ir1-translator return-from ((name &optional value) start next result)
  #!+sb-doc
  "RETURN-FROM block-name value-form

Evaluate the VALUE-FORM, returning its values from the lexically enclosing
block BLOCK-NAME. This is constrained to be used only within the dynamic
extent of the block."
  ;; old comment:
  ;;   We make NEXT start a block just so that it will have a block
  ;;   assigned. People assume that when they pass a ctran into
  ;;   IR1-CONVERT as NEXT, it will have a block when it is done.
  ;; KLUDGE: Note that this block is basically fictitious. In the code
  ;;   (BLOCK B (RETURN-FROM B) (SETQ X 3))
  ;; it's the block which answers the question "which block is
  ;; the (SETQ X 3) in?" when the right answer is that (SETQ X 3) is
  ;; dead code and so doesn't really have a block at all. The existence
  ;; of this block, and that way that it doesn't explicitly say
  ;; "I'm actually nowhere at all" makes some logic (e.g.
  ;; BLOCK-HOME-LAMBDA-OR-NULL) more obscure, and it might be better
  ;; to get rid of it, perhaps using a special placeholder value
  ;; to indicate the orphanedness of the code.
  (ctran-starts-block next)
  (let* ((found (or (lexenv-find name blocks)
                    (compiler-error "return for unknown block: ~S" name)))
         (exit-ctran (second found))
         (value-ctran (make-ctran))
         (value-lvar (make-lvar))
         (entry (first found))
         (exit (make-exit :entry entry
                          :value value-lvar)))
    (when (ctran-deleted-p exit-ctran)
      (throw 'locall-already-let-converted exit-ctran))
    (setf (lvar-dest value-lvar) exit)
    (ir1-convert start value-ctran value-lvar value)
    (push exit (entry-exits entry))
    (link-node-to-previous-ctran exit value-ctran)
    (let ((home-lambda (ctran-home-lambda-or-null start)))
      (when home-lambda
        (sset-adjoin entry (lambda-calls-or-closes home-lambda))))
    (use-continuation exit exit-ctran (third found))))

;;; Return a list of the segments of a TAGBODY. Each segment looks
;;; like (<tag> <form>* (go <next tag>)). That is, we break up the
;;; tagbody into segments of non-tag statements, and explicitly
;;; represent the drop-through with a GO. The first segment has a
;;; dummy NIL tag, since it represents code before the first tag. Note
;;; however that NIL may appear as the tag of an inner segment. The
;;; last segment (which may also be the first segment) ends in NIL
;;; rather than a GO.
(defun parse-tagbody (body)
  (declare (list body))
  (collect ((tags)
            (segments))
    (let ((current body))
      (loop
       (let ((next-segment (member-if #'atom current)))
         (unless next-segment
           (segments `(,@current nil))
           (return))
         (let ((tag (car next-segment)))
           (when (member tag (tags))
             (compiler-error
              "The tag ~S appears more than once in a tagbody."
              tag))
           (unless (or (symbolp tag) (integerp tag))
             (compiler-error "~S is not a legal go tag." tag))
           (tags tag)
           (segments `(,@(ldiff current next-segment) (go ,tag))))
         (setq current (rest next-segment))))
      (mapcar #'cons (cons nil (tags)) (segments)))))

;;; Set up the cleanup, emitting the entry node. Then make a block for
;;; each tag, building up the tag list for LEXENV-TAGS as we go.
;;; Finally, convert each segment with the precomputed Start and Cont
;;; values.
(def-ir1-translator tagbody ((&rest statements) start next result)
  #!+sb-doc
  "TAGBODY {tag | statement}*

Define tags for use with GO. The STATEMENTS are evaluated in order, skipping
TAGS, and NIL is returned. If a statement contains a GO to a defined TAG
within the lexical scope of the form, then control is transferred to the next
statement following that tag. A TAG must be an integer or a symbol. A
STATEMENT must be a list. Other objects are illegal within the body."
  (start-block start)
  (ctran-starts-block next)
  (let* ((dummy (make-ctran))
         (entry (make-entry))
         (segments (parse-tagbody statements))
         (cleanup (make-cleanup :kind :tagbody
                                :mess-up entry)))
    (push entry (lambda-entries (lexenv-lambda *lexenv*)))
    (setf (entry-cleanup entry) cleanup)
    (link-node-to-previous-ctran entry start)
    (use-ctran entry dummy)

    (collect ((tags)
              (starts)
              (ctrans))
      (starts dummy)
      (dolist (segment (rest segments))
        (let* ((tag-ctran (make-ctran))
               (tag (list (car segment) entry tag-ctran)))
          (ctrans tag-ctran)
          (starts tag-ctran)
          (ctran-starts-block tag-ctran)
          (tags tag)))
      (ctrans next)

      (let ((*lexenv* (make-lexenv :cleanup cleanup :tags (tags))))
        (mapc (lambda (segment start end)
                (ir1-convert-progn-body start end
                                        (when (eq end next) result)
                                        (rest segment)))
              segments (starts) (ctrans))))))

;;; Emit an EXIT node without any value.
(def-ir1-translator go ((tag) start next result)
  #!+sb-doc
  "GO tag

Transfer control to the named TAG in the lexically enclosing TAGBODY. This is
constrained to be used only within the dynamic extent of the TAGBODY."
  (ctran-starts-block next)
  (let* ((found (or (lexenv-find tag tags :test #'eql)
                    (compiler-error "attempt to GO to nonexistent tag: ~S"
                                    tag)))
         (entry (first found))
         (exit (make-exit :entry entry)))
    (push exit (entry-exits entry))
    (link-node-to-previous-ctran exit start)
    (let ((home-lambda (ctran-home-lambda-or-null start)))
      (when home-lambda
        (sset-adjoin entry (lambda-calls-or-closes home-lambda))))
    (use-ctran exit (second found))))

;;;; translators for compiler-magic special forms

;;; This handles EVAL-WHEN in non-top-level forms. (EVAL-WHENs in top
;;; level forms are picked off and handled by PROCESS-TOPLEVEL-FORM,
;;; so that they're never seen at this level.)
;;;
;;; ANSI "3.2.3.1 Processing of Top Level Forms" says that processing
;;; of non-top-level EVAL-WHENs is very simple:
;;;   EVAL-WHEN forms cause compile-time evaluation only at top level.
;;;   Both :COMPILE-TOPLEVEL and :LOAD-TOPLEVEL situation specifications
;;;   are ignored for non-top-level forms. For non-top-level forms, an
;;;   eval-when specifying the :EXECUTE situation is treated as an
;;;   implicit PROGN including the forms in the body of the EVAL-WHEN
;;;   form; otherwise, the forms in the body are ignored.
(def-ir1-translator eval-when ((situations &rest forms) start next result)
  #!+sb-doc
  "EVAL-WHEN (situation*) form*

Evaluate the FORMS in the specified SITUATIONS (any of :COMPILE-TOPLEVEL,
:LOAD-TOPLEVEL, or :EXECUTE, or (deprecated) COMPILE, LOAD, or EVAL)."
  (multiple-value-bind (ct lt e) (parse-eval-when-situations situations)
    (declare (ignore ct lt))
    (ir1-convert-progn-body start next result (and e forms)))
  (values))

;;; common logic for MACROLET and SYMBOL-MACROLET
;;;
;;; Call DEFINITIONIZE-FUN on each element of DEFINITIONS to find its
;;; in-lexenv representation, stuff the results into *LEXENV*, and
;;; call FUN (with no arguments).
(defun %funcall-in-foomacrolet-lexenv (definitionize-fun
                                       definitionize-keyword
                                       definitions
                                       fun)
  (declare (type function definitionize-fun fun)
           (type (member :vars :funs) definitionize-keyword))
  (unless (listp definitions)
    (compiler-error "Malformed ~s definitions: ~s"
                    (case definitionize-keyword
                      (:vars 'symbol-macrolet)
                      (:funs 'macrolet))
                    definitions))
  (let* ((processed-definitions (mapcar definitionize-fun definitions))
         (*lexenv* (make-lexenv definitionize-keyword processed-definitions)))
    ;; Do this after processing, since the definitions can be malformed.
    (unless (= (length definitions)
               (length (remove-duplicates definitions :key #'first)))
      (compiler-style-warn "Duplicate definitions in ~S" definitions))
    ;; I wonder how much of an compiler performance penalty this
    ;; non-constant keyword is.
    (funcall fun definitionize-keyword processed-definitions)))

;;; Tweak LEXENV to include the DEFINITIONS from a MACROLET, then
;;; call FUN (with no arguments).
;;;
;;; This is split off from the IR1 convert method so that it can be
;;; shared by the special-case top level MACROLET processing code, and
;;; further split so that the special-case MACROLET processing code in
;;; EVAL can likewise make use of it.
(defun macrolet-definitionize-fun (context lexenv)
  (flet ((fail (control &rest args)
           (ecase context
             (:compile (apply #'compiler-error control args))
             (:eval (error 'simple-program-error
                           :format-control control
                           :format-arguments args)))))
    (lambda (definition)
      (unless (list-of-length-at-least-p definition 2)
        (fail "The list ~S is too short to be a legal local macro definition."
              definition))
      (destructuring-bind (name arglist &body body) definition
        (unless (symbolp name)
          (fail "The local macro name ~S is not a symbol." name))
        (when (fboundp name)
          (program-assert-symbol-home-package-unlocked
           context name "binding ~A as a local macro"))
        (unless (listp arglist)
          (fail "The local macro argument list ~S is not a list."
                arglist))
        `(,name macro .
                ,(compile-in-lexenv
                  nil
                  (make-macro-lambda nil arglist body 'macrolet name)
                  lexenv))))))

(defun funcall-in-macrolet-lexenv (definitions fun context)
  (%funcall-in-foomacrolet-lexenv
   (macrolet-definitionize-fun context (make-restricted-lexenv *lexenv*))
   :funs
   definitions
   fun))

(def-ir1-translator macrolet ((definitions &rest body) start next result)
  #!+sb-doc
  "MACROLET ({(name lambda-list form*)}*) body-form*

Evaluate the BODY-FORMS in an environment with the specified local macros
defined. NAME is the local macro name, LAMBDA-LIST is a DEFMACRO style
destructuring lambda list, and the FORMS evaluate to the expansion."
  (funcall-in-macrolet-lexenv
   definitions
   (lambda (&key funs)
     (declare (ignore funs))
     (ir1-translate-locally body start next result))
   :compile))

(defun symbol-macrolet-definitionize-fun (context)
  (flet ((fail (control &rest args)
           (ecase context
             (:compile (apply #'compiler-error control args))
             (:eval (error 'simple-program-error
                           :format-control control
                           :format-arguments args)))))
    (lambda (definition)
      (unless (proper-list-of-length-p definition 2)
        (fail "malformed symbol/expansion pair: ~S" definition))
      (destructuring-bind (name expansion) definition
        (unless (symbolp name)
          (fail "The local symbol macro name ~S is not a symbol." name))
        (when (or (boundp name) (eq (info :variable :kind name) :macro))
          (program-assert-symbol-home-package-unlocked
           context name "binding ~A as a local symbol-macro"))
        (let ((kind (info :variable :kind name)))
          (when (member kind '(:special :constant :global))
            (fail "Attempt to bind a ~(~A~) variable with SYMBOL-MACROLET: ~S"
                  kind name)))
        ;; A magical cons that MACROEXPAND-1 understands.
        `(,name . (macro . ,expansion))))))

(defun funcall-in-symbol-macrolet-lexenv (definitions fun context)
  (%funcall-in-foomacrolet-lexenv
   (symbol-macrolet-definitionize-fun context)
   :vars
   definitions
   fun))

(def-ir1-translator symbol-macrolet
    ((macrobindings &body body) start next result)
  #!+sb-doc
  "SYMBOL-MACROLET ({(name expansion)}*) decl* form*

Define the NAMES as symbol macros with the given EXPANSIONS. Within the
body, references to a NAME will effectively be replaced with the EXPANSION."
  (funcall-in-symbol-macrolet-lexenv
   macrobindings
   (lambda (&key vars)
     (ir1-translate-locally body start next result :vars vars))
   :compile))

;;;; %PRIMITIVE
;;;;
;;;; Uses of %PRIMITIVE are either expanded into Lisp code or turned
;;;; into a funny function.

;;; Carefully evaluate a list of forms, returning a list of the results.
(defun eval-info-args (args)
  (declare (list args))
  (handler-case (mapcar #'eval args)
    (error (condition)
      (compiler-error "Lisp error during evaluation of info args:~%~A"
                      condition))))

;;; Convert to the %%PRIMITIVE funny function. The first argument is
;;; the template, the second is a list of the results of any
;;; codegen-info args, and the remaining arguments are the runtime
;;; arguments.
;;;
;;; We do various error checking now so that we don't bomb out with
;;; a fatal error during IR2 conversion.
;;;
;;; KLUDGE: It's confusing having multiple names floating around for
;;; nearly the same concept: PRIMITIVE, TEMPLATE, VOP. Now that CMU
;;; CL's *PRIMITIVE-TRANSLATORS* stuff is gone, we could call
;;; primitives VOPs, rename TEMPLATE to VOP-TEMPLATE, rename
;;; BACKEND-TEMPLATE-NAMES to BACKEND-VOPS, and rename %PRIMITIVE to
;;; VOP or %VOP.. -- WHN 2001-06-11
;;; FIXME: Look at doing this ^, it doesn't look too hard actually.
(def-ir1-translator %primitive ((name &rest args) start next result)
  (declare (type symbol name))
  (let* ((template (or (gethash name *backend-template-names*)
                       (bug "undefined primitive ~A" name)))
         (required (length (template-arg-types template)))
         (info (template-info-arg-count template))
         (min (+ required info))
         (nargs (length args)))
    (if (template-more-args-type template)
        (when (< nargs min)
          (bug "Primitive ~A was called with ~R argument~:P, ~
                but wants at least ~R."
               name
               nargs
               min))
        (unless (= nargs min)
          (bug "Primitive ~A was called with ~R argument~:P, ~
                but wants exactly ~R."
               name
               nargs
               min)))

    (when (template-conditional-p template)
      (bug "%PRIMITIVE was used with a conditional template."))

    (when (template-more-results-type template)
      (bug "%PRIMITIVE was used with an unknown values template."))

    (ir1-convert start next result
                 `(%%primitive ',template
                               ',(eval-info-args
                                  (subseq args required min))
                               ,@(subseq args 0 required)
                               ,@(subseq args min)))))

;;;; QUOTE

(def-ir1-translator quote ((thing) start next result)
  #!+sb-doc
  "QUOTE value

Return VALUE without evaluating it."
  (reference-constant start next result thing))

(defun name-context ()
  ;; Name of the outermost non-NIL BLOCK, or the source namestring
  ;; of the source file.
  (let ((context
          (or (car (find-if (lambda (b)
                              (let ((name (pop b)))
                                (and name
                                     ;; KLUDGE: High debug adds this block on
                                     ;; some platforms.
                                     #!-unwind-to-frame-and-call-vop
                                     (neq 'return-value-tag name)
                                     ;; KLUDGE: CATCH produces blocks whose
                                     ;; cleanup is :CATCH.
                                     (neq :catch (cleanup-kind (entry-cleanup (pop b)))))))
                            (lexenv-blocks *lexenv*) :from-end t))
              *source-namestring*
              (let* ((p (or sb!xc:*compile-file-truename* *load-truename*)))
                (when p
                  #+sb-xc-host (lpnify-namestring (namestring p) (pathname-directory p) (pathname-type p))
                  #-sb-xc-host (namestring p))))))
    (when context
      (list :in context))))

;;;; FUNCTION and NAMED-LAMBDA
(defun name-lambdalike (thing)
  (case (car thing)
    ((named-lambda)
     (or (second thing)
         `(lambda ,(strip-lambda-list (third thing) :name) ,(name-context))))
    ((lambda)
     `(lambda ,(strip-lambda-list (second thing) :name) ,@(name-context)))
    ((lambda-with-lexenv)
     ;; FIXME: Get the original DEFUN name here.
     `(lambda ,(fifth thing)))
    (otherwise
     (compiler-error "Not a valid lambda expression:~%  ~S"
                     thing))))

(defun fun-name-leaf (thing)
  (cond
    ((typep thing
            '(cons (member lambda named-lambda lambda-with-lexenv)))
     (values (ir1-convert-lambdalike
              thing :debug-name (name-lambdalike thing))
             t))
    ((legal-fun-name-p thing)
     (values (find-lexically-apparent-fun
              thing "as the argument to FUNCTION")
             nil))
    (t
     (compiler-error "~S is not a legal function name." thing))))

(def-ir1-translator %%allocate-closures ((&rest leaves) start next result)
  (aver (eq result 'nil))
  (let ((lambdas leaves))
    (ir1-convert start next result `(%allocate-closures ',lambdas))
    (let ((allocator (node-dest (ctran-next start))))
      (dolist (lambda lambdas)
        (setf (functional-allocator lambda) allocator)))))

(defmacro with-fun-name-leaf ((leaf thing start &key global-function) &body body)
  `(multiple-value-bind (,leaf allocate-p)
       (if ,global-function
           (find-global-fun ,thing t)
           (fun-name-leaf ,thing))
     (if allocate-p
         (let ((.new-start. (make-ctran)))
           (ir1-convert ,start .new-start. nil `(%%allocate-closures ,leaf))
           (let ((,start .new-start.))
             ,@body))
         (locally
             ,@body))))

(def-ir1-translator function ((thing) start next result)
  #!+sb-doc
  "FUNCTION name

Return the lexically apparent definition of the function NAME. NAME may also
be a lambda expression."
  (with-fun-name-leaf (leaf thing start)
    (reference-leaf start next result leaf)))

;;; Like FUNCTION, but ignores local definitions and inline
;;; expansions, and doesn't nag about undefined functions.
;;; Used for optimizing things like (FUNCALL 'FOO).
(def-ir1-translator global-function ((thing) start next result)
  (with-fun-name-leaf (leaf thing start :global-function t)
    (reference-leaf start next result leaf)))

(defun constant-global-fun-name (thing)
  (let ((constantp (sb!xc:constantp thing)))
    (when constantp
      (let ((name (constant-form-value thing)))
        (when (legal-fun-name-p name)
          name)))))

(defun lvar-constant-global-fun-name (lvar)
  (when (constant-lvar-p lvar)
    (let ((name (lvar-value lvar)))
      (when (legal-fun-name-p name)
        name))))

(defun ensure-source-fun-form (source &optional give-up)
  (let ((op (when (consp source) (car source))))
    (cond ((eq op '%coerce-callable-to-fun)
           (ensure-source-fun-form (second source)))
          ((member op '(function global-function lambda named-lambda))
           (values source nil))
          (t
           (let ((cname (constant-global-fun-name source)))
             (if cname
                 (values `(global-function ,cname) nil)
                 (values `(%coerce-callable-to-fun ,source) give-up)))))))

(defun ensure-lvar-fun-form (lvar lvar-name &optional give-up)
  (aver (and lvar-name (symbolp lvar-name)))
  (if (csubtypep (lvar-type lvar) (specifier-type 'function))
      lvar-name
      (let ((cname (lvar-constant-global-fun-name lvar)))
        (cond (cname
               `(global-function ,cname))
              (give-up
               (give-up-ir1-transform "not known to be a function"))
              (t
               `(%coerce-callable-to-fun ,lvar-name))))))

;;;; FUNCALL

;;; FUNCALL is implemented on %FUNCALL, which can only call functions
;;; (not symbols). %FUNCALL is used directly in some places where the
;;; call should always be open-coded even if FUNCALL is :NOTINLINE.
(deftransform funcall ((function &rest args) * *)
  (let ((arg-names (make-gensym-list (length args))))
    `(lambda (function ,@arg-names)
       (declare (ignorable function))
       `(%funcall ,(ensure-lvar-fun-form function 'function) ,@arg-names))))

(def-ir1-translator %funcall ((function &rest args) start next result)
  ;; MACROEXPAND so that (LAMBDA ...) forms arriving here don't get an
  ;; extra cast inserted for them.
  (let ((function (%macroexpand function *lexenv*)))
    (if (typep function '(cons (member function global-function) (cons t null)))
        (with-fun-name-leaf (leaf (cadr function) start
                                  :global-function (eq (car function)
                                                       'global-function))
          (ir1-convert start next result `(,leaf ,@args)))
        (let ((ctran (make-ctran))
              (fun-lvar (make-lvar)))
          (ir1-convert start ctran fun-lvar `(the function ,function))
          (ir1-convert-combination-args fun-lvar ctran next result args)))))

;;; This source transform exists to reduce the amount of work for the
;;; compiler. If the called function is a FUNCTION form, then convert
;;; directly to %FUNCALL, instead of waiting around for type
;;; inference.
(define-source-transform funcall (function &rest args)
  `(%funcall ,(ensure-source-fun-form function) ,@args))

(deftransform %coerce-callable-to-fun ((thing) * * :node node)
  "optimize away possible call to FDEFINITION at runtime"
  (ensure-lvar-fun-form thing 'thing t))

(define-source-transform %coerce-callable-to-fun (thing)
  (ensure-source-fun-form thing t))

;;;; LET and LET*
;;;;
;;;; (LET and LET* can't be implemented as macros due to the fact that
;;;; any pervasive declarations also affect the evaluation of the
;;;; arguments.)

;;; Given a list of binding specifiers in the style of LET, return:
;;;  1. The list of var structures for the variables bound.
;;;  2. The initial value form for each variable.
;;;
;;; The variable names are checked for legality and globally special
;;; variables are marked as such. Context is the name of the form, for
;;; error reporting purposes.
(declaim (ftype (function (list symbol) (values list list))
                extract-let-vars))
(defun extract-let-vars (bindings context)
  (collect ((vars)
            (vals)
            (names))
    (flet ((get-var (name)
             (varify-lambda-arg name
                                (if (eq context 'let*)
                                    nil
                                    (names))
                                context)))
      (dolist (spec bindings)
        (cond ((atom spec)
               (let ((var (get-var spec)))
                 (vars var)
                 (names spec)
                 (vals nil)))
              (t
               (unless (proper-list-of-length-p spec 1 2)
                 (compiler-error "The ~S binding spec ~S is malformed."
                                 context
                                 spec))
               (let* ((name (first spec))
                      (var (get-var name)))
                 (vars var)
                 (names name)
                 (vals (second spec)))))))
    (dolist (name (names))
      (when (eq (info :variable :kind name) :macro)
        (program-assert-symbol-home-package-unlocked
         :compile name "lexically binding symbol-macro ~A")))
    (values (vars) (vals))))

(def-ir1-translator let ((bindings &body body) start next result)
  #!+sb-doc
  "LET ({(var [value]) | var}*) declaration* form*

During evaluation of the FORMS, bind the VARS to the result of evaluating the
VALUE forms. The variables are bound in parallel after all of the VALUES forms
have been evaluated."
  (cond ((null bindings)
         (ir1-translate-locally body start next result))
        ((listp bindings)
         (multiple-value-bind (forms decls) (parse-body body nil)
           (multiple-value-bind (vars values) (extract-let-vars bindings 'let)
             (binding* ((ctran (make-ctran))
                        (fun-lvar (make-lvar))
                        ((next result)
                         (processing-decls (decls vars nil next result
                                                  post-binding-lexenv)
                           (let ((fun (ir1-convert-lambda-body
                                       forms
                                       vars
                                       :post-binding-lexenv post-binding-lexenv
                                       :debug-name (debug-name 'let bindings))))
                             (reference-leaf start ctran fun-lvar fun))
                           (values next result))))
               (ir1-convert-combination-args fun-lvar ctran next result values)))))
        (t
         (compiler-error "Malformed LET bindings: ~S." bindings))))

(def-ir1-translator let* ((bindings &body body)
                          start next result)
  #!+sb-doc
  "LET* ({(var [value]) | var}*) declaration* form*

Similar to LET, but the variables are bound sequentially, allowing each VALUE
form to reference any of the previous VARS."
  (if (listp bindings)
      (multiple-value-bind (forms decls) (parse-body body nil)
        (multiple-value-bind (vars values) (extract-let-vars bindings 'let*)
          (processing-decls (decls vars nil next result post-binding-lexenv)
            (ir1-convert-aux-bindings start
                                      next
                                      result
                                      forms
                                      vars
                                      values
                                      post-binding-lexenv))))
      (compiler-error "Malformed LET* bindings: ~S." bindings)))

;;; logic shared between IR1 translators for LOCALLY, MACROLET,
;;; and SYMBOL-MACROLET
;;;
;;; Note that all these things need to preserve toplevel-formness,
;;; but we don't need to worry about that within an IR1 translator,
;;; since toplevel-formness is picked off by PROCESS-TOPLEVEL-FOO
;;; forms before we hit the IR1 transform level.
(defun ir1-translate-locally (body start next result &key vars funs)
  (declare (type ctran start next) (type (or lvar null) result)
           (type list body))
  (multiple-value-bind (forms decls) (parse-body body nil)
    (processing-decls (decls vars funs next result)
      (ir1-convert-progn-body start next result forms))))

(def-ir1-translator locally ((&body body) start next result)
  #!+sb-doc
  "LOCALLY declaration* form*

Sequentially evaluate the FORMS in a lexical environment where the
DECLARATIONS have effect. If LOCALLY is a top level form, then the FORMS are
also processed as top level forms."
  (ir1-translate-locally body start next result))

;;;; FLET and LABELS

;;; Given a list of local function specifications in the style of
;;; FLET, return lists of the function names and of the lambdas which
;;; are their definitions.
;;;
;;; The function names are checked for legality. CONTEXT is the name
;;; of the form, for error reporting.
(declaim (ftype (function (list symbol) (values list list)) extract-flet-vars))
(defun extract-flet-vars (definitions context)
  (collect ((names)
            (defs))
    (dolist (def definitions)
      (when (or (atom def) (< (length def) 2))
        (compiler-error "The ~S definition spec ~S is malformed." context def))

      (let ((name (first def)))
        (check-fun-name name)
        (when (fboundp name)
          (program-assert-symbol-home-package-unlocked
           :compile name "binding ~A as a local function"))
        (names name)
        (multiple-value-bind (forms decls doc) (parse-body (cddr def) t)
          (defs `(lambda ,(second def)
                   ,@(when doc (list doc))
                   ,@decls
                   (block ,(fun-name-block-name name)
                     . ,forms))))))
    (values (names) (defs))))

(defun ir1-convert-fbindings (start next result funs body)
  (let ((ctran (make-ctran))
        (dx-p (find-if #'leaf-dynamic-extent funs)))
    (when dx-p
      (ctran-starts-block ctran)
      (ctran-starts-block next))
    (ir1-convert start ctran nil `(%%allocate-closures ,@funs))
    (cond (dx-p
           (let* ((dummy (make-ctran))
                  (entry (make-entry))
                  (cleanup (make-cleanup :kind :dynamic-extent
                                         :mess-up entry
                                         :info (list (node-dest
                                                      (ctran-next start))))))
             (push entry (lambda-entries (lexenv-lambda *lexenv*)))
             (setf (entry-cleanup entry) cleanup)
             (link-node-to-previous-ctran entry ctran)
             (use-ctran entry dummy)

             (let ((*lexenv* (make-lexenv :cleanup cleanup)))
               (ir1-convert-progn-body dummy next result body))))
          (t (ir1-convert-progn-body ctran next result body)))))

(def-ir1-translator flet ((definitions &body body)
                          start next result)
  #!+sb-doc
  "FLET ({(name lambda-list declaration* form*)}*) declaration* body-form*

Evaluate the BODY-FORMS with local function definitions. The bindings do
not enclose the definitions; any use of NAME in the FORMS will refer to the
lexically apparent function definition in the enclosing environment."
  (multiple-value-bind (forms decls) (parse-body body nil)
    (unless (listp definitions)
      (compiler-error "Malformed FLET definitions: ~s" definitions))
    (multiple-value-bind (names defs)
        (extract-flet-vars definitions 'flet)
      (let ((fvars (mapcar (lambda (n d)
                             (ir1-convert-lambda
                              d :source-name n
                                :maybe-add-debug-catch t
                                :debug-name
                                (debug-name 'flet n t)))
                           names defs)))
        (processing-decls (decls nil fvars next result)
          (let ((*lexenv* (make-lexenv :funs (pairlis names fvars))))
            (ir1-convert-fbindings start next result fvars forms)))))))

(def-ir1-translator labels ((definitions &body body) start next result)
  #!+sb-doc
  "LABELS ({(name lambda-list declaration* form*)}*) declaration* body-form*

Evaluate the BODY-FORMS with local function definitions. The bindings enclose
the new definitions, so the defined functions can call themselves or each
other."
  (multiple-value-bind (forms decls) (parse-body body nil)
    (unless (listp definitions)
      (compiler-error "Malformed LABELS definitions: ~s" definitions))
    (multiple-value-bind (names defs)
        (extract-flet-vars definitions 'labels)
      (let* (;; dummy LABELS functions, to be used as placeholders
             ;; during construction of real LABELS functions
             (placeholder-funs (mapcar (lambda (name)
                                         (make-functional
                                          :%source-name name
                                          :%debug-name (debug-name
                                                        'labels-placeholder
                                                        name)))
                                       names))
             ;; (like PAIRLIS but guaranteed to preserve ordering:)
             (placeholder-fenv (mapcar #'cons names placeholder-funs))
             ;; the real LABELS functions, compiled in a LEXENV which
             ;; includes the dummy LABELS functions
             (real-funs
              (let ((*lexenv* (make-lexenv :funs placeholder-fenv)))
                (mapcar (lambda (name def)
                          (ir1-convert-lambda def
                                              :source-name name
                                              :maybe-add-debug-catch t
                                              :debug-name (debug-name 'labels name t)))
                        names defs))))

        ;; Modify all the references to the dummy function leaves so
        ;; that they point to the real function leaves.
        (loop for real-fun in real-funs and
              placeholder-cons in placeholder-fenv do
              (substitute-leaf real-fun (cdr placeholder-cons))
              (setf (cdr placeholder-cons) real-fun))

        ;; Voila.
        (processing-decls (decls nil real-funs next result)
          (let ((*lexenv* (make-lexenv
                           ;; Use a proper FENV here (not the
                           ;; placeholder used earlier) so that if the
                           ;; lexical environment is used for inline
                           ;; expansion we'll get the right functions.
                           :funs (pairlis names real-funs))))
            (ir1-convert-fbindings start next result real-funs forms)))))))


;;;; the THE special operator, and friends

;;; A logic shared among THE and TRULY-THE.
(defun the-in-policy (type value policy start next result)
  (let ((type (if (ctype-p type) type
                   (compiler-values-specifier-type type))))
    (cond ((or (eq type *wild-type*)
               (eq type *universal-type*)
               (and (leaf-p value)
                    (values-subtypep (make-single-value-type (leaf-type value))
                                     type))
               (and (sb!xc:constantp value)
                    (or (not (values-type-p type))
                        (values-type-may-be-single-value-p type))
                    (ctypep (constant-form-value value)
                            (single-value-type type))))
           (ir1-convert start next result value))
          (t (let ((value-ctran (make-ctran))
                   (value-lvar (make-lvar)))
               (ir1-convert start value-ctran value-lvar value)
               (let ((cast (make-cast value-lvar type policy)))
                 (link-node-to-previous-ctran cast value-ctran)
                 (setf (lvar-dest value-lvar) cast)
                 (use-continuation cast next result)))))))

;;; Assert that FORM evaluates to the specified type (which may be a
;;; VALUES type). TYPE may be a type specifier or (as a hack) a CTYPE.
(def-ir1-translator the ((value-type form) start next result)
  #!+sb-doc
  "Specifies that the values returned by FORM conform to the VALUE-TYPE.

CLHS specifies that the consequences are undefined if any result is
not of the declared type, but SBCL treats declarations as assertions
as long as SAFETY is at least 2, in which case incorrect type
information will result in a runtime type-error instead of leading to
eg. heap corruption. This is however expressly non-portable: use
CHECK-TYPE instead of THE to catch type-errors at runtime. THE is best
considered an optimization tool to inform the compiler about types it
is unable to derive from other declared types."
  (the-in-policy value-type form (lexenv-policy *lexenv*) start next result))

;;; This is like the THE special form, except that it believes
;;; whatever you tell it. It will never generate a type check, but
;;; will cause a warning if the compiler can prove the assertion is
;;; wrong.
;;;
;;; For the benefit of code-walkers we also add a macro-expansion. (Using INFO
;;; directly to get around safeguards for adding a macro-expansion for special
;;; operator.) Because :FUNCTION :KIND remains :SPECIAL-FORM, the compiler
;;; never uses the macro -- but manually calling its MACRO-FUNCTION or
;;; MACROEXPANDing TRULY-THE forms does.
(def-ir1-translator truly-the ((value-type form) start next result)
  #!+sb-doc
  "Specifies that the values returned by FORM conform to the
VALUE-TYPE, and causes the compiler to trust this information
unconditionally.

Consequences are undefined if any result is not of the declared type
-- typical symptoms including memory corruptions. Use with great
care."
  (the-in-policy value-type form **zero-typecheck-policy** start next result))

(def-ir1-translator bound-cast ((array bound index) start next result)
  (let ((check-bound-tran (make-ctran))
        (index-ctran (make-ctran))
        (index-lvar (make-lvar)))
    ;; CHECK-BOUND transform ensure that INDEX won't be evaluated twice
    (ir1-convert start check-bound-tran nil `(%check-bound ,array ,bound ,index))
    (ir1-convert check-bound-tran index-ctran index-lvar index)
    (let* ((check-bound-combination (ctran-use check-bound-tran))
           (array (first (combination-args check-bound-combination)))
           (bound (second (combination-args check-bound-combination)))
           (derived (constant-lvar-p bound))
           (type (specifier-type (if derived
                                     `(integer 0 (,(lvar-value bound)))
                                     '(and unsigned-byte fixnum))))
           (cast (make-bound-cast :value index-lvar
                                  :asserted-type type
                                  :type-to-check type
                                  :derived-type (coerce-to-values type)
                                  :check check-bound-combination
                                  :derived derived
                                  :array array
                                  :bound bound)))
      (link-node-to-previous-ctran cast index-ctran)
      (setf (lvar-dest index-lvar) cast)
      (use-continuation cast next result))))
#-sb-xc-host
(setf (info :function :macro-function 'truly-the)
      (lambda (whole env)
        (declare (ignore env))
        `(the ,@(cdr whole))))

;;;; SETQ

(defun explode-setq (form err-fun)
  (collect ((sets))
    (do ((op (car form))
         (thing (cdr form) (cddr thing)))
        ((endp thing) (sets))
      (if (endp (cdr thing))
          (funcall err-fun "odd number of args to ~A: ~S" op form)
          (sets `(,op ,(first thing) ,(second thing)))))))

;;; If there is a definition in LEXENV-VARS, just set that, otherwise
;;; look at the global information. If the name is for a constant,
;;; then error out.
(def-ir1-translator setq ((&whole source &rest things) start next result)
  (if (proper-list-of-length-p things 2)
      (let* ((name (first things))
             (value-form (second things))
             (leaf (or (lexenv-find name vars) (find-free-var name))))
        (etypecase leaf
            (leaf
             (when (constant-p leaf)
               (compiler-error "~S is a constant and thus can't be set." name))
             (when (lambda-var-p leaf)
               (let ((home-lambda (ctran-home-lambda-or-null start)))
                 (when home-lambda
                   (sset-adjoin leaf (lambda-calls-or-closes home-lambda))))
               (when (lambda-var-ignorep leaf)
                 ;; ANSI's definition of "Declaration IGNORE, IGNORABLE"
                 ;; requires that this be a STYLE-WARNING, not a full warning.
                 (compiler-style-warn
                  "~S is being set even though it was declared to be ignored."
                  name)))
             (if (and (global-var-p leaf) (eq :unknown (global-var-kind leaf)))
                 ;; For undefined variables go through SET, so that we can catch
                 ;; constant modifications.
                 (ir1-convert start next result `(set ',name ,value-form))
                 (setq-var start next result leaf value-form)))
            (cons
             (aver (eq (car leaf) 'macro))
             ;; Allow *MACROEXPAND-HOOK* to see NAME get expanded,
             ;; not just see a use of SETF on the new place.
             (ir1-convert start next result `(setf ,name ,(second things))))
            (heap-alien-info
             (ir1-convert start next result
                          `(%set-heap-alien ',leaf ,(second things))))))
      (ir1-convert-progn-body start next result
                              (explode-setq source 'compiler-error))))

;;; This is kind of like REFERENCE-LEAF, but we generate a SET node.
;;; This should only need to be called in SETQ.
(defun setq-var (start next result var value)
  (declare (type ctran start next) (type (or lvar null) result)
           (type basic-var var))
  (let ((dest-ctran (make-ctran))
        (dest-lvar (make-lvar))
        (type (or (lexenv-find var type-restrictions)
                  (leaf-type var))))
    (ir1-convert start dest-ctran dest-lvar `(the ,(type-specifier type)
                                                  ,value))
    (let ((res (make-set :var var :value dest-lvar)))
      (setf (lvar-dest dest-lvar) res)
      (setf (leaf-ever-used var) t)
      (push res (basic-var-sets var))
      (link-node-to-previous-ctran res dest-ctran)
      (use-continuation res next result))))

;;;; CATCH, THROW and UNWIND-PROTECT

;;; We turn THROW into a MULTIPLE-VALUE-CALL of a magical function,
;;; since as as far as IR1 is concerned, it has no interesting
;;; properties other than receiving multiple-values.
(def-ir1-translator throw ((tag result) start next result-lvar)
  #!+sb-doc
  "THROW tag form

Do a non-local exit, return the values of FORM from the CATCH whose tag is EQ
to TAG."
  (ir1-convert start next result-lvar
               `(multiple-value-call #'%throw ,tag ,result)))

;;; This is a special special form used to instantiate a cleanup as
;;; the current cleanup within the body. KIND is the kind of cleanup
;;; to make, and MESS-UP is a form that does the mess-up action. We
;;; make the MESS-UP be the USE of the MESS-UP form's continuation,
;;; and introduce the cleanup into the lexical environment. We
;;; back-patch the ENTRY-CLEANUP for the current cleanup to be the new
;;; cleanup, since this inner cleanup is the interesting one.
(def-ir1-translator %within-cleanup
    ((kind mess-up &body body) start next result)
  (let ((dummy (make-ctran))
        (dummy2 (make-ctran)))
    (ir1-convert start dummy nil mess-up)
    (let* ((mess-node (ctran-use dummy))
           (cleanup (make-cleanup :kind kind
                                  :mess-up mess-node))
           (old-cup (lexenv-cleanup *lexenv*))
           (*lexenv* (make-lexenv :cleanup cleanup)))
      (setf (entry-cleanup (cleanup-mess-up old-cup)) cleanup)
      (ir1-convert dummy dummy2 nil '(%cleanup-point))
      (ir1-convert-progn-body dummy2 next result body))))

;;; This is a special special form that makes an "escape function"
;;; which returns unknown values from named block. We convert the
;;; function, set its kind to :ESCAPE, and then reference it. The
;;; :ESCAPE kind indicates that this function's purpose is to
;;; represent a non-local control transfer, and that it might not
;;; actually have to be compiled.
;;;
;;; Note that environment analysis replaces references to escape
;;; functions with references to the corresponding NLX-INFO structure.
(def-ir1-translator %escape-fun ((tag) start next result)
  (let ((fun (let ((*allow-instrumenting* nil))
               (ir1-convert-lambda
                `(lambda ()
                   (return-from ,tag (%unknown-values)))
                :debug-name (debug-name 'escape-fun tag))))
        (ctran (make-ctran)))
    (setf (functional-kind fun) :escape)
    (ir1-convert start ctran nil `(%%allocate-closures ,fun))
    (reference-leaf ctran next result fun)))

;;; Yet another special special form. This one looks up a local
;;; function and smashes it to a :CLEANUP function, as well as
;;; referencing it.
(def-ir1-translator %cleanup-fun ((name) start next result)
  ;; FIXME: Should this not be :TEST #'EQUAL? What happens to
  ;; (SETF FOO) here?
  (let ((fun (lexenv-find name funs)))
    (aver (lambda-p fun))
    (setf (functional-kind fun) :cleanup)
    (reference-leaf start next result fun)))

(def-ir1-translator catch ((tag &body body) start next result)
  #!+sb-doc
  "CATCH tag form*

Evaluate TAG and instantiate it as a catcher while the body forms are
evaluated in an implicit PROGN. If a THROW is done to TAG within the dynamic
scope of the body, then control will be transferred to the end of the body and
the thrown values will be returned."
  ;; We represent the possibility of the control transfer by making an
  ;; "escape function" that does a lexical exit, and instantiate the
  ;; cleanup using %WITHIN-CLEANUP.
  (ir1-convert
   start next result
   (with-unique-names (exit-block)
     `(block ,exit-block
        (%within-cleanup
         :catch (%catch (%escape-fun ,exit-block) ,tag)
         ,@body)))))

(def-ir1-translator unwind-protect
    ((protected &body cleanup) start next result)
  #!+sb-doc
  "UNWIND-PROTECT protected cleanup*

Evaluate the form PROTECTED, returning its values. The CLEANUP forms are
evaluated whenever the dynamic scope of the PROTECTED form is exited (either
due to normal completion or a non-local exit such as THROW)."
  ;; UNWIND-PROTECT is similar to CATCH, but hairier. We make the
  ;; cleanup forms into a local function so that they can be referenced
  ;; both in the case where we are unwound and in any local exits. We
  ;; use %CLEANUP-FUN on this to indicate that reference by
  ;; %UNWIND-PROTECT isn't "real", and thus doesn't cause creation of
  ;; an XEP.
  (ir1-convert
   start next result
   (with-unique-names (cleanup-fun drop-thru-tag exit-tag next start count)
     `(flet ((,cleanup-fun ()
               ,@cleanup
               nil))
        ;; FIXME: If we ever get DYNAMIC-EXTENT working, then
        ;; ,CLEANUP-FUN should probably be declared DYNAMIC-EXTENT,
        ;; and something can be done to make %ESCAPE-FUN have
        ;; dynamic extent too.
        (declare (dynamic-extent #',cleanup-fun))
        (block ,drop-thru-tag
          (multiple-value-bind (,next ,start ,count)
              (block ,exit-tag
                (%within-cleanup
                    :unwind-protect
                    (%unwind-protect (%escape-fun ,exit-tag)
                                     (%cleanup-fun ,cleanup-fun))
                  (return-from ,drop-thru-tag ,protected)))
            (declare (optimize (insert-debug-catch 0)))
            (,cleanup-fun)
            (%continue-unwind ,next ,start ,count)))))))

;;;; multiple-value stuff

(def-ir1-translator multiple-value-call ((fun &rest args) start next result)
  #!+sb-doc
  "MULTIPLE-VALUE-CALL function values-form*

Call FUNCTION, passing all the values of each VALUES-FORM as arguments,
values from the first VALUES-FORM making up the first argument, etc."
  (let* ((ctran (make-ctran))
         (fun-lvar (make-lvar))
         (node (if args
                   ;; If there are arguments, MULTIPLE-VALUE-CALL
                   ;; turns into an MV-COMBINATION.
                   (make-mv-combination fun-lvar)
                   ;; If there are no arguments, then we convert to a
                   ;; normal combination, ensuring that a MV-COMBINATION
                   ;; always has at least one argument. This can be
                   ;; regarded as an optimization, but it is more
                   ;; important for simplifying compilation of
                   ;; MV-COMBINATIONS.
                   (make-combination fun-lvar))))
    (ir1-convert start ctran fun-lvar (ensure-source-fun-form fun))
    (setf (lvar-dest fun-lvar) node)
    (collect ((arg-lvars))
      (let ((this-start ctran))
        (dolist (arg args)
          (let ((this-ctran (make-ctran))
                (this-lvar (make-lvar node)))
            (ir1-convert this-start this-ctran this-lvar arg)
            (setq this-start this-ctran)
            (arg-lvars this-lvar)))
        (link-node-to-previous-ctran node this-start)
        (use-continuation node next result)
        (setf (basic-combination-args node) (arg-lvars))))))

(def-ir1-translator multiple-value-prog1
    ((values-form &rest forms) start next result)
  #!+sb-doc
  "MULTIPLE-VALUE-PROG1 values-form form*

Evaluate VALUES-FORM and then the FORMS, but return all the values of
VALUES-FORM."
  (let ((dummy (make-ctran)))
    (ctran-starts-block dummy)
    (ir1-convert start dummy result values-form)
    (ir1-convert-progn-body dummy next nil forms)))

;;;; interface to defining macros

;;; Old CMUCL comment:
;;;
;;;   Return a new source path with any stuff intervening between the
;;;   current path and the first form beginning with NAME stripped
;;;   off.  This is used to hide the guts of DEFmumble macros to
;;;   prevent annoying error messages.
;;;
;;; Now that we have implementations of DEFmumble macros in terms of
;;; EVAL-WHEN, this function is no longer used.  However, it might be
;;; worth figuring out why it was used, and maybe doing analogous
;;; munging to the functions created in the expanders for the macros.
(defun revert-source-path (name)
  (do ((path *current-path* (cdr path)))
      ((null path) *current-path*)
    (let ((first (first path)))
      (when (or (eq first name)
                (eq first 'original-source-start))
        (return path)))))

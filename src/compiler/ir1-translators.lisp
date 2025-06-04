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

(in-package "SB-C")

;;;; special forms for control

(def-ir1-translator progn ((&rest forms) start next result)
  "PROGN form*

Evaluates each FORM in order, returning the values of the last form. With no
forms, returns NIL."
  (ir1-convert-progn-body start next result forms))

(def-ir1-translator if ((test then &optional else) start next result)
  "IF predicate then [else]

If PREDICATE evaluates to true, evaluate THEN and return its values,
otherwise evaluate ELSE and return its values. ELSE defaults to NIL."
  (let* ((pred-ctran (make-ctran))
         (pred-lvar (make-lvar))
         (then-ctran (make-ctran))
         (then-block (ctran-starts-block then-ctran))
         (else-ctran (make-ctran))
         (else-block (ctran-starts-block else-ctran))
         (node (make-if pred-lvar then-block else-block)))
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
      (ir1-convert (if path
                       (let ((*current-path* path))
                         (instrument-coverage then-ctran :then test))
                       then-ctran)
                   next result then)
      (ir1-convert (if path
                       (let ((*current-path* path))
                         (instrument-coverage else-ctran :else test))
                       else-ctran)
                   next result else))))

(def-ir1-translator jump-table ((index &rest targets) start next result)
  (declare (inline make-jump-table))
  (aver targets)
  (let* ((index-ctran (make-ctran))
         (index-lvar (make-lvar))
         (node (make-jump-table index-lvar)))
    (setf (lvar-dest index-lvar) node)
    (ir1-convert start index-ctran index-lvar index)
    (link-node-to-previous-ctran node index-ctran)

    (let ((start-block (ctran-block index-ctran)))
      (setf (block-last start-block) node)
      (ctran-starts-block next)
      (setf (jump-table-targets node)
            (loop for (index . target) in targets
                  for block = (if (block-p target)
                                  target
                                  (let* ((ctran (make-ctran))
                                         (block (ctran-starts-block ctran)))
                                    (ir1-convert-progn-body ctran next result target)
                                    block))
                  do
                  (unless (memq block (block-succ start-block))
                    (link-blocks start-block block))
                  collect (cons index block))))))

;;; then or else can be already converted blocks
(def-ir1-translator if-to-blocks ((test then &optional else) start next result)
  (flet ((to-block (x)
           (if (block-p x)
               (values nil x)
               (let ((ctran (make-ctran)))
                 (values ctran (ctran-starts-block ctran))))))
    (multiple-value-bind (then-ctran then-block) (to-block then)
      (multiple-value-bind (else-ctran else-block) (to-block else)
        (let* ((pred-ctran (make-ctran))
               (pred-lvar (make-lvar))
               (node (make-if pred-lvar then-block else-block)))
          (setf (lvar-dest pred-lvar) node)
          (ir1-convert start pred-ctran pred-lvar test)
          (link-node-to-previous-ctran node pred-ctran)

          (let ((start-block (ctran-block pred-ctran)))
            (setf (block-last start-block) node)
            (ctran-starts-block next)
            (link-blocks start-block then-block)
            (link-blocks start-block else-block))
          (when then-ctran
            (ir1-convert then-ctran next result then))
          (when else-ctran
            (ir1-convert else-ctran next result else)))))))

(def-ir1-translator to-lvar ((lvar block form) start next result)
  (ir1-convert start (block-start block) lvar form))

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
  "BLOCK name form*

Evaluate the FORMS as a PROGN. Within the lexical scope of the body,
RETURN-FROM can be used to exit the form."
  (unless (symbolp name)
    (compiler-error "The block name ~S is not a symbol." name))
  (ctran-starts-block next)
  (let* ((dummy (make-ctran))
         (entry (make-entry))
         (cleanup (make-cleanup :block entry)))
    (push entry (lambda-entries (lexenv-lambda *lexenv*)))
    (setf (entry-cleanup entry) cleanup)
    (link-node-to-previous-ctran entry start)
    (use-ctran entry dummy)

    (let* ((env-entry (list entry next result))
           (*lexenv* (make-lexenv :blocks (list (cons name env-entry))
                                  :cleanup cleanup)))
      (setf (cleanup-block cleanup) env-entry)
      (ir1-convert-progn-body dummy next result forms))))

;;; We make NEXT start a block just so that it will have a block
;;; assigned. People assume that when they pass a ctran into
;;; IR1-CONVERT as NEXT, it will have a block when it is done.
(def-ir1-translator return-from ((name &optional value) start next result)
  "RETURN-FROM name value

Evaluate the VALUE, returning its values from the lexically enclosing
block NAME. This is constrained to be used only within the dynamic
extent of the block."
  (ctran-starts-block next)
  (let* ((found (or (lexenv-find name blocks)
                    (compiler-error "return for unknown block: ~S" name)))
         (exit-ctran (second found))
         (value-ctran (make-ctran))
         (value-lvar (make-lvar))
         (entry (first found))
         (exit (make-exit entry value-lvar)))
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
  "TAGBODY {tag | statement}*

Define tags for use with GO. The STATEMENTS are evaluated in order, skipping
TAGS, and NIL is returned. If a statement contains a GO to a defined TAG
within the lexical scope of the form, then control is transferred to the next
statement following that tag. A TAG must be an integer or a symbol. A
STATEMENT must be a list. Other objects are illegal within the body."
  (ctran-starts-block next)
  (let* ((dummy (make-ctran))
         (entry (make-entry))
         (segments (parse-tagbody statements))
         (cleanup (make-cleanup :tagbody entry)))
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
  "GO tag

Transfer control to the named TAG in the lexically enclosing TAGBODY. This is
constrained to be used only within the dynamic extent of the TAGBODY."
  (ctran-starts-block next)
  (let* ((found (or (lexenv-find tag tags :test #'eql)
                    (compiler-error "attempt to GO to nonexistent tag: ~S"
                                    tag)))
         (entry (first found))
         (exit (make-exit entry)))
    (when (ctran-deleted-p (second found))
      (throw 'locall-already-let-converted (second found)))
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
;;; call FUN with the processed definitions.
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
      (compiler-warn "Duplicate definitions in ~S" definitions))
    (funcall fun processed-definitions)))

;;; Tweak LEXENV to include the DEFINITIONS from a MACROLET, then
;;; call FUN (with no arguments).
;;;
;;; This is split off from the IR1 convert method so that it can be
;;; shared by the special-case top level MACROLET processing code, and
;;; further split so that the special-case MACROLET processing code in
;;; EVAL can likewise make use of it.
(defun macrolet-definitionize-fun (context lexenv)
  (let ((signal-via (ecase context
                      (:compile #'compiler-error)
                      (:eval #'%program-error))))
    (flet ((fail (control &rest args)
             (apply signal-via control args)))
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
          (let (source-info
                tlf)
            #-sb-xc-host
            (when (boundp '*source-paths*)
              (push `(declare (source-form ,definition)) body)
              (setf source-info *source-info*
                    tlf (let ((path (or (get-source-path definition)
                                        (and (boundp '*current-path*)
                                             *current-path*))))
                          (source-path-tlf-number path))))
            `(,name macro .
                    ;; I guess the reason we want to compile here rather than possibly
                    ;; using an interpreted lambda is that we generate the usual gamut
                    ;; of style-warnings and such. One might wonder if this could somehow
                    ;; go through the front-most part of the front-end, to deal with
                    ;; semantics, but then generate an interpreted function or something
                    ;; more quick to emit than machine code.
                    ,(compile-in-lexenv
                      (make-macro-lambda nil arglist body 'macrolet name)
                      lexenv
                      nil source-info tlf t nil))))))))

(defun funcall-in-macrolet-lexenv (definitions fun context)
  (%funcall-in-foomacrolet-lexenv
   (macrolet-definitionize-fun
    context
    (make-restricted-lexenv *lexenv*
                            ;; Avoid efficiency notes
                            ;; and in general there's no need to optimizes macrolets
                            (augment-policy speed 1
                                            (lexenv-policy *lexenv*))))
   :funs
   definitions
   fun))

(def-ir1-translator macrolet ((definitions &rest body) start next result)
  "MACROLET ({(name lambda-list form*)}*) body-form*

Evaluate the BODY-FORMS in an environment with the specified local macros
defined. NAME is the local macro name, LAMBDA-LIST is a DEFMACRO style
destructuring lambda list, and the FORMS evaluate to the expansion."
  (funcall-in-macrolet-lexenv
   definitions
   (lambda (&optional funs)
     (ir1-translate-locally body start next result :funs funs))
   :compile))

(defun symbol-macrolet-definitionize-fun (context)
  (let ((signal-via (ecase context
                      (:compile #'compiler-error)
                      (:eval #'%program-error))))
    (flet ((fail (control &rest args)
             (apply signal-via control args)))
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
          `(,name . (macro . ,expansion)))))))

(defun funcall-in-symbol-macrolet-lexenv (definitions fun context)
  (%funcall-in-foomacrolet-lexenv
   (symbol-macrolet-definitionize-fun context)
   :vars
   definitions
   fun))

(def-ir1-translator symbol-macrolet
    ((macrobindings &body body) start next result)
  "SYMBOL-MACROLET ({(name expansion)}*) decl* form*

Define the NAMES as symbol macros with the given EXPANSIONS. Within the
body, references to a NAME will effectively be replaced with the EXPANSION."
  (funcall-in-symbol-macrolet-lexenv
   macrobindings
   (lambda (&optional vars)
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
         (required (- (vop-info-num-args  template)
                      (if (template-more-args-type template)
                          1
                          0)))
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
                 `(%%primitive ',template ,@args))))

(defmacro inline-%primitive (template &rest args)
  (let* ((required (length (template-arg-types template)))
         (info (template-info-arg-count template))
         (min (+ required info))
         (nargs (length args)))
    (if (template-more-args-type template)
        (when (< nargs min)
          (bug "Primitive was called with ~R argument~:P, ~
                but wants at least ~R."
               nargs
               min))
        (unless (= nargs min)
          (bug "Primitive was called with ~R argument~:P, ~
                but wants exactly ~R."
               nargs
               min)))

    (when (template-conditional-p template)
      (bug "%PRIMITIVE was used with a conditional template."))

    (when (template-more-results-type template)
      (bug "%PRIMITIVE was used with an unknown values template."))

    `(%%primitive ',template ,@args)))

;;;; QUOTE

(def-ir1-translator quote ((thing) start next result)
  "QUOTE value

Return VALUE without evaluating it."
  (reference-constant start next result thing))

;;; We now have a switch to decide whether relative pathnames
;;; can be stored in fasl files as their source name.
;;; Regardless of what ANSI says must be bound to *fooNAME* specials,
;;; the only sane choice for this switch is to use untruenames,
;;; because compilers should record source filenames _as_given_ for
;;; later consumption by linkers and debuggers. Attempting to convert
;;; to a truename fails miserably on symlink forests and
;;; content-addressable filesystems. Compare to a typical C compiler:
;;;  $ mkdir -p /tmp/a/b/c/d/e/f/g
;;;  $ cd /tmp/a/b/c/d
;;;  $ touch e/f/g/h.i
;;;  $ cc -g3 -S -o - e/f/g/h.i | grep file
;;;    .file        "h.i"
;;;    .file 1 "e/f/g/h.i"
;;; It has emitted a debug info with just a tail, and a debug info with
;;; the unaltered pathname, and not a truename in sight.
;;;
;;; But in Lisp we merge pathnames with the defaults, because CLHS says that
;;; "*compile-file-pathname* ... is bound to (pathname (merge-pathnames input-file))."
;;; and similarly for *load-pathname*.
;;; It's an implementation detail that we use the same information
;;; to bind *compile-file-pathname* and to store in the fasl though.
;;; But CLHS does *NOT* say that that the pathname of a source files as
;;; represented in its corresponding compiled object is a fully merged name
;;; - how would it? obtaining the source path isn't a specified thing -
;;; and that's our loophole to allow the choice.
;;; Another loophole is that *DEFAULT-PATHNAME-DEFAULTS* can always be #"".

;;; We could invent internal variables, like *COMPILE-FILE-PATHNAME-UNMERGED*
;;; to hold the right (useful) thing, when the *COMPILE-FILE-PATHNAME*
;;; holds the un-useful thing.
;;; Anyway, long story short, merging is all the more wrong when we went
;;; to heroic efforts to reverse-engineer the original pathname by
;;; scanning for "src/" as used to be done in LPNIFY-NAMESTRING.
;;; So name-contexts, at least in self-build, should use untruenameized
;;; un-merged pathnames. I'm not daring enough to change it for everyone.
;;; It defaults to what it should, and is changed before saving the image.
;;;
;;; FIXME: can't we just get rid of this and _never_ use TRUENAME?
(declaim (type (member pathname truename) *name-context-file-path-selector*))
(defglobal *name-context-file-path-selector* 'pathname)

(defun name-context ()
  ;; Name of the outermost non-NIL BLOCK, or the source namestring
  ;; of the source file.
  (let ((context
          (or (car (find-if (lambda (b)
                              (let ((name (pop b)))
                                (and name
                                     ;; KLUDGE: High debug adds this block on
                                     ;; some platforms.
                                     #-unwind-to-frame-and-call-vop
                                     (neq 'return-value-tag name)
                                     ;; KLUDGE: CATCH produces blocks whose
                                     ;; cleanup is :CATCH.
                                     (neq :catch (cleanup-kind (entry-cleanup (pop b)))))))
                            (lexenv-blocks *lexenv*) :from-end t))
              *source-namestring*
              (awhen (case *name-context-file-path-selector*
                       (pathname (or *compile-file-pathname* *load-pathname*))
                       (truename (or *compile-file-truename* *load-truename*)))
                (namestring it)))))
    (when context
      (list :in context))))

;;;; FUNCTION and NAMED-LAMBDA

;;; `(NAMED-LAMBDA ,NAME ,@REST) is like `(FUNCTION (LAMBDA ,@REST)),
;;; except that the value of NAME is passed to the compiler for use in
;;; creation of debug information for the resulting function.
;;;
;;; NAME can be a legal function name or some arbitrary other thing.
;;;
;;; If NAME is a legal function name, then the caller should be
;;; planning to set (FDEFINITION NAME) to the created function.
;;; (Otherwise the debug names will be inconsistent and thus
;;; unnecessarily confusing.)
;;;
;;; Arbitrary other things are appropriate for naming things which are
;;; not the FDEFINITION of NAME. E.g.
;;;   NAME = (:FLET FOO BAR)
;;; for the FLET function in
;;;   (DEFUN BAR (X)
;;;     (FLET ((FOO (Y) (+ X Y)))
;;;       FOO))
;;; or
;;;   NAME = (:METHOD PRINT-OBJECT :AROUND (STARSHIP T))
;;; for the function used to implement
;;;   (DEFMETHOD PRINT-OBJECT :AROUND ((SS STARSHIP) STREAM) ...).
(defun name-lambdalike (thing)
  (case (car thing)
    ((named-lambda)
     (or (second thing)
         `(lambda ,(strip-lambda-list (third thing) :name) ,(name-context))))
    ((lambda)
     `(lambda ,(strip-lambda-list (second thing) :name) ,@(name-context)))
    (otherwise
     (compiler-error "Not a valid lambda expression:~%  ~S"
                     thing))))

(defun enclose (start next funs)
  (let ((enclose (make-enclose funs)))
    (link-node-to-previous-ctran enclose start)
    (use-ctran enclose next)
    (dolist (fun funs)
      (setf (functional-enclose fun) enclose))))

;;; Get the leaf corresponding to THING, allocating and converting it
;;; if it's a lambda expression or otherwise finding the lexically
;;; apparent function associated to it.
(defun find-or-convert-fun-leaf (thing start)
  (cond
    ((typep thing '(cons (member lambda named-lambda)))
     (let ((ctran (make-ctran))
           (leaf (ir1-convert-lambdalike thing
                                         :debug-name (name-lambdalike thing))))
       (enclose start ctran (list leaf))
       (values leaf ctran)))
    ((legal-fun-name-p thing)
     (values (find-lexically-apparent-fun thing "as the argument to FUNCTION")
             start))
    (t
     (compiler-error "~S is not a legal function name." thing))))

;;; Convert a lambda without referencing it, side-effecting the
;;; compile-time environment. This is useful for converting named
;;; lambdas not meant to have entry points during block compilation,
;;; since references will create entry points.
(def-ir1-translator %refless-defun ((thing) start next result)
  (ir1-convert-lambdalike thing :debug-name (name-lambdalike thing))
  (ir1-convert start next result nil))

(def-ir1-translator function ((thing) start next result)
  "FUNCTION name

Return the lexically apparent definition of the function NAME. NAME may also
be a lambda expression."
  (multiple-value-bind (leaf start)
      (find-or-convert-fun-leaf thing start)
    (reference-leaf start next result leaf)))

;;; Like FUNCTION, but ignores local definitions and inline
;;; expansions, and doesn't nag about undefined functions.
;;; Used for optimizing things like (FUNCALL 'FOO).
(def-ir1-translator global-function ((thing) start next result)
  (reference-leaf start next result (find-global-fun thing t)))

;;; Return T if THING is a constant value and either a symbol (if EXTENDEDP is NIL)
;;; or an extended-function-name (if EXTENDEDP is T).
;;; Note however that whether THING actually names something is irrelevant.
;;; This test is only if it _could_ name something. Return NIL if THING is a function
;;; or a constant form evaluating to a function, or not a legal designator at all.
(defun constant-global-fun-name (thing lexenv extendedp)
  (let ((constantp (constantp thing lexenv)))
    (when constantp
      (let ((name (constant-form-value thing lexenv)))
        (when (if extendedp
                  (legal-fun-name-p name)
                  (symbolp name))
          name)))))

(defun lvar-constant-global-fun-name (lvar)
  (when (constant-lvar-p lvar)
    (let ((name (lvar-value lvar)))
      (when (legal-fun-name-p name)
        name))))

(defun ensure-source-fun-form (source lexenv &key (coercer '%coerce-callable-for-call)
                                             (extendedp t) give-up)
  (let ((op (when (consp source) (car source))))
    (cond ((memq op '(%coerce-callable-to-fun %coerce-callable-for-call))
           (ensure-source-fun-form (second source) lexenv :coercer coercer))
          ((member op '(function global-function lambda named-lambda))
           (values source nil))
          (t
           (let ((cname (constant-global-fun-name source lexenv extendedp)))
             (if cname
                 (values `(global-function ,cname) nil)
                 (values `(,coercer ,source) give-up)))))))

(defun source-variable-or-else (lvar fallback)
  (let ((uses (principal-lvar-use lvar)) leaf name)
    (or (and (ref-p uses)
             (leaf-has-source-name-p (setf leaf (ref-leaf uses)))
             (symbolp (setf name (leaf-source-name leaf)))
             ;; assume users don't hand-write gensyms
             (cl:symbol-package name)
             name)
        fallback)))

;;;; FUNCALL
(def-ir1-translator %funcall ((function &rest args) start next result)
  ;; MACROEXPAND so that (LAMBDA ...) forms arriving here don't get an
  ;; extra cast inserted for them.
  (let ((function (handler-case (%macroexpand function *lexenv*)
                    (error () function))))
    (if (typep function '(cons (member function global-function) (cons t null)))
        ;; We manually frob the function to get the leaf like this so
        ;; we let setf functions have their source transforms fire.
        (destructuring-bind (operator definition) function
          (multiple-value-bind (leaf start)
              (ecase operator
                (function (find-or-convert-fun-leaf definition start))
                (global-function (values (find-global-fun definition t) start)))
            (ir1-convert-common-functoid start next result `(,leaf ,@args) leaf)))
        (let ((ctran (make-ctran))
              (fun-lvar (make-lvar)))
          (ir1-convert start ctran fun-lvar `(the function ,function))
          (ir1-convert-combination-args fun-lvar ctran next result args)))))

(def-ir1-translator %funcall-lvar ((function &rest args) start next result)
  (ir1-convert-combination-args function start next result args))

(def-ir1-translator %funcall-no-nargs ((function &rest args) start next result)
  (let ((ctran (make-ctran))
        (fun-lvar (make-lvar)))
    (ir1-convert start ctran fun-lvar `(the function ,function))
    (ir1-convert-combination-args fun-lvar ctran next result args :pass-nargs nil)))

;;; This source transform exists to reduce the amount of work for the
;;; compiler. If the called function is a FUNCTION form, then convert
;;; directly to %FUNCALL, instead of waiting around for type
;;; inference.
(define-source-transform funcall (function &rest args &environment env)
  ;; This transform should not allow violating the type constraint on FUNCALL
  ;; by sneaking in a use of #'name via %FUNCALL in cases where the name is
  ;; an extended-function-designator instead of just function-designator.
  `(%funcall ,(ensure-source-fun-form function env
                                      :coercer '%coerce-callable-for-call
                                      :extendedp nil)
             ,@args))

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
                extract-letish-vars))
(defun extract-letish-vars (bindings context)
  (collect ((vars)
            (vals))
    (let ((names (unless (eq context 'let*)
                   (make-repeated-name-check :context context))))
      (dolist (spec bindings)
        (with-current-source-form (spec)
          (multiple-value-bind (name value)
              (cond ((atom spec)
                     (values spec nil))
                    (t
                     (unless (proper-list-of-length-p spec 1 2)
                       (compiler-error "The ~S binding spec ~S is malformed."
                                       context spec))
                     (values (first spec) (second spec))))
            (check-variable-name-for-binding
             name :context context :allow-symbol-macro nil)
            (unless (eq context 'let*)
              (funcall names name))
            (vars (varify-lambda-arg name spec))
            (vals value)))))
    (values (vars) (vals))))

(defun parse-letish (bindings body context)
  (multiple-value-bind (forms declarations) (parse-body body nil)
    (with-current-source-form (bindings)
      (unless (listp bindings)
        (compiler-error "Malformed ~A bindings: ~S." context bindings))
      (multiple-value-call #'values
        (extract-letish-vars bindings context) forms declarations))))

(def-ir1-translator let ((bindings &body body) start next result)
  "LET ({(var [value]) | var}*) declaration* form*

During evaluation of the FORMS, bind the VARS to the result of evaluating the
VALUE forms. The variables are bound in parallel after all of the VALUES forms
have been evaluated."
  (cond ((null bindings)
         (ir1-translate-locally body start next result))
        ;; This is just to avoid leaking non-standard special forms
        ;; into macroexpanded code
        #-c-stack-is-control-stack
        ((and (equal bindings '((*alien-stack-pointer* *alien-stack-pointer*))))
         (ir1-convert start next result
                      (let ((nsp (gensym "NSP")))
                        `(let ((,nsp (%primitive current-nsp)))
                           (restoring-nsp ,nsp ,@body)))))
        (t
         (multiple-value-bind (vars values forms decls)
             (parse-letish bindings body 'let)
           (let ((ctran (make-ctran))
                 (fun-lvar (make-lvar)))
             (multiple-value-bind (*lexenv* result-type post-binding-lexenv)
                 (process-decls decls vars nil :binding-form-p t)
               (declare (ignore result-type))
               (let ((fun (ir1-convert-lambda-body
                           forms
                           vars
                           :post-binding-lexenv post-binding-lexenv
                           :debug-name (debug-name 'let
                                                   (mapcar #'leaf-source-name
                                                           vars)))))
                 (reference-leaf start ctran fun-lvar fun)))
             (ir1-convert-combination-args fun-lvar ctran next result values
                                           :arg-source-forms bindings))))))

(def-ir1-translator let* ((bindings &body body)
                          start next result)
  "LET* ({(var [value]) | var}*) declaration* form*

Similar to LET, but the variables are bound sequentially, allowing each VALUE
form to reference any of the previous VARS."
  (multiple-value-bind (vars values forms decls) (parse-letish bindings body 'let*)
    (multiple-value-bind (*lexenv* result-type post-binding-lexenv)
        (process-decls decls vars nil :binding-form-p t)
      (declare (ignore result-type))
      (ir1-convert-aux-bindings
       start next result forms vars values post-binding-lexenv
       :value-source-forms bindings))))

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
    (let ((*lexenv* (process-decls decls vars funs)))
      (ir1-convert-progn-body start next result forms))))

(def-ir1-translator locally ((&body body) start next result)
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
(declaim (ftype (function (list symbol) (values list list)) extract-fletish-vars))
(defun extract-fletish-vars (definitions context)
  (collect ((names)
            (defs))
    (dolist (definition definitions)
      (with-current-source-form (definition)
        (unless (list-of-length-at-least-p definition 2)
          (compiler-error "The ~S definition spec ~S is malformed."
                          context definition))
        (destructuring-bind (name lambda-list &body body) definition
          (check-fun-name name)
          (when (fboundp name)
            (program-assert-symbol-home-package-unlocked
             :compile name "binding ~A as a local function"))
          (names name)
          (multiple-value-bind (forms decls doc) (parse-body body t)
            (defs `(lambda ,lambda-list
                     ,@(when doc (list doc))
                     ,@decls
                     (block ,(fun-name-block-name name)
                       . ,forms)))))))
    (let ((names (names)))
      (unless (= (length names)
                 (length (remove-duplicates names :test #'equal)))
        (compiler-warn "Duplicate definitions in ~S" definitions))
      (values names (defs)))))

(defun parse-fletish (definitions body context)
  (multiple-value-bind (forms declarations) (parse-body body nil)
    (with-current-source-form (definitions)
      (unless (listp definitions)
        (compiler-error "Malformed ~A definitions: ~S." context definitions))
      (multiple-value-call #'values
        (extract-fletish-vars definitions context) forms declarations))))

;;; This is similar to IR1-CONVERT-PROGN-BODY except that code to
;;; potentially make a closure for each FUN in FUNS is emitted, and
;;; then the body is converted as usual.
;;;
;;; When one of these FUNS is declared dynamic extent, we make a
;;; cleanup with the ENCLOSE as the MESS-UP node and introduce it into
;;; the lexical environment to convert the body in. We force NEXT to
;;; start a block outside of this cleanup, causing cleanup code to be
;;; emitted when the scope is exited.
(defun ir1-convert-fbindings (start next result funs body)
  (let ((enclose-ctran (make-ctran)))
    (enclose start enclose-ctran funs)
    (cond ((some #'leaf-dynamic-extent funs)
           (ctran-starts-block next)
           (let* ((enclose (ctran-use enclose-ctran))
                  (dynamic-extent (make-dynamic-extent))
                  (cleanup (make-cleanup :dynamic-extent dynamic-extent))
                  (dynamic-extent-ctran (make-ctran)))
             (setf (enclose-dynamic-extent enclose) dynamic-extent)
             (setf (dynamic-extent-cleanup dynamic-extent) cleanup)
             (link-node-to-previous-ctran dynamic-extent enclose-ctran)
             (use-ctran dynamic-extent dynamic-extent-ctran)
             (push dynamic-extent
                   (lambda-dynamic-extents (node-home-lambda dynamic-extent)))
             (let ((*lexenv* (make-lexenv :cleanup cleanup)))
               (ir1-convert-progn-body dynamic-extent-ctran next result body))))
          (t
           (ir1-convert-progn-body enclose-ctran next result body)))))

(def-ir1-translator flet ((definitions &body body)
                          start next result)
  "FLET ({(name lambda-list declaration* form*)}*) declaration* body-form*

Evaluate the BODY-FORMS with local function definitions. The bindings do
not enclose the definitions; any use of NAME in the FORMS will refer to the
lexically apparent function definition in the enclosing environment."
  (multiple-value-bind (names defs forms decls)
      (parse-fletish definitions body 'flet)
    (let* ((fvars (mapcar (lambda (name def original)
                            (let ((*current-path* (ensure-source-path original)))
                              (ir1-convert-lambda
                               def
                               :source-name name
                               :maybe-add-debug-catch t
                               :debug-name
                               (let ((n (if (and (symbolp name)
                                                 (not (cl:symbol-package name)))
                                            (string name)
                                            name)))
                                 (debug-name 'flet n t)))))
                          names defs definitions))
           (*lexenv* (make-lexenv :default (process-decls decls nil fvars)
                                  :funs (pairlis names fvars))))
      (ir1-convert-fbindings start next result fvars forms))))

;;; For LABELS, we have to create dummy function vars and add them to
;;; the function namespace while converting the functions. We then
;;; modify all the references to these leaves so that they point to
;;; the real functional leaves. We also backpatch the FENV so that if
;;; the lexical environment is used for inline expansion we will get
;;; the right functions.
(def-ir1-translator labels ((definitions &body body) start next result)
  "LABELS ({(name lambda-list declaration* form*)}*) declaration* body-form*

Evaluate the BODY-FORMS with local function definitions. The bindings enclose
the new definitions, so the defined functions can call themselves or each
other."
  (multiple-value-bind (names defs forms decls)
      (parse-fletish definitions body 'labels)
    (let* ((new-fenv
             ;; (like PAIRLIS but guaranteed to preserve ordering:)
             (mapcar (lambda (name)
                       (cons name
                             (make-functional
                              :%source-name name
                              :%debug-name (debug-name
                                            'labels-placeholder
                                            name))))
                     names))
           ;; the real LABELS functions, compiled in a LEXENV which
           ;; includes the dummy LABELS functions
           (real-funs
             (let ((*lexenv* (make-lexenv :funs new-fenv)))
               (mapcar (lambda (name def original)
                         (let ((*current-path* (ensure-source-path original)))
                           (ir1-convert-lambda def
                                               :source-name name
                                               :maybe-add-debug-catch t
                                               :debug-name (debug-name 'labels name t))))
                       names defs definitions))))
      (loop for real-fun in real-funs and cons in new-fenv do
        (substitute-leaf real-fun (cdr cons))
        (setf (cdr cons) real-fun))
      (let ((*lexenv* (make-lexenv
                       :default (process-decls decls nil real-funs)
                       :funs (pairlis names real-funs))))
        (ir1-convert-fbindings start next result real-funs forms)))))


;;;; the THE special operator, and friends

;;; A logic shared among THE and TRULY-THE.
(defun the-in-policy (type value policy start next result)
  (let ((type (cond ((ctype-p type)
                     type)
                    ((compiler-values-specifier-type type))
                    (t
                     (ir1-convert start next result value)
                     (return-from the-in-policy)))))
    (cond ((or (eq type *wild-type*)
               (eq type *universal-type*)
               (and (leaf-p value)
                    (not (fun-designator-type-p type))
                    (values-subtypep (make-single-value-type (leaf-type value))
                                     type))
               (and (not (fun-designator-type-p type))
                    (constantp value)
                    (or (not (values-type-p type))
                        (values-type-may-be-single-value-p type))
                    (ctypep (constant-form-value value)
                            (single-value-type type))))
           (ir1-convert start next result value)
           nil) ;; NIL is important, older SBCLs miscompiled (values &optional x) casts
          (t
           (let* ((value-ctran (make-ctran))
                  (value-lvar (make-lvar))
                  (cast (make-cast value-lvar type policy)))
             (ir1-convert start value-ctran value-lvar value)
             (link-node-to-previous-ctran cast value-ctran)
             (setf (lvar-dest value-lvar) cast)
             (use-continuation cast next result)
             (when (eq type *empty-type*)
               (maybe-terminate-block cast t))
             cast)))))

;;; Assert that FORM evaluates to the specified type (which may be a
;;; VALUES type). TYPE may be a type specifier or (as a hack) a CTYPE.
(def-ir1-translator the ((value-type form) start next result)
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
  "Specifies that the values returned by FORM conform to the
VALUE-TYPE, and causes the compiler to trust this information
unconditionally.

Consequences are undefined if any result is not of the declared type
-- typical symptoms including memory corruptions. Use with great
care."
  (the-in-policy value-type form **zero-typecheck-policy** start next result))

;;; THE with some options for the CAST
(def-ir1-translator the* (((type &key context silent-conflict
                                       derive-type-only
                                       truly
                                       source-form
                                       use-annotations
                                       restart)
                           form)
                          start next result)
  (let ((type (cond ((ctype-p type)
                     type)
                    ((compiler-values-specifier-type type))
                    (t
                     (ir1-convert start next result
                                  `(progn
                                     ,form
                                     (error "Bad type specifier: ~a"
                                            ',type)))
                     (return-from ir1-convert-the*))))
        (*current-path* (if source-form
                            (ensure-source-path source-form)
                            (ensure-source-path form)))
        (context (cond (restart
                        (cons :restart context))
                       (context))))
    (cond (derive-type-only
           ;; For something where we really know the type and need no mismatch checking,
           ;; e.g. structure accessors
           (let ((before-uses (and result
                                   (lvar-uses result))))
             (ir1-convert start next result form)
             (when result
               ;; Would be great for IR1-CONVERT to return the uses it creates
               (let ((new-uses (lvar-uses result)))
                 (derive-node-type (cond ((consp new-uses)
                                          (loop for other in (cdr new-uses)
                                                do (aver (or (exit-p other)
                                                             (eq before-uses other)
                                                             (and (consp before-uses)
                                                                  (memq other before-uses)))))
                                          (the (not exit) (car new-uses)))
                                         (t
                                          new-uses))
                                   type)))))
          (use-annotations
           (ir1-convert start next result form)
           (when result
             (add-annotation result
                             (make-lvar-type-annotation :type type
                                                        :source-path *current-path*
                                                        :context context))))
          (t
           (let* ((policy (lexenv-policy *lexenv*))
                  (cast (the-in-policy type form (if truly
                                                     **zero-typecheck-policy**
                                                     policy)
                                       start next result)))
             (when cast
               (setf (cast-context cast) context)
               (setf (cast-silent-conflict cast) silent-conflict)))))))

(def-ir1-translator with-annotations ((annotations form)
                                      start next result)
  (ir1-convert start next result form)
  (when result
    (loop for annotation in annotations
          do (add-annotation
              result
              annotation))))

(def-ir1-translator with-source-form ((source-form form)
                                      start next result)
  (let ((*current-path* (if source-form
                            (ensure-source-path source-form)
                            *current-path*)))
    (ir1-convert start next result form)))

(def-ir1-translator with-source-path ((source-path form)
                                      start next result)
  (let ((*current-path* source-path))
    (ir1-convert start next result form)))

#-sb-xc-host
(setf (info :function :macro-function 'truly-the)
      (lambda (whole env)
        (declare (ignore env))
        `(the ,@(cdr whole)))
      (info :function :macro-function 'the*)
      (lambda (whole env)
        (declare (ignore env))
        (destructuring-bind
              (the* (type &key restart context use-annotations &allow-other-keys) form)
            whole
          (declare (ignore the*))
          (cond (restart
                 (let* ((val (gensym "VAL"))
                        (head (gensym "HEAD"))
                        (ctype (careful-specifier-type type))
                        (type (if (and ctype (fun-type-p ctype)) 'function type)))
                   `(let ((,val ,form))
                      (if (typep ,val ',type)
                          ,val
                          (block nil
                            (let ((sb-kernel::*type-error-no-check-restart*
                                    (lambda (value) (return value))))
                              (tagbody
                                 ,head
                                 (restart-case
                                     (error 'type-error :context ',context
                                                        :datum ,val :expected-type ',type)
                                   (use-value (value)
                                     :report (lambda (stream)
                                               (format stream "Use specified value."))
                                     :interactive read-evaluated-form
                                     (setq ,val value)))
                                 (when (typep ,val ',type)
                                   (return ,val))
                                 (go ,head))))))))
                (use-annotations
                 `(progn ,@(cddr whole)))
                (t
                 `(the ,(caadr whole) ,@(cddr whole))))))
      (info :function :macro-function 'with-source-form)
      (lambda (whole env)
        (declare (ignore env))
        `(progn ,@(cddr whole))))

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
        (maybe-note-undefined-variable-reference leaf name)
        (etypecase leaf
          (leaf
           (when (constant-p leaf)
             (compiler-error "~S is a constant and thus can't be set." name))
           (when (lambda-var-p leaf)
             (let ((home-lambda (ctran-home-lambda-or-null start)))
               (when (and home-lambda (neq (lambda-var-home leaf) home-lambda))
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
           (type basic-var var)
           (inline make-set))
  (let ((dest-ctran (make-ctran))
        (dest-lvar (make-lvar))
        (type (or (lexenv-find var type-restrictions)
                  (leaf-type var))))
    (ir1-convert start dest-ctran dest-lvar `(the ,(type-specifier type)
                                                  ,value))
    (let ((res (make-set var dest-lvar)))
      (setf (lvar-dest dest-lvar) res)
      (cond (result ; SETQ with a result counts as a REF also
             (setf (leaf-ever-used var) t))
            ((not (leaf-ever-used var)) ; doesn't count as a REF
             ;; (EVER-USED might already be T, leave it alone if so)
             (setf (leaf-ever-used var) 'set)))
      (push res (basic-var-sets var))
      (link-node-to-previous-ctran res dest-ctran)
      (use-continuation res next result))))

;;;; CATCH, THROW and UNWIND-PROTECT

;;; We turn THROW into a MULTIPLE-VALUE-CALL of a magical function,
;;; since as far as IR1 is concerned, it has no interesting
;;; properties other than receiving multiple-values.
(def-ir1-translator throw ((tag result) start next result-lvar)
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
           (cleanup (make-cleanup kind mess-node))
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
    (setf (functional-kind fun) (functional-kind-attributes escape))
    (enclose start ctran (list fun))
    (reference-leaf ctran next result fun)))

;;; Yet another special special form. This one looks up a local
;;; function and smashes it to a :CLEANUP function, as well as
;;; referencing it.
(def-ir1-translator %cleanup-fun ((name) start next result)
  ;; FIXME: Should this not be :TEST #'EQUAL? What happens to
  ;; (SETF FOO) here?
  (let ((fun (lexenv-find name funs)))
    (aver (lambda-p fun))
    (setf (functional-kind fun) (functional-kind-attributes cleanup))
    (reference-leaf start next result fun)))

(def-ir1-translator catch ((tag &body body) start next result)
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

;;; Since NSP is restored on unwind we only need to protect against
;;; local transfers of control, basically the same as special
;;; bindings.
;;; Needs to be wrapped in a LET (let ((nsp (current-nsp))) (restoring-nsp nsp body))
;;; The LET is needed because the cleanup can be emitted multiple
;;; times, but there's no reference to NSP before EMIT-CLEANUPS.
;;; Passing NSP to the dummy %CLEANUP-FUN keeps it alive.
#-c-stack-is-control-stack
(def-ir1-translator restoring-nsp
    ((nsp &body body) start next result)
  (let ((cleanup (make-cleanup :restore-nsp))
        (nsp-ctran (make-ctran))
        (cleanup-ctran (make-ctran)))
    (ir1-convert start nsp-ctran nil nsp)
    (setf (cleanup-mess-up cleanup) (ctran-use nsp-ctran))
    (let ((*lexenv* (make-lexenv :cleanup cleanup)))
      ;; KLUDGE: reference NSP twice so that the LET doesn't get
      ;; deleted before EMIT-CLEANUPS
      (ir1-convert nsp-ctran cleanup-ctran nil `(%cleanup-point ,nsp ,nsp))
      (ir1-convert-progn-body cleanup-ctran next result
                              body))))

(def-ir1-translator unwind-protect
    ((protected &body cleanup) start next result)
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
   (with-unique-names (cleanup-fun drop-thru-tag exit-tag . #-no-continue-unwind (next start count)
                                                            #+no-continue-unwind ())
     `(flet ((,cleanup-fun ()
               ,@cleanup
               (values)))
        ;; FIXME: Maybe %ESCAPE-FUN could dynamic extent too.
        (declare (dynamic-extent #',cleanup-fun))
        #-no-continue-unwind
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
            (%unwind ,next ,start ,count)))
        #+no-continue-unwind
        (block ,drop-thru-tag
          (block ,exit-tag
            (%within-cleanup
             :unwind-protect
             (%unwind-protect (%escape-fun ,exit-tag)
                              (%cleanup-fun ,cleanup-fun))
             (return-from ,drop-thru-tag ,protected)))
          (locally
              (declare (optimize (insert-debug-catch 0)))
            (,cleanup-fun)
            (%continue-unwind)))))))

(def-ir1-translator inspect-unwinding
    ((protected inspect-fun) start next result)
  (ir1-convert
   start next result
   (with-unique-names (drop-thru-tag exit-tag next . #-no-continue-unwind (start count)
                                                     #+no-continue-unwind ())
     `(block ,drop-thru-tag
        #-no-continue-unwind
        (multiple-value-bind (,next ,start ,count)
            (block ,exit-tag
              (%within-cleanup
               :unwind-protect
               (%unwind-protect (%escape-fun ,exit-tag)
                                nil)
               (return-from ,drop-thru-tag ,protected)))
          (declare (optimize (insert-debug-catch 0)))
          (funcall ,inspect-fun ,next)
          (%unwind ,next ,start ,count))
        #+no-continue-unwind
        (let ((,next
                (block ,exit-tag
                  (%within-cleanup
                   :unwind-protect
                   (%unwind-protect (%escape-fun ,exit-tag)
                                    nil)
                   (return-from ,drop-thru-tag ,protected)))))
          (declare (optimize (insert-debug-catch 0)))
          (funcall ,inspect-fun ,next)
          (%continue-unwind))))))

;;; Evaluate CLEANUP iff PROTECTED does a non-local exit.
(def-ir1-translator nlx-protect
    ((protected &body cleanup) start next result)
  (ir1-convert
   start next result
   (with-unique-names (drop-thru-tag exit-tag . #-no-continue-unwind (next start count)
                                                #+no-continue-unwind ())
     `(block ,drop-thru-tag
        #+no-continue-unwind
        (progn
          (block ,exit-tag
            (%within-cleanup
             :unwind-protect
             (%unwind-protect (%escape-fun ,exit-tag)
                              nil)
             (return-from ,drop-thru-tag ,protected)))
          (locally
              (declare (optimize (insert-debug-catch 0)))
            ,@cleanup
            (%continue-unwind)))
        #-no-continue-unwind
        (multiple-value-bind (,next ,start ,count)
            (block ,exit-tag
              (%within-cleanup
               :unwind-protect
               (%unwind-protect (%escape-fun ,exit-tag)
                                nil)
               (return-from ,drop-thru-tag ,protected)))
          (declare (optimize (insert-debug-catch 0)))
          ,@cleanup
          (%unwind ,next ,start ,count))))))

;;;; multiple-value stuff

(def-ir1-translator multiple-value-call ((fun &rest args) start next result)
  "MULTIPLE-VALUE-CALL function values-form*

Call FUNCTION, passing all the values of each VALUES-FORM as arguments,
values from the first VALUES-FORM making up the first argument, etc."
  (declare (inline make-mv-combination))
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
    (ir1-convert start ctran fun-lvar
                 (ensure-source-fun-form fun *lexenv*
                                         :coercer '%coerce-callable-for-call
                                         :extendedp nil))
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

;;; MULTIPLE-VALUE-PROG1 is represented in IR1 by having the
;;; VALUES-FORM code use a VALUE lvar that gets handed off to
;;; RESULT. In other words, as the result continuation isn't
;;; IMMEDIATELY-USED-P by the nodes that compute the result, we have
;;; to interpose a DELAY node using RESULT immediately so that the
;;; result continuation can assume that it is immediately used. This
;;; is important here because MULTIPLE-VALUE-PROG1 is the only special
;;; form that produces code where lvar substitution is potentially
;;; incorrect.
(def-ir1-translator multiple-value-prog1
    ((values-form &rest forms) start next result)
  "MULTIPLE-VALUE-PROG1 values-form form*

Evaluate VALUES-FORM and then the FORMS, but return all the values of
VALUES-FORM."
  (let* ((value-ctran (make-ctran))
         (forms-ctran (make-ctran))
         (value-lvar (make-lvar))
         (delay (make-delay value-lvar)))
    (ctran-starts-block value-ctran)
    (ir1-convert start value-ctran value-lvar values-form)
    (ir1-convert-progn-body value-ctran forms-ctran nil forms)
    (link-node-to-previous-ctran delay forms-ctran)
    (setf (lvar-dest value-lvar) delay)
    (use-continuation delay next result)))


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

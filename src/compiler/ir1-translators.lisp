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

;;;; control special forms

(def-ir1-translator progn ((&rest forms) start cont)
  #!+sb-doc
  "Progn Form*
  Evaluates each Form in order, returning the values of the last form. With no
  forms, returns NIL."
  (ir1-convert-progn-body start cont forms))

(def-ir1-translator if ((test then &optional else) start cont)
  #!+sb-doc
  "If Predicate Then [Else]
  If Predicate evaluates to non-null, evaluate Then and returns its values,
  otherwise evaluate Else and return its values. Else defaults to NIL."
  (let* ((pred (make-continuation))
	 (then-cont (make-continuation))
	 (then-block (continuation-starts-block then-cont))
	 (else-cont (make-continuation))
	 (else-block (continuation-starts-block else-cont))
	 (dummy-cont (make-continuation))
	 (node (make-if :test pred
			:consequent then-block
			:alternative else-block)))
    (setf (continuation-dest pred) node)
    (ir1-convert start pred test)
    (prev-link node pred)
    (use-continuation node dummy-cont)

    (let ((start-block (continuation-block pred)))
      (setf (block-last start-block) node)
      (continuation-starts-block cont)

      (link-blocks start-block then-block)
      (link-blocks start-block else-block))

    (ir1-convert then-cont cont then)
    (ir1-convert else-cont cont else)))

;;;; BLOCK and TAGBODY

;;;; We make an ENTRY node to mark the start and a :ENTRY cleanup to
;;;; mark its extent. When doing GO or RETURN-FROM, we emit an EXIT
;;;; node.

;;; Make a :ENTRY cleanup and emit an ENTRY node, then convert the
;;; body in the modified environment. We make CONT start a block now,
;;; since if it was done later, the block would be in the wrong
;;; environment.
(def-ir1-translator block ((name &rest forms) start cont)
  #!+sb-doc
  "Block Name Form*
  Evaluate the Forms as a PROGN. Within the lexical scope of the body,
  (RETURN-FROM Name Value-Form) can be used to exit the form, returning the
  result of Value-Form."
  (unless (symbolp name)
    (compiler-error "The block name ~S is not a symbol." name))
  (continuation-starts-block cont)
  (let* ((dummy (make-continuation))
	 (entry (make-entry))
	 (cleanup (make-cleanup :kind :block
				:mess-up entry)))
    (push entry (lambda-entries (lexenv-lambda *lexenv*)))
    (setf (entry-cleanup entry) cleanup)
    (prev-link entry start)
    (use-continuation entry dummy)
    
    (let* ((env-entry (list entry cont))
           (*lexenv* (make-lexenv :blocks (list (cons name env-entry))
				  :cleanup cleanup)))
      (push env-entry (continuation-lexenv-uses cont))
      (ir1-convert-progn-body dummy cont forms))))


(def-ir1-translator return-from ((name &optional value) start cont)
  #!+sb-doc
  "Return-From Block-Name Value-Form
  Evaluate the Value-Form, returning its values from the lexically enclosing
  BLOCK Block-Name. This is constrained to be used only within the dynamic
  extent of the BLOCK."
  ;; CMU CL comment:
  ;;   We make CONT start a block just so that it will have a block
  ;;   assigned. People assume that when they pass a continuation into
  ;;   IR1-CONVERT as CONT, it will have a block when it is done.
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
  (continuation-starts-block cont)
  (let* ((found (or (lexenv-find name blocks)
		    (compiler-error "return for unknown block: ~S" name)))
	 (value-cont (make-continuation))
	 (entry (first found))
	 (exit (make-exit :entry entry
			  :value value-cont)))
    (push exit (entry-exits entry))
    (setf (continuation-dest value-cont) exit)
    (ir1-convert start value-cont value)
    (prev-link exit value-cont)
    (let ((home-lambda (continuation-home-lambda-or-null start)))
      (when home-lambda
	(push entry (lambda-calls-or-closes home-lambda))))
    (use-continuation exit (second found))))

;;; Return a list of the segments of a TAGBODY. Each segment looks
;;; like (<tag> <form>* (go <next tag>)). That is, we break up the
;;; tagbody into segments of non-tag statements, and explicitly
;;; represent the drop-through with a GO. The first segment has a
;;; dummy NIL tag, since it represents code before the first tag. The
;;; last segment (which may also be the first segment) ends in NIL
;;; rather than a GO.
(defun parse-tagbody (body)
  (declare (list body))
  (collect ((segments))
    (let ((current (cons nil body)))
      (loop
	(let ((tag-pos (position-if (complement #'listp) current :start 1)))
	  (unless tag-pos
	    (segments `(,@current nil))
	    (return))
	  (let ((tag (elt current tag-pos)))
	    (when (assoc tag (segments))
	      (compiler-error
	       "The tag ~S appears more than once in the tagbody."
	       tag))
	    (unless (or (symbolp tag) (integerp tag))
	      (compiler-error "~S is not a legal tagbody statement." tag))
	    (segments `(,@(subseq current 0 tag-pos) (go ,tag))))
	  (setq current (nthcdr tag-pos current)))))
    (segments)))

;;; Set up the cleanup, emitting the entry node. Then make a block for
;;; each tag, building up the tag list for LEXENV-TAGS as we go.
;;; Finally, convert each segment with the precomputed Start and Cont
;;; values.
(def-ir1-translator tagbody ((&rest statements) start cont)
  #!+sb-doc
  "Tagbody {Tag | Statement}*
  Define tags for used with GO. The Statements are evaluated in order
  (skipping Tags) and NIL is returned. If a statement contains a GO to a
  defined Tag within the lexical scope of the form, then control is transferred
  to the next statement following that tag. A Tag must an integer or a
  symbol. A statement must be a list. Other objects are illegal within the
  body."
  (continuation-starts-block cont)
  (let* ((dummy (make-continuation))
	 (entry (make-entry))
	 (segments (parse-tagbody statements))
	 (cleanup (make-cleanup :kind :tagbody
				:mess-up entry)))
    (push entry (lambda-entries (lexenv-lambda *lexenv*)))
    (setf (entry-cleanup entry) cleanup)
    (prev-link entry start)
    (use-continuation entry dummy)

    (collect ((tags)
	      (starts)
	      (conts))
      (starts dummy)
      (dolist (segment (rest segments))
	(let* ((tag-cont (make-continuation))
               (tag (list (car segment) entry tag-cont)))          
	  (conts tag-cont)
	  (starts tag-cont)
	  (continuation-starts-block tag-cont)
          (tags tag)
          (push (cdr tag) (continuation-lexenv-uses tag-cont))))
      (conts cont)

      (let ((*lexenv* (make-lexenv :cleanup cleanup :tags (tags))))
	(mapc (lambda (segment start cont)
		(ir1-convert-progn-body start cont (rest segment)))
	      segments (starts) (conts))))))

;;; Emit an EXIT node without any value.
(def-ir1-translator go ((tag) start cont)
  #!+sb-doc
  "Go Tag
  Transfer control to the named Tag in the lexically enclosing TAGBODY. This
  is constrained to be used only within the dynamic extent of the TAGBODY."
  (continuation-starts-block cont)
  (let* ((found (or (lexenv-find tag tags :test #'eql)
		    (compiler-error "attempt to GO to nonexistent tag: ~S"
				    tag)))
	 (entry (first found))
	 (exit (make-exit :entry entry)))
    (push exit (entry-exits entry))
    (prev-link exit start)
    (let ((home-lambda (continuation-home-lambda-or-null start)))
      (when home-lambda
	(push entry (lambda-calls-or-closes home-lambda))))
    (use-continuation exit (second found))))

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
(def-ir1-translator eval-when ((situations &rest forms) start cont)
  #!+sb-doc
  "EVAL-WHEN (Situation*) Form*
  Evaluate the Forms in the specified Situations (any of :COMPILE-TOPLEVEL,
  :LOAD-TOPLEVEL, or :EXECUTE, or (deprecated) COMPILE, LOAD, or EVAL)."
  (multiple-value-bind (ct lt e) (parse-eval-when-situations situations)
    (declare (ignore ct lt))
    (ir1-convert-progn-body start cont (and e forms)))
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
  (declare (type function definitionize-fun fun))
  (declare (type (member :variables :functions) definitionize-keyword))
  (declare (type list definitions))
  (unless (= (length definitions)
             (length (remove-duplicates definitions :key #'first)))
    (compiler-style-warning "duplicate definitions in ~S" definitions))
  (let* ((processed-definitions (mapcar definitionize-fun definitions))
         (*lexenv* (make-lexenv definitionize-keyword processed-definitions)))
    (funcall fun)))

;;; Tweak *LEXENV* to include the DEFINITIONS from a MACROLET, then
;;; call FUN (with no arguments).
;;;
;;; This is split off from the IR1 convert method so that it can be
;;; shared by the special-case top level MACROLET processing code.
(defun funcall-in-macrolet-lexenv (definitions fun)
  (%funcall-in-foomacrolet-lexenv
   (lambda (definition)
     (unless (list-of-length-at-least-p definition 2)
       (compiler-error
	"The list ~S is too short to be a legal local macro definition."
	definition))
     (destructuring-bind (name arglist &body body) definition
       (unless (symbolp name)
	 (compiler-error "The local macro name ~S is not a symbol." name))
       (let ((whole (gensym "WHOLE"))
	     (environment (gensym "ENVIRONMENT")))
	 (multiple-value-bind (body local-decls)
	     (parse-defmacro arglist whole body name 'macrolet
			     :environment environment)
	   `(,name macro .
		   ,(compile nil
			     `(lambda (,whole ,environment)
				,@local-decls
				(block ,name ,body))))))))
   :functions
   definitions
   fun))

(def-ir1-translator macrolet ((definitions &rest body) start cont)
  #!+sb-doc
  "MACROLET ({(Name Lambda-List Form*)}*) Body-Form*
  Evaluate the Body-Forms in an environment with the specified local macros
  defined. Name is the local macro name, Lambda-List is the DEFMACRO style
  destructuring lambda list, and the Forms evaluate to the expansion. The
  Forms are evaluated in the null environment."
  (funcall-in-macrolet-lexenv definitions
			      (lambda ()
				(ir1-translate-locally body start cont))))

(defun funcall-in-symbol-macrolet-lexenv (definitions fun)
  (%funcall-in-foomacrolet-lexenv
   (lambda (definition)
     (unless (proper-list-of-length-p definition 2)
       (compiler-error "malformed symbol/expansion pair: ~S" definition))
     (destructuring-bind (name expansion) definition
       (unless (symbolp name)
         (compiler-error
          "The local symbol macro name ~S is not a symbol."
          name))
       `(,name . (MACRO . ,expansion))))
   :variables
   definitions
   fun))
  
(def-ir1-translator symbol-macrolet ((macrobindings &body body) start cont)
  #!+sb-doc
  "SYMBOL-MACROLET ({(Name Expansion)}*) Decl* Form*
  Define the Names as symbol macros with the given Expansions. Within the
  body, references to a Name will effectively be replaced with the Expansion."
  (funcall-in-symbol-macrolet-lexenv
   macrobindings
   (lambda ()
     (ir1-translate-locally body start cont))))

;;; not really a special form, but..
(def-ir1-translator declare ((&rest stuff) start cont)
  (declare (ignore stuff))
  ;; We ignore START and CONT too, but we can't use DECLARE IGNORE to
  ;; tell the compiler about it here, because the DEF-IR1-TRANSLATOR
  ;; macro would put the DECLARE in the wrong place, so..
  start cont
  (compiler-error "misplaced declaration"))

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
(def-ir1-translator %primitive ((name &rest args) start cont)
  (unless (symbolp name)
    (compiler-error "internal error: Primitive name ~S is not a symbol." name))
  (let* ((template (or (gethash name *backend-template-names*)
		       (compiler-error
			"internal error: Primitive name ~A is not defined."
			name)))
	 (required (length (template-arg-types template)))
	 (info (template-info-arg-count template))
	 (min (+ required info))
	 (nargs (length args)))
    (if (template-more-args-type template)
	(when (< nargs min)
	  (compiler-error "internal error: Primitive ~A was called ~
                           with ~R argument~:P, ~
	    		   but wants at least ~R."
			  name
			  nargs
			  min))
	(unless (= nargs min)
	  (compiler-error "internal error: Primitive ~A was called ~
                           with ~R argument~:P, ~
			   but wants exactly ~R."
			  name
			  nargs
			  min)))

    (when (eq (template-result-types template) :conditional)
      (compiler-error
       "%PRIMITIVE was used with a conditional template."))

    (when (template-more-results-type template)
      (compiler-error
       "%PRIMITIVE was used with an unknown values template."))

    (ir1-convert start
		 cont
		 `(%%primitive ',template
			       ',(eval-info-args
				  (subseq args required min))
			       ,@(subseq args 0 required)
			       ,@(subseq args min)))))

;;;; QUOTE and FUNCTION

(def-ir1-translator quote ((thing) start cont)
  #!+sb-doc
  "QUOTE Value
  Return Value without evaluating it."
  (reference-constant start cont thing))

(def-ir1-translator function ((thing) start cont)
  #!+sb-doc
  "FUNCTION Name
  Return the lexically apparent definition of the function Name. Name may also
  be a lambda."
  (if (consp thing)
      (case (car thing)
	((lambda)
	 (reference-leaf start
			 cont
			 (ir1-convert-lambda thing
					     :debug-name (debug-namify
							  "#'~S" thing))))
	((setf)
	 (let ((var (find-lexically-apparent-function
		     thing "as the argument to FUNCTION")))
	   (reference-leaf start cont var)))
	((instance-lambda)
	 (let ((res (ir1-convert-lambda `(lambda ,@(cdr thing))
					:debug-name (debug-namify "#'~S"
								  thing))))
	   (setf (getf (functional-plist res) :fin-function) t)
	   (reference-leaf start cont res)))
	(t
	 (compiler-error "~S is not a legal function name." thing)))
      (let ((var (find-lexically-apparent-function
		  thing "as the argument to FUNCTION")))
	(reference-leaf start cont var))))

;;;; FUNCALL

;;; FUNCALL is implemented on %FUNCALL, which can only call functions
;;; (not symbols). %FUNCALL is used directly in some places where the
;;; call should always be open-coded even if FUNCALL is :NOTINLINE.
(deftransform funcall ((function &rest args) * * :when :both)
  (let ((arg-names (make-gensym-list (length args))))
    `(lambda (function ,@arg-names)
       (%funcall ,(if (csubtypep (continuation-type function)
				 (specifier-type 'function))
		      'function
		      '(%coerce-callable-to-fun function))
		 ,@arg-names))))

(def-ir1-translator %funcall ((function &rest args) start cont)
  (let ((fun-cont (make-continuation)))
    (ir1-convert start fun-cont function)
    (assert-continuation-type fun-cont (specifier-type 'function))
    (ir1-convert-combination-args fun-cont cont args)))

;;; This source transform exists to reduce the amount of work for the
;;; compiler. If the called function is a FUNCTION form, then convert
;;; directly to %FUNCALL, instead of waiting around for type
;;; inference.
(define-source-transform funcall (function &rest args)
  (if (and (consp function) (eq (car function) 'function))
      `(%funcall ,function ,@args)
      (values nil t)))

(deftransform %coerce-callable-to-fun ((thing) (function) *
				       :when :both
				       :important t)
  "optimize away possible call to FDEFINITION at runtime"
  'thing)

;;;; LET and LET*
;;;;
;;;; (LET and LET* can't be implemented as macros due to the fact that
;;;; any pervasive declarations also affect the evaluation of the
;;;; arguments.)

;;; Given a list of binding specifiers in the style of Let, return:
;;;  1. The list of var structures for the variables bound.
;;;  2. The initial value form for each variable.
;;;
;;; The variable names are checked for legality and globally special
;;; variables are marked as such. Context is the name of the form, for
;;; error reporting purposes.
(declaim (ftype (function (list symbol) (values list list list))
		extract-let-variables))
(defun extract-let-variables (bindings context)
  (collect ((vars)
	    (vals)
	    (names))
    (flet ((get-var (name)
	     (varify-lambda-arg name
				(if (eq context 'let*)
				    nil
				    (names)))))
      (dolist (spec bindings)
	(cond ((atom spec)
	       (let ((var (get-var spec)))
		 (vars var)
		 (names (cons spec var))
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

    (values (vars) (vals) (names))))

(def-ir1-translator let ((bindings &body body)
			 start cont)
  #!+sb-doc
  "LET ({(Var [Value]) | Var}*) Declaration* Form*
  During evaluation of the Forms, bind the Vars to the result of evaluating the
  Value forms. The variables are bound in parallel after all of the Values are
  evaluated."
  (multiple-value-bind (forms decls) (sb!sys:parse-body body nil)
    (multiple-value-bind (vars values) (extract-let-variables bindings 'let)
      (let* ((*lexenv* (process-decls decls vars nil cont))
	     (fun-cont (make-continuation))
	     (fun (ir1-convert-lambda-body
		   forms vars :debug-name (debug-namify "LET ~S" bindings))))
	(reference-leaf start fun-cont fun)
	(ir1-convert-combination-args fun-cont cont values)))))

(def-ir1-translator let* ((bindings &body body)
			  start cont)
  #!+sb-doc
  "LET* ({(Var [Value]) | Var}*) Declaration* Form*
  Similar to LET, but the variables are bound sequentially, allowing each Value
  form to reference any of the previous Vars."
  (multiple-value-bind (forms decls) (sb!sys:parse-body body nil)
    (multiple-value-bind (vars values) (extract-let-variables bindings 'let*)
      (let ((*lexenv* (process-decls decls vars nil cont)))
	(ir1-convert-aux-bindings start cont forms vars values)))))

;;; logic shared between IR1 translators for LOCALLY, MACROLET,
;;; and SYMBOL-MACROLET
;;;
;;; Note that all these things need to preserve toplevel-formness,
;;; but we don't need to worry about that within an IR1 translator,
;;; since toplevel-formness is picked off by PROCESS-TOPLEVEL-FOO
;;; forms before we hit the IR1 transform level.
(defun ir1-translate-locally (body start cont)
  (declare (type list body) (type continuation start cont))
  (multiple-value-bind (forms decls) (sb!sys:parse-body body nil)
    (let ((*lexenv* (process-decls decls nil nil cont)))
      (ir1-convert-aux-bindings start cont forms nil nil))))

(def-ir1-translator locally ((&body body) start cont)
  #!+sb-doc
  "LOCALLY Declaration* Form*
  Sequentially evaluate the Forms in a lexical environment where the
  the Declarations have effect. If LOCALLY is a top level form, then
  the Forms are also processed as top level forms."
  (ir1-translate-locally body start cont))

;;;; FLET and LABELS

;;; Given a list of local function specifications in the style of
;;; FLET, return lists of the function names and of the lambdas which
;;; are their definitions.
;;;
;;; The function names are checked for legality. CONTEXT is the name
;;; of the form, for error reporting.
(declaim (ftype (function (list symbol) (values list list))
		extract-flet-variables))
(defun extract-flet-variables (definitions context)
  (collect ((names)
	    (defs))
    (dolist (def definitions)
      (when (or (atom def) (< (length def) 2))
	(compiler-error "The ~S definition spec ~S is malformed." context def))

      (let ((name (first def)))
	(check-fun-name name)
	(names name)
	(multiple-value-bind (forms decls) (sb!sys:parse-body (cddr def))
	  (defs `(lambda ,(second def)
		   ,@decls
		   (block ,(fun-name-block-name name)
		     . ,forms))))))
    (values (names) (defs))))

(def-ir1-translator flet ((definitions &body body)
			  start cont)
  #!+sb-doc
  "FLET ({(Name Lambda-List Declaration* Form*)}*) Declaration* Body-Form*
  Evaluate the Body-Forms with some local function definitions. The bindings
  do not enclose the definitions; any use of Name in the Forms will refer to
  the lexically apparent function definition in the enclosing environment."
  (multiple-value-bind (forms decls) (sb!sys:parse-body body nil)
    (multiple-value-bind (names defs)
	(extract-flet-variables definitions 'flet)
      (let* ((fvars (mapcar (lambda (n d)
  			      (ir1-convert-lambda d
						  :source-name n
						  :debug-name (debug-namify
							       "FLET ~S" n)))
			    names defs))
	     (*lexenv* (make-lexenv
			:default (process-decls decls nil fvars cont)
			:functions (pairlis names fvars))))
	(ir1-convert-progn-body start cont forms)))))

(def-ir1-translator labels ((definitions &body body) start cont)
  #!+sb-doc
  "LABELS ({(Name Lambda-List Declaration* Form*)}*) Declaration* Body-Form*
  Evaluate the Body-Forms with some local function definitions. The bindings
  enclose the new definitions, so the defined functions can call themselves or
  each other."
  (multiple-value-bind (forms decls) (sb!sys:parse-body body nil)
    (multiple-value-bind (names defs)
	(extract-flet-variables definitions 'labels)
      (let* (;; dummy LABELS functions, to be used as placeholders
             ;; during construction of real LABELS functions
	     (placeholder-funs (mapcar (lambda (name)
					 (make-functional
					  :%source-name name
					  :%debug-name (debug-namify
							"LABELS placeholder ~S"
							name)))
				       names))
	     ;; (like PAIRLIS but guaranteed to preserve ordering:)
	     (placeholder-fenv (mapcar #'cons names placeholder-funs))
             ;; the real LABELS functions, compiled in a LEXENV which
             ;; includes the dummy LABELS functions
	     (real-funs
	      (let ((*lexenv* (make-lexenv
			       :functions placeholder-fenv)))
		(mapcar (lambda (name def)
			  (ir1-convert-lambda def
					      :source-name name
					      :debug-name (debug-namify
							   "LABELS ~S" name)))
			names defs))))

        ;; Modify all the references to the dummy function leaves so
        ;; that they point to the real function leaves.
	(loop for real-fun in real-funs and
	      placeholder-cons in placeholder-fenv do
	      (substitute-leaf real-fun (cdr placeholder-cons))
	      (setf (cdr placeholder-cons) real-fun))

        ;; Voila.
	(let ((*lexenv* (make-lexenv
			 :default (process-decls decls nil real-funs cont)
                         ;; Use a proper FENV here (not the
                         ;; placeholder used earlier) so that if the
                         ;; lexical environment is used for inline
                         ;; expansion we'll get the right functions.
                         :functions (pairlis names real-funs))))
	  (ir1-convert-progn-body start cont forms))))))

;;;; the THE special operator, and friends

;;; Do stuff to recognize a THE or VALUES declaration. CONT is the
;;; continuation that the assertion applies to, TYPE is the type
;;; specifier and LEXENV is the current lexical environment. NAME is
;;; the name of the declaration we are doing, for use in error
;;; messages.
;;;
;;; This is somewhat involved, since a type assertion may only be made
;;; on a continuation, not on a node. We can't just set the
;;; continuation asserted type and let it go at that, since there may
;;; be parallel THE's for the same continuation, i.e.
;;;     (if ...
;;;	 (the foo ...)
;;;	 (the bar ...))
;;;
;;; In this case, our representation can do no better than the union
;;; of these assertions. And if there is a branch with no assertion,
;;; we have nothing at all. We really need to recognize scoping, since
;;; we need to be able to discern between parallel assertions (which
;;; we union) and nested ones (which we intersect).
;;;
;;; We represent the scoping by throwing our innermost (intersected)
;;; assertion on CONT into the TYPE-RESTRICTIONS. As we go down, we
;;; intersect our assertions together. If CONT has no uses yet, we
;;; have not yet bottomed out on the first COND branch; in this case
;;; we optimistically assume that this type will be the one we end up
;;; with, and set the ASSERTED-TYPE to it. We can never get better
;;; than the type that we have the first time we bottom out. Later
;;; THE's (or the absence thereof) can only weaken this result.
;;;
;;; We make this work by getting USE-CONTINUATION to do the unioning
;;; across COND branches. We can't do it here, since we don't know how
;;; many branches there are going to be.
(defun do-the-stuff (type cont lexenv name)
  (declare (type continuation cont) (type lexenv lexenv))
  (let* ((ctype (values-specifier-type type))
	 (old-type (or (lexenv-find cont type-restrictions)
		       *wild-type*))
	 (intersects (values-types-equal-or-intersect old-type ctype))
	 (int (values-type-intersection old-type ctype))
	 (new (if intersects int old-type)))
    (when (null (find-uses cont))
      (setf (continuation-asserted-type cont) new))
    (when (and (not intersects)
	       (not (policy *lexenv*
			    (= inhibit-warnings 3)))) ;FIXME: really OK to suppress?
      (compiler-warning
       "The type ~S in ~S declaration conflicts with an enclosing assertion:~%   ~S"
       (type-specifier ctype)
       name
       (type-specifier old-type)))
    (make-lexenv :type-restrictions `((,cont . ,new))
		 :default lexenv)))

;;; Assert that FORM evaluates to the specified type (which may be a
;;; VALUES type).
;;;
;;; FIXME: In a version of CMU CL that I used at Cadabra ca. 20000101,
;;; this didn't seem to expand into an assertion, at least for ALIEN
;;; values. Check that SBCL doesn't have this problem.
(def-ir1-translator the ((type value) start cont)
  (let ((*lexenv* (do-the-stuff type cont *lexenv* 'the)))
    (ir1-convert start cont value)))

;;; This is like the THE special form, except that it believes
;;; whatever you tell it. It will never generate a type check, but
;;; will cause a warning if the compiler can prove the assertion is
;;; wrong.
;;;
;;; Since the CONTINUATION-DERIVED-TYPE is computed as the union of
;;; its uses's types, setting it won't work. Instead we must intersect
;;; the type with the uses's DERIVED-TYPE.
(def-ir1-translator truly-the ((type value) start cont)
  #!+sb-doc
  (declare (inline member))
  (let ((type (values-specifier-type type))
	(old (find-uses cont)))
    (ir1-convert start cont value)
    (do-uses (use cont)
      (unless (member use old :test #'eq)
	(derive-node-type use type)))))

;;;; SETQ

;;; If there is a definition in LEXENV-VARIABLES, just set that,
;;; otherwise look at the global information. If the name is for a
;;; constant, then error out.
(def-ir1-translator setq ((&whole source &rest things) start cont)
  (let ((len (length things)))
    (when (oddp len)
      (compiler-error "odd number of args to SETQ: ~S" source))
    (if (= len 2)
	(let* ((name (first things))
	       (leaf (or (lexenv-find name variables)
			 (find-free-variable name))))
	  (etypecase leaf
	    (leaf
	     (when (constant-p leaf)
	       (compiler-error "~S is a constant and thus can't be set." name))
	     (when (lambda-var-p leaf)
	       (let ((home-lambda (continuation-home-lambda-or-null start)))
		 (when home-lambda
		   (pushnew leaf (lambda-calls-or-closes home-lambda))))
	       (when (lambda-var-ignorep leaf)
		 ;; ANSI's definition of "Declaration IGNORE, IGNORABLE"
		 ;; requires that this be a STYLE-WARNING, not a full warning.
		 (compiler-style-warning
		  "~S is being set even though it was declared to be ignored."
		  name)))
	     (set-variable start cont leaf (second things)))
	    (cons
	     (aver (eq (car leaf) 'MACRO))
	     (ir1-convert start cont `(setf ,(cdr leaf) ,(second things))))
	    (heap-alien-info
	     (ir1-convert start cont
			  `(%set-heap-alien ',leaf ,(second things))))))
	(collect ((sets))
	  (do ((thing things (cddr thing)))
	      ((endp thing)
	       (ir1-convert-progn-body start cont (sets)))
	    (sets `(setq ,(first thing) ,(second thing))))))))

;;; This is kind of like REFERENCE-LEAF, but we generate a SET node.
;;; This should only need to be called in SETQ.
(defun set-variable (start cont var value)
  (declare (type continuation start cont) (type basic-var var))
  (let ((dest (make-continuation)))
    (setf (continuation-asserted-type dest) (leaf-type var))
    (ir1-convert start dest value)
    (let ((res (make-set :var var :value dest)))
      (setf (continuation-dest dest) res)
      (setf (leaf-ever-used var) t)
      (push res (basic-var-sets var))
      (prev-link res dest)
      (use-continuation res cont))))

;;;; CATCH, THROW and UNWIND-PROTECT

;;; We turn THROW into a multiple-value-call of a magical function,
;;; since as as far as IR1 is concerned, it has no interesting
;;; properties other than receiving multiple-values.
(def-ir1-translator throw ((tag result) start cont)
  #!+sb-doc
  "Throw Tag Form
  Do a non-local exit, return the values of Form from the CATCH whose tag
  evaluates to the same thing as Tag."
  (ir1-convert start cont
	       `(multiple-value-call #'%throw ,tag ,result)))

;;; This is a special special form used to instantiate a cleanup as
;;; the current cleanup within the body. KIND is the kind of cleanup
;;; to make, and MESS-UP is a form that does the mess-up action. We
;;; make the MESS-UP be the USE of the MESS-UP form's continuation,
;;; and introduce the cleanup into the lexical environment. We
;;; back-patch the ENTRY-CLEANUP for the current cleanup to be the new
;;; cleanup, since this inner cleanup is the interesting one.
(def-ir1-translator %within-cleanup ((kind mess-up &body body) start cont)
  (let ((dummy (make-continuation))
	(dummy2 (make-continuation)))
    (ir1-convert start dummy mess-up)
    (let* ((mess-node (continuation-use dummy))
	   (cleanup (make-cleanup :kind kind
				  :mess-up mess-node))
	   (old-cup (lexenv-cleanup *lexenv*))
	   (*lexenv* (make-lexenv :cleanup cleanup)))
      (setf (entry-cleanup (cleanup-mess-up old-cup)) cleanup)
      (ir1-convert dummy dummy2 '(%cleanup-point))
      (ir1-convert-progn-body dummy2 cont body))))

;;; This is a special special form that makes an "escape function"
;;; which returns unknown values from named block. We convert the
;;; function, set its kind to :ESCAPE, and then reference it. The
;;; :ESCAPE kind indicates that this function's purpose is to
;;; represent a non-local control transfer, and that it might not
;;; actually have to be compiled.
;;;
;;; Note that environment analysis replaces references to escape
;;; functions with references to the corresponding NLX-INFO structure.
(def-ir1-translator %escape-function ((tag) start cont)
  (let ((fun (ir1-convert-lambda
	      `(lambda ()
		 (return-from ,tag (%unknown-values)))
	      :debug-name (debug-namify "escape function for ~S" tag))))
    (setf (functional-kind fun) :escape)
    (reference-leaf start cont fun)))

;;; Yet another special special form. This one looks up a local
;;; function and smashes it to a :CLEANUP function, as well as
;;; referencing it.
(def-ir1-translator %cleanup-function ((name) start cont)
  (let ((fun (lexenv-find name functions)))
    (aver (lambda-p fun))
    (setf (functional-kind fun) :cleanup)
    (reference-leaf start cont fun)))

;;; We represent the possibility of the control transfer by making an
;;; "escape function" that does a lexical exit, and instantiate the
;;; cleanup using %WITHIN-CLEANUP.
(def-ir1-translator catch ((tag &body body) start cont)
  #!+sb-doc
  "Catch Tag Form*
  Evaluates Tag and instantiates it as a catcher while the body forms are
  evaluated in an implicit PROGN. If a THROW is done to Tag within the dynamic
  scope of the body, then control will be transferred to the end of the body
  and the thrown values will be returned."
  (ir1-convert
   start cont
   (let ((exit-block (gensym "EXIT-BLOCK-")))
     `(block ,exit-block
	(%within-cleanup
	    :catch
	    (%catch (%escape-function ,exit-block) ,tag)
	  ,@body)))))

;;; UNWIND-PROTECT is similar to CATCH, but hairier. We make the
;;; cleanup forms into a local function so that they can be referenced
;;; both in the case where we are unwound and in any local exits. We
;;; use %CLEANUP-FUNCTION on this to indicate that reference by
;;; %UNWIND-PROTECT isn't "real", and thus doesn't cause creation of
;;; an XEP.
(def-ir1-translator unwind-protect ((protected &body cleanup) start cont)
  #!+sb-doc
  "Unwind-Protect Protected Cleanup*
  Evaluate the form Protected, returning its values. The cleanup forms are
  evaluated whenever the dynamic scope of the Protected form is exited (either
  due to normal completion or a non-local exit such as THROW)."
  (ir1-convert
   start cont
   (let ((cleanup-fun (gensym "CLEANUP-FUN-"))
	 (drop-thru-tag (gensym "DROP-THRU-TAG-"))
	 (exit-tag (gensym "EXIT-TAG-"))
	 (next (gensym "NEXT"))
	 (start (gensym "START"))
	 (count (gensym "COUNT")))
     `(flet ((,cleanup-fun () ,@cleanup nil))
	;; FIXME: If we ever get DYNAMIC-EXTENT working, then
	;; ,CLEANUP-FUN should probably be declared DYNAMIC-EXTENT,
	;; and something can be done to make %ESCAPE-FUNCTION have
	;; dynamic extent too.
	(block ,drop-thru-tag
	  (multiple-value-bind (,next ,start ,count)
	      (block ,exit-tag
		(%within-cleanup
		    :unwind-protect
		    (%unwind-protect (%escape-function ,exit-tag)
				     (%cleanup-function ,cleanup-fun))
		  (return-from ,drop-thru-tag ,protected)))
	    (,cleanup-fun)
	    (%continue-unwind ,next ,start ,count)))))))

;;;; multiple-value stuff

;;; If there are arguments, MULTIPLE-VALUE-CALL turns into an
;;; MV-COMBINATION.
;;;
;;; If there are no arguments, then we convert to a normal
;;; combination, ensuring that a MV-COMBINATION always has at least
;;; one argument. This can be regarded as an optimization, but it is
;;; more important for simplifying compilation of MV-COMBINATIONS.
(def-ir1-translator multiple-value-call ((fun &rest args) start cont)
  #!+sb-doc
  "MULTIPLE-VALUE-CALL Function Values-Form*
  Call Function, passing all the values of each Values-Form as arguments,
  values from the first Values-Form making up the first argument, etc."
  (let* ((fun-cont (make-continuation))
	 (node (if args
		   (make-mv-combination fun-cont)
		   (make-combination fun-cont))))
    (ir1-convert start fun-cont
		 (if (and (consp fun) (eq (car fun) 'function))
		     fun
		     `(%coerce-callable-to-fun ,fun)))
    (setf (continuation-dest fun-cont) node)
    (assert-continuation-type fun-cont
			      (specifier-type '(or function symbol)))
    (collect ((arg-conts))
      (let ((this-start fun-cont))
	(dolist (arg args)
	  (let ((this-cont (make-continuation node)))
	    (ir1-convert this-start this-cont arg)
	    (setq this-start this-cont)
	    (arg-conts this-cont)))
	(prev-link node this-start)
	(use-continuation node cont)
	(setf (basic-combination-args node) (arg-conts))))))

;;; MULTIPLE-VALUE-PROG1 is represented implicitly in IR1 by having a
;;; the result code use result continuation (CONT), but transfer
;;; control to the evaluation of the body. In other words, the result
;;; continuation isn't IMMEDIATELY-USED-P by the nodes that compute
;;; the result.
;;;
;;; In order to get the control flow right, we convert the result with
;;; a dummy result continuation, then convert all the uses of the
;;; dummy to be uses of CONT. If a use is an EXIT, then we also
;;; substitute CONT for the dummy in the corresponding ENTRY node so
;;; that they are consistent. Note that this doesn't amount to
;;; changing the exit target, since the control destination of an exit
;;; is determined by the block successor; we are just indicating the
;;; continuation that the result is delivered to.
;;;
;;; We then convert the body, using another dummy continuation in its
;;; own block as the result. After we are done converting the body, we
;;; move all predecessors of the dummy end block to CONT's block.
;;;
;;; Note that we both exploit and maintain the invariant that the CONT
;;; to an IR1 convert method either has no block or starts the block
;;; that control should transfer to after completion for the form.
;;; Nested MV-PROG1's work because during conversion of the result
;;; form, we use dummy continuation whose block is the true control
;;; destination.
(def-ir1-translator multiple-value-prog1 ((result &rest forms) start cont)
  #!+sb-doc
  "MULTIPLE-VALUE-PROG1 Values-Form Form*
  Evaluate Values-Form and then the Forms, but return all the values of
  Values-Form."
  (continuation-starts-block cont)
  (let* ((dummy-result (make-continuation))
	 (dummy-start (make-continuation))
	 (cont-block (continuation-block cont)))
    (continuation-starts-block dummy-start)
    (ir1-convert start dummy-start result)

    (substitute-continuation-uses cont dummy-start)

    (continuation-starts-block dummy-result)
    (ir1-convert-progn-body dummy-start dummy-result forms)
    (let ((end-block (continuation-block dummy-result)))
      (dolist (pred (block-pred end-block))
	(unlink-blocks pred end-block)
	(link-blocks pred cont-block))
      (aver (not (continuation-dest dummy-result)))
      (delete-continuation dummy-result)
      (remove-from-dfo end-block))))

;;;; interface to defining macros

;;;; FIXME:
;;;;   classic CMU CL comment:
;;;;     DEFMACRO and DEFUN expand into calls to %DEFxxx functions
;;;;     so that we get a chance to see what is going on. We define
;;;;     IR1 translators for these functions which look at the
;;;;     definition and then generate a call to the %%DEFxxx function.
;;;; Alas, this implementation doesn't do the right thing for
;;;; non-toplevel uses of these forms, so this should probably
;;;; be changed to use EVAL-WHEN instead.

;;; Return a new source path with any stuff intervening between the
;;; current path and the first form beginning with NAME stripped off.
;;; This is used to hide the guts of DEFmumble macros to prevent
;;; annoying error messages.
(defun revert-source-path (name)
  (do ((path *current-path* (cdr path)))
      ((null path) *current-path*)
    (let ((first (first path)))
      (when (or (eq first name)
		(eq first 'original-source-start))
	(return path)))))

;;; Warn about incompatible or illegal definitions and add the macro
;;; to the compiler environment.
;;;
;;; Someday we could check for macro arguments being incompatibly
;;; redefined. Doing this right will involve finding the old macro
;;; lambda-list and comparing it with the new one.
(def-ir1-translator %defmacro ((qname qdef lambda-list doc) start cont
			       :kind :function)
  (let (;; QNAME is typically a quoted name. I think the idea is to
	;; let %DEFMACRO work as an ordinary function when
	;; interpreting. Whatever the reason the quote is there, we
	;; don't want it any more. -- WHN 19990603
	(name (eval qname))
	;; QDEF should be a sharp-quoted definition. We don't want to
	;; make a function of it just yet, so we just drop the
	;; sharp-quote.
	(def (progn
	       (aver (eq 'function (first qdef)))
	       (aver (proper-list-of-length-p qdef 2))
	       (second qdef))))

    (/show "doing IR1 translator for %DEFMACRO" name)

    (unless (symbolp name)
      (compiler-error "The macro name ~S is not a symbol." name))

    (ecase (info :function :kind name)
      ((nil))
      (:function
       (remhash name *free-functions*)
       (undefine-fun-name name)
       (compiler-warning
	"~S is being redefined as a macro when it was ~
         previously ~(~A~) to be a function."
	name
	(info :function :where-from name)))
      (:macro)
      (:special-form
       (compiler-error "The special form ~S can't be redefined as a macro."
		       name)))

    (setf (info :function :kind name) :macro
	  (info :function :where-from name) :defined
	  (info :function :macro-function name) (coerce def 'function))

    (let* ((*current-path* (revert-source-path 'defmacro))
	   (fun (ir1-convert-lambda def 
				    :debug-name (debug-namify "DEFMACRO ~S"
							      name))))
      (setf (functional-arg-documentation fun) (eval lambda-list))

      (ir1-convert start cont `(%%defmacro ',name ,fun ,doc)))

    (when sb!xc:*compile-print*
      ;; FIXME: It would be nice to convert this, and the other places
      ;; which create compiler diagnostic output prefixed by
      ;; semicolons, to use some common utility which automatically
      ;; prefixes all its output with semicolons. (The addition of
      ;; semicolon prefixes was introduced ca. sbcl-0.6.8.10 as the
      ;; "MNA compiler message patch", and implemented by modifying a
      ;; bunch of output statements on a case-by-case basis, which
      ;; seems unnecessarily error-prone and unclear, scattering
      ;; implicit information about output style throughout the
      ;; system.) Starting by rewriting COMPILER-MUMBLE to add
      ;; semicolon prefixes would be a good start, and perhaps also:
      ;;   * Add semicolon prefixes for "FOO assembled" messages emitted 
      ;;     when e.g. src/assembly/x86/assem-rtns.lisp is processed.
      ;;   * At least some debugger output messages deserve semicolon
      ;;     prefixes too:
      ;;     ** restarts table
      ;;     ** "Within the debugger, you can type HELP for help."
      (compiler-mumble "~&; converted ~S~%" name))))

(def-ir1-translator %define-compiler-macro ((name def lambda-list doc)
					    start cont
					    :kind :function)
  (let ((name (eval name))
	(def (second def))) ; We don't want to make a function just yet...

    (when (eq (info :function :kind name) :special-form)
      (compiler-error "attempt to define a compiler-macro for special form ~S"
		      name))

    (setf (info :function :compiler-macro-function name)
	  (coerce def 'function))

    (let* ((*current-path* (revert-source-path 'define-compiler-macro))
	   (fun (ir1-convert-lambda def 
				    :debug-name (debug-namify
						 "DEFINE-COMPILER-MACRO ~S"
						 name))))
      (setf (functional-arg-documentation fun) (eval lambda-list))

      (ir1-convert start cont `(%%define-compiler-macro ',name ,fun ,doc)))

    (when sb!xc:*compile-print*
      (compiler-mumble "~&; converted ~S~%" name))))

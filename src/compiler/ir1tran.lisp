;;;; This file contains code which does the translation from Lisp code
;;;; to the first intermediate representation (IR1).

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

(declaim (special *compiler-error-bailout*))

;;; *SOURCE-PATHS* is a hashtable from source code forms to the path
;;; taken through the source to reach the form. This provides a way to
;;; keep track of the location of original source forms, even when
;;; macroexpansions and other arbitary permutations of the code
;;; happen. This table is initialized by calling FIND-SOURCE-PATHS on
;;; the original source.
(declaim (hash-table *source-paths*))
(defvar *source-paths*)

;;; *CURRENT-COMPONENT* is the COMPONENT structure which we link
;;; blocks into as we generate them. This just serves to glue the
;;; emitted blocks together until local call analysis and flow graph
;;; canonicalization figure out what is really going on. We need to
;;; keep track of all the blocks generated so that we can delete them
;;; if they turn out to be unreachable.
;;;
;;; FIXME: It's confusing having one variable named *CURRENT-COMPONENT*
;;; and another named *COMPONENT-BEING-COMPILED*. (In CMU CL they
;;; were called *CURRENT-COMPONENT* and *COMPILE-COMPONENT* respectively,
;;; which was also confusing.)
(declaim (type (or component null) *current-component*))
(defvar *current-component*)

;;; *CURRENT-PATH* is the source path of the form we are currently
;;; translating. See NODE-SOURCE-PATH in the NODE structure.
(declaim (list *current-path*))
(defvar *current-path*)

(defvar *derive-function-types* nil
  "Should the compiler assume that function types will never change,
  so that it can use type information inferred from current definitions
  to optimize code which uses those definitions? Setting this true
  gives non-ANSI, early-CMU-CL behavior. It can be useful for improving
  the efficiency of stable code.")

;;;; namespace management utilities

;;; Return a GLOBAL-VAR structure usable for referencing the global
;;; function NAME.
(defun find-free-really-fun (name)
  (unless (info :function :kind name)
    (setf (info :function :kind name) :function)
    (setf (info :function :where-from name) :assumed))

  (let ((where (info :function :where-from name)))
    (when (and (eq where :assumed)
	       ;; In the ordinary target Lisp, it's silly to report
	       ;; undefinedness when the function is defined in the
	       ;; running Lisp. But at cross-compile time, the current
	       ;; definedness of a function is irrelevant to the
	       ;; definedness at runtime, which is what matters.
	       #-sb-xc-host (not (fboundp name)))
      (note-undefined-reference name :function))
    (make-global-var :kind :global-function
		     :%source-name name
		     :type (if (or *derive-function-types*
				   (eq where :declared))
			       (info :function :type name)
			       (specifier-type 'function))
		     :where-from where)))

;;; Has the *FREE-FUNS* entry FREE-FUN become invalid?
;;;
;;; In CMU CL, the answer was implicitly always true, so this 
;;; predicate didn't exist.
;;;
;;; This predicate was added to fix bug 138 in SBCL. In some obscure
;;; circumstances, it was possible for a *FREE-FUNS* entry to contain a
;;; DEFINED-FUN whose DEFINED-FUN-FUNCTIONAL object contained IR1
;;; stuff (NODEs, BLOCKs...) referring to an already compiled (aka
;;; "dead") component. When this IR1 stuff was reused in a new
;;; component, under further obscure circumstances it could be used by
;;; WITH-IR1-ENVIRONMENT-FROM-NODE to generate a binding for
;;; *CURRENT-COMPONENT*. At that point things got all confused, since
;;; IR1 conversion was sending code to a component which had already
;;; been compiled and would never be compiled again.
(defun invalid-free-fun-p (free-fun)
  ;; There might be other reasons that *FREE-FUN* entries could
  ;; become invalid, but the only one we've been bitten by so far
  ;; (sbcl-0.pre7.118) is this one:
  (and (defined-fun-p free-fun)
       (let ((functional (defined-fun-functional free-fun)))
	 (or (and functional
		  (eql (functional-kind functional) :deleted))
	     (and (lambda-p functional)
		  (or
		   ;; (The main reason for this first test is to bail
		   ;; out early in cases where the LAMBDA-COMPONENT
		   ;; call in the second test would fail because links
		   ;; it needs are uninitialized or invalid.)
		   ;;
		   ;; If the BIND node for this LAMBDA is null, then
		   ;; according to the slot comments, the LAMBDA has
		   ;; been deleted or its call has been deleted. In
		   ;; that case, it seems rather questionable to reuse
		   ;; it, and certainly it shouldn't be necessary to
		   ;; reuse it, so we cheerfully declare it invalid.
		   (null (lambda-bind functional))
		   ;; If this IR1 stuff belongs to a dead component,
		   ;; then we can't reuse it without getting into
		   ;; bizarre confusion.
		   (eql (component-info (lambda-component functional))
			:dead)))))))

;;; If NAME already has a valid entry in *FREE-FUNS*, then return
;;; the value. Otherwise, make a new GLOBAL-VAR using information from
;;; the global environment and enter it in *FREE-FUNS*. If NAME
;;; names a macro or special form, then we error out using the
;;; supplied context which indicates what we were trying to do that
;;; demanded a function.
(declaim (ftype (function (t string) global-var) find-free-fun))
(defun find-free-fun (name context)
  (or (let ((old-free-fun (gethash name *free-funs*)))
	(and (not (invalid-free-fun-p old-free-fun))
	     old-free-fun))
      (ecase (info :function :kind name)
	;; FIXME: The :MACRO and :SPECIAL-FORM cases could be merged.
	(:macro
	 (compiler-error "The macro name ~S was found ~A." name context))
	(:special-form
	 (compiler-error "The special form name ~S was found ~A."
			 name
			 context))
	((:function nil)
	 (check-fun-name name)
	 (note-if-setf-fun-and-macro name)
	 (let ((expansion (fun-name-inline-expansion name))
	       (inlinep (info :function :inlinep name)))
	   (setf (gethash name *free-funs*)
		 (if (or expansion inlinep)
		     (make-defined-fun
		      :%source-name name
		      :inline-expansion expansion
		      :inlinep inlinep
		      :where-from (info :function :where-from name)
		      :type (info :function :type name))
		     (find-free-really-fun name))))))))

;;; Return the LEAF structure for the lexically apparent function
;;; definition of NAME.
(declaim (ftype (function (t string) leaf) find-lexically-apparent-fun))
(defun find-lexically-apparent-fun (name context)
  (let ((var (lexenv-find name funs :test #'equal)))
    (cond (var
	   (unless (leaf-p var)
	     (aver (and (consp var) (eq (car var) 'macro)))
	     (compiler-error "found macro name ~S ~A" name context))
	   var)
	  (t
	   (find-free-fun name context)))))

;;; Return the LEAF node for a global variable reference to NAME. If
;;; NAME is already entered in *FREE-VARS*, then we just return the
;;; corresponding value. Otherwise, we make a new leaf using
;;; information from the global environment and enter it in
;;; *FREE-VARS*. If the variable is unknown, then we emit a warning.
(declaim (ftype (function (t) (or leaf cons heap-alien-info)) find-free-var))
(defun find-free-var (name)
  (unless (symbolp name)
    (compiler-error "Variable name is not a symbol: ~S." name))
  (or (gethash name *free-vars*)
      (let ((kind (info :variable :kind name))
	    (type (info :variable :type name))
	    (where-from (info :variable :where-from name)))
	(when (and (eq where-from :assumed) (eq kind :global))
	  (note-undefined-reference name :variable))
	(setf (gethash name *free-vars*)
	      (case kind
		(:alien
		 (info :variable :alien-info name))
                ;; FIXME: The return value in this case should really be
                ;; of type SB!C::LEAF.  I don't feel too badly about it,
                ;; because the MACRO idiom is scattered throughout this
                ;; file, but it should be cleaned up so we're not
                ;; throwing random conses around.  --njf 2002-03-23
                (:macro
                 (let ((expansion (info :variable :macro-expansion name))
                       (type (type-specifier (info :variable :type name))))
                   `(MACRO . (the ,type ,expansion))))
		(:constant
		 (let ((value (info :variable :constant-value name)))
		   (make-constant :value value
				  :%source-name name
				  :type (ctype-of value)
				  :where-from where-from)))
		(t
		 (make-global-var :kind kind
				  :%source-name name
				  :type type
				  :where-from where-from)))))))

;;; Grovel over CONSTANT checking for any sub-parts that need to be
;;; processed with MAKE-LOAD-FORM. We have to be careful, because
;;; CONSTANT might be circular. We also check that the constant (and
;;; any subparts) are dumpable at all.
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; The EVAL-WHEN is necessary for #.(1+ LIST-TO-HASH-TABLE-THRESHOLD) 
  ;; below. -- AL 20010227
  (def!constant list-to-hash-table-threshold 32))
(defun maybe-emit-make-load-forms (constant)
  (let ((things-processed nil)
	(count 0))
    ;; FIXME: Does this LIST-or-HASH-TABLE messiness give much benefit?
    (declare (type (or list hash-table) things-processed)
	     (type (integer 0 #.(1+ list-to-hash-table-threshold)) count)
	     (inline member))
    (labels ((grovel (value)
	       ;; Unless VALUE is an object which which obviously
	       ;; can't contain other objects
	       (unless (typep value
			      '(or #-sb-xc-host unboxed-array
				   symbol
				   number
				   character
				   string))
		 (etypecase things-processed
		   (list
		    (when (member value things-processed :test #'eq)
		      (return-from grovel nil))
		    (push value things-processed)
		    (incf count)
		    (when (> count list-to-hash-table-threshold)
		      (let ((things things-processed))
			(setf things-processed
			      (make-hash-table :test 'eq))
			(dolist (thing things)
			  (setf (gethash thing things-processed) t)))))
		   (hash-table
		    (when (gethash value things-processed)
		      (return-from grovel nil))
		    (setf (gethash value things-processed) t)))
		 (typecase value
		   (cons
		    (grovel (car value))
		    (grovel (cdr value)))
		   (simple-vector
		    (dotimes (i (length value))
		      (grovel (svref value i))))
		   ((vector t)
		    (dotimes (i (length value))
		      (grovel (aref value i))))
		   ((simple-array t)
		    ;; Even though the (ARRAY T) branch does the exact
		    ;; same thing as this branch we do this separately
		    ;; so that the compiler can use faster versions of
		    ;; array-total-size and row-major-aref.
		    (dotimes (i (array-total-size value))
		      (grovel (row-major-aref value i))))
		   ((array t)
		    (dotimes (i (array-total-size value))
		      (grovel (row-major-aref value i))))
		   (;; In the target SBCL, we can dump any instance,
		    ;; but in the cross-compilation host,
		    ;; %INSTANCE-FOO functions don't work on general
		    ;; instances, only on STRUCTURE!OBJECTs.
		    #+sb-xc-host structure!object
		    #-sb-xc-host instance
		    (when (emit-make-load-form value)
		      (dotimes (i (%instance-length value))
			(grovel (%instance-ref value i)))))
		   (t
		    (compiler-error
		     "Objects of type ~S can't be dumped into fasl files."
		     (type-of value)))))))
      (grovel constant)))
  (values))

;;;; some flow-graph hacking utilities

;;; This function sets up the back link between the node and the
;;; continuation which continues at it.
(defun link-node-to-previous-continuation (node cont)
  (declare (type node node) (type continuation cont))
  (aver (not (continuation-next cont)))
  (setf (continuation-next cont) node)
  (setf (node-prev node) cont))

;;; This function is used to set the continuation for a node, and thus
;;; determine what receives the value and what is evaluated next. If
;;; the continuation has no block, then we make it be in the block
;;; that the node is in. If the continuation heads its block, we end
;;; our block and link it to that block. If the continuation is not
;;; currently used, then we set the DERIVED-TYPE for the continuation
;;; to that of the node, so that a little type propagation gets done.
;;;
;;; We also deal with a bit of THE's semantics here: we weaken the
;;; assertion on CONT to be no stronger than the assertion on CONT in
;;; our scope. See the IR1-CONVERT method for THE.
#!-sb-fluid (declaim (inline use-continuation))
(defun use-continuation (node cont)
  (declare (type node node) (type continuation cont))
  (let ((node-block (continuation-block (node-prev node))))
    (case (continuation-kind cont)
      (:unused
       (setf (continuation-block cont) node-block)
       (setf (continuation-kind cont) :inside-block)
       (setf (continuation-use cont) node)
       (setf (node-cont node) cont))
      (t
       (%use-continuation node cont)))))
(defun %use-continuation (node cont)
  (declare (type node node) (type continuation cont) (inline member))
  (let ((block (continuation-block cont))
	(node-block (continuation-block (node-prev node))))
    (aver (eq (continuation-kind cont) :block-start))
    (when (block-last node-block)
      (error "~S has already ended." node-block))
    (setf (block-last node-block) node)
    (when (block-succ node-block)
      (error "~S already has successors." node-block))
    (setf (block-succ node-block) (list block))
    (when (memq node-block (block-pred block))
      (error "~S is already a predecessor of ~S." node-block block))
    (push node-block (block-pred block))
    (add-continuation-use node cont)
    (unless (eq (continuation-asserted-type cont) *wild-type*)
      (let ((new (values-type-union (continuation-asserted-type cont)
				    (or (lexenv-find cont type-restrictions)
					*wild-type*))))
	(when (type/= new (continuation-asserted-type cont))
	  (setf (continuation-asserted-type cont) new)
	  (reoptimize-continuation cont))))))

;;;; exported functions

;;; This function takes a form and the top level form number for that
;;; form, and returns a lambda representing the translation of that
;;; form in the current global environment. The returned lambda is a
;;; top level lambda that can be called to cause evaluation of the
;;; forms. This lambda is in the initial component. If FOR-VALUE is T,
;;; then the value of the form is returned from the function,
;;; otherwise NIL is returned.
;;;
;;; This function may have arbitrary effects on the global environment
;;; due to processing of EVAL-WHENs. All syntax error checking is
;;; done, with erroneous forms being replaced by a proxy which signals
;;; an error if it is evaluated. Warnings about possibly inconsistent
;;; or illegal changes to the global environment will also be given.
;;;
;;; We make the initial component and convert the form in a PROGN (and
;;; an optional NIL tacked on the end.) We then return the lambda. We
;;; bind all of our state variables here, rather than relying on the
;;; global value (if any) so that IR1 conversion will be reentrant.
;;; This is necessary for EVAL-WHEN processing, etc.
;;;
;;; The hashtables used to hold global namespace info must be
;;; reallocated elsewhere. Note also that *LEXENV* is not bound, so
;;; that local macro definitions can be introduced by enclosing code.
(defun ir1-toplevel (form path for-value)
  (declare (list path))
  (let* ((*current-path* path)
	 (component (make-empty-component))
	 (*current-component* component))
    (setf (component-name component) "initial component")
    (setf (component-kind component) :initial)
    (let* ((forms (if for-value `(,form) `(,form nil)))
	   (res (ir1-convert-lambda-body
		 forms ()
		 :debug-name (debug-namify "top level form ~S" form))))
      (setf (functional-entry-fun res) res
	    (functional-arg-documentation res) ()
	    (functional-kind res) :toplevel)
      res)))

;;; *CURRENT-FORM-NUMBER* is used in FIND-SOURCE-PATHS to compute the
;;; form number to associate with a source path. This should be bound
;;; to an initial value of 0 before the processing of each truly
;;; top level form.
(declaim (type index *current-form-number*))
(defvar *current-form-number*)

;;; This function is called on freshly read forms to record the
;;; initial location of each form (and subform.) Form is the form to
;;; find the paths in, and TLF-NUM is the top level form number of the
;;; truly top level form.
;;;
;;; This gets a bit interesting when the source code is circular. This
;;; can (reasonably?) happen in the case of circular list constants.
(defun find-source-paths (form tlf-num)
  (declare (type index tlf-num))
  (let ((*current-form-number* 0))
    (sub-find-source-paths form (list tlf-num)))
  (values))
(defun sub-find-source-paths (form path)
  (unless (gethash form *source-paths*)
    (setf (gethash form *source-paths*)
	  (list* 'original-source-start *current-form-number* path))
    (incf *current-form-number*)
    (let ((pos 0)
	  (subform form)
	  (trail form))
      (declare (fixnum pos))
      (macrolet ((frob ()
		   '(progn
		      (when (atom subform) (return))
		      (let ((fm (car subform)))
			(when (consp fm)
			  (sub-find-source-paths fm (cons pos path)))
			(incf pos))
		      (setq subform (cdr subform))
		      (when (eq subform trail) (return)))))
	(loop
	  (frob)
	  (frob)
	  (setq trail (cdr trail)))))))

;;;; IR1-CONVERT, macroexpansion and special form dispatching

(macrolet (;; Bind *COMPILER-ERROR-BAILOUT* to a function that throws
	   ;; out of the body and converts a proxy form instead.
	   (ir1-error-bailout ((start
				cont
				form
				&optional
				(proxy ``(error 'simple-program-error
					  :format-control "execution of a form compiled with errors:~% ~S"
					  :format-arguments (list ',,form))))
			       &body body)
			      (let ((skip (gensym "SKIP")))
				`(block ,skip
				   (catch 'ir1-error-abort
				     (let ((*compiler-error-bailout*
					    (lambda ()
					      (throw 'ir1-error-abort nil))))
				       ,@body
				       (return-from ,skip nil)))
				   (ir1-convert ,start ,cont ,proxy)))))

  ;; Translate FORM into IR1. The code is inserted as the NEXT of the
  ;; continuation START. CONT is the continuation which receives the
  ;; value of the FORM to be translated. The translators call this
  ;; function recursively to translate their subnodes.
  ;;
  ;; As a special hack to make life easier in the compiler, a LEAF
  ;; IR1-converts into a reference to that LEAF structure. This allows
  ;; the creation using backquote of forms that contain leaf
  ;; references, without having to introduce dummy names into the
  ;; namespace.
  (declaim (ftype (function (continuation continuation t) (values)) ir1-convert))
  (defun ir1-convert (start cont form)
    (ir1-error-bailout (start cont form)
      (let ((*current-path* (or (gethash form *source-paths*)
				(cons form *current-path*))))
	(if (atom form)
	    (cond ((and (symbolp form) (not (keywordp form)))
		   (ir1-convert-var start cont form))
		  ((leaf-p form)
		   (reference-leaf start cont form))
		  (t
		   (reference-constant start cont form)))
	    (let ((opname (car form)))
	      (cond ((symbolp opname)
		     (let ((lexical-def (lexenv-find opname funs)))
		       (typecase lexical-def
			 (null (ir1-convert-global-functoid start cont form))
			 (functional
			  (ir1-convert-local-combination start
							 cont
							 form
							 lexical-def))
			 (global-var
			  (ir1-convert-srctran start cont lexical-def form))
			 (t
			  (aver (and (consp lexical-def)
				     (eq (car lexical-def) 'macro)))
			  (ir1-convert start cont
				       (careful-expand-macro (cdr lexical-def)
							     form))))))
		    ((or (atom opname) (not (eq (car opname) 'lambda)))
		     (compiler-error "illegal function call"))
		    (t
		     ;; implicitly (LAMBDA ..) because the LAMBDA
		     ;; expression is the CAR of an executed form
		     (ir1-convert-combination start
					      cont
					      form
					      (ir1-convert-lambda
					       opname
					       :debug-name (debug-namify
							    "LAMBDA CAR ~S"
							    opname)))))))))
    (values))

  ;; Generate a reference to a manifest constant, creating a new leaf
  ;; if necessary. If we are producing a fasl file, make sure that
  ;; MAKE-LOAD-FORM gets used on any parts of the constant that it
  ;; needs to be.
  (defun reference-constant (start cont value)
    (declare (type continuation start cont)
	     (inline find-constant))
    (ir1-error-bailout
     (start cont value '(error "attempt to reference undumpable constant"))
     (when (producing-fasl-file)
       (maybe-emit-make-load-forms value))
     (let* ((leaf (find-constant value))
	    (res (make-ref (leaf-type leaf) leaf)))
       (push res (leaf-refs leaf))
       (link-node-to-previous-continuation res start)
       (use-continuation res cont)))
    (values)))

;;; Add FUNCTIONAL to the COMPONENT-REANALYZE-FUNCTIONALS, unless it's
;;; some trivial type for which reanalysis is a trivial no-op, or
;;; unless it doesn't belong in this component at all.
;;;
;;; FUNCTIONAL is returned.
(defun maybe-reanalyze-functional (functional)

  (aver (not (eql (functional-kind functional) :deleted))) ; bug 148
  (aver-live-component *current-component*)

  ;; When FUNCTIONAL is of a type for which reanalysis isn't a trivial
  ;; no-op
  (when (typep functional '(or optional-dispatch clambda))

    ;; When FUNCTIONAL knows its component
    (when (lambda-p functional) 
      (aver (eql (lambda-component functional) *current-component*)))

    (pushnew functional
	     (component-reanalyze-functionals *current-component*)))

  functional)

;;; Generate a REF node for LEAF, frobbing the LEAF structure as
;;; needed. If LEAF represents a defined function which has already
;;; been converted, and is not :NOTINLINE, then reference the
;;; functional instead.
(defun reference-leaf (start cont leaf)
  (declare (type continuation start cont) (type leaf leaf))
  (with-continuation-type-assertion
      (cont (or (lexenv-find leaf type-restrictions) *wild-type*)
            "in DECLARE")
    (let* ((leaf (or (and (defined-fun-p leaf)
                          (not (eq (defined-fun-inlinep leaf)
                                   :notinline))
                          (let ((functional (defined-fun-functional leaf)))
                            (when (and functional
                                       (not (functional-kind functional)))
                              (maybe-reanalyze-functional functional))))
                     leaf))
           (res (make-ref (leaf-type leaf)
                          leaf)))
      (push res (leaf-refs leaf))
      (setf (leaf-ever-used leaf) t)
      (link-node-to-previous-continuation res start)
      (use-continuation res cont))))

;;; Convert a reference to a symbolic constant or variable. If the
;;; symbol is entered in the LEXENV-VARS we use that definition,
;;; otherwise we find the current global definition. This is also
;;; where we pick off symbol macro and alien variable references.
(defun ir1-convert-var (start cont name)
  (declare (type continuation start cont) (symbol name))
  (let ((var (or (lexenv-find name vars) (find-free-var name))))
    (etypecase var
      (leaf
       (when (lambda-var-p var)
	 (let ((home (continuation-home-lambda-or-null start)))
	   (when home
	     (pushnew var (lambda-calls-or-closes home))))
	 (when (lambda-var-ignorep var)
	   ;; (ANSI's specification for the IGNORE declaration requires
	   ;; that this be a STYLE-WARNING, not a full WARNING.)
	   (compiler-style-warn "reading an ignored variable: ~S" name)))
       (reference-leaf start cont var))
      (cons
       (aver (eq (car var) 'MACRO))
       (ir1-convert start cont (cdr var)))
      (heap-alien-info
       (ir1-convert start cont `(%heap-alien ',var)))))
  (values))

;;; Convert anything that looks like a special form, global function
;;; or compiler-macro call.
(defun ir1-convert-global-functoid (start cont form)
  (declare (type continuation start cont) (list form))
  (let* ((fun-name (first form))
	 (translator (info :function :ir1-convert fun-name))
	 (cmacro-fun (sb!xc:compiler-macro-function fun-name *lexenv*)))
    (cond (translator
	   (when cmacro-fun
	     (compiler-warn "ignoring compiler macro for special form"))
	   (funcall translator start cont form))
	  ((and cmacro-fun
		;; gotcha: If you look up the DEFINE-COMPILER-MACRO
		;; macro in the ANSI spec, you might think that
		;; suppressing compiler-macro expansion when NOTINLINE
		;; is some pre-ANSI hack. However, if you look up the
		;; NOTINLINE declaration, you'll find that ANSI
		;; requires this behavior after all.
		(not (eq (info :function :inlinep fun-name) :notinline)))
	   (let ((res (careful-expand-macro cmacro-fun form)))
	     (if (eq res form)
		 (ir1-convert-global-functoid-no-cmacro
		  start cont form fun-name)
		 (ir1-convert start cont res))))
	  (t
	   (ir1-convert-global-functoid-no-cmacro start cont form fun-name)))))

;;; Handle the case of where the call was not a compiler macro, or was
;;; a compiler macro and passed.
(defun ir1-convert-global-functoid-no-cmacro (start cont form fun)
  (declare (type continuation start cont) (list form))
  ;; FIXME: Couldn't all the INFO calls here be converted into
  ;; standard CL functions, like MACRO-FUNCTION or something?
  ;; And what happens with lexically-defined (MACROLET) macros
  ;; here, anyway?
  (ecase (info :function :kind fun)
    (:macro
     (ir1-convert start
		  cont
		  (careful-expand-macro (info :function :macro-function fun)
					form)))
    ((nil :function)
     (ir1-convert-srctran start
			  cont
			  (find-free-fun fun "shouldn't happen! (no-cmacro)")
			  form))))

(defun muffle-warning-or-die ()
  (muffle-warning)
  (bug "no MUFFLE-WARNING restart"))

;;; Expand FORM using the macro whose MACRO-FUNCTION is FUN, trapping
;;; errors which occur during the macroexpansion.
(defun careful-expand-macro (fun form)
  (let (;; a hint I (WHN) wish I'd known earlier
	(hint "(hint: For more precise location, try *BREAK-ON-SIGNALS*.)"))
    (flet (;; Return a string to use as a prefix in error reporting,
	   ;; telling something about which form caused the problem.
	   (wherestring ()
	     (let ((*print-pretty* nil)
		   ;; We rely on the printer to abbreviate FORM. 
		   (*print-length* 3)
		   (*print-level* 1))
	       (format
		nil
		#-sb-xc-host "(in macroexpansion of ~S)"
		;; longer message to avoid ambiguity "Was it the xc host
		;; or the cross-compiler which encountered the problem?"
		#+sb-xc-host "(in cross-compiler macroexpansion of ~S)"
		form))))
      (handler-bind ((style-warning (lambda (c)
				      (compiler-style-warn
				       "~@<~A~:@_~A~@:_~A~:>"
				       (wherestring) hint c)
				      (muffle-warning-or-die)))
		     ;; KLUDGE: CMU CL in its wisdom (version 2.4.6 for
                     ;; Debian Linux, anyway) raises a CL:WARNING
                     ;; condition (not a CL:STYLE-WARNING) for undefined
                     ;; symbols when converting interpreted functions,
                     ;; causing COMPILE-FILE to think the file has a real
                     ;; problem, causing COMPILE-FILE to return FAILURE-P
                     ;; set (not just WARNINGS-P set). Since undefined
                     ;; symbol warnings are often harmless forward
                     ;; references, and since it'd be inordinately painful
                     ;; to try to eliminate all such forward references,
                     ;; these warnings are basically unavoidable. Thus, we
                     ;; need to coerce the system to work through them,
                     ;; and this code does so, by crudely suppressing all
                     ;; warnings in cross-compilation macroexpansion. --
                     ;; WHN 19990412
                     #+(and cmu sb-xc-host)
                     (warning (lambda (c)
                                (compiler-note
                                 "~@<~A~:@_~
                                  ~A~:@_~
                                  ~@<(KLUDGE: That was a non-STYLE WARNING. ~
                                  Ordinarily that would cause compilation to ~
                                  fail. However, since we're running under ~
                                  CMU CL, and since CMU CL emits non-STYLE ~
                                  warnings for safe, hard-to-fix things (e.g. ~
                                  references to not-yet-defined functions) ~
                                  we're going to have to ignore it and ~
                                  proceed anyway. Hopefully we're not ~
                                  ignoring anything  horrible here..)~:@>~:>"
                                 (wherestring)
                                 c)
                                (muffle-warning-or-die)))
		     #-(and cmu sb-xc-host)
		     (warning (lambda (c)
				(compiler-warn "~@<~A~:@_~A~@:_~A~:>"
					       (wherestring) hint c)
				(muffle-warning-or-die)))
                     (error (lambda (c)
                              (compiler-error "~@<~A~:@_~A~@:_~A~:>"
                                              (wherestring) hint c))))
        (funcall sb!xc:*macroexpand-hook* fun form *lexenv*)))))

;;;; conversion utilities

;;; Convert a bunch of forms, discarding all the values except the
;;; last. If there aren't any forms, then translate a NIL.
(declaim (ftype (function (continuation continuation list) (values))
		ir1-convert-progn-body))
(defun ir1-convert-progn-body (start cont body)
  (if (endp body)
      (reference-constant start cont nil)
      (let ((this-start start)
	    (forms body))
	(loop
	  (let ((form (car forms)))
	    (when (endp (cdr forms))
	      (ir1-convert this-start cont form)
	      (return))
	    (let ((this-cont (make-continuation)))
	      (ir1-convert this-start this-cont form)
	      (setq this-start this-cont
		    forms (cdr forms)))))))
  (values))

;;;; converting combinations

;;; Convert a function call where the function FUN is a LEAF. FORM is
;;; the source for the call. We return the COMBINATION node so that
;;; the caller can poke at it if it wants to.
(declaim (ftype (function (continuation continuation list leaf) combination)
		ir1-convert-combination))
(defun ir1-convert-combination (start cont form fun)
  (let ((fun-cont (make-continuation)))
    (reference-leaf start fun-cont fun)
    (ir1-convert-combination-args fun-cont cont (cdr form))))

;;; Convert the arguments to a call and make the COMBINATION
;;; node. FUN-CONT is the continuation which yields the function to
;;; call. ARGS is the list of arguments for the call, which defaults
;;; to the cdr of source. We return the COMBINATION node.
(defun ir1-convert-combination-args (fun-cont cont args)
  (declare (type continuation fun-cont cont) (list args))
  (let ((node (make-combination fun-cont)))
    (setf (continuation-dest fun-cont) node)
    (assert-continuation-type fun-cont
			      (specifier-type '(or function symbol)))
    (setf (continuation-%externally-checkable-type fun-cont) nil)
    (collect ((arg-conts))
      (let ((this-start fun-cont))
	(dolist (arg args)
	  (let ((this-cont (make-continuation node)))
	    (ir1-convert this-start this-cont arg)
	    (setq this-start this-cont)
	    (arg-conts this-cont)))
	(link-node-to-previous-continuation node this-start)
	(use-continuation node cont)
	(setf (combination-args node) (arg-conts))))
    node))

;;; Convert a call to a global function. If not :NOTINLINE, then we do
;;; source transforms and try out any inline expansion. If there is no
;;; expansion, but is :INLINE, then give an efficiency note (unless a
;;; known function which will quite possibly be open-coded.) Next, we
;;; go to ok-combination conversion.
(defun ir1-convert-srctran (start cont var form)
  (declare (type continuation start cont) (type global-var var))
  (let ((inlinep (when (defined-fun-p var)
		   (defined-fun-inlinep var))))
    (if (eq inlinep :notinline)
	(ir1-convert-combination start cont form var)
	(let ((transform (info :function
			       :source-transform
			       (leaf-source-name var))))
	  (if transform
	      (multiple-value-bind (result pass) (funcall transform form)
		(if pass
		    (ir1-convert-maybe-predicate start cont form var)
		    (ir1-convert start cont result)))
	      (ir1-convert-maybe-predicate start cont form var))))))

;;; If the function has the PREDICATE attribute, and the CONT's DEST
;;; isn't an IF, then we convert (IF <form> T NIL), ensuring that a
;;; predicate always appears in a conditional context.
;;;
;;; If the function isn't a predicate, then we call
;;; IR1-CONVERT-COMBINATION-CHECKING-TYPE.
(defun ir1-convert-maybe-predicate (start cont form var)
  (declare (type continuation start cont) (list form) (type global-var var))
  (let ((info (info :function :info (leaf-source-name var))))
    (if (and info
	     (ir1-attributep (fun-info-attributes info) predicate)
	     (not (if-p (continuation-dest cont))))
	(ir1-convert start cont `(if ,form t nil))
	(ir1-convert-combination-checking-type start cont form var))))

;;; Actually really convert a global function call that we are allowed
;;; to early-bind.
;;;
;;; If we know the function type of the function, then we check the
;;; call for syntactic legality with respect to the declared function
;;; type. If it is impossible to determine whether the call is correct
;;; due to non-constant keywords, then we give up, marking the call as
;;; :FULL to inhibit further error messages. We return true when the
;;; call is legal.
;;;
;;; If the call is legal, we also propagate type assertions from the
;;; function type to the arg and result continuations. We do this now
;;; so that IR1 optimize doesn't have to redundantly do the check
;;; later so that it can do the type propagation.
(defun ir1-convert-combination-checking-type (start cont form var)
  (declare (type continuation start cont) (list form) (type leaf var))
  (let* ((node (ir1-convert-combination start cont form var))
	 (fun-cont (basic-combination-fun node))
	 (type (leaf-type var)))
    (when (validate-call-type node type t)
      (setf (continuation-%derived-type fun-cont) type)
      (setf (continuation-reoptimize fun-cont) nil)
      (setf (continuation-%type-check fun-cont) nil)))
  (values))

;;; Convert a call to a local function, or if the function has already
;;; been LET converted, then throw FUNCTIONAL to
;;; LOCALL-ALREADY-LET-CONVERTED. The THROW should only happen when we
;;; are converting inline expansions for local functions during
;;; optimization.
(defun ir1-convert-local-combination (start cont form functional)

  ;; The test here is for "when LET converted", as a translation of
  ;; the old CMU CL comments into code. Unfortunately, the old CMU CL
  ;; comments aren't specific enough to tell whether the correct
  ;; translation is FUNCTIONAL-SOMEWHAT-LETLIKE-P or
  ;; FUNCTIONAL-LETLIKE-P or what. The old CMU CL code assumed that
  ;; any non-null FUNCTIONAL-KIND meant that the function "had been
  ;; LET converted", which might even be right, but seems fragile, so
  ;; we try to be pickier.
  (when (or
	 ;; looks LET-converted
	 (functional-somewhat-letlike-p functional)
	 ;; It's possible for a LET-converted function to end up
	 ;; deleted later. In that case, for the purposes of this
	 ;; analysis, it is LET-converted: LET-converted functionals
	 ;; are too badly trashed to expand them inline, and deleted
	 ;; LET-converted functionals are even worse.
	 (eql (functional-kind functional) :deleted))
    (throw 'locall-already-let-converted functional))
  ;; Any other non-NIL KIND value is a case we haven't found a
  ;; justification for, and at least some such values (e.g. :EXTERNAL
  ;; and :TOPLEVEL) seem obviously wrong.
  (aver (null (functional-kind functional)))

  (ir1-convert-combination start
			   cont
			   form
			   (maybe-reanalyze-functional functional)))

;;;; PROCESS-DECLS

;;; Given a list of LAMBDA-VARs and a variable name, return the
;;; LAMBDA-VAR for that name, or NIL if it isn't found. We return the
;;; *last* variable with that name, since LET* bindings may be
;;; duplicated, and declarations always apply to the last.
(declaim (ftype (function (list symbol) (or lambda-var list))
		find-in-bindings))
(defun find-in-bindings (vars name)
  (let ((found nil))
    (dolist (var vars)
      (cond ((leaf-p var)
	     (when (eq (leaf-source-name var) name)
	       (setq found var))
	     (let ((info (lambda-var-arg-info var)))
	       (when info
		 (let ((supplied-p (arg-info-supplied-p info)))
		   (when (and supplied-p
			      (eq (leaf-source-name supplied-p) name))
		     (setq found supplied-p))))))
	    ((and (consp var) (eq (car var) name))
	     (setf found (cdr var)))))
    found))

;;; Called by PROCESS-DECLS to deal with a variable type declaration.
;;; If a LAMBDA-VAR being bound, we intersect the type with the var's
;;; type, otherwise we add a type restriction on the var. If a symbol
;;; macro, we just wrap a THE around the expansion.
(defun process-type-decl (decl res vars)
  (declare (list decl vars) (type lexenv res))
  (let ((type (compiler-specifier-type (first decl))))
    (collect ((restr nil cons)
	      (new-vars nil cons))
      (dolist (var-name (rest decl))
	(let* ((bound-var (find-in-bindings vars var-name))
	       (var (or bound-var
			(lexenv-find var-name vars)
			(find-free-var var-name))))
	  (etypecase var
	    (leaf
	     (let* ((old-type (or (lexenv-find var type-restrictions)
				  (leaf-type var)))
		    (int (if (or (fun-type-p type)
				 (fun-type-p old-type))
			     type
			     (type-approx-intersection2 old-type type))))
	       (cond ((eq int *empty-type*)
		      (unless (policy *lexenv* (= inhibit-warnings 3))
			(compiler-warn
			 "The type declarations ~S and ~S for ~S conflict."
			 (type-specifier old-type) (type-specifier type)
			 var-name)))
		     (bound-var (setf (leaf-type bound-var) int))
		     (t
		      (restr (cons var int))))))
	    (cons
	     ;; FIXME: non-ANSI weirdness
	     (aver (eq (car var) 'MACRO))
	     (new-vars `(,var-name . (MACRO . (the ,(first decl)
						   ,(cdr var))))))
	    (heap-alien-info
	     (compiler-error
	      "~S is an alien variable, so its type can't be declared."
	      var-name)))))

      (if (or (restr) (new-vars))
	  (make-lexenv :default res
		       :type-restrictions (restr)
		       :vars (new-vars))
	  res))))

;;; This is somewhat similar to PROCESS-TYPE-DECL, but handles
;;; declarations for function variables. In addition to allowing
;;; declarations for functions being bound, we must also deal with
;;; declarations that constrain the type of lexically apparent
;;; functions.
(defun process-ftype-decl (spec res names fvars)
  (declare (type type-specifier spec)
           (type list names fvars)
           (type lexenv res))
  (let ((type (compiler-specifier-type spec)))
    (collect ((res nil cons))
      (dolist (name names)
	(let ((found (find name fvars
			   :key #'leaf-source-name
			   :test #'equal)))
	  (cond
	   (found
	    (setf (leaf-type found) type)
	    (assert-definition-type found type
				    :unwinnage-fun #'compiler-note
				    :where "FTYPE declaration"))
	   (t
	    (res (cons (find-lexically-apparent-fun
			name "in a function type declaration")
		       type))))))
      (if (res)
	  (make-lexenv :default res :type-restrictions (res))
	  res))))

;;; Process a special declaration, returning a new LEXENV. A non-bound
;;; special declaration is instantiated by throwing a special variable
;;; into the variables.
(defun process-special-decl (spec res vars)
  (declare (list spec vars) (type lexenv res))
  (collect ((new-venv nil cons))
    (dolist (name (cdr spec))
      (let ((var (find-in-bindings vars name)))
	(etypecase var
	  (cons
	   (aver (eq (car var) 'MACRO))
	   (compiler-error
	    "~S is a symbol-macro and thus can't be declared special."
	    name))
	  (lambda-var
	   (when (lambda-var-ignorep var)
	     ;; ANSI's definition for "Declaration IGNORE, IGNORABLE"
	     ;; requires that this be a STYLE-WARNING, not a full WARNING.
	     (compiler-style-warn
	      "The ignored variable ~S is being declared special."
	      name))
	   (setf (lambda-var-specvar var)
		 (specvar-for-binding name)))
	  (null
	   (unless (assoc name (new-venv) :test #'eq)
	     (new-venv (cons name (specvar-for-binding name))))))))
    (if (new-venv)
	(make-lexenv :default res :vars (new-venv))
	res)))

;;; Return a DEFINED-FUN which copies a GLOBAL-VAR but for its INLINEP.
(defun make-new-inlinep (var inlinep)
  (declare (type global-var var) (type inlinep inlinep))
  (let ((res (make-defined-fun
	      :%source-name (leaf-source-name var)
	      :where-from (leaf-where-from var)
	      :type (leaf-type var)
	      :inlinep inlinep)))
    (when (defined-fun-p var)
      (setf (defined-fun-inline-expansion res)
	    (defined-fun-inline-expansion var))
      (setf (defined-fun-functional res)
	    (defined-fun-functional var)))
    res))

;;; Parse an inline/notinline declaration. If it's a local function we're
;;; defining, set its INLINEP. If a global function, add a new FENV entry.
(defun process-inline-decl (spec res fvars)
  (let ((sense (cdr (assoc (first spec) *inlinep-translations* :test #'eq)))
	(new-fenv ()))
    (dolist (name (rest spec))
      (let ((fvar (find name fvars
			:key #'leaf-source-name
			:test #'equal)))
	(if fvar
	    (setf (functional-inlinep fvar) sense)
	    (let ((found
		   (find-lexically-apparent-fun
		    name "in an inline or notinline declaration")))
	      (etypecase found
		(functional
		 (when (policy *lexenv* (>= speed inhibit-warnings))
		   (compiler-note "ignoring ~A declaration not at ~
				   definition of local function:~%  ~S"
				  sense name)))
		(global-var
		 (push (cons name (make-new-inlinep found sense))
		       new-fenv)))))))

    (if new-fenv
	(make-lexenv :default res :funs new-fenv)
	res)))

;;; like FIND-IN-BINDINGS, but looks for #'FOO in the FVARS
(defun find-in-bindings-or-fbindings (name vars fvars)
  (declare (list vars fvars))
  (if (consp name)
      (destructuring-bind (wot fn-name) name
	(unless (eq wot 'function)
	  (compiler-error "The function or variable name ~S is unrecognizable."
			  name))
	(find fn-name fvars :key #'leaf-source-name :test #'equal))
      (find-in-bindings vars name)))

;;; Process an ignore/ignorable declaration, checking for various losing
;;; conditions.
(defun process-ignore-decl (spec vars fvars)
  (declare (list spec vars fvars))
  (dolist (name (rest spec))
    (let ((var (find-in-bindings-or-fbindings name vars fvars)))
      (cond
       ((not var)
	;; ANSI's definition for "Declaration IGNORE, IGNORABLE"
	;; requires that this be a STYLE-WARNING, not a full WARNING.
	(compiler-style-warn "declaring unknown variable ~S to be ignored"
			     name))
       ;; FIXME: This special case looks like non-ANSI weirdness.
       ((and (consp var) (consp (cdr var)) (eq (cadr var) 'macro))
	;; Just ignore the IGNORE decl.
	)
       ((functional-p var)
	(setf (leaf-ever-used var) t))
       ((and (lambda-var-specvar var) (eq (first spec) 'ignore))
	;; ANSI's definition for "Declaration IGNORE, IGNORABLE"
	;; requires that this be a STYLE-WARNING, not a full WARNING.
	(compiler-style-warn "declaring special variable ~S to be ignored"
			     name))
       ((eq (first spec) 'ignorable)
	(setf (leaf-ever-used var) t))
       (t
	(setf (lambda-var-ignorep var) t)))))
  (values))

;;; FIXME: This is non-ANSI, so the default should be T, or it should
;;; go away, I think.
(defvar *suppress-values-declaration* nil
  #!+sb-doc
  "If true, processing of the VALUES declaration is inhibited.")

;;; Process a single declaration spec, augmenting the specified LEXENV
;;; RES and returning it as a result. VARS and FVARS are as described in
;;; PROCESS-DECLS.
(defun process-1-decl (raw-spec res vars fvars cont)
  (declare (type list raw-spec vars fvars))
  (declare (type lexenv res))
  (declare (type continuation cont))
  (let ((spec (canonized-decl-spec raw-spec)))
    (case (first spec)
      (special (process-special-decl spec res vars))
      (ftype
       (unless (cdr spec)
	 (compiler-error "no type specified in FTYPE declaration: ~S" spec))
       (process-ftype-decl (second spec) res (cddr spec) fvars))
      ((inline notinline maybe-inline)
       (process-inline-decl spec res fvars))
      ((ignore ignorable)
       (process-ignore-decl spec vars fvars)
       res)
      (optimize
       (make-lexenv
	:default res
	:policy (process-optimize-decl spec (lexenv-policy res))))
      (type
       (process-type-decl (cdr spec) res vars))
      (values
       (if *suppress-values-declaration*
	   res
	   (let ((types (cdr spec)))
	     (ir1ize-the-or-values (if (eql (length types) 1)
				       (car types)
				       `(values ,@types))
				   cont
				   res
				   "in VALUES declaration"))))
      (dynamic-extent
       (when (policy *lexenv* (> speed inhibit-warnings))
	 (compiler-note
	  "compiler limitation: ~
        ~%  There's no special support for DYNAMIC-EXTENT (so it's ignored)."))
       res)
      (t
       (unless (info :declaration :recognized (first spec))
	 (compiler-warn "unrecognized declaration ~S" raw-spec))
       res))))

;;; Use a list of DECLARE forms to annotate the lists of LAMBDA-VAR
;;; and FUNCTIONAL structures which are being bound. In addition to
;;; filling in slots in the leaf structures, we return a new LEXENV
;;; which reflects pervasive special and function type declarations,
;;; (NOT)INLINE declarations and OPTIMIZE declarations. CONT is the
;;; continuation affected by VALUES declarations.
;;;
;;; This is also called in main.lisp when PROCESS-FORM handles a use
;;; of LOCALLY.
(defun process-decls (decls vars fvars cont &optional (env *lexenv*))
  (declare (list decls vars fvars) (type continuation cont))
  (dolist (decl decls)
    (dolist (spec (rest decl))
      (unless (consp spec)
	(compiler-error "malformed declaration specifier ~S in ~S" spec decl))
      (setq env (process-1-decl spec env vars fvars cont))))
  env)

;;; Return the SPECVAR for NAME to use when we see a local SPECIAL
;;; declaration. If there is a global variable of that name, then
;;; check that it isn't a constant and return it. Otherwise, create an
;;; anonymous GLOBAL-VAR.
(defun specvar-for-binding (name)
  (cond ((not (eq (info :variable :where-from name) :assumed))
	 (let ((found (find-free-var name)))
	   (when (heap-alien-info-p found)
	     (compiler-error
	      "~S is an alien variable and so can't be declared special."
	      name))
	   (unless (global-var-p found)
	     (compiler-error
	      "~S is a constant and so can't be declared special."
	      name))
	   found))
	(t
	 (make-global-var :kind :special
			  :%source-name name
			  :where-from :declared))))

;;;; LAMBDA hackery

;;;; Note: Take a look at the compiler-overview.tex section on "Hairy
;;;; function representation" before you seriously mess with this
;;;; stuff.

;;; Verify that the NAME is a legal name for a variable and return a
;;; VAR structure for it, filling in info if it is globally special.
;;; If it is losing, we punt with a COMPILER-ERROR. NAMES-SO-FAR is a
;;; list of names which have previously been bound. If the NAME is in
;;; this list, then we error out.
(declaim (ftype (function (t list) lambda-var) varify-lambda-arg))
(defun varify-lambda-arg (name names-so-far)
  (declare (inline member))
  (unless (symbolp name)
    (compiler-error "The lambda variable ~S is not a symbol." name))
  (when (member name names-so-far :test #'eq)
    (compiler-error "The variable ~S occurs more than once in the lambda list."
		    name))
  (let ((kind (info :variable :kind name)))
    (when (or (keywordp name) (eq kind :constant))
      (compiler-error "The name of the lambda variable ~S is already in use to name a constant."
		      name))
    (cond ((eq kind :special)
	   (let ((specvar (find-free-var name)))
	     (make-lambda-var :%source-name name
			      :type (leaf-type specvar)
			      :where-from (leaf-where-from specvar)
			      :specvar specvar)))
	  (t
	   (make-lambda-var :%source-name name)))))

;;; Make the default keyword for a &KEY arg, checking that the keyword
;;; isn't already used by one of the VARS.
(declaim (ftype (function (symbol list t) keyword) make-keyword-for-arg))
(defun make-keyword-for-arg (symbol vars keywordify)
  (let ((key (if (and keywordify (not (keywordp symbol)))
		 (keywordicate symbol)
		 symbol)))
    (dolist (var vars)
      (let ((info (lambda-var-arg-info var)))
	(when (and info
		   (eq (arg-info-kind info) :keyword)
		   (eq (arg-info-key info) key))
	  (compiler-error
	   "The keyword ~S appears more than once in the lambda list."
	   key))))
    key))

;;; Parse a lambda list into a list of VAR structures, stripping off
;;; any &AUX bindings. Each arg name is checked for legality, and
;;; duplicate names are checked for. If an arg is globally special,
;;; the var is marked as :SPECIAL instead of :LEXICAL. &KEY,
;;; &OPTIONAL and &REST args are annotated with an ARG-INFO structure
;;; which contains the extra information. If we hit something losing,
;;; we bug out with COMPILER-ERROR. These values are returned:
;;;  1. a list of the var structures for each top level argument;
;;;  2. a flag indicating whether &KEY was specified;
;;;  3. a flag indicating whether other &KEY args are allowed;
;;;  4. a list of the &AUX variables; and
;;;  5. a list of the &AUX values.
(declaim (ftype (function (list) (values list boolean boolean list list))
		make-lambda-vars))
(defun make-lambda-vars (list)
  (multiple-value-bind (required optional restp rest keyp keys allowp auxp aux
			morep more-context more-count)
      (parse-lambda-list list)
    (declare (ignore auxp)) ; since we just iterate over AUX regardless
    (collect ((vars)
	      (names-so-far)
	      (aux-vars)
	      (aux-vals))
      (flet (;; PARSE-DEFAULT deals with defaults and supplied-p args
	     ;; for optionals and keywords args.
	     (parse-default (spec info)
	       (when (consp (cdr spec))
		 (setf (arg-info-default info) (second spec))
		 (when (consp (cddr spec))
		   (let* ((supplied-p (third spec))
			  (supplied-var (varify-lambda-arg supplied-p
							   (names-so-far))))
		     (setf (arg-info-supplied-p info) supplied-var)
		     (names-so-far supplied-p)
		     (when (> (length (the list spec)) 3)
		       (compiler-error
			"The list ~S is too long to be an arg specifier."
			spec)))))))
	
	(dolist (name required)
	  (let ((var (varify-lambda-arg name (names-so-far))))
	    (vars var)
	    (names-so-far name)))
	
	(dolist (spec optional)
	  (if (atom spec)
	      (let ((var (varify-lambda-arg spec (names-so-far))))
		(setf (lambda-var-arg-info var)
		      (make-arg-info :kind :optional))
		(vars var)
		(names-so-far spec))
	      (let* ((name (first spec))
		     (var (varify-lambda-arg name (names-so-far)))
		     (info (make-arg-info :kind :optional)))
		(setf (lambda-var-arg-info var) info)
		(vars var)
		(names-so-far name)
		(parse-default spec info))))
	
	(when restp
	  (let ((var (varify-lambda-arg rest (names-so-far))))
	    (setf (lambda-var-arg-info var) (make-arg-info :kind :rest))
	    (vars var)
	    (names-so-far rest)))

	(when morep
	  (let ((var (varify-lambda-arg more-context (names-so-far))))
	    (setf (lambda-var-arg-info var)
		  (make-arg-info :kind :more-context))
	    (vars var)
	    (names-so-far more-context))
	  (let ((var (varify-lambda-arg more-count (names-so-far))))
	    (setf (lambda-var-arg-info var)
		  (make-arg-info :kind :more-count))
	    (vars var)
	    (names-so-far more-count)))
	
	(dolist (spec keys)
	  (cond
	   ((atom spec)
	    (let ((var (varify-lambda-arg spec (names-so-far))))
	      (setf (lambda-var-arg-info var)
		    (make-arg-info :kind :keyword
				   :key (make-keyword-for-arg spec
							      (vars)
							      t)))
	      (vars var)
	      (names-so-far spec)))
	   ((atom (first spec))
	    (let* ((name (first spec))
		   (var (varify-lambda-arg name (names-so-far)))
		   (info (make-arg-info
			  :kind :keyword
			  :key (make-keyword-for-arg name (vars) t))))
	      (setf (lambda-var-arg-info var) info)
	      (vars var)
	      (names-so-far name)
	      (parse-default spec info)))
	   (t
	    (let ((head (first spec)))
	      (unless (proper-list-of-length-p head 2)
		(error "malformed &KEY argument specifier: ~S" spec))
	      (let* ((name (second head))
		     (var (varify-lambda-arg name (names-so-far)))
		     (info (make-arg-info
			    :kind :keyword
			    :key (make-keyword-for-arg (first head)
						       (vars)
						       nil))))
		(setf (lambda-var-arg-info var) info)
		(vars var)
		(names-so-far name)
		(parse-default spec info))))))
	
	(dolist (spec aux)
	  (cond ((atom spec)
		 (let ((var (varify-lambda-arg spec nil)))
		   (aux-vars var)
		   (aux-vals nil)
		   (names-so-far spec)))
		(t
		 (unless (proper-list-of-length-p spec 1 2)
		   (compiler-error "malformed &AUX binding specifier: ~S"
				   spec))
		 (let* ((name (first spec))
			(var (varify-lambda-arg name nil)))
		   (aux-vars var)
		   (aux-vals (second spec))
		   (names-so-far name)))))

	(values (vars) keyp allowp (aux-vars) (aux-vals))))))

;;; This is similar to IR1-CONVERT-PROGN-BODY except that we
;;; sequentially bind each AUX-VAR to the corresponding AUX-VAL before
;;; converting the body. If there are no bindings, just convert the
;;; body, otherwise do one binding and recurse on the rest.
;;;
;;; FIXME: This could and probably should be converted to use
;;; SOURCE-NAME and DEBUG-NAME. But I (WHN) don't use &AUX bindings,
;;; so I'm not motivated. Patches will be accepted...
(defun ir1-convert-aux-bindings (start cont body aux-vars aux-vals)
  (declare (type continuation start cont) (list body aux-vars aux-vals))
  (if (null aux-vars)
      (ir1-convert-progn-body start cont body)
      (let ((fun-cont (make-continuation))
	    (fun (ir1-convert-lambda-body body
					  (list (first aux-vars))
					  :aux-vars (rest aux-vars)
					  :aux-vals (rest aux-vals)
					  :debug-name (debug-namify
						       "&AUX bindings ~S"
						       aux-vars))))
	(reference-leaf start fun-cont fun)
	(ir1-convert-combination-args fun-cont cont
				      (list (first aux-vals)))))
  (values))

;;; This is similar to IR1-CONVERT-PROGN-BODY except that code to bind
;;; the SPECVAR for each SVAR to the value of the variable is wrapped
;;; around the body. If there are no special bindings, we just convert
;;; the body, otherwise we do one special binding and recurse on the
;;; rest.
;;;
;;; We make a cleanup and introduce it into the lexical environment.
;;; If there are multiple special bindings, the cleanup for the blocks
;;; will end up being the innermost one. We force CONT to start a
;;; block outside of this cleanup, causing cleanup code to be emitted
;;; when the scope is exited.
(defun ir1-convert-special-bindings (start cont body aux-vars aux-vals svars)
  (declare (type continuation start cont)
	   (list body aux-vars aux-vals svars))
  (cond
   ((null svars)
    (ir1-convert-aux-bindings start cont body aux-vars aux-vals))
   (t
    (continuation-starts-block cont)
    (let ((cleanup (make-cleanup :kind :special-bind))
	  (var (first svars))
	  (next-cont (make-continuation))
	  (nnext-cont (make-continuation)))
      (ir1-convert start next-cont
		   `(%special-bind ',(lambda-var-specvar var) ,var))
      (setf (cleanup-mess-up cleanup) (continuation-use next-cont))
      (let ((*lexenv* (make-lexenv :cleanup cleanup)))
	(ir1-convert next-cont nnext-cont '(%cleanup-point))
	(ir1-convert-special-bindings nnext-cont cont body aux-vars aux-vals
				      (rest svars))))))
  (values))

;;; Create a lambda node out of some code, returning the result. The
;;; bindings are specified by the list of VAR structures VARS. We deal
;;; with adding the names to the LEXENV-VARS for the conversion. The
;;; result is added to the NEW-FUNCTIONALS in the *CURRENT-COMPONENT*
;;; and linked to the component head and tail.
;;;
;;; We detect special bindings here, replacing the original VAR in the
;;; lambda list with a temporary variable. We then pass a list of the
;;; special vars to IR1-CONVERT-SPECIAL-BINDINGS, which actually emits
;;; the special binding code.
;;;
;;; We ignore any ARG-INFO in the VARS, trusting that someone else is
;;; dealing with &nonsense.
;;;
;;; AUX-VARS is a list of VAR structures for variables that are to be
;;; sequentially bound. Each AUX-VAL is a form that is to be evaluated
;;; to get the initial value for the corresponding AUX-VAR. 
(defun ir1-convert-lambda-body (body
				vars
				&key
				aux-vars
				aux-vals
				result
				(source-name '.anonymous.)
				debug-name)
  (declare (list body vars aux-vars aux-vals)
	   (type (or continuation null) result))

  ;; We're about to try to put new blocks into *CURRENT-COMPONENT*.
  (aver-live-component *current-component*)

  (let* ((bind (make-bind))
	 (lambda (make-lambda :vars vars
			      :bind bind
			      :%source-name source-name
			      :%debug-name debug-name))
	 (result (or result (make-continuation))))

    ;; just to check: This function should fail internal assertions if
    ;; we didn't set up a valid debug name above.
    ;;
    ;; (In SBCL we try to make everything have a debug name, since we
    ;; lack the omniscient perspective the original implementors used
    ;; to decide which things didn't need one.)
    (functional-debug-name lambda)

    (setf (lambda-home lambda) lambda)
    (collect ((svars)
	      (new-venv nil cons))

      (dolist (var vars)
	;; As far as I can see, LAMBDA-VAR-HOME should never have
	;; been set before. Let's make sure. -- WHN 2001-09-29
	(aver (null (lambda-var-home var)))
	(setf (lambda-var-home var) lambda)
	(let ((specvar (lambda-var-specvar var)))
	  (cond (specvar
		 (svars var)
		 (new-venv (cons (leaf-source-name specvar) specvar)))
		(t
		 (note-lexical-binding (leaf-source-name var))
		 (new-venv (cons (leaf-source-name var) var))))))

      (let ((*lexenv* (make-lexenv :vars (new-venv)
				   :lambda lambda
				   :cleanup nil)))
	(setf (bind-lambda bind) lambda)
	(setf (node-lexenv bind) *lexenv*)
	
	(let ((cont1 (make-continuation))
	      (cont2 (make-continuation)))
	  (continuation-starts-block cont1)
	  (link-node-to-previous-continuation bind cont1)
	  (use-continuation bind cont2)
	  (ir1-convert-special-bindings cont2 result body
					aux-vars aux-vals (svars)))

	(let ((block (continuation-block result)))
	  (when block
	    (let ((return (make-return :result result :lambda lambda))
		  (tail-set (make-tail-set :funs (list lambda)))
		  (dummy (make-continuation)))
	      (setf (lambda-tail-set lambda) tail-set)
	      (setf (lambda-return lambda) return)
	      (setf (continuation-dest result) return)
              (setf (continuation-%externally-checkable-type result) nil)
	      (setf (block-last block) return)
	      (link-node-to-previous-continuation return result)
	      (use-continuation return dummy))
	    (link-blocks block (component-tail *current-component*))))))

    (link-blocks (component-head *current-component*) (node-block bind))
    (push lambda (component-new-functionals *current-component*))

    lambda))

;;; Create the actual entry-point function for an optional entry
;;; point. The lambda binds copies of each of the VARS, then calls FUN
;;; with the argument VALS and the DEFAULTS. Presumably the VALS refer
;;; to the VARS by name. The VALS are passed in in reverse order.
;;;
;;; If any of the copies of the vars are referenced more than once,
;;; then we mark the corresponding var as EVER-USED to inhibit
;;; "defined but not read" warnings for arguments that are only used
;;; by default forms.
(defun convert-optional-entry (fun vars vals defaults)
  (declare (type clambda fun) (list vars vals defaults))
  (let* ((fvars (reverse vars))
	 (arg-vars (mapcar (lambda (var)
			     (unless (lambda-var-specvar var)
			       (note-lexical-binding (leaf-source-name var)))
			     (make-lambda-var
			      :%source-name (leaf-source-name var)
			      :type (leaf-type var)
			      :where-from (leaf-where-from var)
			      :specvar (lambda-var-specvar var)))
			   fvars))
	 (fun (ir1-convert-lambda-body `((%funcall ,fun
						   ,@(reverse vals)
						   ,@defaults))
				       arg-vars
				       :debug-name "&OPTIONAL processor")))
    (mapc (lambda (var arg-var)
	    (when (cdr (leaf-refs arg-var))
	      (setf (leaf-ever-used var) t)))
	  fvars arg-vars)
    fun))

;;; This function deals with supplied-p vars in optional arguments. If
;;; the there is no supplied-p arg, then we just call
;;; IR1-CONVERT-HAIRY-ARGS on the remaining arguments, and generate a
;;; optional entry that calls the result. If there is a supplied-p
;;; var, then we add it into the default vars and throw a T into the
;;; entry values. The resulting entry point function is returned.
(defun generate-optional-default-entry (res default-vars default-vals
					    entry-vars entry-vals
					    vars supplied-p-p body
					    aux-vars aux-vals cont
					    source-name debug-name)
  (declare (type optional-dispatch res)
	   (list default-vars default-vals entry-vars entry-vals vars body
		 aux-vars aux-vals)
	   (type (or continuation null) cont))
  (let* ((arg (first vars))
	 (arg-name (leaf-source-name arg))
	 (info (lambda-var-arg-info arg))
	 (supplied-p (arg-info-supplied-p info))
	 (ep (if supplied-p
		 (ir1-convert-hairy-args
		  res
		  (list* supplied-p arg default-vars)
		  (list* (leaf-source-name supplied-p) arg-name default-vals)
		  (cons arg entry-vars)
		  (list* t arg-name entry-vals)
		  (rest vars) t body aux-vars aux-vals cont
		  source-name debug-name)
		 (ir1-convert-hairy-args
		  res
		  (cons arg default-vars)
		  (cons arg-name default-vals)
		  (cons arg entry-vars)
		  (cons arg-name entry-vals)
		  (rest vars) supplied-p-p body aux-vars aux-vals cont
		  source-name debug-name))))

    (convert-optional-entry ep default-vars default-vals
			    (if supplied-p
				(list (arg-info-default info) nil)
				(list (arg-info-default info))))))

;;; Create the MORE-ENTRY function for the OPTIONAL-DISPATCH RES.
;;; ENTRY-VARS and ENTRY-VALS describe the fixed arguments. REST is
;;; the var for any &REST arg. KEYS is a list of the &KEY arg vars.
;;;
;;; The most interesting thing that we do is parse keywords. We create
;;; a bunch of temporary variables to hold the result of the parse,
;;; and then loop over the supplied arguments, setting the appropriate
;;; temps for the supplied keyword. Note that it is significant that
;;; we iterate over the keywords in reverse order --- this implements
;;; the CL requirement that (when a keyword appears more than once)
;;; the first value is used.
;;;
;;; If there is no supplied-p var, then we initialize the temp to the
;;; default and just pass the temp into the main entry. Since
;;; non-constant &KEY args are forcibly given a supplied-p var, we
;;; know that the default is constant, and thus safe to evaluate out
;;; of order.
;;;
;;; If there is a supplied-p var, then we create temps for both the
;;; value and the supplied-p, and pass them into the main entry,
;;; letting it worry about defaulting.
;;;
;;; We deal with :ALLOW-OTHER-KEYS by delaying unknown keyword errors
;;; until we have scanned all the keywords.
(defun convert-more-entry (res entry-vars entry-vals rest morep keys)
  (declare (type optional-dispatch res) (list entry-vars entry-vals keys))
  (collect ((arg-vars)
	    (arg-vals (reverse entry-vals))
	    (temps)
	    (body))

    (dolist (var (reverse entry-vars))
      (arg-vars (make-lambda-var :%source-name (leaf-source-name var)
				 :type (leaf-type var)
				 :where-from (leaf-where-from var))))

    (let* ((n-context (gensym "N-CONTEXT-"))
	   (context-temp (make-lambda-var :%source-name n-context))
	   (n-count (gensym "N-COUNT-"))
	   (count-temp (make-lambda-var :%source-name n-count
					:type (specifier-type 'index))))

      (arg-vars context-temp count-temp)

      (when rest
	(arg-vals `(%listify-rest-args ,n-context ,n-count)))
      (when morep
	(arg-vals n-context)
	(arg-vals n-count))

      (when (optional-dispatch-keyp res)
	(let ((n-index (gensym "N-INDEX-"))
	      (n-key (gensym "N-KEY-"))
	      (n-value-temp (gensym "N-VALUE-TEMP-"))
	      (n-allowp (gensym "N-ALLOWP-"))
	      (n-losep (gensym "N-LOSEP-"))
	      (allowp (or (optional-dispatch-allowp res)
			  (policy *lexenv* (zerop safety))))
              (found-allow-p nil))

	  (temps `(,n-index (1- ,n-count)) n-key n-value-temp)
	  (body `(declare (fixnum ,n-index) (ignorable ,n-key ,n-value-temp)))

	  (collect ((tests))
	    (dolist (key keys)
	      (let* ((info (lambda-var-arg-info key))
		     (default (arg-info-default info))
		     (keyword (arg-info-key info))
		     (supplied-p (arg-info-supplied-p info))
		     (n-value (gensym "N-VALUE-"))
                     (clause (cond (supplied-p
                                    (let ((n-supplied (gensym "N-SUPPLIED-")))
                                      (temps n-supplied)
                                      (arg-vals n-value n-supplied)
                                      `((eq ,n-key ',keyword)
                                        (setq ,n-supplied t)
                                        (setq ,n-value ,n-value-temp))))
                                   (t
                                    (arg-vals n-value)
                                    `((eq ,n-key ',keyword)
                                      (setq ,n-value ,n-value-temp))))))
		(when (and (not allowp) (eq keyword :allow-other-keys))
                  (setq found-allow-p t)
                  (setq clause (append clause `((setq ,n-allowp ,n-value-temp)))))

                (temps `(,n-value ,default))
		(tests clause)))

	    (unless allowp
	      (temps n-allowp n-losep)
              (unless found-allow-p
                (tests `((eq ,n-key :allow-other-keys)
                         (setq ,n-allowp ,n-value-temp))))
	      (tests `(t
		       (setq ,n-losep ,n-key))))

	    (body
	     `(when (oddp ,n-count)
		(%odd-key-args-error)))

	    (body
	     `(locally
		(declare (optimize (safety 0)))
		(loop
		  (when (minusp ,n-index) (return))
		  (setf ,n-value-temp (%more-arg ,n-context ,n-index))
		  (decf ,n-index)
		  (setq ,n-key (%more-arg ,n-context ,n-index))
		  (decf ,n-index)
		  (cond ,@(tests)))))

	    (unless allowp
	      (body `(when (and ,n-losep (not ,n-allowp))
		       (%unknown-key-arg-error ,n-losep)))))))

      (let ((ep (ir1-convert-lambda-body
		 `((let ,(temps)
		     ,@(body)
		     (%funcall ,(optional-dispatch-main-entry res)
			       . ,(arg-vals)))) ; FIXME: What is the '.'? ,@?
		 (arg-vars)
		 :debug-name (debug-namify "~S processing" '&more))))
	(setf (optional-dispatch-more-entry res) ep))))

  (values))

;;; This is called by IR1-CONVERT-HAIRY-ARGS when we run into a &REST
;;; or &KEY arg. The arguments are similar to that function, but we
;;; split off any &REST arg and pass it in separately. REST is the
;;; &REST arg var, or NIL if there is no &REST arg. KEYS is a list of
;;; the &KEY argument vars.
;;;
;;; When there are &KEY arguments, we introduce temporary gensym
;;; variables to hold the values while keyword defaulting is in
;;; progress to get the required sequential binding semantics.
;;;
;;; This gets interesting mainly when there are &KEY arguments with
;;; supplied-p vars or non-constant defaults. In either case, pass in
;;; a supplied-p var. If the default is non-constant, we introduce an
;;; IF in the main entry that tests the supplied-p var and decides
;;; whether to evaluate the default or not. In this case, the real
;;; incoming value is NIL, so we must union NULL with the declared
;;; type when computing the type for the main entry's argument.
(defun ir1-convert-more (res default-vars default-vals entry-vars entry-vals
			     rest more-context more-count keys supplied-p-p
			     body aux-vars aux-vals cont
			     source-name debug-name)
  (declare (type optional-dispatch res)
	   (list default-vars default-vals entry-vars entry-vals keys body
		 aux-vars aux-vals)
	   (type (or continuation null) cont))
  (collect ((main-vars (reverse default-vars))
	    (main-vals default-vals cons)
	    (bind-vars)
	    (bind-vals))
    (when rest
      (main-vars rest)
      (main-vals '()))
    (when more-context
      (main-vars more-context)
      (main-vals nil)
      (main-vars more-count)
      (main-vals 0))

    (dolist (key keys)
      (let* ((info (lambda-var-arg-info key))
	     (default (arg-info-default info))
	     (hairy-default (not (sb!xc:constantp default)))
	     (supplied-p (arg-info-supplied-p info))
	     (n-val (make-symbol (format nil
					 "~A-DEFAULTING-TEMP"
					 (leaf-source-name key))))
	     (key-type (leaf-type key))
	     (val-temp (make-lambda-var
			:%source-name n-val
			:type (if hairy-default
				  (type-union key-type (specifier-type 'null))
				  key-type))))
	(main-vars val-temp)
	(bind-vars key)
	(cond ((or hairy-default supplied-p)
	       (let* ((n-supplied (gensym "N-SUPPLIED-"))
		      (supplied-temp (make-lambda-var
				      :%source-name n-supplied)))
		 (unless supplied-p
		   (setf (arg-info-supplied-p info) supplied-temp))
		 (when hairy-default
		   (setf (arg-info-default info) nil))
		 (main-vars supplied-temp)
		 (cond (hairy-default
			(main-vals nil nil)
			(bind-vals `(if ,n-supplied ,n-val ,default)))
		       (t
			(main-vals default nil)
			(bind-vals n-val)))
		 (when supplied-p
		   (bind-vars supplied-p)
		   (bind-vals n-supplied))))
	      (t
	       (main-vals (arg-info-default info))
	       (bind-vals n-val)))))

    (let* ((main-entry (ir1-convert-lambda-body
			body (main-vars)
			:aux-vars (append (bind-vars) aux-vars)
			:aux-vals (append (bind-vals) aux-vals)
			:result cont
			:debug-name (debug-namify "varargs entry for ~A"
						  (as-debug-name source-name
								 debug-name))))
	   (last-entry (convert-optional-entry main-entry default-vars
					       (main-vals) ())))
      (setf (optional-dispatch-main-entry res) main-entry)
      (convert-more-entry res entry-vars entry-vals rest more-context keys)

      (push (if supplied-p-p
		(convert-optional-entry last-entry entry-vars entry-vals ())
		last-entry)
	    (optional-dispatch-entry-points res))
      last-entry)))

;;; This function generates the entry point functions for the
;;; OPTIONAL-DISPATCH RES. We accomplish this by recursion on the list
;;; of arguments, analyzing the arglist on the way down and generating
;;; entry points on the way up.
;;;
;;; DEFAULT-VARS is a reversed list of all the argument vars processed
;;; so far, including supplied-p vars. DEFAULT-VALS is a list of the
;;; names of the DEFAULT-VARS.
;;;
;;; ENTRY-VARS is a reversed list of processed argument vars,
;;; excluding supplied-p vars. ENTRY-VALS is a list things that can be
;;; evaluated to get the values for all the vars from the ENTRY-VARS.
;;; It has the var name for each required or optional arg, and has T
;;; for each supplied-p arg.
;;;
;;; VARS is a list of the LAMBDA-VAR structures for arguments that
;;; haven't been processed yet. SUPPLIED-P-P is true if a supplied-p
;;; argument has already been processed; only in this case are the
;;; DEFAULT-XXX and ENTRY-XXX different.
;;;
;;; The result at each point is a lambda which should be called by the
;;; above level to default the remaining arguments and evaluate the
;;; body. We cause the body to be evaluated by converting it and
;;; returning it as the result when the recursion bottoms out.
;;;
;;; Each level in the recursion also adds its entry point function to
;;; the result OPTIONAL-DISPATCH. For most arguments, the defaulting
;;; function and the entry point function will be the same, but when
;;; SUPPLIED-P args are present they may be different.
;;;
;;; When we run into a &REST or &KEY arg, we punt out to
;;; IR1-CONVERT-MORE, which finishes for us in this case.
(defun ir1-convert-hairy-args (res default-vars default-vals
                                   entry-vars entry-vals
                                   vars supplied-p-p body aux-vars
                                   aux-vals cont
				   source-name debug-name)
  (declare (type optional-dispatch res)
           (list default-vars default-vals entry-vars entry-vals vars body
                 aux-vars aux-vals)
           (type (or continuation null) cont))
  (cond ((not vars)
         (if (optional-dispatch-keyp res)
             ;; Handle &KEY with no keys...
             (ir1-convert-more res default-vars default-vals
                               entry-vars entry-vals
                               nil nil nil vars supplied-p-p body aux-vars
                               aux-vals cont source-name debug-name)
             (let ((fun (ir1-convert-lambda-body
			 body (reverse default-vars)
			 :aux-vars aux-vars
			 :aux-vals aux-vals
			 :result cont
			 :debug-name (debug-namify
				      "hairy arg processor for ~A"
				      (as-debug-name source-name
						     debug-name)))))
               (setf (optional-dispatch-main-entry res) fun)
               (push (if supplied-p-p
                         (convert-optional-entry fun entry-vars entry-vals ())
                         fun)
                     (optional-dispatch-entry-points res))
               fun)))
        ((not (lambda-var-arg-info (first vars)))
         (let* ((arg (first vars))
                (nvars (cons arg default-vars))
                (nvals (cons (leaf-source-name arg) default-vals)))
           (ir1-convert-hairy-args res nvars nvals nvars nvals
                                   (rest vars) nil body aux-vars aux-vals
                                   cont
				   source-name debug-name)))
        (t
         (let* ((arg (first vars))
                (info (lambda-var-arg-info arg))
                (kind (arg-info-kind info)))
           (ecase kind
             (:optional
              (let ((ep (generate-optional-default-entry
                         res default-vars default-vals
                         entry-vars entry-vals vars supplied-p-p body
                         aux-vars aux-vals cont
			 source-name debug-name)))
                (push (if supplied-p-p
                          (convert-optional-entry ep entry-vars entry-vals ())
                          ep)
                      (optional-dispatch-entry-points res))
                ep))
             (:rest
              (ir1-convert-more res default-vars default-vals
                                entry-vars entry-vals
                                arg nil nil (rest vars) supplied-p-p body
                                aux-vars aux-vals cont
				source-name debug-name))
             (:more-context
              (ir1-convert-more res default-vars default-vals
                                entry-vars entry-vals
                                nil arg (second vars) (cddr vars) supplied-p-p
                                body aux-vars aux-vals cont
				source-name debug-name))
             (:keyword
              (ir1-convert-more res default-vars default-vals
                                entry-vars entry-vals
                                nil nil nil vars supplied-p-p body aux-vars
                                aux-vals cont source-name debug-name)))))))

;;; This function deals with the case where we have to make an
;;; OPTIONAL-DISPATCH to represent a LAMBDA. We cons up the result and
;;; call IR1-CONVERT-HAIRY-ARGS to do the work. When it is done, we
;;; figure out the MIN-ARGS and MAX-ARGS.
(defun ir1-convert-hairy-lambda (body vars keyp allowp aux-vars aux-vals cont
				      &key
				      (source-name '.anonymous.)
				      (debug-name (debug-namify
						   "OPTIONAL-DISPATCH ~S"
						   vars)))
  (declare (list body vars aux-vars aux-vals) (type continuation cont))
  (let ((res (make-optional-dispatch :arglist vars
				     :allowp allowp
				     :keyp keyp
				     :%source-name source-name
				     :%debug-name debug-name))
	(min (or (position-if #'lambda-var-arg-info vars) (length vars))))
    (aver-live-component *current-component*)
    (push res (component-new-functionals *current-component*))
    (ir1-convert-hairy-args res () () () () vars nil body aux-vars aux-vals
			    cont source-name debug-name)
    (setf (optional-dispatch-min-args res) min)
    (setf (optional-dispatch-max-args res)
	  (+ (1- (length (optional-dispatch-entry-points res))) min))

    (flet ((frob (ep)
	     (when ep
	       (setf (functional-kind ep) :optional)
	       (setf (leaf-ever-used ep) t)
	       (setf (lambda-optional-dispatch ep) res))))
      (dolist (ep (optional-dispatch-entry-points res)) (frob ep))
      (frob (optional-dispatch-more-entry res))
      (frob (optional-dispatch-main-entry res)))

    res))

;;; Convert a LAMBDA form into a LAMBDA leaf or an OPTIONAL-DISPATCH leaf.
(defun ir1-convert-lambda (form &key (source-name '.anonymous.) debug-name)

  (unless (consp form)
    (compiler-error "A ~S was found when expecting a lambda expression:~%  ~S"
		    (type-of form)
		    form))
  (unless (eq (car form) 'lambda)
    (compiler-error "~S was expected but ~S was found:~%  ~S"
		    'lambda
		    (car form)
		    form))
  (unless (and (consp (cdr form)) (listp (cadr form)))
    (compiler-error
     "The lambda expression has a missing or non-list lambda list:~%  ~S"
     form))

  (multiple-value-bind (vars keyp allow-other-keys aux-vars aux-vals)
      (make-lambda-vars (cadr form))
    (multiple-value-bind (forms decls) (parse-body (cddr form))
      (let* ((result-cont (make-continuation))
	     (*lexenv* (process-decls decls
				      (append aux-vars vars)
				      nil result-cont))
	     (res (if (or (find-if #'lambda-var-arg-info vars) keyp)
		      (ir1-convert-hairy-lambda forms vars keyp
						allow-other-keys
						aux-vars aux-vals result-cont
						:source-name source-name
						:debug-name debug-name)
		      (ir1-convert-lambda-body forms vars
					       :aux-vars aux-vars
					       :aux-vals aux-vals
					       :result result-cont
					       :source-name source-name
					       :debug-name debug-name))))
	(setf (functional-inline-expansion res) form)
	(setf (functional-arg-documentation res) (cadr form))
	res))))

;;;; defining global functions

;;; Convert FUN as a lambda in the null environment, but use the
;;; current compilation policy. Note that FUN may be a
;;; LAMBDA-WITH-LEXENV, so we may have to augment the environment to
;;; reflect the state at the definition site.
(defun ir1-convert-inline-lambda (fun &key
				      (source-name '.anonymous.)
				      debug-name)
  (destructuring-bind (decls macros symbol-macros &rest body)
		      (if (eq (car fun) 'lambda-with-lexenv)
			  (cdr fun)
			  `(() () () . ,(cdr fun)))
    (let ((*lexenv* (make-lexenv
		     :default (process-decls decls nil nil
					     (make-continuation)
					     (make-null-lexenv))
		     :vars (copy-list symbol-macros)
		     :funs (mapcar (lambda (x)
				     `(,(car x) .
				       (macro . ,(coerce (cdr x) 'function))))
				   macros)
		     :policy (lexenv-policy *lexenv*))))
      (ir1-convert-lambda `(lambda ,@body)
			  :source-name source-name
			  :debug-name debug-name))))

;;; Get a DEFINED-FUN object for a function we are about to define. If
;;; the function has been forward referenced, then substitute for the
;;; previous references.
(defun get-defined-fun (name)
  (proclaim-as-fun-name name)
  (let ((found (find-free-fun name "shouldn't happen! (defined-fun)")))
    (note-name-defined name :function)
    (cond ((not (defined-fun-p found))
	   (aver (not (info :function :inlinep name)))
	   (let* ((where-from (leaf-where-from found))
		  (res (make-defined-fun
			:%source-name name
			:where-from (if (eq where-from :declared)
					:declared :defined)
			:type (leaf-type found))))
	     (substitute-leaf res found)
	     (setf (gethash name *free-funs*) res)))
	  ;; If *FREE-FUNS* has a previously converted definition
	  ;; for this name, then blow it away and try again.
	  ((defined-fun-functional found)
	   (remhash name *free-funs*)
	   (get-defined-fun name))
	  (t found))))

;;; Check a new global function definition for consistency with
;;; previous declaration or definition, and assert argument/result
;;; types if appropriate. This assertion is suppressed by the
;;; EXPLICIT-CHECK attribute, which is specified on functions that
;;; check their argument types as a consequence of type dispatching.
;;; This avoids redundant checks such as NUMBERP on the args to +, etc.
(defun assert-new-definition (var fun)
  (let ((type (leaf-type var))
	(for-real (eq (leaf-where-from var) :declared))
	(info (info :function :info (leaf-source-name var))))
    (assert-definition-type
     fun type
     ;; KLUDGE: Common Lisp is such a dynamic language that in general
     ;; all we can do here in general is issue a STYLE-WARNING. It
     ;; would be nice to issue a full WARNING in the special case of
     ;; of type mismatches within a compilation unit (as in section
     ;; 3.2.2.3 of the spec) but at least as of sbcl-0.6.11, we don't
     ;; keep track of whether the mismatched data came from the same
     ;; compilation unit, so we can't do that. -- WHN 2001-02-11
     :lossage-fun #'compiler-style-warn
     :unwinnage-fun (cond (info #'compiler-style-warn)
			  (for-real #'compiler-note)
			  (t nil))
     :really-assert
     (and for-real
	  (not (and info
		    (ir1-attributep (fun-info-attributes info)
				    explicit-check))))
     :where (if for-real
		"previous declaration"
		"previous definition"))))

;;; Convert a lambda doing all the basic stuff we would do if we were
;;; converting a DEFUN. In the old CMU CL system, this was used both
;;; by the %DEFUN translator and for global inline expansion, but
;;; since sbcl-0.pre7.something %DEFUN does things differently.
;;; FIXME: And now it's probably worth rethinking whether this
;;; function is a good idea.
;;;
;;; Unless a :INLINE function, we temporarily clobber the inline
;;; expansion. This prevents recursive inline expansion of
;;; opportunistic pseudo-inlines.
(defun ir1-convert-lambda-for-defun (lambda var expansion converter)
  (declare (cons lambda) (function converter) (type defined-fun var))
  (let ((var-expansion (defined-fun-inline-expansion var)))
    (unless (eq (defined-fun-inlinep var) :inline)
      (setf (defined-fun-inline-expansion var) nil))
    (let* ((name (leaf-source-name var))
	   (fun (funcall converter lambda
			 :source-name name))
	   (fun-info (info :function :info name)))
      (setf (functional-inlinep fun) (defined-fun-inlinep var))
      (assert-new-definition var fun)
      (setf (defined-fun-inline-expansion var) var-expansion)
      ;; If definitely not an interpreter stub, then substitute for
      ;; any old references.
      (unless (or (eq (defined-fun-inlinep var) :notinline)
		  (not *block-compile*)
		  (and fun-info
		       (or (fun-info-transforms fun-info)
			   (fun-info-templates fun-info)
			   (fun-info-ir2-convert fun-info))))
	(substitute-leaf fun var)
	;; If in a simple environment, then we can allow backward
	;; references to this function from following top level forms.
	(when expansion (setf (defined-fun-functional var) fun)))
      fun)))

;;; the even-at-compile-time part of DEFUN
;;;
;;; The INLINE-EXPANSION is a LAMBDA-WITH-LEXENV, or NIL if there is
;;; no inline expansion.
(defun %compiler-defun (name lambda-with-lexenv)

  (let ((defined-fun nil)) ; will be set below if we're in the compiler
    
    (when (boundp '*lexenv*) ; when in the compiler
      (when sb!xc:*compile-print*
	(compiler-mumble "~&; recognizing DEFUN ~S~%" name))
      (remhash name *free-funs*)
      (setf defined-fun (get-defined-fun name)))

    (become-defined-fun-name name)

    (cond (lambda-with-lexenv
	   (setf (info :function :inline-expansion-designator name)
		 lambda-with-lexenv)
	   (when defined-fun 
	     (setf (defined-fun-inline-expansion defined-fun)
		   lambda-with-lexenv)))
	  (t
	   (clear-info :function :inline-expansion-designator name)))

    ;; old CMU CL comment:
    ;;   If there is a type from a previous definition, blast it,
    ;;   since it is obsolete.
    (when (and defined-fun
	       (eq (leaf-where-from defined-fun) :defined))
      (setf (leaf-type defined-fun)
	    ;; FIXME: If this is a block compilation thing, shouldn't
	    ;; we be setting the type to the full derived type for the
	    ;; definition, instead of this most general function type?
	    (specifier-type 'function))))

  (values))

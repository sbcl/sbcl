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

(defvar *fun-names-in-this-file* nil)

;;; *ALLOW-DEBUG-CATCH-TAG* controls whether we should allow the
;;; insertion a (CATCH ...) around code to allow the debugger RETURN
;;; command to function.
(defvar *allow-debug-catch-tag* t)

;;;; namespace management utilities

(defun fun-lexically-notinline-p (name)
  (let ((fun (lexenv-find name funs :test #'equal)))
    ;; a declaration will trump a proclamation
    (if (and fun (defined-fun-p fun))
	(eq (defined-fun-inlinep fun) :notinline)
	(eq (info :function :inlinep name) :notinline))))

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
    (make-global-var
     :kind :global-function
     :%source-name name
     :type (if (or *derive-function-types*
		   (eq where :declared)
		   (and (member name *fun-names-in-this-file* :test #'equal)
			(not (fun-lexically-notinline-p name))))
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
(declaim (ftype (sfunction (t string) global-var) find-free-fun))
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
		      :type (if (eq inlinep :notinline)
				(specifier-type 'function)
				(info :function :type name)))
		     (find-free-really-fun name))))))))

;;; Return the LEAF structure for the lexically apparent function
;;; definition of NAME.
(declaim (ftype (sfunction (t string) leaf) find-lexically-apparent-fun))
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
(declaim (ftype (sfunction (t) (or leaf cons heap-alien-info)) find-free-var))
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
;;; ctran which continues at it.
(defun link-node-to-previous-ctran (node ctran)
  (declare (type node node) (type ctran ctran))
  (aver (not (ctran-next ctran)))
  (setf (ctran-next ctran) node)
  (setf (node-prev node) ctran))

;;; This function is used to set the ctran for a node, and thus
;;; determine what is evaluated next. If the ctran has no block, then
;;; we make it be in the block that the node is in. If the ctran heads
;;; its block, we end our block and link it to that block.
#!-sb-fluid (declaim (inline use-ctran))
(defun use-ctran (node ctran)
  (declare (type node node) (type ctran ctran))
  (if (eq (ctran-kind ctran) :unused)
      (let ((node-block (ctran-block (node-prev node))))
        (setf (ctran-block ctran) node-block)
        (setf (ctran-kind ctran) :inside-block)
        (setf (ctran-use ctran) node)
        (setf (node-next node) ctran))
      (%use-ctran node ctran)))
(defun %use-ctran (node ctran)
  (declare (type node node) (type ctran ctran) (inline member))
  (let ((block (ctran-block ctran))
	(node-block (ctran-block (node-prev node))))
    (aver (eq (ctran-kind ctran) :block-start))
    (when (block-last node-block)
      (error "~S has already ended." node-block))
    (setf (block-last node-block) node)
    (when (block-succ node-block)
      (error "~S already has successors." node-block))
    (setf (block-succ node-block) (list block))
    (when (memq node-block (block-pred block))
      (error "~S is already a predecessor of ~S." node-block block))
    (push node-block (block-pred block))))

;;; This function is used to set the ctran for a node, and thus
;;; determine what receives the value.
(defun use-lvar (node lvar)
  (declare (type valued-node node) (type (or lvar null) lvar))
  (aver (not (node-lvar node)))
  (when lvar
    (setf (node-lvar node) lvar)
    (cond ((null (lvar-uses lvar))
           (setf (lvar-uses lvar) node))
          ((listp (lvar-uses lvar))
           (aver (not (memq node (lvar-uses lvar))))
           (push node (lvar-uses lvar)))
          (t
           (aver (neq node (lvar-uses lvar)))
           (setf (lvar-uses lvar) (list node (lvar-uses lvar)))))
    (reoptimize-lvar lvar)))

#!-sb-fluid(declaim (inline use-continuation))
(defun use-continuation (node ctran lvar)
  (use-ctran node ctran)
  (use-lvar node lvar))

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
		 :debug-name (debug-namify "top level form " form))))
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

(declaim (ftype (sfunction (ctran ctran (or lvar null) t) (values))
		ir1-convert))
(macrolet (;; Bind *COMPILER-ERROR-BAILOUT* to a function that throws
	   ;; out of the body and converts a proxy form instead.
	   (ir1-error-bailout ((start next result
				form
				&optional
				(proxy ``(error 'simple-program-error
					  :format-control "execution of a form compiled with errors:~% ~S"
					  :format-arguments (list ',,form))))
			       &body body)
			      (with-unique-names (skip)
				`(block ,skip
				   (catch 'ir1-error-abort
				     (let ((*compiler-error-bailout*
					    (lambda ()
					      (throw 'ir1-error-abort nil))))
				       ,@body
				       (return-from ,skip nil)))
				   (ir1-convert ,start ,next ,result ,proxy)))))

  ;; Translate FORM into IR1. The code is inserted as the NEXT of the
  ;; CTRAN START. RESULT is the LVAR which receives the value of the
  ;; FORM to be translated. The translators call this function
  ;; recursively to translate their subnodes.
  ;;
  ;; As a special hack to make life easier in the compiler, a LEAF
  ;; IR1-converts into a reference to that LEAF structure. This allows
  ;; the creation using backquote of forms that contain leaf
  ;; references, without having to introduce dummy names into the
  ;; namespace.
  (defun ir1-convert (start next result form)
    (ir1-error-bailout (start next result form)
      (let ((*current-path* (or (gethash form *source-paths*)
				(cons form *current-path*))))
	(if (atom form)
	    (cond ((and (symbolp form) (not (keywordp form)))
		   (ir1-convert-var start next result form))
		  ((leaf-p form)
		   (reference-leaf start next result form))
		  (t
		   (reference-constant start next result form)))
	    (let ((opname (car form)))
	      (cond ((or (symbolp opname) (leaf-p opname))
		     (let ((lexical-def (if (leaf-p opname)
                                            opname
                                            (lexenv-find opname funs))))
		       (typecase lexical-def
			 (null
                          (ir1-convert-global-functoid start next result
                                                       form))
			 (functional
			  (ir1-convert-local-combination start next result
							 form
							 lexical-def))
			 (global-var
			  (ir1-convert-srctran start next result
                                               lexical-def form))
			 (t
			  (aver (and (consp lexical-def)
				     (eq (car lexical-def) 'macro)))
			  (ir1-convert start next result
				       (careful-expand-macro (cdr lexical-def)
							     form))))))
		    ((or (atom opname) (not (eq (car opname) 'lambda)))
		     (compiler-error "illegal function call"))
		    (t
		     ;; implicitly (LAMBDA ..) because the LAMBDA
		     ;; expression is the CAR of an executed form
		     (ir1-convert-combination start next result
					      form
					      (ir1-convert-lambda
					       opname
					       :debug-name (debug-namify
							    "LAMBDA CAR "
							    opname)
					       :allow-debug-catch-tag t))))))))
    (values))

  ;; Generate a reference to a manifest constant, creating a new leaf
  ;; if necessary. If we are producing a fasl file, make sure that
  ;; MAKE-LOAD-FORM gets used on any parts of the constant that it
  ;; needs to be.
  (defun reference-constant (start next result value)
    (declare (type ctran start next)
             (type (or lvar null) result)
	     (inline find-constant))
    (ir1-error-bailout
     (start next result value '(error "attempt to reference undumpable constant"))
     (when (producing-fasl-file)
       (maybe-emit-make-load-forms value))
     (let* ((leaf (find-constant value))
	    (res (make-ref leaf)))
       (push res (leaf-refs leaf))
       (link-node-to-previous-ctran res start)
       (use-continuation res next result)))
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
(defun reference-leaf (start next result leaf)
  (declare (type ctran start next) (type (or lvar null) result) (type leaf leaf))
  (when (functional-p leaf)
    (assure-functional-live-p leaf))
  (let* ((type (lexenv-find leaf type-restrictions))
         (leaf (or (and (defined-fun-p leaf)
                        (not (eq (defined-fun-inlinep leaf)
                                 :notinline))
                        (let ((functional (defined-fun-functional leaf)))
                          (when (and functional
                                     (not (functional-kind functional)))
                            (maybe-reanalyze-functional functional))))
                   (when (and (lambda-p leaf)
                              (memq (functional-kind leaf)
                                    '(nil :optional)))
                     (maybe-reanalyze-functional leaf))
                   leaf))
         (ref (make-ref leaf)))
    (push ref (leaf-refs leaf))
    (setf (leaf-ever-used leaf) t)
    (link-node-to-previous-ctran ref start)
    (cond (type (let* ((ref-ctran (make-ctran))
                       (ref-lvar (make-lvar))
                       (cast (make-cast ref-lvar
                                        (make-single-value-type type)
                                        (lexenv-policy *lexenv*))))
                  (setf (lvar-dest ref-lvar) cast)
                  (use-continuation ref ref-ctran ref-lvar)
                  (link-node-to-previous-ctran cast ref-ctran)
                  (use-continuation cast next result)))
          (t (use-continuation ref next result)))))

;;; Convert a reference to a symbolic constant or variable. If the
;;; symbol is entered in the LEXENV-VARS we use that definition,
;;; otherwise we find the current global definition. This is also
;;; where we pick off symbol macro and alien variable references.
(defun ir1-convert-var (start next result name)
  (declare (type ctran start next) (type (or lvar null) result) (symbol name))
  (let ((var (or (lexenv-find name vars) (find-free-var name))))
    (etypecase var
      (leaf
       (when (lambda-var-p var)
	 (let ((home (ctran-home-lambda-or-null start)))
	   (when home
	     (pushnew var (lambda-calls-or-closes home))))
	 (when (lambda-var-ignorep var)
	   ;; (ANSI's specification for the IGNORE declaration requires
	   ;; that this be a STYLE-WARNING, not a full WARNING.)
	   #-sb-xc-host
	   (compiler-style-warn "reading an ignored variable: ~S" name)
	   ;; there's no need for us to accept ANSI's lameness when
	   ;; processing our own code, though.
	   #+sb-xc-host
	   (compiler-warn "reading an ignored variable: ~S" name)))
       (reference-leaf start next result var))
      (cons
       (aver (eq (car var) 'MACRO))
       ;; FIXME: [Free] type declarations. -- APD, 2002-01-26
       (ir1-convert start next result (cdr var)))
      (heap-alien-info
       (ir1-convert start next result `(%heap-alien ',var)))))
  (values))

;;; Convert anything that looks like a special form, global function
;;; or compiler-macro call.
(defun ir1-convert-global-functoid (start next result form)
  (declare (type ctran start next) (type (or lvar null) result) (list form))
  (let* ((fun-name (first form))
	 (translator (info :function :ir1-convert fun-name))
	 (cmacro-fun (sb!xc:compiler-macro-function fun-name *lexenv*)))
    (cond (translator
	   (when cmacro-fun
	     (compiler-warn "ignoring compiler macro for special form"))
	   (funcall translator start next result form))
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
		  start next result form fun-name)
		 (ir1-convert start next result res))))
	  (t
	   (ir1-convert-global-functoid-no-cmacro start next result
                                                  form fun-name)))))

;;; Handle the case of where the call was not a compiler macro, or was
;;; a compiler macro and passed.
(defun ir1-convert-global-functoid-no-cmacro (start next result form fun)
  (declare (type ctran start next) (type (or lvar null) result)
           (list form))
  ;; FIXME: Couldn't all the INFO calls here be converted into
  ;; standard CL functions, like MACRO-FUNCTION or something?
  ;; And what happens with lexically-defined (MACROLET) macros
  ;; here, anyway?
  (ecase (info :function :kind fun)
    (:macro
     (ir1-convert start next result
		  (careful-expand-macro (info :function :macro-function fun)
					form)))
    ((nil :function)
     (ir1-convert-srctran start next result
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
                                (compiler-notify
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
(declaim (ftype (sfunction (ctran ctran (or lvar null) list) (values))
		ir1-convert-progn-body))
(defun ir1-convert-progn-body (start next result body)
  (if (endp body)
      (reference-constant start next result nil)
      (let ((this-start start)
	    (forms body))
	(loop
	  (let ((form (car forms)))
	    (when (endp (cdr forms))
	      (ir1-convert this-start next result form)
	      (return))
	    (let ((this-ctran (make-ctran)))
	      (ir1-convert this-start this-ctran nil form)
	      (setq this-start this-ctran
		    forms (cdr forms)))))))
  (values))

;;;; converting combinations

;;; Convert a function call where the function FUN is a LEAF. FORM is
;;; the source for the call. We return the COMBINATION node so that
;;; the caller can poke at it if it wants to.
(declaim (ftype (sfunction (ctran ctran (or lvar null) list leaf) combination)
		ir1-convert-combination))
(defun ir1-convert-combination (start next result form fun)
  (let ((ctran (make-ctran))
        (fun-lvar (make-lvar)))
    (ir1-convert start ctran fun-lvar `(the (or function symbol) ,fun))
    (ir1-convert-combination-args fun-lvar ctran next result (cdr form))))

;;; Convert the arguments to a call and make the COMBINATION
;;; node. FUN-LVAR yields the function to call. ARGS is the list of
;;; arguments for the call, which defaults to the cdr of source. We
;;; return the COMBINATION node.
(defun ir1-convert-combination-args (fun-lvar start next result args)
  (declare (type ctran start next)
           (type lvar fun-lvar)
           (type (or lvar null) result)
           (list args))
  (let ((node (make-combination fun-lvar)))
    (setf (lvar-dest fun-lvar) node)
    (collect ((arg-lvars))
      (let ((this-start start))
	(dolist (arg args)
	  (let ((this-ctran (make-ctran))
                (this-lvar (make-lvar node)))
	    (ir1-convert this-start this-ctran this-lvar arg)
	    (setq this-start this-ctran)
	    (arg-lvars this-lvar)))
	(link-node-to-previous-ctran node this-start)
	(use-continuation node next result)
	(setf (combination-args node) (arg-lvars))))
    node))

;;; Convert a call to a global function. If not :NOTINLINE, then we do
;;; source transforms and try out any inline expansion. If there is no
;;; expansion, but is :INLINE, then give an efficiency note (unless a
;;; known function which will quite possibly be open-coded.) Next, we
;;; go to ok-combination conversion.
(defun ir1-convert-srctran (start next result var form)
  (declare (type ctran start next) (type (or lvar null) result)
           (type global-var var))
  (let ((inlinep (when (defined-fun-p var)
		   (defined-fun-inlinep var))))
    (if (eq inlinep :notinline)
	(ir1-convert-combination start next result form var)
	(let ((transform (info :function
			       :source-transform
			       (leaf-source-name var))))
          (if transform
              (multiple-value-bind (transformed pass) (funcall transform form)
                (if pass
                    (ir1-convert-maybe-predicate start next result form var)
		    (ir1-convert start next result transformed)))
              (ir1-convert-maybe-predicate start next result form var))))))

;;; If the function has the PREDICATE attribute, and the RESULT's DEST
;;; isn't an IF, then we convert (IF <form> T NIL), ensuring that a
;;; predicate always appears in a conditional context.
;;;
;;; If the function isn't a predicate, then we call
;;; IR1-CONVERT-COMBINATION-CHECKING-TYPE.
(defun ir1-convert-maybe-predicate (start next result form var)
  (declare (type ctran start next)
           (type (or lvar null) result)
           (list form)
           (type global-var var))
  (let ((info (info :function :info (leaf-source-name var))))
    (if (and info
	     (ir1-attributep (fun-info-attributes info) predicate)
	     (not (if-p (and result (lvar-dest result)))))
	(ir1-convert start next result `(if ,form t nil))
	(ir1-convert-combination-checking-type start next result form var))))

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
;;; function type to the arg and result lvars. We do this now so that
;;; IR1 optimize doesn't have to redundantly do the check later so
;;; that it can do the type propagation.
(defun ir1-convert-combination-checking-type (start next result form var)
  (declare (type ctran start next) (type (or lvar null) result)
           (list form)
           (type leaf var))
  (let* ((node (ir1-convert-combination start next result form var))
	 (fun-lvar (basic-combination-fun node))
	 (type (leaf-type var)))
    (when (validate-call-type node type t)
      (setf (lvar-%derived-type fun-lvar)
            (make-single-value-type type))
      (setf (lvar-reoptimize fun-lvar) nil)))
  (values))

;;; Convert a call to a local function, or if the function has already
;;; been LET converted, then throw FUNCTIONAL to
;;; LOCALL-ALREADY-LET-CONVERTED. The THROW should only happen when we
;;; are converting inline expansions for local functions during
;;; optimization.
(defun ir1-convert-local-combination (start next result form functional)
  (assure-functional-live-p functional)
  (ir1-convert-combination start next result
			   form
			   (maybe-reanalyze-functional functional)))

;;;; PROCESS-DECLS

;;; Given a list of LAMBDA-VARs and a variable name, return the
;;; LAMBDA-VAR for that name, or NIL if it isn't found. We return the
;;; *last* variable with that name, since LET* bindings may be
;;; duplicated, and declarations always apply to the last.
(declaim (ftype (sfunction (list symbol) (or lambda-var list))
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
             (flet ((process-var (var bound-var)
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
                               (restr (cons var int)))))))
               (process-var var bound-var)
               (awhen (and (lambda-var-p var)
                           (lambda-var-specvar var))
                      (process-var it nil))))
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
  (declare (type list names fvars)
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
				    :unwinnage-fun #'compiler-notify
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

;;; Return a DEFINED-FUN which copies a GLOBAL-VAR but for its INLINEP
;;; (and TYPE if notinline).
(defun make-new-inlinep (var inlinep)
  (declare (type global-var var) (type inlinep inlinep))
  (let ((res (make-defined-fun
	      :%source-name (leaf-source-name var)
	      :where-from (leaf-where-from var)
	      :type (if (and (eq inlinep :notinline)
			     (not (eq (leaf-where-from var) :declared)))
			(specifier-type 'function)
			(leaf-type var))
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
		   (compiler-notify "ignoring ~A declaration not at ~
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
       ((and (consp var) (eq (car var) 'macro))
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

(defun process-dx-decl (names vars)
  (flet ((maybe-notify (control &rest args)
	   (when (policy *lexenv* (> speed inhibit-warnings))
	     (apply #'compiler-notify control args))))
    (if (policy *lexenv* (= stack-allocate-dynamic-extent 3))
	(dolist (name names)
	  (cond
	    ((symbolp name)
	     (let* ((bound-var (find-in-bindings vars name))
		    (var (or bound-var
			     (lexenv-find name vars)
			     (find-free-var name))))
	       (etypecase var
		 (leaf
		  (if bound-var
		      (setf (leaf-dynamic-extent var) t)
		      (maybe-notify
		       "ignoring DYNAMIC-EXTENT declaration for free ~S"
		       name)))
		 (cons
		  (compiler-error "DYNAMIC-EXTENT on symbol-macro: ~S" name))
		 (heap-alien-info
		  (compiler-error "DYNAMIC-EXTENT on heap-alien-info: ~S"
				  name)))))
	    ((and (consp name)
		  (eq (car name) 'function)
		  (null (cddr name))
		  (valid-function-name-p (cadr name)))
	     (maybe-notify "ignoring DYNAMIC-EXTENT declaration for ~S" name))
	    (t (compiler-error "DYNAMIC-EXTENT on a weird thing: ~S" name))))
      (maybe-notify "ignoring DYNAMIC-EXTENT declarations for ~S" names))))

;;; FIXME: This is non-ANSI, so the default should be T, or it should
;;; go away, I think.
(defvar *suppress-values-declaration* nil
  #!+sb-doc
  "If true, processing of the VALUES declaration is inhibited.")

;;; Process a single declaration spec, augmenting the specified LEXENV
;;; RES. Return RES and result type. VARS and FVARS are as described
;;; PROCESS-DECLS.
(defun process-1-decl (raw-spec res vars fvars)
  (declare (type list raw-spec vars fvars))
  (declare (type lexenv res))
  (let ((spec (canonized-decl-spec raw-spec))
        (result-type *wild-type*))
    (values
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
       (muffle-conditions
	(make-lexenv
	 :default res
	 :handled-conditions (process-muffle-conditions-decl
			      spec (lexenv-handled-conditions res))))
       (unmuffle-conditions
	(make-lexenv
	 :default res
	 :handled-conditions (process-unmuffle-conditions-decl
			      spec (lexenv-handled-conditions res))))
       (type
        (process-type-decl (cdr spec) res vars))
       (values
        (unless *suppress-values-declaration*
          (let ((types (cdr spec)))
            (setq result-type
                  (compiler-values-specifier-type
                   (if (singleton-p types)
                       (car types)
                       `(values ,@types)))))
          res))
       (dynamic-extent
	(process-dx-decl (cdr spec) vars)
        res)
       (t
        (unless (info :declaration :recognized (first spec))
          (compiler-warn "unrecognized declaration ~S" raw-spec))
        res))
     result-type)))

;;; Use a list of DECLARE forms to annotate the lists of LAMBDA-VAR
;;; and FUNCTIONAL structures which are being bound. In addition to
;;; filling in slots in the leaf structures, we return a new LEXENV,
;;; which reflects pervasive special and function type declarations,
;;; (NOT)INLINE declarations and OPTIMIZE declarations, and type of
;;; VALUES declarations.
;;;
;;; This is also called in main.lisp when PROCESS-FORM handles a use
;;; of LOCALLY.
(defun process-decls (decls vars fvars &optional (env *lexenv*))
  (declare (list decls vars fvars))
  (let ((result-type *wild-type*))
    (dolist (decl decls)
      (dolist (spec (rest decl))
        (unless (consp spec)
          (compiler-error "malformed declaration specifier ~S in ~S" spec decl))
        (multiple-value-bind (new-env new-result-type)
            (process-1-decl spec env vars fvars)
          (setq env new-env)
          (unless (eq new-result-type *wild-type*)
            (setq result-type
                  (values-type-intersection result-type new-result-type))))))
    (values env result-type)))

(defun %processing-decls (decls vars fvars ctran lvar fun)
  (multiple-value-bind (*lexenv* result-type)
      (process-decls decls vars fvars)
    (cond ((eq result-type *wild-type*)
           (funcall fun ctran lvar))
          (t
           (let ((value-ctran (make-ctran))
                 (value-lvar (make-lvar)))
             (multiple-value-prog1
                 (funcall fun value-ctran value-lvar)
               (let ((cast (make-cast value-lvar result-type
                                      (lexenv-policy *lexenv*))))
                 (link-node-to-previous-ctran cast value-ctran)
                 (setf (lvar-dest value-lvar) cast)
                 (use-continuation cast ctran lvar))))))))
(defmacro processing-decls ((decls vars fvars ctran lvar) &body forms)
  (check-type ctran symbol)
  (check-type lvar symbol)
  `(%processing-decls ,decls ,vars ,fvars ,ctran ,lvar
                      (lambda (,ctran ,lvar) ,@forms)))

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

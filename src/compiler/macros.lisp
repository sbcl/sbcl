;;;; miscellaneous types and macros used in writing the compiler

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

(file-comment
  "$Header$")

(declaim (special *wild-type* *universal-type* *compiler-error-context*))

;;; An INLINEP value describes how a function is called. The values have these
;;; meanings:
;;;	NIL	No declaration seen: do whatever you feel like, but don't dump
;;;		an inline expansion.
;;; :NOTINLINE  NOTINLINE declaration seen: always do full function call.
;;;    :INLINE	INLINE declaration seen: save expansion, expanding to it if
;;;		policy favors.
;;; :MAYBE-INLINE
;;;		Retain expansion, but only use it opportunistically.
(deftype inlinep () '(member :inline :maybe-inline :notinline nil))

;;;; the POLICY macro

(defparameter *policy-parameter-slots*
  '((speed . cookie-speed) (space . cookie-space) (safety . cookie-safety)
    (cspeed . cookie-cspeed) (brevity . cookie-brevity)
    (debug . cookie-debug)))

;;; Find all the policy parameters which are actually mentioned in Stuff,
;;; returning the names in a list. We assume everything is evaluated.
(eval-when (:compile-toplevel :load-toplevel :execute)
(defun find-used-parameters (stuff)
  (if (atom stuff)
      (if (assoc stuff *policy-parameter-slots*) (list stuff) ())
      (collect ((res () nunion))
	(dolist (arg (cdr stuff) (res))
	  (res (find-used-parameters arg))))))
) ; EVAL-WHEN

;;; This macro provides some syntactic sugar for querying the settings of
;;; the compiler policy parameters.
(defmacro policy (node &rest conditions)
  #!+sb-doc
  "Policy Node Condition*
  Test whether some conditions apply to the current compiler policy for Node.
  Each condition is a predicate form which accesses the policy values by
  referring to them as the variables SPEED, SPACE, SAFETY, CSPEED, BREVITY and
  DEBUG. The results of all the conditions are combined with AND and returned
  as the result.

  Node is a form which is evaluated to obtain the node which the policy is for.
  If Node is NIL, then we use the current policy as defined by *DEFAULT-COOKIE*
  and *CURRENT-COOKIE*. This option is only well defined during IR1
  conversion."
  (let* ((form `(and ,@conditions))
	 (n-cookie (gensym))
	 (binds (mapcar
		 #'(lambda (name)
		     (let ((slot (cdr (assoc name *policy-parameter-slots*))))
		       `(,name (,slot ,n-cookie))))
		 (find-used-parameters form))))
    `(let* ((,n-cookie (lexenv-cookie
			,(if node
			     `(node-lexenv ,node)
			     '*lexenv*)))
	    ,@binds)
       ,form)))

;;;; source-hacking defining forms

;;; Passed to PARSE-DEFMACRO when we want compiler errors instead of real
;;; errors.
#!-sb-fluid (declaim (inline convert-condition-into-compiler-error))
(defun convert-condition-into-compiler-error (datum &rest stuff)
  (if (stringp datum)
      (apply #'compiler-error datum stuff)
      (compiler-error "~A"
		      (if (symbolp datum)
			  (apply #'make-condition datum stuff)
			  datum))))

;;; Parse DEFMACRO-style lambda-list, setting things up so that a
;;; compiler error happens if the syntax is invalid.
(defmacro def-ir1-translator (name (lambda-list start-var cont-var
						&key (kind :special-form))
				   &body body)
  #!+sb-doc
  "Def-IR1-Translator Name (Lambda-List Start-Var Cont-Var {Key Value}*)
		      [Doc-String] Form*
  Define a function that converts a Special-Form or other magical thing into
  IR1. Lambda-List is a defmacro style lambda list. Start-Var and Cont-Var
  are bound to the start and result continuations for the resulting IR1.
  This keyword is defined:
      Kind
	  The function kind to associate with Name (default :special-form)."
  (let ((fn-name (symbolicate "IR1-CONVERT-" name))
	(n-form (gensym))
	(n-env (gensym)))
    (multiple-value-bind (body decls doc)
	(parse-defmacro lambda-list n-form body name "special form"
			:environment n-env
			:error-fun 'convert-condition-into-compiler-error)
      `(progn
	 (declaim (ftype (function (continuation continuation t) (values))
			 ,fn-name))
	 (defun ,fn-name (,start-var ,cont-var ,n-form)
	   (let ((,n-env *lexenv*))
	     ,@decls
	     ,body
	     (values)))
	 ,@(when doc
	     `((setf (fdocumentation ',name 'function) ,doc)))
	 ;; FIXME: Evidently "there can only be one!" -- we overwrite any
	 ;; other :IR1-CONVERT value. This deserves a warning, I think.
	 (setf (info :function :ir1-convert ',name) #',fn-name)
	 (setf (info :function :kind ',name) ,kind)
	 ;; It's nice to do this for error checking in the target
	 ;; SBCL, but it's not nice to do this when we're running in
	 ;; the cross-compilation host Lisp, which owns the
	 ;; SYMBOL-FUNCTION of its COMMON-LISP symbols.
	 #-sb-xc-host
	 ,@(when (eq kind :special-form)
	     `((setf (symbol-function ',name)
		     (lambda (&rest rest)
		       (declare (ignore rest))
		       (error "can't FUNCALL the SYMBOL-FUNCTION of ~
			       special forms")))))))))

;;; Similar to DEF-IR1-TRANSLATOR, except that we pass if the syntax is
;;; invalid.
(defmacro def-source-transform (name lambda-list &body body)
  #!+sb-doc
  "Def-Source-Transform Name Lambda-List Form*
  Define a macro-like source-to-source transformation for the function Name.
  A source transform may \"pass\" by returning a non-nil second value. If the
  transform passes, then the form is converted as a normal function call. If
  the supplied arguments are not compatible with the specified lambda-list,
  then the transform automatically passes.

  Source-Transforms may only be defined for functions. Source transformation
  is not attempted if the function is declared Notinline. Source transforms
  should not examine their arguments. If it matters how the function is used,
  then Deftransform should be used to define an IR1 transformation.

  If the desirability of the transformation depends on the current Optimize
  parameters, then the Policy macro should be used to determine when to pass."
  (let ((fn-name
	 (if (listp name)
	     (collect ((pieces))
	       (dolist (piece name)
		 (pieces "-")
		 (pieces piece))
	       (apply #'symbolicate "SOURCE-TRANSFORM" (pieces)))
	     (symbolicate "SOURCE-TRANSFORM-" name)))
	(n-form (gensym))
	(n-env (gensym)))
    (multiple-value-bind (body decls)
	(parse-defmacro lambda-list n-form body name "form"
			:environment n-env
			:error-fun `(lambda (&rest stuff)
				      (declare (ignore stuff))
				      (return-from ,fn-name
					(values nil t))))
      `(progn
	 (defun ,fn-name (,n-form)
	   (let ((,n-env *lexenv*))
	     ,@decls
	     ,body))
	 (setf (info :function :source-transform ',name) #',fn-name)))))

(defmacro def-primitive-translator (name lambda-list &body body)
  #!+sb-doc
  "DEF-PRIMITIVE-TRANSLATOR Name Lambda-List Form*
  Define a function that converts a use of (%PRIMITIVE Name ...) into Lisp
  code. Lambda-List is a DEFMACRO-style lambda list."
  (let ((fn-name (symbolicate "PRIMITIVE-TRANSLATE-" name))
	(n-form (gensym))
	(n-env (gensym)))
    (multiple-value-bind (body decls)
	(parse-defmacro lambda-list n-form body name "%primitive"
			:environment n-env
			:error-fun 'convert-condition-into-compiler-error)
      `(progn
	 (defun ,fn-name (,n-form)
	   (let ((,n-env *lexenv*))
	     ,@decls
	     ,body))
	 (setf (gethash ',name *primitive-translators*) ',fn-name)))))

;;;; boolean attribute utilities
;;;;
;;;; We need to maintain various sets of boolean attributes for known
;;;; functions and VOPs. To save space and allow for quick set
;;;; operations, we represent the attributes as bits in a fixnum.

(deftype attributes () 'fixnum)

(eval-when (:compile-toplevel :load-toplevel :execute)

;;; Given a list of attribute names and an alist that translates them
;;; to masks, return the OR of the masks.
(defun compute-attribute-mask (names alist)
  (collect ((res 0 logior))
    (dolist (name names)
      (let ((mask (cdr (assoc name alist))))
	(unless mask
	  (error "unknown attribute name: ~S" name))
	(res mask)))
    (res)))

) ; EVAL-WHEN

;;; Parse the specification and generate some accessor macros.
;;;
;;; KLUDGE: This is expanded out twice, by cut-and-paste, in a
;;;   (DEF!MACRO FOO (..) .. CL:GET-SETF-EXPANSION ..)
;;;   #+SB-XC-HOST
;;;   (SB!XC:DEFMACRO FOO (..) .. SB!XC:GET-SETF-EXPANSION ..)
;;; arrangement, in order to get it to work in cross-compilation. This
;;; duplication should be removed, perhaps by rewriting the macro in a
;;; more cross-compiler-friendly way, or perhaps just by using some
;;; (MACROLET ((FROB ..)) .. FROB .. FROB) form, but I don't want to
;;; do it now, because the system isn't running yet, so it'd be too
;;; hard to check that my changes were correct -- WHN 19990806
(def!macro def-boolean-attribute (name &rest attribute-names)
  #!+sb-doc
  "Def-Boolean-Attribute Name Attribute-Name*
  Define a new class of boolean attributes, with the attributes having the
  specified Attribute-Names. Name is the name of the class, which is used to
  generate some macros to manipulate sets of the attributes:

    NAME-attributep attributes attribute-name*
      Return true if one of the named attributes is present, false otherwise.
      When set with SETF, updates the place Attributes setting or clearing the
      specified attributes.

    NAME-attributes attribute-name*
      Return a set of the named attributes."

  (let ((const-name (symbolicate name "-ATTRIBUTE-TRANSLATIONS"))
	(test-name (symbolicate name "-ATTRIBUTEP")))
    (collect ((alist))
      (do ((mask 1 (ash mask 1))
	   (names attribute-names (cdr names)))
	  ((null names))
	(alist (cons (car names) mask)))

      `(progn
	 (eval-when (:compile-toplevel :load-toplevel :execute)
	   (defconstant ,const-name ',(alist)))

	 (defmacro ,test-name (attributes &rest attribute-names)
	   "Automagically generated boolean attribute test function. See
	    Def-Boolean-Attribute."
	   `(logtest ,(compute-attribute-mask attribute-names ,const-name)
		     (the attributes ,attributes)))

	 (define-setf-expander ,test-name (place &rest attributes
						 &environment env)
	   "Automagically generated boolean attribute setter. See
	    Def-Boolean-Attribute."
	   #-sb-xc-host (declare (type sb!c::lexenv env))
	   ;; FIXME: It would be better if &ENVIRONMENT arguments
	   ;; were automatically declared to have type LEXENV by the
	   ;; hairy-argument-handling code.
	   (multiple-value-bind (temps values stores set get)
	       (get-setf-expansion place env)
	     (when (cdr stores)
	       (error "multiple store variables for ~S" place))
	     (let ((newval (gensym))
		   (n-place (gensym))
		   (mask (compute-attribute-mask attributes ,const-name)))
	       (values `(,@temps ,n-place)
		       `(,@values ,get)
		       `(,newval)
		       `(let ((,(first stores)
			       (if ,newval
				   (logior ,n-place ,mask)
				   (logand ,n-place ,(lognot mask)))))
			  ,set
			  ,newval)
		       `(,',test-name ,n-place ,@attributes)))))

	 (defmacro ,(symbolicate name "-ATTRIBUTES") (&rest attribute-names)
	   "Automagically generated boolean attribute creation function. See
	    Def-Boolean-Attribute."
	   (compute-attribute-mask attribute-names ,const-name))))))
;;; #+SB-XC-HOST SB!XC:DEFMACRO version is in late-macros.lisp. -- WHN 19990806

;;; And now for some gratuitous pseudo-abstraction...
(defmacro attributes-union (&rest attributes)
  #!+sb-doc
  "Returns the union of all the sets of boolean attributes which are its
  arguments."
  `(the attributes
	(logior ,@(mapcar #'(lambda (x) `(the attributes ,x)) attributes))))
(defmacro attributes-intersection (&rest attributes)
  #!+sb-doc
  "Returns the intersection of all the sets of boolean attributes which are its
  arguments."
  `(the attributes
	(logand ,@(mapcar #'(lambda (x) `(the attributes ,x)) attributes))))
(declaim (ftype (function (attributes attributes) boolean) attributes=))
#!-sb-fluid (declaim (inline attributes=))
(defun attributes= (attr1 attr2)
  #!+sb-doc
  "Returns true if the attributes present in Attr1 are identical to those in
  Attr2."
  (eql attr1 attr2))

;;;; lambda-list parsing utilities
;;;;
;;;; IR1 transforms, optimizers and type inferencers need to be able
;;;; to parse the IR1 representation of a function call using a
;;;; standard function lambda-list.

(eval-when (:compile-toplevel :load-toplevel :execute)

;;; Given a DEFTRANSFORM-style lambda-list, generate code that parses
;;; the arguments of a combination with respect to that lambda-list.
;;; BODY is the the list of forms which are to be evaluated within the
;;; bindings. ARGS is the variable that holds list of argument
;;; continuations. ERROR-FORM is a form which is evaluated when the
;;; syntax of the supplied arguments is incorrect or a non-constant
;;; argument keyword is supplied. Defaults and other gunk are ignored.
;;; The second value is a list of all the arguments bound. We make the
;;; variables IGNORABLE so that we don't have to manually declare them
;;; Ignore if their only purpose is to make the syntax work.
(declaim (ftype (function (list list symbol t) list) parse-deftransform))
(defun parse-deftransform (lambda-list body args error-form)
  (multiple-value-bind (req opt restp rest keyp keys allowp)
      (parse-lambda-list lambda-list)
    (let* ((min-args (length req))
	   (max-args (+ min-args (length opt)))
	   (n-keys (gensym)))
      (collect ((binds)
		(vars)
		(pos 0 +)
		(keywords))
	(dolist (arg req)
	  (vars arg)
	  (binds `(,arg (nth ,(pos) ,args)))
	  (pos 1))

	(dolist (arg opt)
	  (let ((var (if (atom arg) arg (first  arg))))
	    (vars var)
	    (binds `(,var (nth ,(pos) ,args)))
	    (pos 1)))

	(when restp
	  (vars rest)
	  (binds `(,rest (nthcdr ,(pos) ,args))))

	(dolist (spec keys)
	  (if (or (atom spec) (atom (first spec)))
	      (let* ((var (if (atom spec) spec (first spec)))
		     (key (intern (symbol-name var) "KEYWORD")))
		(vars var)
		(binds `(,var (find-keyword-continuation ,n-keys ,key)))
		(keywords key))
	      (let* ((head (first spec))
		     (var (second head))
		     (key (first head)))
		(vars var)
		(binds `(,var (find-keyword-continuation ,n-keys ,key)))
		(keywords key))))

	(let ((n-length (gensym))
	      (limited-legal (not (or restp keyp))))
	  (values
	   `(let ((,n-length (length ,args))
		  ,@(when keyp `((,n-keys (nthcdr ,(pos) ,args)))))
	      (unless (and
		       ;; FIXME: should be PROPER-LIST-OF-LENGTH-P
		       ,(if limited-legal
			    `(<= ,min-args ,n-length ,max-args)
			    `(<= ,min-args ,n-length))
		       ,@(when keyp
			   (if allowp
			       `((check-keywords-constant ,n-keys))
			       `((check-transform-keys ,n-keys ',(keywords))))))
		,error-form)
	      (let ,(binds)
		(declare (ignorable ,@(vars)))
		,@body))
	   (vars)))))))

) ; EVAL-WHEN

;;;; DEFTRANSFORM

;;; Parse the lambda-list and generate code to test the policy and
;;; automatically create the result lambda.
(defmacro deftransform (name (lambda-list &optional (arg-types '*)
					  (result-type '*)
					  &key result policy node defun-only
					  eval-name important (when :native))
			     &body body-decls-doc)
  #!+sb-doc
  "Deftransform Name (Lambda-List [Arg-Types] [Result-Type] {Key Value}*)
	       Declaration* [Doc-String] Form*
  Define an IR1 transformation for NAME. An IR1 transformation computes a
  lambda that replaces the function variable reference for the call. A
  transform may pass (decide not to transform the call) by calling the
  GIVE-UP-IR1-TRANSFORM function. LAMBDA-LIST both determines how the
  current call is parsed and specifies the LAMBDA-LIST for the resulting
  lambda.

  We parse the call and bind each of the lambda-list variables to the
  continuation which represents the value of the argument. When parsing
  the call, we ignore the defaults, and always bind the variables for
  unsupplied arguments to NIL. If a required argument is missing, an
  unknown keyword is supplied, or an argument keyword is not a constant,
  then the transform automatically passes. The DECLARATIONS apply to the
  bindings made by DEFTRANSFORM at transformation time, rather than to
  the variables of the resulting lambda. Bound-but-not-referenced
  warnings are suppressed for the lambda-list variables. The DOC-STRING
  is used when printing efficiency notes about the defined transform.

  Normally, the body evaluates to a form which becomes the body of an
  automatically constructed lambda. We make LAMBDA-LIST the lambda-list
  for the lambda, and automatically insert declarations of the argument
  and result types. If the second value of the body is non-null, then it
  is a list of declarations which are to be inserted at the head of the
  lambda. Automatic lambda generation may be inhibited by explicitly
  returning a lambda from the body.

  The ARG-TYPES and RESULT-TYPE are used to create a function type
  which the call must satisfy before transformation is attempted. The
  function type specifier is constructed by wrapping (FUNCTION ...)
  around these values, so the lack of a restriction may be specified by
  omitting the argument or supplying *. The argument syntax specified in
  the ARG-TYPES need not be the same as that in the LAMBDA-LIST, but the
  transform will never happen if the syntaxes can't be satisfied
  simultaneously. If there is an existing transform for the same
  function that has the same type, then it is replaced with the new
  definition.

  These are the legal keyword options:
    :Result - A variable which is bound to the result continuation.
    :Node   - A variable which is bound to the combination node for the call.
    :Policy - A form which is supplied to the POLICY macro to determine whether
	      this transformation is appropriate. If the result is false, then
	      the transform automatically passes.
    :Eval-Name
    	    - The name and argument/result types are actually forms to be
	      evaluated. Useful for getting closures that transform similar
	      functions.
    :Defun-Only
	    - Don't actually instantiate a transform, instead just DEFUN
	      Name with the specified transform definition function. This may
	      be later instantiated with %DEFTRANSFORM.
    :Important
	    - If supplied and non-NIL, note this transform as ``important,''
	      which means efficiency notes will be generated when this
	      transform fails even if brevity=speed (but not if brevity>speed)
    :When {:Native | :Byte | :Both}
	    - Indicates whether this transform applies to native code,
	      byte-code or both (default :native.)"

  (when (and eval-name defun-only)
    (error "can't specify both DEFUN-ONLY and EVAL-NAME"))
  (multiple-value-bind (body decls doc) (parse-body body-decls-doc)
    (let ((n-args (gensym))
	  (n-node (or node (gensym)))
	  (n-decls (gensym))
	  (n-lambda (gensym))
	  (decls-body `(,@decls ,@body)))
      (multiple-value-bind (parsed-form vars)
	  (parse-deftransform lambda-list
			      (if policy
				  `((unless (policy ,n-node ,policy)
				      (give-up-ir1-transform))
				    ,@decls-body)
				  body)
			      n-args
			      '(give-up-ir1-transform))
	(let ((stuff
	       `((,n-node)
		 (let* ((,n-args (basic-combination-args ,n-node))
			,@(when result
			    `((,result (node-cont ,n-node)))))
		   (multiple-value-bind (,n-lambda ,n-decls)
		       ,parsed-form
		     (if (and (consp ,n-lambda) (eq (car ,n-lambda) 'lambda))
			 ,n-lambda
		       `(lambda ,',lambda-list
			  (declare (ignorable ,@',vars))
			  ,@,n-decls
			  ,,n-lambda)))))))
	  (if defun-only
	      `(defun ,name ,@(when doc `(,doc)) ,@stuff)
	      `(%deftransform
		,(if eval-name name `',name)
		,(if eval-name
		     ``(function ,,arg-types ,,result-type)
		     `'(function ,arg-types ,result-type))
		#'(lambda ,@stuff)
		,doc
		,(if important t nil)
		,when)))))))

;;;; DEFKNOWN and DEFOPTIMIZER

;;; This macro should be the way that all implementation independent
;;; information about functions is made known to the compiler.
;;;
;;; FIXME: The comment above suggests that perhaps some of my added
;;; FTYPE declarations are in poor taste. Should I change my
;;; declarations, or change the comment, or what?
;;;
;;; FIXME: DEFKNOWN is needed only at build-the-system time. Figure
;;; out some way to keep it from appearing in the target system.
(defmacro defknown (name arg-types result-type &optional (attributes '(any))
			 &rest keys)
  #!+sb-doc
  "Defknown Name Arg-Types Result-Type [Attributes] {Key Value}*
  Declare the function Name to be a known function. We construct a type
  specifier for the function by wrapping (FUNCTION ...) around the Arg-Types
  and Result-Type. Attributes is an unevaluated list of boolean
  attributes of the function. These attributes are meaningful here:
      call
	 May call functions that are passed as arguments. In order
	 to determine what other effects are present, we must find
	 the effects of all arguments that may be functions.

      unsafe
	 May incorporate arguments in the result or somehow pass
	 them upward.

      unwind
	 May fail to return during correct execution. Errors
	 are O.K.

      any
	 The (default) worst case. Includes all the other bad
	 things, plus any other possible bad thing.

      foldable
	 May be constant-folded. The function has no side effects,
	 but may be affected by side effects on the arguments. E.g.
	 SVREF, MAPC.

      flushable
	 May be eliminated if value is unused. The function has
	 no side effects except possibly CONS. If a function is
	 defined to signal errors, then it is not flushable even
	 if it is movable or foldable.

      movable
	 May be moved with impunity. Has no side effects except
	 possibly CONS,and is affected only by its arguments.

      predicate
	  A true predicate likely to be open-coded. This is a
	  hint to IR1 conversion that it should ensure calls always
	  appear as an IF test. Not usually specified to Defknown,
	  since this is implementation dependent, and is usually
	  automatically set by the Define-VOP :Conditional option.

  Name may also be a list of names, in which case the same information
  is given to all the names. The keywords specify the initial values
  for various optimizers that the function might have."
  (when (and (intersection attributes '(any call unwind))
	     (intersection attributes '(movable)))
    (error "function cannot have both good and bad attributes: ~S" attributes))

  `(%defknown ',(if (and (consp name)
			 (not (eq (car name) 'setf)))
		    name
		    (list name))
	      '(function ,arg-types ,result-type)
	      (ir1-attributes ,@(if (member 'any attributes)
				    (union '(call unsafe unwind) attributes)
				    attributes))
	      ,@keys))

;;; Create a function which parses combination args according to 
;;; LAMBDA-LIST, optionally storing it in a FUNCTION-INFO slot.
(defmacro defoptimizer (what (lambda-list &optional (n-node (gensym))
					  &rest vars)
			     &body body)
  #!+sb-doc
  "Defoptimizer (Function Kind) (Lambda-List [Node-Var] Var*)
		Declaration* Form*
  Define some Kind of optimizer for the named Function. Function must be a
  known function. Lambda-List is used to parse the arguments to the
  combination as in Deftransform. If the argument syntax is invalid or there
  are non-constant keys, then we simply return NIL.

  The function is DEFUN'ed as Function-Kind-OPTIMIZER. Possible kinds are
  DERIVE-TYPE, OPTIMIZER, LTN-ANNOTATE and IR2-CONVERT. If a symbol is
  specified instead of a (Function Kind) list, then we just do a DEFUN with the
  symbol as its name, and don't do anything with the definition. This is
  useful for creating optimizers to be passed by name to DEFKNOWN.

  If supplied, Node-Var is bound to the combination node being optimized. If
  additional Vars are supplied, then they are used as the rest of the optimizer
  function's lambda-list. LTN-ANNOTATE methods are passed an additional POLICY
  argument, and IR2-CONVERT methods are passed an additional IR2-BLOCK
  argument."

  (let ((name (if (symbolp what) what
		  (symbolicate (first what) "-" (second what) "-OPTIMIZER"))))

    (let ((n-args (gensym)))
      `(progn
	(defun ,name (,n-node ,@vars)
	  (let ((,n-args (basic-combination-args ,n-node)))
	    ,(parse-deftransform lambda-list body n-args
				 `(return-from ,name nil))))
	,@(when (consp what)
	    `((setf (,(symbolicate "FUNCTION-INFO-" (second what))
		     (function-info-or-lose ',(first what)))
		    #',name)))))))

;;;; IR groveling macros

(defmacro do-blocks ((block-var component &optional ends result) &body body)
  #!+sb-doc
  "Do-Blocks (Block-Var Component [Ends] [Result-Form]) {Declaration}* {Form}*
  Iterate over the blocks in a component, binding Block-Var to each block in
  turn. The value of Ends determines whether to iterate over dummy head and
  tail blocks:
    NIL   -- Skip Head and Tail (the default)
    :Head -- Do head but skip tail
    :Tail -- Do tail but skip head
    :Both -- Do both head and tail

  If supplied, Result-Form is the value to return."
  (unless (member ends '(nil :head :tail :both))
    (error "losing ENDS value: ~S" ends))
  (let ((n-component (gensym))
	(n-tail (gensym)))
    `(let* ((,n-component ,component)
	    (,n-tail ,(if (member ends '(:both :tail))
			  nil
			  `(component-tail ,n-component))))
       (do ((,block-var ,(if (member ends '(:both :head))
			     `(component-head ,n-component)
			     `(block-next (component-head ,n-component)))
			(block-next ,block-var)))
	   ((eq ,block-var ,n-tail) ,result)
	 ,@body))))
(defmacro do-blocks-backwards ((block-var component &optional ends result) &body body)
  #!+sb-doc
  "Do-Blocks-Backwards (Block-Var Component [Ends] [Result-Form]) {Declaration}* {Form}*
  Like Do-Blocks, only iterate over the blocks in reverse order."
  (unless (member ends '(nil :head :tail :both))
    (error "losing ENDS value: ~S" ends))
  (let ((n-component (gensym))
	(n-head (gensym)))
    `(let* ((,n-component ,component)
	    (,n-head ,(if (member ends '(:both :head))
			  nil
			  `(component-head ,n-component))))
       (do ((,block-var ,(if (member ends '(:both :tail))
			     `(component-tail ,n-component)
			     `(block-prev (component-tail ,n-component)))
			(block-prev ,block-var)))
	   ((eq ,block-var ,n-head) ,result)
	 ,@body))))

;;; Could change it not to replicate the code someday perhaps...
(defmacro do-uses ((node-var continuation &optional result) &body body)
  #!+sb-doc
  "Do-Uses (Node-Var Continuation [Result]) {Declaration}* {Form}*
  Iterate over the uses of Continuation, binding Node to each one
  successively."
  (once-only ((n-cont continuation))
    `(ecase (continuation-kind ,n-cont)
       (:unused)
       (:inside-block
	(block nil
	  (let ((,node-var (continuation-use ,n-cont)))
	    ,@body
	    ,result)))
       ((:block-start :deleted-block-start)
	(dolist (,node-var (block-start-uses (continuation-block ,n-cont))
			   ,result)
	  ,@body)))))

;;; In the forward case, we terminate on Last-Cont so that we don't
;;; have to worry about our termination condition being changed when
;;; new code is added during the iteration. In the backward case, we
;;; do NODE-PREV before evaluating the body so that we can keep going
;;; when the current node is deleted.
;;;
;;; When RESTART-P is supplied to DO-NODES, we start iterating over
;;; again at the beginning of the block when we run into a
;;; continuation whose block differs from the one we are trying to
;;; iterate over, either beacuse the block was split, or because a
;;; node was deleted out from under us (hence its block is NIL.) If
;;; the block start is deleted, we just punt. With RESTART-P, we are
;;; also more careful about termination, re-indirecting the BLOCK-LAST
;;; each time.
(defmacro do-nodes ((node-var cont-var block &key restart-p) &body body)
  #!+sb-doc
  "Do-Nodes (Node-Var Cont-Var Block {Key Value}*) {Declaration}* {Form}*
  Iterate over the nodes in Block, binding Node-Var to the each node and
  Cont-Var to the node's Cont. The only keyword option is Restart-P, which
  causes iteration to be restarted when a node is deleted out from under us (if
  not supplied, this is an error.)"
  (let ((n-block (gensym))
	(n-last-cont (gensym)))
    `(let* ((,n-block ,block)
	    ,@(unless restart-p
		`((,n-last-cont (node-cont (block-last ,n-block))))))
       (do* ((,node-var (continuation-next (block-start ,n-block))
			,(if restart-p
			     `(cond
			       ((eq (continuation-block ,cont-var) ,n-block)
				(assert (continuation-next ,cont-var))
				(continuation-next ,cont-var))
			       (t
				(let ((start (block-start ,n-block)))
				  (unless (eq (continuation-kind start)
					      :block-start)
				    (return nil))
				  (continuation-next start))))
			     `(continuation-next ,cont-var)))
	     (,cont-var (node-cont ,node-var) (node-cont ,node-var)))
	    (())
	 ,@body
	 (when ,(if restart-p
		    `(eq ,node-var (block-last ,n-block))
		    `(eq ,cont-var ,n-last-cont))
	   (return nil))))))
(defmacro do-nodes-backwards ((node-var cont-var block) &body body)
  #!+sb-doc
  "Do-Nodes-Backwards (Node-Var Cont-Var Block) {Declaration}* {Form}*
  Like Do-Nodes, only iterates in reverse order."
  (let ((n-block (gensym))
	(n-start (gensym))
	(n-last (gensym))
	(n-next (gensym)))
    `(let* ((,n-block ,block)
	    (,n-start (block-start ,n-block))
	    (,n-last (block-last ,n-block)))
       (do* ((,cont-var (node-cont ,n-last) ,n-next)
	     (,node-var ,n-last (continuation-use ,cont-var))
	     (,n-next (node-prev ,node-var) (node-prev ,node-var)))
	    (())
	 ,@body
	 (when (eq ,n-next ,n-start)
	   (return nil))))))

;;; The lexical environment is presumably already null...
(defmacro with-ir1-environment (node &rest forms)
  #!+sb-doc
  "With-IR1-Environment Node Form*
  Bind the IR1 context variables so that IR1 conversion can be done after the
  main conversion pass has finished."
  (let ((n-node (gensym)))
    `(let* ((,n-node ,node)
	    (*current-component* (block-component (node-block ,n-node)))
	    (*lexenv* (node-lexenv ,n-node))
	    (*current-path* (node-source-path ,n-node)))
       ,@forms)))

;;; Bind the hashtables used for keeping track of global variables,
;;; functions, &c. Also establish condition handlers.
(defmacro with-ir1-namespace (&body forms)
  `(let ((*free-variables* (make-hash-table :test 'eq))
	 (*free-functions* (make-hash-table :test 'equal))
	 (*constants* (make-hash-table :test 'equal))
	 (*source-paths* (make-hash-table :test 'eq)))
     (handler-bind ((compiler-error #'compiler-error-handler)
		    (style-warning #'compiler-style-warning-handler)
		    (warning #'compiler-warning-handler))
       ,@forms)))

(defmacro lexenv-find (name slot &key test)
  #!+sb-doc
  "LEXENV-FIND Name Slot {Key Value}*
  Look up Name in the lexical environment namespace designated by Slot,
  returning the <value, T>, or <NIL, NIL> if no entry. The :TEST keyword
  may be used to determine the name equality predicate."
  (once-only ((n-res `(assoc ,name (,(symbolicate "LEXENV-" slot) *lexenv*)
			     :test ,(or test '#'eq))))
    `(if ,n-res
	 (values (cdr ,n-res) t)
	 (values nil nil))))

;;; These functions are called by the expansion of the DEFPRINTER
;;; macro to do the actual printing.
(declaim (ftype (function (symbol t stream &optional t) (values))
		defprinter-prin1 defprinter-princ))
(defun defprinter-prin1 (name value stream &optional indent)
  (declare (ignore indent))
  (defprinter-prinx #'prin1 name value stream))
(defun defprinter-princ (name value stream &optional indent)
  (declare (ignore indent))
  (defprinter-prinx #'princ name value stream))
(defun defprinter-prinx (prinx name value stream)
  (declare (type function prinx))
  (write-char #\space stream)
  (when *print-pretty*
    (pprint-newline :linear stream))
  (format stream ":~A " name)
  (funcall prinx value stream)
  (values))

;; Define some kind of reasonable PRINT-OBJECT method for a STRUCTURE-OBJECT.
;;
;; NAME is the name of the structure class, and CONC-NAME is the same as in
;; DEFSTRUCT.
;;
;; The SLOT-DESCS describe how each slot should be printed. Each SLOT-DESC can
;; be a slot name, indicating that the slot should simply be printed. A
;; SLOT-DESC may also be a list of a slot name and other stuff. The other stuff
;; is composed of keywords followed by expressions. The expressions are
;; evaluated with the variable which is the slot name bound to the value of the
;; slot. These keywords are defined:
;;
;; :PRIN1    Print the value of the expression instead of the slot value.
;; :PRINC    Like :PRIN1, only princ the value
;; :TEST     Only print something if the test is true.
;;
;; If no printing thing is specified then the slot value is printed as PRIN1.
;;
;; The structure being printed is bound to STRUCTURE and the stream is bound to
;; STREAM.
(defmacro defprinter ((name &key (conc-name (concatenate 'simple-string
							 (symbol-name name)
							 "-")))
		      &rest slot-descs)
  (flet ((sref (slot-name)
	   `(,(symbolicate conc-name slot-name) structure)))
    (collect ((prints))
      (dolist (slot-desc slot-descs)
	(if (atom slot-desc)
	  (prints `(defprinter-prin1 ',slot-desc ,(sref slot-desc) stream))
	  (let ((sname (first slot-desc))
		(test t))
	    (collect ((stuff))
	      (do ((option (rest slot-desc) (cddr option)))
		  ((null option)
		   (prints
		    `(let ((,sname ,(sref sname)))
		       (when ,test
			 ,@(or (stuff)
			       `((defprinter-prin1 ',sname ,sname
				   stream)))))))
		(case (first option)
		  (:prin1
		   (stuff `(defprinter-prin1 ',sname ,(second option)
			     stream)))
		  (:princ
		   (stuff `(defprinter-princ ',sname ,(second option)
			     stream)))
		  (:test (setq test (second option)))
		  (t
		   (error "bad DEFPRINTER option: ~S" (first option)))))))))

      `(def!method print-object ((structure ,name) stream)
	 (print-unreadable-object (structure stream :type t)
	   (pprint-logical-block (stream nil)
	     ;;(pprint-indent :current 2 stream)
	     ,@(prints)))))))

;;;; the Event statistics/trace utility

;;; FIXME: This seems to be useful for troubleshooting and
;;; experimentation, not for ordinary use, so it should probably
;;; become conditional on SB-SHOW.

(eval-when (:compile-toplevel :load-toplevel :execute)

(defstruct event-info
  ;; The name of this event.
  (name (required-argument) :type symbol)
  ;; The string rescribing this event.
  (description (required-argument) :type string)
  ;; The name of the variable we stash this in.
  (var (required-argument) :type symbol)
  ;; The number of times this event has happened.
  (count 0 :type fixnum)
  ;; The level of significance of this event.
  (level (required-argument) :type unsigned-byte)
  ;; If true, a function that gets called with the node that the event
  ;; happened to.
  (action nil :type (or function null)))

;;; A hashtable from event names to event-info structures.
(defvar *event-info* (make-hash-table :test 'eq))

;;; Return the event info for Name or die trying.
(declaim (ftype (function (t) event-info) event-info-or-lose))
(defun event-info-or-lose (name)
  (let ((res (gethash name *event-info*)))
    (unless res
      (error "~S is not the name of an event." name))
    res))

) ; EVAL-WHEN

(declaim (ftype (function (symbol) fixnum) event-count))
(defun event-count (name)
  #!+sb-doc
  "Return the number of times that Event has happened."
  (event-info-count (event-info-or-lose name)))

(declaim (ftype (function (symbol) (or function null)) event-action))
(defun event-action (name)
  #!+sb-doc
  "Return the function that is called when Event happens. If this is null,
  there is no action. The function is passed the node to which the event
  happened, or NIL if there is no relevant node. This may be set with SETF."
  (event-info-action (event-info-or-lose name)))
(declaim (ftype (function (symbol (or function null)) (or function null))
		%set-event-action))
(defun %set-event-action (name new-value)
  (setf (event-info-action (event-info-or-lose name))
	new-value))
(defsetf event-action %set-event-action)

(declaim (ftype (function (symbol) unsigned-byte) event-level))
(defun event-level (name)
  #!+sb-doc
  "Return the non-negative integer which represents the level of significance
  of the event Name. This is used to determine whether to print a message when
  the event happens. This may be set with SETF."
  (event-info-level (event-info-or-lose name)))
(declaim (ftype (function (symbol unsigned-byte) unsigned-byte) %set-event-level))
(defun %set-event-level (name new-value)
  (setf (event-info-level (event-info-or-lose name))
	new-value))
(defsetf event-level %set-event-level)

;;; Make an EVENT-INFO structure and stash it in a variable so we can
;;; get at it quickly.
(defmacro defevent (name description &optional (level 0))
  #!+sb-doc
  "Defevent Name Description
  Define a new kind of event. Name is a symbol which names the event and
  Description is a string which describes the event. Level (default 0) is the
  level of significance associated with this event; it is used to determine
  whether to print a Note when the event happens."
  (let ((var-name (symbolicate "*" name "-EVENT-INFO*")))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defvar ,var-name
	 (make-event-info :name ',name
			  :description ',description
			  :var ',var-name
			  :level ,level))
       (setf (gethash ',name *event-info*) ,var-name)
       ',name)))

(declaim (type unsigned-byte *event-note-threshold*))
(defvar *event-note-threshold* 1
  #!+sb-doc
  "This variable is a non-negative integer specifying the lowest level of
  event that will print a note when it occurs.")

;;; Increment the counter and do any action. Mumble about the event if
;;; policy indicates.
(defmacro event (name &optional node)
  #!+sb-doc
  "Event Name Node
  Note that the event with the specified Name has happened. Node is evaluated
  to determine the node to which the event happened."
  `(%event ,(event-info-var (event-info-or-lose name)) ,node))

(declaim (ftype (function (&optional unsigned-byte stream) (values)) event-statistics))
(defun event-statistics (&optional (min-count 1) (stream *standard-output*))
  #!+sb-doc
  "Print a listing of events and their counts, sorted by the count. Events
  that happened fewer than Min-Count times will not be printed. Stream is the
  stream to write to."
  (collect ((info))
    (maphash #'(lambda (k v)
		 (declare (ignore k))
		 (when (>= (event-info-count v) min-count)
		   (info v)))
	     *event-info*)
    (dolist (event (sort (info) #'> :key #'event-info-count))
      (format stream "~6D: ~A~%" (event-info-count event)
	      (event-info-description event)))
    (values))
  (values))

(declaim (ftype (function nil (values)) clear-event-statistics))
(defun clear-event-statistics ()
  (maphash #'(lambda (k v)
	       (declare (ignore k))
	       (setf (event-info-count v) 0))
	   *event-info*)
  (values))

;;;; functions on directly-linked lists (linked through specialized
;;;; NEXT operations)

#!-sb-fluid (declaim (inline find-in position-in map-in))

(defun find-in (next
		element
		list
		&key
		(key #'identity)
		(test #'eql test-p)
		(test-not nil not-p))
  #!+sb-doc
  "Find Element in a null-terminated List linked by the accessor function
  Next. Key, Test and Test-Not are the same as for generic sequence
  functions."
  (when (and test-p not-p)
    (error "It's silly to supply both :TEST and :TEST-NOT arguments."))
  (if not-p
      (do ((current list (funcall next current)))
	  ((null current) nil)
	(unless (funcall test-not (funcall key current) element)
	  (return current)))
      (do ((current list (funcall next current)))
	  ((null current) nil)
	(when (funcall test (funcall key current) element)
	  (return current)))))

(defun position-in (next
		    element
		    list
		    &key
		    (key #'identity)
		    (test #'eql test-p)
		    (test-not nil not-p))
  #!+sb-doc
  "Return the position of Element (or NIL if absent) in a null-terminated List
  linked by the accessor function Next. Key, Test and Test-Not are the same as
  for generic sequence functions."
  (when (and test-p not-p)
    (error "It's silly to supply both :TEST and :TEST-NOT arguments."))
  (if not-p
      (do ((current list (funcall next current))
	   (i 0 (1+ i)))
	  ((null current) nil)
	(unless (funcall test-not (funcall key current) element)
	  (return i)))
      (do ((current list (funcall next current))
	   (i 0 (1+ i)))
	  ((null current) nil)
	(when (funcall test (funcall key current) element)
	  (return i)))))

(defun map-in (next function list)
  #!+sb-doc
  "Map Function over the elements in a null-terminated List linked by the
  accessor function Next, returning a list of the results."
  (collect ((res))
    (do ((current list (funcall next current)))
	((null current))
      (res (funcall function current)))
    (res)))

;;; KLUDGE: This is expanded out twice, by cut-and-paste, in a
;;;   (DEF!MACRO FOO (..) .. CL:GET-SETF-EXPANSION ..)
;;;   #+SB-XC-HOST
;;;   (SB!XC:DEFMACRO FOO (..) .. SB!XC:GET-SETF-EXPANSION ..)
;;; arrangement, in order to get it to work in cross-compilation. This
;;; duplication should be removed, perhaps by rewriting the macro in a more
;;; cross-compiler-friendly way, or perhaps just by using some (MACROLET ((FROB
;;; ..)) .. FROB .. FROB) form, or perhaps by completely eliminating this macro
;;; and its partner PUSH-IN, but I don't want to do it now, because the system
;;; isn't running yet, so it'd be too hard to check that my changes were
;;; correct -- WHN 19990806
(def!macro deletef-in (next place item &environment env)
  (multiple-value-bind (temps vals stores store access)
      (get-setf-expansion place env)
    (when (cdr stores)
      (error "multiple store variables for ~S" place))
    (let ((n-item (gensym))
	  (n-place (gensym))
	  (n-current (gensym))
	  (n-prev (gensym)))
      `(let* (,@(mapcar #'list temps vals)
	      (,n-place ,access)
	      (,n-item ,item))
	 (if (eq ,n-place ,n-item)
	     (let ((,(first stores) (,next ,n-place)))
	       ,store)
	     (do ((,n-prev ,n-place ,n-current)
		  (,n-current (,next ,n-place)
			      (,next ,n-current)))
		 ((eq ,n-current ,n-item)
		  (setf (,next ,n-prev)
			(,next ,n-current)))))
	 (values)))))
;;; #+SB-XC-HOST SB!XC:DEFMACRO version is in late-macros.lisp. -- WHN 19990806

;;; KLUDGE: This is expanded out twice, by cut-and-paste, in a
;;;   (DEF!MACRO FOO (..) .. CL:GET-SETF-EXPANSION ..)
;;;   #+SB-XC-HOST
;;;   (SB!XC:DEFMACRO FOO (..) .. SB!XC:GET-SETF-EXPANSION ..)
;;; arrangement, in order to get it to work in cross-compilation. This
;;; duplication should be removed, perhaps by rewriting the macro in a more
;;; cross-compiler-friendly way, or perhaps just by using some (MACROLET ((FROB
;;; ..)) .. FROB .. FROB) form, or perhaps by completely eliminating this macro
;;; and its partner DELETEF-IN, but I don't want to do it now, because the
;;; system isn't running yet, so it'd be too hard to check that my changes were
;;; correct -- WHN 19990806
(def!macro push-in (next item place &environment env)
  #!+sb-doc
  "Push Item onto a list linked by the accessor function Next that is stored in
  Place."
  (multiple-value-bind (temps vals stores store access)
      (get-setf-expansion place env)
    (when (cdr stores)
      (error "multiple store variables for ~S" place))
    `(let (,@(mapcar #'list temps vals)
	   (,(first stores) ,item))
       (setf (,next ,(first stores)) ,access)
       ,store
       (values))))
;;; #+SB-XC-HOST SB!XC:DEFMACRO version is in late-macros.lisp. -- WHN 19990806

(defmacro position-or-lose (&rest args)
  `(or (position ,@args)
       (error "Shouldn't happen?")))

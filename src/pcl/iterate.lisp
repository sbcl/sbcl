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

(in-package "SB-ITERATE")

(sb-int:file-comment
  "$Header$")

;;; Are warnings to be issued for iterate/gather forms that aren't optimized?
;;;   NIL   => never
;;;   :USER => those resulting from user code
;;;   T     => always, even if it's the iteration macro that's suboptimal.
(defvar *iterate-warnings* :any)

;;; ITERATE macro
(defmacro iterate (clauses &body body &environment env)
  (optimize-iterate-form clauses body env))

(defun
 simple-expand-iterate-form
 (clauses body)

 ;; Expand ITERATE. This is the "formal semantics" expansion, which we never
 ;; use.
 (let*
  ((block-name (gensym))
   (bound-var-lists (mapcar #'(lambda (clause)
				     (let ((names (first clause)))
					  (if (listp names)
					      names
					      (list names))))
			   clauses))
   (generator-vars (mapcar #'(lambda (clause)
				    (declare (ignore clause))
				    (gensym))
			  clauses)))
  `(block ,block-name
       (let*
	,(mapcan #'(lambda (gvar clause var-list)
		     ;; For each clause, bind a generator temp to the clause,
		     ;; then bind the specified var(s).
		     (cons (list gvar (second clause))
			   (copy-list var-list)))
		generator-vars clauses bound-var-lists)

	;; Note bug in formal semantics: there can be declarations in the head
	;; of BODY; they go here, rather than inside loop.
	(loop
	 ,@(mapcar
	    #'(lambda (var-list gen-var)
		;; Set each bound variable (or set of vars) to the result of
		;; calling the corresponding generator.
		`(multiple-value-setq ,var-list
		   (funcall ,gen-var #'(lambda nil (return-from
						       ,block-name)))))
	    bound-var-lists generator-vars)
	 ,@body)))))

;;; temporary variable names used by ITERATE expansions
(defparameter *iterate-temp-vars-list*
  '(iterate-temp-1 iterate-temp-2 iterate-temp-3 iterate-temp-4
    iterate-temp-5 iterate-temp-6 iterate-temp-7 iterate-temp-8))

(defun
 optimize-iterate-form
 (clauses body iterate-env)
 (let*
  ((temp-vars *iterate-temp-vars-list*)
   (block-name (gensym))
   (finish-form `(return-from ,block-name))
   (bound-vars (mapcan #'(lambda (clause)
				(let ((names (first clause)))
				     (if (listp names)
					 (copy-list names)
					 (list names))))
		      clauses))
   iterate-decls generator-decls update-forms bindings leftover-body)
  (do ((tail bound-vars (cdr tail)))
      ((null tail))
    ;; Check for duplicates
    (when (member (car tail)
		 (cdr tail))
	(warn "Variable appears more than once in ITERATE: ~S" (car tail))))
  (flet
   ((get-iterate-temp nil

	   ;; Make temporary var. Note that it is ok to re-use these symbols
	   ;; in each iterate, because they are not used within BODY.
	   (or (pop temp-vars)
	       (gensym))))
   (dolist (clause clauses)
       (cond
	((or (not (consp clause))
	     (not (consp (cdr clause))))
	 (warn "bad syntax in ITERATE: clause not of form (var iterator): ~S"
	       clause))
	(t
	 (unless (null (cddr clause))
		(warn
       "probable parenthesis error in ITERATE clause--more than 2 elements: ~S"
		      clause))
	 (multiple-value-bind
	  (let-body binding-type let-bindings localdecls otherdecls extra-body)
	  (expand-into-let (second clause)
		 'iterate iterate-env)

	  ;; We have expanded the generator clause and parsed it into
	  ;; its LET pieces.
	  (prog*
	   ((vars (first clause))
	    gen-args renamed-vars)
	   (setq vars (if (listp vars)
			  (copy-list vars)
			  (list vars)))
					       ; VARS is now a (fresh) list of
					       ; all iteration vars bound in
					       ; this clause
	   (cond
	    ((eq let-body :abort)
					       ; Already issued a warning
					       ; about malformedness
	     )
	    ((null (setq let-body (function-lambda-p let-body 1)))
					       ; Not of the expected form
	     (let ((generator (second clause)))
		  (cond ((and (consp generator)
			      (fboundp (car generator)))
					       ; It looks ok--a macro or
					       ; function here--so the guy who
					       ; wrote it just didn't do it in
					       ; an optimizable way
			 (maybe-warn :definition "could not optimize iterate clause ~S because generator not of form (LET[*] ... (FUNCTION (LAMBDA (finish) ...)))"
				generator))
			(t		     ; Perhaps it's just a
					       ; misspelling?  Probably user
					       ; error
			   (maybe-warn :user
				"Iterate operator in clause ~S is not fboundp."
				generator)))
		  (setq let-body :abort)))
	    (t

	     ;; We have something of the form #'(LAMBDA (finisharg) ...),
	     ;; possibly with some LET bindings around it. LET-BODY =
	     ;; ((finisharg) ...).
	     (setq let-body (cdr let-body))
	     (setq gen-args (pop let-body))
	     (when let-bindings

		 ;; The first transformation we want to perform is
		 ;; "LET-eversion": turn (let* ((generator (let (..bindings..)
		 ;; #'(lambda ...)))) ..body..) into (let* (..bindings..
		 ;; (generator #'(lambda ...))) ..body..). This
		 ;; transformation is valid if nothing in body refers to any
		 ;; of the bindings, something we can ensure by
		 ;; alpha-converting the inner let (substituting new names for
		 ;; each var). Of course, none of those vars can be special,
		 ;; but we already checked for that above.
		 (multiple-value-setq (let-bindings renamed-vars)
			(rename-let-bindings let-bindings binding-type
			       iterate-env leftover-body #'get-iterate-temp))
		 (setq leftover-body nil)
					       ; If there was any leftover
					       ; from previous, it is now
					       ; consumed.
		 )

	     ;; The second transformation is substituting the body of the
	     ;; generator (LAMBDA (finish-arg) . gen-body) for its appearance
	     ;; in the update form (funcall generator #'(lambda ()
	     ;; finish-form)), then simplifying that form. The requirement
	     ;; for this part is that the generator body not refer to any
	     ;; variables that are bound between the generator binding and the
	     ;; appearance in the loop body. The only variables bound in that
	     ;; interval are generator temporaries, which have unique names so
	     ;; are no problem, and the iteration variables remaining for
	     ;; subsequent clauses. We'll discover the story as we walk the
	     ;; body.
	     (multiple-value-bind (finishdecl other rest)
		 (parse-declarations let-body gen-args)
	      (declare (ignore finishdecl))
					       ; Pull out declares, if any,
					       ; separating out the one(s)
					       ; referring to the finish arg,
					       ; which we will throw away.
	      (when other
					       ; Combine remaining decls with
					       ; decls extracted from the LET,
					       ; if any.
		  (setq otherdecls (nconc otherdecls other)))
	      (setq let-body (cond
			      (otherdecls
					       ; There are interesting
					       ; declarations, so have to keep
					       ; it wrapped.
			       `(let nil (declare ,@otherdecls)
				     ,@rest))
			      ((null (cdr rest))
					       ; Only one form left
			       (first rest))
			      (t `(progn ,@rest)))))
	     (unless (eq (setq let-body (iterate-transform-body let-body
					       iterate-env renamed-vars
					       (first gen-args)
					       finish-form bound-vars clause))
			 :abort)

		 ;; Skip the rest if transformation failed. Warning has
		 ;; already been issued.

		 ;; Note possible further optimization: if LET-BODY expanded
		 ;; into (prog1 oldvalue prepare-for-next-iteration), as so
		 ;; many do, then we could in most cases split the PROG1 into
		 ;; two pieces: do the (setq var oldvalue) here, and do the
		 ;; prepare-for-next-iteration at the bottom of the loop.
		 ;; This does a slight optimization of the PROG1 and also
		 ;; rearranges the code in a way that a reasonably clever
		 ;; compiler might detect how to get rid of redundant
		 ;; variables altogether (such as happens with INTERVAL and
		 ;; LIST-TAILS); that would make the whole thing closer to
		 ;; what you might have coded by hand. However, to do this
		 ;; optimization, we need to ensure that (a) the
		 ;; prepare-for-next-iteration refers freely to no vars other
		 ;; than the internal vars we have extracted from the LET, and
		 ;; (b) that the code has no side effects. These are both
		 ;; true for all the iterators defined by this module, but how
		 ;; shall we represent side-effect info and/or tap into the
		 ;; compiler's knowledge of same?
		 (when localdecls
					       ; There were declarations for
					       ; the generator locals--have to
					       ; keep them for later, and
					       ; rename the vars mentioned
		     (setq
		      generator-decls
		      (nconc
		       generator-decls
		       (mapcar
			#'(lambda
			   (decl)
			   (let ((head (car decl)))
				(cons head (if (eq head 'type)
					       (cons (second decl)
						     (sublis renamed-vars
							    (cddr decl)))
					       (sublis renamed-vars
						      (cdr decl))))))
			localdecls)))))))

	   ;; Finished analyzing clause now. LET-BODY is the form which, when
	   ;; evaluated, returns updated values for the iteration variable(s)
	   ;; VARS.
	   (when (eq let-body :abort)

	       ;; Some punt case: go with the formal semantics: bind a var to
	       ;; the generator, then call it in the update section
	       (let
		((gvar (get-iterate-temp))
		 (generator (second clause)))
		(setq
		 let-bindings
		 (list (list gvar
			     (cond
			      ;; FIXME: This conditional was here with this
			      ;; comment in old CMU CL PCL. Does Python really
			      ;; think it's unreachable?
			      ;;#-cmu ; Python thinks this is unreachable.
			      (leftover-body
					       ; Have to use this up
			       `(progn ,@(prog1 leftover-body (setq
								  leftover-body
								    nil))
				       generator))
			      (t generator)))))
		(setq let-body `(funcall ,gvar #'(lambda nil ,finish-form)))))
	   (push (mv-setq (copy-list vars)
			let-body)
		 update-forms)
	   (dolist (v vars)
	     (declare (ignore v))
	     ;; Pop off the vars we have now bound from the list of vars to
	     ;; watch out for -- we'll bind them right now.
	     (pop bound-vars))
	   (setq bindings
		 (nconc bindings let-bindings
			(cond (extra-body
			       ;; There was some computation to do after the
			       ;; bindings--here's our chance.
			       (cons (list (first vars)
					   `(progn ,@extra-body nil))
				     (rest vars)))
			      (t vars))))))))))
  (do ((tail body (cdr tail)))
      ((not (and (consp tail)
		 (consp (car tail))
		 (eq (caar tail)
		     'declare)))

       ;; TAIL now points at first non-declaration. If there were
       ;; declarations, pop them off so they appear in the right place
       (unless (eq tail body)
	   (setq iterate-decls (ldiff body tail))
	   (setq body tail))))
  `(block ,block-name
       (let* ,bindings ,@(and generator-decls
			      `((declare ,@generator-decls)))
	     ,@iterate-decls
	     ,@leftover-body
	     (loop ,@(nreverse update-forms)
		   ,@body)))))

(defun expand-into-let (clause parent-name env)

       ;; Return values: Body, LET[*], bindings, localdecls, otherdecls, extra
       ;; body, where BODY is a single form. If multiple forms in a LET, the
       ;; preceding forms are returned as extra body. Returns :ABORT if it
       ;; issued a punt warning.
       (prog ((expansion clause)
	      expandedp binding-type let-bindings let-body)
	     expand
	     (multiple-value-setq (expansion expandedp)
		    (macroexpand-1 expansion env))
	     (cond ((not (consp expansion))
					       ; Shouldn't happen
		    )
		   ((symbolp (setq binding-type (first expansion)))
		    (case binding-type
			((let let*)
			   (setq let-bindings (second expansion))
					       ; List of variable bindings
			   (setq let-body (cddr expansion))
			   (go handle-let))))
		   ((and (consp binding-type)
			 (eq (car binding-type)
			     'lambda)
			 (not (find-if #'(lambda (x)
						(member x lambda-list-keywords)
						)
				     (setq let-bindings (second binding-type)))
			      )
			 (eql (length (second expansion))
			      (length let-bindings))
			 (null (cddr expansion)))
					       ; A simple LAMBDA form can be
					       ; treated as LET
		    (setq let-body (cddr binding-type))
		    (setq let-bindings (mapcar #'list let-bindings (second
								    expansion))
			  )
		    (setq binding-type 'let)
		    (go handle-let)))

	     ;; Fall thru if not a LET
	     (cond (expandedp		  ; try expanding again
			  (go expand))
		   (t			  ; Boring--return form as the
					       ; body
		      (return expansion)))
	     handle-let
	     (return (let ((locals (variables-from-let let-bindings))
			   extra-body specials)
			  (multiple-value-bind (localdecls otherdecls let-body)
			      (parse-declarations let-body locals)
			   (cond ((setq specials (extract-special-bindings
						  locals localdecls))
				  (maybe-warn (cond ((find-if #'variable-globally-special-p
							    specials)
					       ; This could be the fault of a
					       ; user proclamation.
						     :user)
						    (t :definition))

	  "Couldn't optimize ~S because expansion of ~S binds specials ~(~S ~)"
					 parent-name clause specials)
				  :abort)
				 (t (values (cond ((not (consp let-body))

					       ; Null body of LET?  unlikely,
					       ; but someone else will likely
					       ; complain
						   nil)
						  ((null (cdr let-body))

					       ; A single expression, which we
					       ; hope is (function
					       ; (lambda...))
						   (first let-body))
						  (t

			  ;; More than one expression. These are forms to
			  ;; evaluate after the bindings but before the
			  ;; generator form is returned. Save them to
			  ;; evaluate in the next convenient place. Note that
			  ;; this is ok, as there is no construct that can
			  ;; cause a LET to return prematurely (without
			  ;; returning also from some surrounding construct).
						     (setq extra-body
							   (butlast let-body))
						     (car (last let-body))))
					   binding-type let-bindings localdecls
					   otherdecls extra-body))))))))

(defun variables-from-let (bindings)

       ;; Return a list of the variables bound in the first argument to LET[*].
       (mapcar #'(lambda (binding)
			(if (consp binding)
			    (first binding)
			    binding))
	      bindings))

(defun iterate-transform-body (let-body iterate-env renamed-vars finish-arg
				     finish-form bound-vars clause)

;;; This is the second major transformation for a single iterate clause.
;;; LET-BODY is the body of the iterator after we have extracted its local
;;; variables and declarations. We have two main tasks: (1) Substitute
;;; internal temporaries for occurrences of the LET variables; the alist
;;; RENAMED-VARS specifies this transformation. (2) Substitute evaluation of
;;; FINISH-FORM for any occurrence of (funcall FINISH-ARG). Along the way, we
;;; check for forms that would invalidate these transformations: occurrence of
;;; FINISH-ARG outside of a funcall, and free reference to any element of
;;; BOUND-VARS. CLAUSE & TYPE are the original ITERATE clause and its type
;;; (ITERATE or ITERATE*), for purpose of error messages. On success, we
;;; return the transformed body; on failure, :ABORT.

       (walk-form let-body iterate-env
	      #'(lambda (form context env)
		       (declare (ignore context))

		       ;; Need to substitute RENAMED-VARS, as well as turn
		       ;; (FUNCALL finish-arg) into the finish form
		       (cond ((symbolp form)
			      (let (renaming)
				   (cond ((and (eq form finish-arg)
					       (variable-same-p form env
						      iterate-env))
					       ; An occurrence of the finish
					       ; arg outside of FUNCALL
					       ; context--I can't handle this
					  (maybe-warn :definition "Couldn't optimize iterate form because generator ~S does something with its FINISH arg besides FUNCALL it."
						 (second clause))
					  (return-from iterate-transform-body
						 :abort))
					 ((and (setq renaming (assoc form
								   renamed-vars
								     ))
					       (variable-same-p form env
						      iterate-env))
					       ; Reference to one of the vars
					       ; we're renaming
					  (cdr renaming))
					 ((and (member form bound-vars)
					       (variable-same-p form env
						      iterate-env))
					       ; FORM is a var that is bound
					       ; in this same ITERATE, or
					       ; bound later in this ITERATE*.
					       ; This is a conflict.
					  (maybe-warn :user "Couldn't optimize iterate form because generator ~S is closed over ~S, in conflict with a subsequent iteration variable."
						 (second clause)
						 form)
					  (return-from iterate-transform-body
						 :abort))
					 (t form))))
			     ((and (consp form)
				   (eq (first form)
				       'funcall)
				   (eq (second form)
				       finish-arg)
				   (variable-same-p (second form)
					  env iterate-env))
					       ; (FUNCALL finish-arg) =>
					       ; finish-form
			      (unless (null (cddr form))
				  (maybe-warn :definition
	"Generator for ~S applied its finish arg to > 0 arguments ~S--ignored."
					 (second clause)
					 (cddr form)))
			      finish-form)
			     (t form)))))

(defun
 parse-declarations
 (tail locals)

 ;; Extract the declarations from the head of TAIL and divide them into 2
 ;; classes: declares about variables in the list LOCALS, and all other
 ;; declarations. Returns 3 values: those 2 lists plus the remainder of TAIL.
 (let
  (localdecls otherdecls form)
  (loop
   (unless (and tail (consp (setq form (car tail)))
		(eq (car form)
		    'declare))
       (return (values localdecls otherdecls tail)))
   (mapc
    #'(lambda
       (decl)
       (case (first decl)
	   ((inline notinline optimize)
					       ; These don't talk about vars
	      (push decl otherdecls))
	   (t				  ; Assume all other kinds are
					       ; for vars
	      (let* ((vars (if (eq (first decl)
				   'type)
			       (cddr decl)
			       (cdr decl)))
		     (l (intersection locals vars))
		     other)
		    (cond
		     ((null l)
					       ; None talk about LOCALS
		      (push decl otherdecls))
		     ((null (setq other (set-difference vars l)))
					       ; All talk about LOCALS
		      (push decl localdecls))
		     (t			; Some of each
			(let ((head (cons 'type (and (eq (first decl)
							 'type)
						     (list (second decl))))))
			     (push (append head other)
				   otherdecls)
			     (push (append head l)
				   localdecls))))))))
    (cdr form))
   (pop tail))))

(defun extract-special-bindings (vars decls)

       ;; Return the subset of VARS that are special, either globally or
       ;; because of a declaration in DECLS
       (let ((specials (remove-if-not #'variable-globally-special-p vars)))
	    (dolist (d decls)
		(when (eq (car d)
			  'special)
		    (setq specials (union specials (intersection vars
							  (cdr d))))))
	    specials))

(defun function-lambda-p (form &optional nargs)

       ;; If FORM is #'(LAMBDA bindings . body) and bindings is of length
       ;; NARGS, return the lambda expression
       (let (args body)
	    (and (consp form)
		 (eq (car form)
		     'function)
		 (consp (setq form (cdr form)))
		 (null (cdr form))
		 (consp (setq form (car form)))
		 (eq (car form)
		     'lambda)
		 (consp (setq body (cdr form)))
		 (listp (setq args (car body)))
		 (or (null nargs)
		     (eql (length args)
			  nargs))
		 form)))

(defun
 rename-let-bindings
 (let-bindings binding-type env leftover-body &optional tempvarfn)

 ;; Perform the alpha conversion required for "LET eversion" of
 ;; (LET[*] LET-BINDINGS . body)--rename each of the variables to an
 ;; internal name. Returns 2 values: a new set of LET bindings and the
 ;; alist of old var names to new (so caller can walk the body doing
 ;; the rest of the renaming). BINDING-TYPE is one of LET or LET*.
 ;; LEFTOVER-BODY is optional list of forms that must be eval'ed
 ;; before the first binding happens. ENV is the macro expansion
 ;; environment, in case we have to walk a LET*. TEMPVARFN is a
 ;; function of no args to return a temporary var; if omitted, we use
 ;; GENSYM.
 (let
  (renamed-vars)
  (values (mapcar #'(lambda (binding)
			   (let ((valueform (cond ((not (consp binding))

					       ; No initial value
						   nil)
						  ((or (eq binding-type
							   'let)
						       (null renamed-vars))

					       ; All bindings are in parallel,
					       ; so none can refer to others
						   (second binding))
						  (t
					       ; In a LET*, have to substitute
					       ; vars in the 2nd and
					       ; subsequent initialization
					       ; forms
						     (rename-variables
						      (second binding)
						      renamed-vars env))))
				 (newvar (if tempvarfn
					     (funcall tempvarfn)
					     (gensym))))
				(push (cons (if (consp binding)
						(first binding)
						binding)
					    newvar)
				      renamed-vars)
					       ; Add new variable to the list
					       ; AFTER we have walked the
					       ; initial value form
				(when leftover-body
				  ;; Previous clause had some computation to do
				  ;; after its bindings. Here is the first
				  ;; opportunity to do it
				  (setq valueform `(progn ,@leftover-body
							  ,valueform))
				  (setq leftover-body nil))
				(list newvar valueform)))
		 let-bindings)
	 renamed-vars)))

(defun rename-variables (form alist env)

       ;; Walks FORM, renaming occurrences of the key variables in ALIST with
       ;; their corresponding values. ENV is FORM's environment, so we can
       ;; make sure we are talking about the same variables.
       (walk-form form env
	      #'(lambda (form context subenv)
		       (declare (ignore context))
		       (let (pair)
			    (cond ((and (symbolp form)
					(setq pair (assoc form alist))
					(variable-same-p form subenv env))
				   (cdr pair))
				  (t form))))))

(defun
 mv-setq
 (vars expr)

 ;; Produces (MULTIPLE-VALUE-SETQ vars expr), except that I'll optimize some
 ;; of the simple cases for benefit of compilers that don't, and I don't care
 ;; what the value is, and I know that the variables need not be set in
 ;; parallel, since they can't be used free in EXPR
 (cond
  ((null vars)
					       ; EXPR is a side-effect
   expr)
  ((not (consp vars))
					       ; This is an error, but I'll
					       ; let MULTIPLE-VALUE-SETQ
					       ; report it
   `(multiple-value-setq ,vars ,expr))
  ((and (listp expr)
	(eq (car expr)
	    'values))

   ;; (mv-setq (a b c) (values x y z)) can be reduced to a parallel setq
   ;; (psetq returns nil, but I don't care about returned value). Do this
   ;; even for the single variable case so that we catch (mv-setq (a) (values
   ;; x y))
   (pop expr)
					       ; VALUES
   `(setq ,@(mapcon #'(lambda (tail)
			     (list (car tail)
				   (cond ((or (cdr tail)
					      (null (cdr expr)))
					       ; One result expression for
					       ; this var
					  (pop expr))
					 (t    ; More expressions than vars,
					       ; so arrange to evaluate all
					       ; the rest now.
					    (cons 'prog1 expr)))))
		   vars)))
  ((null (cdr vars))
					       ; Simple one variable case
   `(setq ,(car vars)
	  ,expr))
  (t					   ; General case--I know nothing
     `(multiple-value-setq ,vars ,expr))))

(defun variable-same-p (var env1 env2)
       (eq (variable-lexical-p var env1)
	   (variable-lexical-p var env2)))

(defun maybe-warn (type &rest warn-args)

       ;; Issue a warning about not being able to optimize this thing. TYPE
       ;; is one of :DEFINITION, meaning the definition is at fault, and
       ;; :USER, meaning the user's code is at fault.
       (when (case *iterate-warnings*
		 ((nil) nil)
		 ((:user) (eq type :user))
		 (t t))
	   (apply #'warn warn-args)))

;;; sample iterators
;;;
;;; FIXME: If they're only samples, can they be commented out?

(defmacro
 interval
 (&whole whole &key from downfrom to downto above below by type)
 (cond
  ((and from downfrom)
   (error "Can't use both FROM and DOWNFROM in ~S" whole))
  ((cdr (remove nil (list to downto above below)))
   (error "Can't use more than one limit keyword in ~S" whole))
  (t
   (let*
    ((down (or downfrom downto above))
     (limit (or to downto above below))
     (inc (cond ((null by)
		 1)
		((constantp by)
					       ; Can inline this increment
		 by))))
    `(let
      ((from ,(or from downfrom 0))
       ,@(and limit `((to ,limit)))
       ,@(and (null inc)
	      `((by ,by))))
      ,@(and type `((declare (type ,type from ,@(and limit '(to))
				   ,@(and (null inc)
					  `(by))))))
      #'(lambda
	 (finish)
	 ,@(cond ((null limit)
					       ; We won't use the FINISH arg.
		  '((declare (ignore finish)))))
	 (prog1 ,(cond (limit		  ; Test the limit. If ok,
					       ; return current value and
					       ; increment, else quit
			      `(if (,(cond (above '>)
					   (below '<)
					   (down '>=)
					   (t '<=))
				    from to)
				   from
				   (funcall finish)))
		       (t		      ; No test
			  'from))
	     (setq from (,(if down
			      '-
			      '+)
			 from
			 ,(or inc 'by))))))))))

(defmacro list-elements (list &key (by '#'cdr))
       `(let ((tail ,list))
	     #'(lambda (finish)
		      (prog1 (if (endp tail)
				 (funcall finish)
				 (first tail))
			  (setq tail (funcall ,by tail))))))

(defmacro list-tails (list &key (by '#'cdr))
       `(let ((tail ,list))
	     #'(lambda (finish)
		      (prog1 (if (endp tail)
				 (funcall finish)
				 tail)
			  (setq tail (funcall ,by tail))))))

(defmacro
 elements
 (sequence)
 "Generates successive elements of SEQUENCE, with second value being the index. Use (ELEMENTS (THE type arg)) if you care about the type."
 (let*
  ((type (and (consp sequence)
	      (eq (first sequence)
		  'the)
	      (second sequence)))
   (accessor (if type
		 (sequence-accessor type)
		 'elt))
   (listp (eq type 'list)))

  ;; If type is given via THE, we may be able to generate a good accessor here
  ;; for the benefit of implementations that aren't smart about (ELT (THE
  ;; STRING FOO)). I'm not bothering to keep the THE inside the body,
  ;; however, since I assume any compiler that would understand (AREF (THE
  ;; SIMPLE-ARRAY S)) would also understand that (AREF S) is the same when I
  ;; bound S to (THE SIMPLE-ARRAY foo) and never modified it.

  ;; If sequence is declared to be a list, it's better to cdr down it, so we
  ;; have some extra cases here. Normally folks would write LIST-ELEMENTS,
  ;; but maybe they wanted to get the index for free...
  `(let* ((index 0)
	  (s ,sequence)
	  ,@(and (not listp)
		 '((size (length s)))))
	 #'(lambda (finish)
		  (values (cond ,(if listp
				     '((not (endp s))
				       (pop s))
				     `((< index size)
				       (,accessor s index)))
				(t (funcall finish)))
			 (prog1 index
			     (setq index (1+ index))))))))

(defmacro
 plist-elements
 (plist)
 "Generates each time 2 items, the indicator and the value."
 `(let ((tail ,plist))
       #'(lambda (finish)
		(values (if (endp tail)
			    (funcall finish)
			    (first tail))
		       (prog1 (if (endp (setq tail (cdr tail)))
				  (funcall finish)
				  (first tail))
			   (setq tail (cdr tail)))))))

(defun sequence-accessor (type)

       ;; returns the function with which most efficiently to make accesses to
       ;; a sequence of type TYPE.
       (case (if (consp type)
					       ; e.g., (VECTOR FLOAT *)
		 (car type)
		 type)
	   ((array simple-array vector) 'aref)
	   (simple-vector 'svref)
	   (string 'char)
	   (simple-string 'schar)
	   (bit-vector 'bit)
	   (simple-bit-vector 'sbit)
	   (t 'elt)))

;; These "iterators" may be withdrawn

(defmacro eachtime (expr)
       `#'(lambda (finish)
		 (declare (ignore finish))
		 ,expr))

(defmacro while (expr)
       `#'(lambda (finish)
		 (unless ,expr (funcall finish))))

(defmacro until (expr)
       `#'(lambda (finish)
		 (when ,expr (funcall finish))))

					       ; GATHERING macro

(defmacro gathering (clauses &body body &environment env)
       (or (optimize-gathering-form clauses body env)
	   (simple-expand-gathering-form clauses body env)))

(defmacro with-gathering (clauses gather-body &body use-body)
       "Binds the variables specified in CLAUSES to the result of (GATHERING clauses gather-body) and evaluates the forms in USE-BODY inside that contour."

       ;; We may optimize this a little better later for those compilers that
       ;; don't do a good job on (m-v-bind vars (... (values ...)) ...).
       `(multiple-value-bind ,(mapcar #'car clauses)
	       (gathering ,clauses ,gather-body)
	       ,@use-body))

(defun
 simple-expand-gathering-form
 (clauses body env)
 (declare (ignore env))

 ;; The "formal semantics" of GATHERING. We use this only in cases that can't
 ;; be optimized.
 (let
  ((acc-names (mapcar #'first (if (symbolp clauses)
					       ; Shorthand using anonymous
					       ; gathering site
				  (setq clauses `((*anonymous-gathering-site*
						   (,clauses))))
				  clauses)))
   (realizer-names (mapcar #'(lambda (binding)
				    (declare (ignore binding))
				    (gensym))
			  clauses)))
  `(multiple-value-call
    #'(lambda
       ,(mapcan #'list acc-names realizer-names)
       (flet ((gather (value &optional (accumulator *anonymous-gathering-site*)
			     )
		     (funcall accumulator value)))
	     ,@body
	     (values ,@(mapcar #'(lambda (rname)
					`(funcall ,rname))
			      realizer-names))))
    ,@(mapcar #'second clauses))))

(defvar *active-gatherers* nil
       "List of GATHERING bindings currently active during macro expansion)")

(defvar *anonymous-gathering-site* nil "Variable used in formal expansion of an abbreviated GATHERING form (one with anonymous gathering site).")

(defun optimize-gathering-form (clauses body gathering-env)
 (let*
  (acc-info leftover-body top-bindings finish-forms top-decls)
  (dolist (clause (if (symbolp clauses)
					       ; a shorthand
		      `((*anonymous-gathering-site* (,clauses)))
		      clauses))
      (multiple-value-bind
       (let-body binding-type let-bindings localdecls otherdecls extra-body)
       (expand-into-let (second clause)
	      'gathering gathering-env)
       (prog*
	((acc-var (first clause))
	 renamed-vars accumulator realizer)
	(when (and (consp let-body)
		   (eq (car let-body)
		       'values)
		   (consp (setq let-body (cdr let-body)))
		   (setq accumulator (function-lambda-p (car let-body)))
		   (consp (setq let-body (cdr let-body)))
		   (setq realizer (function-lambda-p (car let-body)
					 0))
		   (null (cdr let-body)))

	    ;; Macro returned something of the form
	    ;;   (VALUES #'(lambda (value) ...)
	    ;;	   #'(lambda () ...)),
	    ;; a function to accumulate values and a function to realize the
	    ;; result.
	    (when binding-type

		;; Gatherer expanded into a LET
		(cond (otherdecls (maybe-warn :definition "Couldn't optimize GATHERING clause ~S because its expansion carries declarations about more than the bound variables: ~S"
					 (second clause)
					 `(declare ,@otherdecls))
			     (go punt)))
		(when let-bindings

		    ;; The first transformation we want to perform is a
		    ;; variant of "LET-eversion": turn
		    ;;   (mv-bind
		    ;;       (acc real)
		    ;;       (let (..bindings..)
		    ;;	 (values #'(lambda ...)
		    ;;		 #'(lambda ...)))
		    ;;     ..body..)
		    ;; into
		    ;;   (let* (..bindings..
		    ;;	  (acc #'(lambda ...))
		    ;;	  (real #'(lambda ...)))
		    ;;     ..body..).
		    ;; This transformation is valid if nothing in body refers
		    ;; to any of the bindings, something we can ensure by
		    ;; alpha-converting the inner let (substituting new names
		    ;; for each var). Of course, none of those vars can be
		    ;; special, but we already checked for that above.
		    (multiple-value-setq (let-bindings renamed-vars)
			   (rename-let-bindings let-bindings binding-type
				  gathering-env leftover-body))
		    (setq top-bindings (nconc top-bindings let-bindings))
		    (setq leftover-body nil)
					       ; If there was any leftover
					       ; from previous, it is now
					       ; consumed
		    ))
	    (setq leftover-body (nconc leftover-body extra-body))
					       ; Computation to do after these
					       ; bindings
	    (push (cons acc-var (rename-and-capture-variables accumulator
				       renamed-vars gathering-env))
		  acc-info)
	    (setq realizer (rename-variables realizer renamed-vars
				  gathering-env))
	    (push (cond ((null (cdddr realizer))
					       ; Simple (LAMBDA () expr) =>
					       ; expr
			 (third realizer))
			(t		     ; There could be declarations
					       ; or something, so leave as a
					       ; LET
			   (cons 'let (cdr realizer))))
		  finish-forms)
	    (unless (null localdecls)
					       ; Declarations about the LET
					       ; variables also has to
					       ; percolate up
		(setq top-decls (nconc top-decls (sublis renamed-vars
							localdecls))))
	    (return))
	(maybe-warn :definition "Couldn't optimize GATHERING clause ~S because its expansion is not of the form (VALUES #'(LAMBDA ...) #'(LAMBDA () ...))"
	       (second clause))
	punt
	(let
	 ((gs (gensym))
	  (expansion `(multiple-value-list ,(second clause))))
					       ; Slow way--bind gensym to the
					       ; macro expansion, and we will
					       ; funcall it in the body
	 (push (list acc-var gs)
	       acc-info)
	 (push `(funcall (cadr ,gs))
	       finish-forms)
	 (setq
	  top-bindings
	  (nconc
	   top-bindings
	   (list (list gs (cond (leftover-body
				 `(progn ,@(prog1 leftover-body
						  (setq leftover-body nil))
					 ,expansion))
				(t expansion))))))))))
  (setq body (walk-gathering-body body gathering-env acc-info))
  (cond ((eq body :abort)
					       ; Couldn't finish expansion
	 nil)
	(t `(let* ,top-bindings
		  ,@(and top-decls `((declare ,@top-decls)))
		  ,body
		  ,(cond ((null (cdr finish-forms))
					       ; just a single value
			  (car finish-forms))
			 (t `(values ,@(reverse finish-forms)))))))))

(defun rename-and-capture-variables (form alist env)

       ;; Walks FORM, renaming occurrences of the key variables in ALIST with
       ;; their corresponding values, and capturing any other free variables.
       ;; Returns a list of the new form and the list of other closed-over
       ;; vars. ENV is FORM's environment, so we can make sure we are talking
       ;; about the same variables.
       (let (closed)
	    (list (walk-form
		   form env
		   #'(lambda (form context subenv)
			    (declare (ignore context))
			    (let (pair)
				 (cond ((or (not (symbolp form))
					    (not (variable-same-p form subenv
							env)))
					       ; non-variable or one that has
					       ; been rebound
					form)
				       ((setq pair (assoc form alist))
					       ; One to rename
					(cdr pair))
				       (t      ; var is free
					  (pushnew form closed)
					  form)))))
		  closed)))

(defun
 walk-gathering-body
 (body gathering-env acc-info)

 ;; Walk the body of (GATHERING (...) . BODY) in environment GATHERING-ENV.
 ;; ACC-INFO is a list of information about each of the gathering "bindings"
 ;; in the form, in the form (var gatheringfn freevars env)
 (let
  ((*active-gatherers* (nconc (mapcar #'car acc-info)
			      *active-gatherers*)))

  ;; *ACTIVE-GATHERERS* tells us what vars are currently legal as GATHER
  ;; targets. This is so that when we encounter a GATHER not belonging to us
  ;; we can know whether to warn about it.
  (walk-form
   (cons 'progn body)
   gathering-env
   #'(lambda
      (form context env)
      (declare (ignore context))
      (let (info site)
	   (cond ((consp form)
		  (cond
		   ((not (eq (car form)
			     'gather))
					       ; We only care about GATHER
		    (when (and (eq (car form)
				   'function)
			       (eq (cadr form)
				   'gather))
					       ; Passed as functional--can't
					       ; macroexpand
			(maybe-warn :user
		   "Can't optimize GATHERING because of reference to #'GATHER."
			       )
			(return-from walk-gathering-body :abort))
		    form)
		   ((setq info (assoc (setq site (if (null (cddr form))

						     '
						     *anonymous-gathering-site*
						     (third form)))
				      acc-info))
					       ; One of ours--expand (GATHER
					       ; value var). INFO = (var
					       ; gatheringfn freevars env)
		    (unless (null (cdddr form))
			   (warn "Extra arguments (> 2) in ~S discarded." form)
			   )
		    (let ((fn (second info)))
			 (cond ((symbolp fn)
					       ; Unoptimized case--just call
					       ; the gatherer. FN is the
					       ; gensym that we bound to the
					       ; list of two values returned
					       ; from the gatherer.
				`(funcall (car ,fn)
					,(second form)))
			       (t	      ; FN = (lambda (value) ...)
				  (dolist (s (third info))
				      (unless (or (variable-same-p s env
							 gathering-env)
						  (and (variable-special-p
							s env)
						       (variable-special-p
							s gathering-env)))

			  ;; Some var used free in the LAMBDA form has been
			  ;; rebound between here and the parent GATHERING
			  ;; form, so can't substitute the lambda. Ok if it's
			  ;; a special reference both here and in the LAMBDA,
			  ;; because then it's not closed over.
					  (maybe-warn :user "Can't optimize GATHERING because the expansion closes over the variable ~S, which is rebound around a GATHER for it."
						 s)
					  (return-from walk-gathering-body
						 :abort)))

			  ;; Return ((lambda (value) ...) actual-value). In
			  ;; many cases we could simplify this further by
			  ;; substitution, but we'd have to be careful (for
			  ;; example, we would need to alpha-convert any LET
			  ;; we found inside). Any decent compiler will do it
			  ;; for us.
				  (list fn (second form))))))
		   ((and (setq info (member site *active-gatherers*))
			 (or (eq site '*anonymous-gathering-site*)
			     (variable-same-p site env (fourth info))))
					       ; Some other GATHERING will
					       ; take care of this form, so
					       ; pass it up for now.
					       ; Environment check is to make
					       ; sure nobody shadowed it
					       ; between here and there
		    form)
		   (t			  ; Nobody's going to handle it
		      (if (eq site '*anonymous-gathering-site*)
					       ; More likely that she forgot
					       ; to mention the site than
					       ; forget to write an anonymous
					       ; gathering.
			  (warn "There is no gathering site specified in ~S."
				form)
			  (warn
	     "The site ~S in ~S is not defined in an enclosing GATHERING form."
				site form))
					       ; Turn it into something else
					       ; so we don't warn twice in the
					       ; nested case
		      `(%orphaned-gather ,@(cdr form)))))
		 ((and (symbolp form)
		       (setq info (assoc form acc-info))
		       (variable-same-p form env gathering-env))
					       ; A variable reference to a
					       ; gather binding from
					       ; environment TEM
		  (maybe-warn :user "Can't optimize GATHERING because site variable ~S is used outside of a GATHER form."
			 form)
		  (return-from walk-gathering-body :abort))
		 (t form)))))))

;; sample gatherers
;;
;; FIXME: If these are only samples, can we delete them?

(defmacro
 collecting
 (&key initial-value)
 `(let* ((head ,initial-value)
	 (tail ,(and initial-value `(last head))))
	(values #'(lambda (value)
			 (if (null head)
			     (setq head (setq tail (list value)))
			     (setq tail (cdr (rplacd tail (list value))))))
	       #'(lambda nil head))))

(defmacro joining (&key initial-value)
       `(let ((result ,initial-value))
	     (values #'(lambda (value)
			      (setq result (nconc result value)))
		    #'(lambda nil result))))

(defmacro
 maximizing
 (&key initial-value)
 `(let ((result ,initial-value))
       (values
	#'(lambda (value)
		 (when ,(cond ((and (constantp initial-value)
				    (not (null (eval initial-value))))
					       ; Initial value is given and we
					       ; know it's not NIL, so leave
					       ; out the null check
			       '(> value result))
			      (t '(or (null result)
				      (> value result))))
		       (setq result value)))
	#'(lambda nil result))))

(defmacro
 minimizing
 (&key initial-value)
 `(let ((result ,initial-value))
       (values
	#'(lambda (value)
		 (when ,(cond ((and (constantp initial-value)
				    (not (null (eval initial-value))))
					       ; Initial value is given and we
					       ; know it's not NIL, so leave
					       ; out the null check
			       '(< value result))
			      (t '(or (null result)
				      (< value result))))
		       (setq result value)))
	#'(lambda nil result))))

(defmacro summing (&key (initial-value 0))
       `(let ((sum ,initial-value))
	     (values #'(lambda (value)
			      (setq sum (+ sum value)))
		    #'(lambda nil sum))))

;;; It's easier to read expanded code if PROG1 gets left alone.
(define-walker-template prog1 (nil return sb-walker::repeat (eval)))

;;;; This file contains code which does the translation of lambda
;;;; forms from Lisp code to the first intermediate representation
;;;; (IR1).

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

;;;; LAMBDA hackery

;;;; Note: Take a look at the compiler-overview.tex section on "Hairy
;;;; function representation" before you seriously mess with this
;;;; stuff.

;;; Verify that the NAME is a legal name for a variable and return a
;;; VAR structure for it, filling in info if it is globally special.
;;; If it is losing, we punt with a COMPILER-ERROR. NAMES-SO-FAR is a
;;; list of names which have previously been bound. If the NAME is in
;;; this list, then we error out.
(declaim (ftype (sfunction (t list) lambda-var) varify-lambda-arg))
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
(declaim (ftype (sfunction (symbol list t) symbol) make-keyword-for-arg))
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
(declaim (ftype (sfunction (list) (values list boolean boolean list list))
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
(defun ir1-convert-aux-bindings (start next result body aux-vars aux-vals)
  (declare (type ctran start next) (type (or lvar null) result)
           (list body aux-vars aux-vals))
  (if (null aux-vars)
      (ir1-convert-progn-body start next result body)
      (let ((ctran (make-ctran))
            (fun-lvar (make-lvar))
	    (fun (ir1-convert-lambda-body body
 					  (list (first aux-vars))
 					  :aux-vars (rest aux-vars)
 					  :aux-vals (rest aux-vals)
 					  :debug-name (debug-namify
 						       "&AUX bindings " 
 						       aux-vars))))
	(reference-leaf start ctran fun-lvar fun)
	(ir1-convert-combination-args fun-lvar ctran next result
				      (list (first aux-vals)))))
  (values))

;;; This is similar to IR1-CONVERT-PROGN-BODY except that code to bind
;;; the SPECVAR for each SVAR to the value of the variable is wrapped
;;; around the body. If there are no special bindings, we just convert
;;; the body, otherwise we do one special binding and recurse on the
;;; rest.
;;;
;;; We make a cleanup and introduce it into the lexical
;;; environment. If there are multiple special bindings, the cleanup
;;; for the blocks will end up being the innermost one. We force NEXT
;;; to start a block outside of this cleanup, causing cleanup code to
;;; be emitted when the scope is exited.
(defun ir1-convert-special-bindings
    (start next result body aux-vars aux-vals svars)
  (declare (type ctran start next) (type (or lvar null) result)
	   (list body aux-vars aux-vals svars))
  (cond
   ((null svars)
    (ir1-convert-aux-bindings start next result body aux-vars aux-vals))
   (t
    (ctran-starts-block next)
    (let ((cleanup (make-cleanup :kind :special-bind))
	  (var (first svars))
	  (bind-ctran (make-ctran))
	  (cleanup-ctran (make-ctran)))
      (ir1-convert start bind-ctran nil
		   `(%special-bind ',(lambda-var-specvar var) ,var))
      (setf (cleanup-mess-up cleanup) (ctran-use bind-ctran))
      (let ((*lexenv* (make-lexenv :cleanup cleanup)))
	(ir1-convert bind-ctran cleanup-ctran nil '(%cleanup-point))
	(ir1-convert-special-bindings cleanup-ctran next result
                                      body aux-vars aux-vals
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
;;; dealing with &NONSENSE, except for &REST vars with DYNAMIC-EXTENT.
;;;
;;; AUX-VARS is a list of VAR structures for variables that are to be
;;; sequentially bound. Each AUX-VAL is a form that is to be evaluated
;;; to get the initial value for the corresponding AUX-VAR.
(defun ir1-convert-lambda-body (body
				vars
				&key
				aux-vars
				aux-vals
				(source-name '.anonymous.)
				debug-name
                                (note-lexical-bindings t))
  (declare (list body vars aux-vars aux-vals))

  ;; We're about to try to put new blocks into *CURRENT-COMPONENT*.
  (aver-live-component *current-component*)

  (let* ((bind (make-bind))
	 (lambda (make-lambda :vars vars
                  :bind bind
                  :%source-name source-name
                  :%debug-name debug-name))
	 (result-ctran (make-ctran))
         (result-lvar (make-lvar)))

    (awhen (lexenv-lambda *lexenv*)
      (push lambda (lambda-children it))
      (setf (lambda-parent lambda) it))

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
	(aver (not (lambda-var-home var)))
	(setf (lambda-var-home var) lambda)
	(let ((specvar (lambda-var-specvar var)))
	  (cond (specvar
		 (svars var)
		 (new-venv (cons (leaf-source-name specvar) specvar)))
		(t
                 (when note-lexical-bindings
                   (note-lexical-binding (leaf-source-name var)))
		 (new-venv (cons (leaf-source-name var) var))))))

      (let ((*lexenv* (make-lexenv :vars (new-venv)
				   :lambda lambda
				   :cleanup nil)))
	(setf (bind-lambda bind) lambda)
	(setf (node-lexenv bind) *lexenv*)

	(let ((block (ctran-starts-block result-ctran)))
	  (let ((return (make-return :result result-lvar :lambda lambda))
                (tail-set (make-tail-set :funs (list lambda))))
            (setf (lambda-tail-set lambda) tail-set)
            (setf (lambda-return lambda) return)
            (setf (lvar-dest result-lvar) return)
            (link-node-to-previous-ctran return result-ctran)
            (setf (block-last block) return))
          (link-blocks block (component-tail *current-component*)))

        (with-component-last-block (*current-component*
                                    (ctran-block result-ctran))
          (let ((prebind-ctran (make-ctran))
                (postbind-ctran (make-ctran)))
            (ctran-starts-block prebind-ctran)
            (link-node-to-previous-ctran bind prebind-ctran)
            (use-ctran bind postbind-ctran)
	    (ir1-convert-special-bindings postbind-ctran result-ctran
                                          result-lvar body
                                          aux-vars aux-vals (svars))))))

    (link-blocks (component-head *current-component*) (node-block bind))
    (push lambda (component-new-functionals *current-component*))

    lambda))

;;; Entry point CLAMBDAs have a special kind
(defun register-entry-point (entry dispatcher)
  (declare (type clambda entry)
           (type optional-dispatch dispatcher))
  (setf (functional-kind entry) :optional)
  (setf (leaf-ever-used entry) t)
  (setf (lambda-optional-dispatch entry) dispatcher)
  entry)

;;; Create the actual entry-point function for an optional entry
;;; point. The lambda binds copies of each of the VARS, then calls FUN
;;; with the argument VALS and the DEFAULTS. Presumably the VALS refer
;;; to the VARS by name. The VALS are passed in the reverse order.
;;;
;;; If any of the copies of the vars are referenced more than once,
;;; then we mark the corresponding var as EVER-USED to inhibit
;;; "defined but not read" warnings for arguments that are only used
;;; by default forms.
(defun convert-optional-entry (fun vars vals defaults)
  (declare (type clambda fun) (list vars vals defaults))
  (let* ((fvars (reverse vars))
	 (arg-vars (mapcar (lambda (var)
			     (make-lambda-var
			      :%source-name (leaf-source-name var)
			      :type (leaf-type var)
			      :where-from (leaf-where-from var)
			      :specvar (lambda-var-specvar var)))
			   fvars))
	 (fun (collect ((default-bindings)
                        (default-vals))
                (dolist (default defaults)
                  (if (constantp default)
                      (default-vals default)
                      (let ((var (gensym)))
                        (default-bindings `(,var ,default))
                        (default-vals var))))
                (ir1-convert-lambda-body `((let (,@(default-bindings))
                                             (%funcall ,fun
                                                       ,@(reverse vals)
                                                       ,@(default-vals))))
                                         arg-vars
                                         :debug-name
                                         (debug-namify "&OPTIONAL processor "
						       (gensym))
                                         :note-lexical-bindings nil))))
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
                                        aux-vars aux-vals
                                        source-name debug-name
                                        force)
  (declare (type optional-dispatch res)
	   (list default-vars default-vals entry-vars entry-vals vars body
		 aux-vars aux-vals))
  (let* ((arg (first vars))
	 (arg-name (leaf-source-name arg))
	 (info (lambda-var-arg-info arg))
	 (default (arg-info-default info))
         (supplied-p (arg-info-supplied-p info))
         (force (or force
                    (not (sb!xc:constantp (arg-info-default info)))))
	 (ep (if supplied-p
		 (ir1-convert-hairy-args
		  res
		  (list* supplied-p arg default-vars)
		  (list* (leaf-source-name supplied-p) arg-name default-vals)
		  (cons arg entry-vars)
		  (list* t arg-name entry-vals)
		  (rest vars) t body aux-vars aux-vals
		  source-name debug-name
                  force)
		 (ir1-convert-hairy-args
		  res
		  (cons arg default-vars)
		  (cons arg-name default-vals)
		  (cons arg entry-vars)
		  (cons arg-name entry-vals)
		  (rest vars) supplied-p-p body aux-vars aux-vals
		  source-name debug-name
                  force))))

    ;; We want to delay converting the entry, but there exist
    ;; problems: hidden references should not be established to
    ;; lambdas of kind NIL should not have (otherwise the compiler
    ;; might let-convert or delete them) and to variables.
    (if (or force
            supplied-p-p ; this entry will be of kind NIL
            (and (lambda-p ep) (eq (lambda-kind ep) nil)))
        (convert-optional-entry ep
                                default-vars default-vals
                                (if supplied-p
                                    (list default nil)
                                    (list default)))
        (delay
         (register-entry-point
           (convert-optional-entry (force ep)
                                   default-vars default-vals
                                   (if supplied-p
                                       (list default nil)
                                       (list default)))
           res)))))

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
	(arg-vals `(%listify-rest-args
		    ,n-context ,n-count)))
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
                  (setq clause
			(append clause `((setq ,n-allowp ,n-value-temp)))))

                (temps `(,n-value ,default))
		(tests clause)))

	    (unless allowp
	      (temps n-allowp n-losep)
              (unless found-allow-p
                (tests `((eq ,n-key :allow-other-keys)
                         (setq ,n-allowp ,n-value-temp))))
	      (tests `(t
		       (setq ,n-losep (list ,n-key)))))

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
		       (%unknown-key-arg-error (car ,n-losep))))))))

      (let ((ep (ir1-convert-lambda-body
		 `((let ,(temps)
		     ,@(body)
		     (%funcall ,(optional-dispatch-main-entry res)
			       ,@(arg-vals))))
		 (arg-vars)
		 :debug-name "&MORE processing"
                 :note-lexical-bindings nil)))
	(setf (optional-dispatch-more-entry res)
              (register-entry-point ep res)))))

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
			     body aux-vars aux-vals
			     source-name debug-name)
  (declare (type optional-dispatch res)
	   (list default-vars default-vals entry-vars entry-vals keys body
		 aux-vars aux-vals))
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
			:debug-name (debug-namify
				     "varargs entry for " source-name debug-name)))
	   (last-entry (convert-optional-entry main-entry default-vars
					       (main-vals) ())))
      (setf (optional-dispatch-main-entry res)
            (register-entry-point main-entry res))
      (convert-more-entry res entry-vars entry-vals rest more-context keys)

      (push (register-entry-point
             (if supplied-p-p
		(convert-optional-entry last-entry entry-vars entry-vals ())
		last-entry)
             res)
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
                               aux-vals
                               source-name debug-name
                               force)
  (declare (type optional-dispatch res)
           (list default-vars default-vals entry-vars entry-vals vars body
                 aux-vars aux-vals))
  (cond ((not vars)
         (if (optional-dispatch-keyp res)
             ;; Handle &KEY with no keys...
             (ir1-convert-more res default-vars default-vals
                               entry-vars entry-vals
                               nil nil nil vars supplied-p-p body aux-vars
                               aux-vals source-name debug-name)
             (let ((fun (ir1-convert-lambda-body
			 body (reverse default-vars)
			 :aux-vars aux-vars
			 :aux-vals aux-vals
			 :debug-name (debug-namify
				      "hairy arg processor for "
				      source-name
				      debug-name))))
               (setf (optional-dispatch-main-entry res) fun)
               (register-entry-point fun res)
               (push (if supplied-p-p
                         (register-entry-point
                          (convert-optional-entry fun entry-vars entry-vals ())
                          res)
                          fun)
                     (optional-dispatch-entry-points res))
               fun)))
        ((not (lambda-var-arg-info (first vars)))
         (let* ((arg (first vars))
                (nvars (cons arg default-vars))
                (nvals (cons (leaf-source-name arg) default-vals)))
           (ir1-convert-hairy-args res nvars nvals nvars nvals
                                   (rest vars) nil body aux-vars aux-vals
				   source-name debug-name
                                   nil)))
        (t
         (let* ((arg (first vars))
                (info (lambda-var-arg-info arg))
                (kind (arg-info-kind info)))
           (ecase kind
             (:optional
              (let ((ep (generate-optional-default-entry
                         res default-vars default-vals
                         entry-vars entry-vals vars supplied-p-p body
                         aux-vars aux-vals
			 source-name debug-name
                         force)))
                ;; See GENERATE-OPTIONAL-DEFAULT-ENTRY.
                (push (if (lambda-p ep)
                          (register-entry-point
                           (if supplied-p-p
                               (convert-optional-entry ep entry-vars entry-vals ())
                               ep)
                           res)
                          (progn (aver (not supplied-p-p))
                                 ep))
                      (optional-dispatch-entry-points res))
                ep))
             (:rest
              (ir1-convert-more res default-vars default-vals
                                entry-vars entry-vals
                                arg nil nil (rest vars) supplied-p-p body
                                aux-vars aux-vals
				source-name debug-name))
             (:more-context
              (ir1-convert-more res default-vars default-vals
                                entry-vars entry-vals
                                nil arg (second vars) (cddr vars) supplied-p-p
                                body aux-vars aux-vals
				source-name debug-name))
             (:keyword
              (ir1-convert-more res default-vars default-vals
                                entry-vars entry-vals
                                nil nil nil vars supplied-p-p body aux-vars
                                aux-vals source-name debug-name)))))))

;;; This function deals with the case where we have to make an
;;; OPTIONAL-DISPATCH to represent a LAMBDA. We cons up the result and
;;; call IR1-CONVERT-HAIRY-ARGS to do the work. When it is done, we
;;; figure out the MIN-ARGS and MAX-ARGS.
(defun ir1-convert-hairy-lambda (body vars keyp allowp aux-vars aux-vals
				      &key
				      (source-name '.anonymous.)
				      (debug-name (debug-namify
						   "OPTIONAL-DISPATCH "
						   vars)))
  (declare (list body vars aux-vars aux-vals))
  (let ((res (make-optional-dispatch :arglist vars
				     :allowp allowp
				     :keyp keyp
				     :%source-name source-name
				     :%debug-name debug-name
                                     :plist `(:ir1-environment
                                              (,*lexenv*
                                               ,*current-path*))))
	(min (or (position-if #'lambda-var-arg-info vars) (length vars))))
    (aver-live-component *current-component*)
    (push res (component-new-functionals *current-component*))
    (ir1-convert-hairy-args res () () () () vars nil body aux-vars aux-vals
			    source-name debug-name nil)
    (setf (optional-dispatch-min-args res) min)
    (setf (optional-dispatch-max-args res)
	  (+ (1- (length (optional-dispatch-entry-points res))) min))

    res))

;;; Convert a LAMBDA form into a LAMBDA leaf or an OPTIONAL-DISPATCH leaf.
(defun ir1-convert-lambda (form &key (source-name '.anonymous.)
                           debug-name
                           allow-debug-catch-tag)

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

  (let ((*allow-debug-catch-tag* (and *allow-debug-catch-tag* allow-debug-catch-tag)))
    (multiple-value-bind (vars keyp allow-other-keys aux-vars aux-vals)
	(make-lambda-vars (cadr form))
      (multiple-value-bind (forms decls) (parse-body (cddr form))
	(binding* (((*lexenv* result-type)
                    (process-decls decls (append aux-vars vars) nil))
                   (forms (if (and *allow-debug-catch-tag*
                                   (policy *lexenv* (>= insert-debug-catch 2)))
                              `((catch (make-symbol "SB-DEBUG-CATCH-TAG")
                                  ,@forms))
                              forms))
                   (forms (if (eq result-type *wild-type*)
                              forms
                              `((the ,result-type (progn ,@forms)))))
                   (res (if (or (find-if #'lambda-var-arg-info vars) keyp)
                            (ir1-convert-hairy-lambda forms vars keyp
                                                      allow-other-keys
                                                      aux-vars aux-vals
                                                      :source-name source-name
                                                      :debug-name debug-name)
                            (ir1-convert-lambda-body forms vars
                                                     :aux-vars aux-vars
                                                     :aux-vals aux-vals
                                                     :source-name source-name
                                                     :debug-name debug-name))))
	  (setf (functional-inline-expansion res) form)
	  (setf (functional-arg-documentation res) (cadr form))
	  res)))))

;;; helper for LAMBDA-like things, to massage them into a form
;;; suitable for IR1-CONVERT-LAMBDA.
;;;
;;; KLUDGE: We cons up a &REST list here, maybe for no particularly
;;; good reason.  It's probably lost in the noise of all the other
;;; consing, but it's still inelegant.  And we force our called
;;; functions to do full runtime keyword parsing, ugh.  -- CSR,
;;; 2003-01-25
(defun ir1-convert-lambdalike (thing &rest args
			       &key (source-name '.anonymous.)
			       debug-name allow-debug-catch-tag)
  (declare (ignorable source-name debug-name allow-debug-catch-tag))
  (ecase (car thing)
    ((lambda) (apply #'ir1-convert-lambda thing args))
    ((instance-lambda)
     (let ((res (apply #'ir1-convert-lambda
		       `(lambda ,@(cdr thing)) args)))
       (setf (getf (functional-plist res) :fin-function) t)
       res))
    ((named-lambda)
     (let ((name (cadr thing)))
       (if (legal-fun-name-p name)
	   (let ((defined-fun-res (get-defined-fun name))
                 (res (apply #'ir1-convert-lambda `(lambda ,@(cddr thing))
			     :source-name name
			     :debug-name nil
			     args)))
	     (assert-global-function-definition-type name res)
             (setf (defined-fun-functional defined-fun-res)
                   res)
             (unless (eq (defined-fun-inlinep defined-fun-res) :notinline)
               (substitute-leaf-if
                (lambda (ref)
                  (policy ref (> recognize-self-calls 0)))
                res defined-fun-res))
	     res)
	   (apply #'ir1-convert-lambda `(lambda ,@(cddr thing))
		  :debug-name name args))))
    ((lambda-with-lexenv) (apply #'ir1-convert-inline-lambda thing args))))

;;;; defining global functions

;;; Convert FUN as a lambda in the null environment, but use the
;;; current compilation policy. Note that FUN may be a
;;; LAMBDA-WITH-LEXENV, so we may have to augment the environment to
;;; reflect the state at the definition site.
(defun ir1-convert-inline-lambda (fun &key
				      (source-name '.anonymous.)
				      debug-name
				      allow-debug-catch-tag)
  (declare (ignore allow-debug-catch-tag))
  (destructuring-bind (decls macros symbol-macros &rest body)
		      (if (eq (car fun) 'lambda-with-lexenv)
			  (cdr fun)
			  `(() () () . ,(cdr fun)))
    (let ((*lexenv* (make-lexenv
		     :default (process-decls decls nil nil
					     (make-null-lexenv))
		     :vars (copy-list symbol-macros)
		     :funs (mapcar (lambda (x)
				     `(,(car x) .
				       (macro . ,(coerce (cdr x) 'function))))
				   macros)
		     :policy (lexenv-policy *lexenv*))))
      (ir1-convert-lambda `(lambda ,@body)
			  :source-name source-name
			  :debug-name debug-name
			  :allow-debug-catch-tag nil))))

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
			  (for-real #'compiler-notify)
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
(defun %compiler-defun (name lambda-with-lexenv compile-toplevel)

  (let ((defined-fun nil)) ; will be set below if we're in the compiler

    (when compile-toplevel
      ;; better be in the compiler
      (aver (boundp '*lexenv*)) 
      (when sb!xc:*compile-print*
	(compiler-mumble "~&; recognizing DEFUN ~S~%" name))
      (remhash name *free-funs*)
      (setf defined-fun (get-defined-fun name))

      (aver (fasl-output-p *compile-object*))
      (if (member name *fun-names-in-this-file* :test #'equal)
	  (warn 'duplicate-definition :name name)
	  (push name *fun-names-in-this-file*)))

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


;;; Entry point utilities

;;; Return a function for the Nth entry point.
(defun optional-dispatch-entry-point-fun (dispatcher n)
  (declare (type optional-dispatch dispatcher)
           (type unsigned-byte n))
  (let* ((env (getf (optional-dispatch-plist dispatcher) :ir1-environment))
         (*lexenv* (first env))
         (*current-path* (second env)))
    (force (nth n (optional-dispatch-entry-points dispatcher)))))

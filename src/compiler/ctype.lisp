;;;; This file contains code which knows about both the type
;;;; representation and the compiler IR1 representation. This stuff is
;;;; used for doing type checking.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

;;; These are the functions that are to be called when a problem is
;;; detected. They are passed format arguments. If null, we don't do
;;; anything. The error function is called when something is
;;; definitely incorrect. The warning function is called when it is
;;; somehow impossible to tell whether the call is correct.
(defvar *error-function*)
(defvar *warning-function*)

;;; The function that we use for type checking. The derived type is
;;; the first argument and the type we are testing against is the
;;; second argument. The function should return values like CSUBTYPEP.
(defvar *test-function*)
;;; FIXME: Why is this a variable? Explain.

(declaim (type (or function null) *error-function* *warning-function
	       *test-function*))

;;; *LOSSAGE-DETECTED* is set when a definite incompatibility is
;;; detected. *SLIME-DETECTED* is set when we can't tell whether the
;;; call is compatible or not.
(defvar *lossage-detected*)
(defvar *slime-detected*)
;;; FIXME: SLIME is vivid and concise, but "DEFINITE-CALL-LOSSAGE" and
;;; "POSSIBLE-CALL-LOSSAGE" would be more mnemonic.

;;; Signal a warning if appropriate and set *LOSSAGE-DETECTED*.
(declaim (ftype (function (string &rest t) (values)) note-lossage note-slime))
(defun note-lossage (format-string &rest format-args)
  (setq *lossage-detected* t)
  (when *error-function*
    (apply *error-function* format-string format-args))
  (values))
(defun note-slime (format-string &rest format-args)
  (setq *slime-detected* t)
  (when *warning-function*
    (apply *warning-function* format-string format-args))
  (values))

(declaim (special *compiler-error-context*))

;;;; stuff for checking a call against a function type
;;;;
;;;; FIXME: This is stuff to look at when I get around to fixing
;;;; function type inference and declarations.

;;; A dummy version of SUBTYPEP useful when we want a functional like
;;; subtypep that always returns true.
(defun always-subtypep (type1 type2)
  (declare (ignore type1 type2))
  (values t t))

;;; Determine whether a use of a function is consistent with its type.
;;; These values are returned:
;;;    T, T: the call is definitely valid.
;;;    NIL, T: the call is definitely invalid.
;;;    NIL, NIL: unable to determine whether the call is valid.
;;;
;;; The Argument-Test function is used to determine whether an
;;; argument type matches the type we are checking against. Similarly,
;;; the Result-Test is used to determine whether the result type
;;; matches the specified result.
;;;
;;; Unlike the argument test, the result test may be called on values
;;; or function types. If Strict-Result is true and safety is
;;; non-zero, then the Node-Derived-Type is always used. Otherwise, if
;;; Cont's Type-Check is true, then the Node-Derived-Type is
;;; intersected with the Cont's Asserted-Type.
;;;
;;; The error and warning functions are functions that are called to
;;; explain the result. We bind *compiler-error-context* to the
;;; combination node so that Compiler-Warning and related functions
;;; will do the right thing if they are supplied.
(defun valid-function-use (call type &key
				((:argument-test *test-function*) #'csubtypep)
				(result-test #'values-subtypep)
				(strict-result nil)
				((:error-function *error-function*))
				((:warning-function *warning-function*)))
  (declare (type function result-test) (type combination call)
	   (type function-type type))
  (let* ((*lossage-detected* nil)
	 (*slime-detected* nil)
	 (*compiler-error-context* call)
	 (args (combination-args call))
	 (nargs (length args))
	 (required (function-type-required type))
	 (min-args (length required))
	 (optional (function-type-optional type))
	 (max-args (+ min-args (length optional)))
	 (rest (function-type-rest type))
	 (keyp (function-type-keyp type)))

    (cond
     ((function-type-wild-args type)
      (do ((i 1 (1+ i))
	   (arg args (cdr arg)))
	  ((null arg))
	(check-arg-type (car arg) *wild-type* i)))
     ((not (or optional keyp rest))
      (if (/= nargs min-args)
	  (note-lossage
	   "The function was called with ~R argument~:P, but wants exactly ~R."
	   nargs min-args)
	  (check-fixed-and-rest args required nil)))
     ((< nargs min-args)
      (note-lossage
       "The function was called with ~R argument~:P, but wants at least ~R."
       nargs min-args))
     ((<= nargs max-args)
      (check-fixed-and-rest args (append required optional) rest))
     ((not (or keyp rest))
      (note-lossage
       "The function was called with ~R argument~:P, but wants at most ~R."
       nargs max-args))
     ((and keyp (oddp (- nargs max-args)))
      (note-lossage
       "The function has an odd number of arguments in the keyword portion."))
     (t
      (check-fixed-and-rest args (append required optional) rest)
      (when keyp
	(check-keywords args max-args type))))

    (let* ((dtype (node-derived-type call))
	   (return-type (function-type-returns type))
	   (cont (node-cont call))
	   (out-type
	    (if (or (not (continuation-type-check cont))
		    (and strict-result (policy call (/= safety 0))))
		dtype
		(values-type-intersection (continuation-asserted-type cont)
					  dtype))))
      (multiple-value-bind (int win) (funcall result-test out-type return-type)
	(cond ((not win)
	       (note-slime "can't tell whether the result is a ~S"
			   (type-specifier return-type)))
	      ((not int)
	       (note-lossage "The result is a ~S, not a ~S."
			     (type-specifier out-type)
			     (type-specifier return-type))))))

    (cond (*lossage-detected* (values nil t))
	  (*slime-detected* (values nil nil))
	  (t (values t t)))))

;;; Check that the derived type of the continuation Cont is compatible
;;; with Type. N is the arg number, for error message purposes. We
;;; return true if arg is definitely o.k. If the type is a magic
;;; CONSTANT-TYPE, then we check for the argument being a constant
;;; value of the specified type. If there is a manifest type error
;;; (DERIVED-TYPE = NIL), then we flame about the asserted type even
;;; when our type is satisfied under the test.
(defun check-arg-type (cont type n)
  (declare (type continuation cont) (type ctype type) (type index n))
  (cond
   ((not (constant-type-p type))
    (let ((ctype (continuation-type cont)))
      (multiple-value-bind (int win) (funcall *test-function* ctype type)
	(cond ((not win)
	       (note-slime "can't tell whether the ~:R argument is a ~S" n
			   (type-specifier type))
	       nil)
	      ((not int)
	       (note-lossage "The ~:R argument is a ~S, not a ~S." n
			     (type-specifier ctype)
			     (type-specifier type))
	       nil)
	      ((eq ctype *empty-type*)
	       (note-slime "The ~:R argument never returns a value." n)
	       nil)
	      (t t)))))
    ((not (constant-continuation-p cont))
     (note-slime "The ~:R argument is not a constant." n)
     nil)
    (t
     (let ((val (continuation-value cont))
	   (type (constant-type-type type)))
       (multiple-value-bind (res win) (ctypep val type)
	 (cond ((not win)
		(note-slime "can't tell whether the ~:R argument is a ~
			     constant ~S:~%  ~S"
			    n (type-specifier type) val)
		nil)
	       ((not res)
		(note-lossage "The ~:R argument is not a constant ~S:~%  ~S"
			      n (type-specifier type) val)
		nil)
	       (t t)))))))

;;; Check that each of the type of each supplied argument intersects
;;; with the type specified for that argument. If we can't tell, then
;;; we complain about the slime.
(declaim (ftype (function (list list (or ctype null)) (values)) check-fixed-and-rest))
(defun check-fixed-and-rest (args types rest)
  (do ((arg args (cdr arg))
       (type types (cdr type))
       (n 1 (1+ n)))
      ((or (null type) (null arg))
       (when rest
	 (dolist (arg arg)
	   (check-arg-type arg rest n)
	   (incf n))))
    (declare (fixnum n))
    (check-arg-type (car arg) (car type) n))
  (values))

;;; Check that the keyword args are of the correct type. Each keyword
;;; should be known and the corresponding argument should be of the
;;; correct type. If the keyword isn't a constant, then we can't tell,
;;; so we note slime.
(declaim (ftype (function (list fixnum function-type) (values)) check-keywords))
(defun check-keywords (args pre-key type)
  (do ((key (nthcdr pre-key args) (cddr key))
       (n (1+ pre-key) (+ n 2)))
      ((null key))
    (declare (fixnum n))
    (let ((k (car key)))
      (cond
       ((not (check-arg-type k (specifier-type 'symbol) n)))
       ((not (constant-continuation-p k))
	(note-slime "The ~:R argument (in keyword position) is not a constant."
		    n))
       (t
	(let* ((name (continuation-value k))
	       (info (find name (function-type-keywords type)
			   :key #'key-info-name)))
	  (cond ((not info)
		 (unless (function-type-allowp type)
		   (note-lossage "~S is not a known argument keyword."
				 name)))
		(t
		 (check-arg-type (second key) (key-info-type info)
				 (1+ n)))))))))
  (values))

;;; Construct a function type from a definition.
;;;
;;; Due to the lack of a (LIST X) type specifier, we can't reconstruct
;;; the &REST type.
(declaim (ftype (function (functional) function-type) definition-type))
(defun definition-type (functional)
  (if (lambda-p functional)
      (make-function-type
       :required (mapcar #'leaf-type (lambda-vars functional))
       :returns (tail-set-type (lambda-tail-set functional)))
      (let ((rest nil))
	(collect ((req)
		  (opt)
		  (keys))
	  (dolist (arg (optional-dispatch-arglist functional))
	    (let ((info (lambda-var-arg-info arg))
		  (type (leaf-type arg)))
	      (if info
		  (ecase (arg-info-kind info)
		    (:required (req type))
		    (:optional (opt type))
		    (:keyword
		     (keys (make-key-info :name (arg-info-keyword info)
					  :type type)))
		    ((:rest :more-context)
		     (setq rest *universal-type*))
		    (:more-count))
		  (req type))))

	  (make-function-type
	   :required (req)
	   :optional (opt)
	   :rest rest
	   :keywords (keys)
	   :keyp (optional-dispatch-keyp functional)
	   :allowp (optional-dispatch-allowp functional)
	   :returns (tail-set-type
		     (lambda-tail-set
		      (optional-dispatch-main-entry functional))))))))

;;;; approximate function types
;;;;
;;;; FIXME: This is stuff to look at when I get around to fixing function
;;;; type inference and declarations.
;;;;
;;;; Approximate function types provide a condensed representation of all the
;;;; different ways that a function has been used. If we have no declared or
;;;; defined type for a function, then we build an approximate function type by
;;;; examining each use of the function. When we encounter a definition or
;;;; proclamation, we can check the actual type for compatibity with the
;;;; previous uses.

(defstruct (approximate-function-type)
  ;; The smallest and largest numbers of arguments that this function has been
  ;; called with.
  (min-args call-arguments-limit :type fixnum)
  (max-args 0 :type fixnum)
  ;; A list of lists of the all the types that have been used in each argument
  ;; position.
  (types () :type list)
  ;; A list of the Approximate-Key-Info structures describing all the things
  ;; that looked like keyword arguments. There are distinct structures
  ;; describing each argument position in which the keyword appeared.
  (keys () :type list))

(defstruct (approximate-key-info)
  ;; The keyword name of this argument. Although keyword names don't have to
  ;; be keywords, we only match on keywords when figuring an approximate type.
  (name (required-argument) :type keyword)
  ;; The position at which this keyword appeared. 0 if it appeared as the
  ;; first argument, etc.
  (position (required-argument) :type fixnum)
  ;; A list of all the argument types that have been used with this keyword.
  (types nil :type list)
  ;; True if this keyword has appeared only in calls with an obvious
  ;; :allow-other-keys.
  (allowp nil :type (member t nil)))

;;; Return an Approximate-Function-Type representing the context of
;;; Call. If Type is supplied and not null, then we merge the
;;; information into the information already accumulated in Type.
(declaim (ftype (function (combination
			   &optional (or approximate-function-type null))
			  approximate-function-type)
		note-function-use))
(defun note-function-use (call &optional type)
  (let* ((type (or type (make-approximate-function-type)))
	 (types (approximate-function-type-types type))
	 (args (combination-args call))
	 (nargs (length args))
	 (allowp (some #'(lambda (x)
			   (and (constant-continuation-p x)
				(eq (continuation-value x) :allow-other-keys)))
			  args)))

    (setf (approximate-function-type-min-args type)
	  (min (approximate-function-type-min-args type) nargs))
    (setf (approximate-function-type-max-args type)
	  (max (approximate-function-type-max-args type) nargs))

    (do ((old types (cdr old))
	 (arg args (cdr arg)))
	((null old)
	 (setf (approximate-function-type-types type)
	       (nconc types
		      (mapcar #'(lambda (x)
				  (list (continuation-type x)))
			      arg))))
      (when (null arg) (return))
      (pushnew (continuation-type (car arg))
	       (car old)
	       :test #'type=))

    (collect ((keys (approximate-function-type-keys type) cons))
      (do ((arg args (cdr arg))
	   (pos 0 (1+ pos)))
	  ((or (null arg) (null (cdr arg)))
	   (setf (approximate-function-type-keys type) (keys)))
	(let ((key (first arg))
	      (val (second arg)))
	  (when (constant-continuation-p key)
	    (let ((name (continuation-value key)))
	      (when (keywordp name)
		(let ((old (find-if
			    #'(lambda (x)
				(and (eq (approximate-key-info-name x) name)
				     (= (approximate-key-info-position x)
					pos)))
			    (keys)))
		      (val-type (continuation-type val)))
		  (cond (old
			 (pushnew val-type
				  (approximate-key-info-types old)
				  :test #'type=)
			 (unless allowp
			   (setf (approximate-key-info-allowp old) nil)))
			(t
			 (keys (make-approximate-key-info
				:name name
				:position pos
				:allowp allowp
				:types (list val-type))))))))))))
    type))

;;; Similar to Valid-Function-Use, but checks an
;;; Approximate-Function-Type against a real function type.
(declaim (ftype (function (approximate-function-type function-type
			   &optional function function function)
			  (values boolean boolean))
		valid-approximate-type))
(defun valid-approximate-type (call-type type &optional
					 (*test-function* #'types-intersect)
					 (*error-function* #'compiler-warning)
					 (*warning-function* #'compiler-note))
  (let* ((*lossage-detected* nil)
	 (*slime-detected* nil)
	 (required (function-type-required type))
	 (min-args (length required))
	 (optional (function-type-optional type))
	 (max-args (+ min-args (length optional)))
	 (rest (function-type-rest type))
	 (keyp (function-type-keyp type)))

    (when (function-type-wild-args type)
      (return-from valid-approximate-type (values t t)))

    (let ((call-min (approximate-function-type-min-args call-type)))
      (when (< call-min min-args)
	(note-lossage
	 "Function previously called with ~R argument~:P, but wants at least ~R."
	 call-min min-args)))

    (let ((call-max (approximate-function-type-max-args call-type)))
      (cond ((<= call-max max-args))
	    ((not (or keyp rest))
	     (note-lossage
	      "Function previously called with ~R argument~:P, but wants at most ~R."
	      call-max max-args))
	    ((and keyp (oddp (- call-max max-args)))
	     (note-lossage
	      "Function previously called with an odd number of arguments in ~
	      the keyword portion.")))

      (when (and keyp (> call-max max-args))
	(check-approximate-keywords call-type max-args type)))

    (check-approximate-fixed-and-rest call-type (append required optional)
				      rest)

    (cond (*lossage-detected* (values nil t))
	  (*slime-detected* (values nil nil))
	  (t (values t t)))))

;;; Check that each of the types used at each arg position is
;;; compatible with the actual type.
(declaim (ftype (function (approximate-function-type list (or ctype null))
			  (values))
		check-approximate-fixed-and-rest))
(defun check-approximate-fixed-and-rest (call-type fixed rest)
  (do ((types (approximate-function-type-types call-type) (cdr types))
       (n 1 (1+ n))
       (arg fixed (cdr arg)))
      ((null types))
    (let ((decl-type (or (car arg) rest)))
      (unless decl-type (return))
      (check-approximate-arg-type (car types) decl-type "~:R" n)))
  (values))

;;; Check that each of the call-types is compatible with Decl-Type,
;;; complaining if not or if we can't tell.
(declaim (ftype (function (list ctype string &rest t) (values))
		check-approximate-arg-type))
(defun check-approximate-arg-type (call-types decl-type context &rest args)
  (let ((losers *empty-type*))
    (dolist (ctype call-types)
      (multiple-value-bind (int win) (funcall *test-function* ctype decl-type)
	(cond
	 ((not win)
	  (note-slime "can't tell whether previous ~? argument type ~S is a ~S"
		      context args (type-specifier ctype) (type-specifier decl-type)))
	 ((not int)
	  (setq losers (type-union ctype losers))))))

    (unless (eq losers *empty-type*)
      (note-lossage "~:(~?~) argument should be a ~S but was a ~S in a previous call."
		    context args (type-specifier decl-type) (type-specifier losers))))
  (values))

;;; Check the types of each manifest keyword that appears in a keyword
;;; argument position. Check the validity of all keys that appeared in
;;; valid keyword positions.
;;;
;;; ### We could check the Approximate-Function-Type-Types to make
;;; sure that all arguments in keyword positions were manifest
;;; keywords.
(defun check-approximate-keywords (call-type max-args type)
  (let ((call-keys (approximate-function-type-keys call-type))
	(keys (function-type-keywords type)))
    (dolist (key keys)
      (let ((name (key-info-name key)))
	(collect ((types nil append))
	  (dolist (call-key call-keys)
	    (let ((pos (approximate-key-info-position call-key)))
	      (when (and (eq (approximate-key-info-name call-key) name)
			 (> pos max-args) (evenp (- pos max-args)))
		(types (approximate-key-info-types call-key)))))
	  (check-approximate-arg-type (types) (key-info-type key) "~S" name))))

    (unless (function-type-allowp type)
      (collect ((names () adjoin))
	(dolist (call-key call-keys)
	  (let ((pos (approximate-key-info-position call-key)))
	    (when (and (> pos max-args) (evenp (- pos max-args))
		       (not (approximate-key-info-allowp call-key)))
	      (names (approximate-key-info-name call-key)))))

	(dolist (name (names))
	  (unless (find name keys :key #'key-info-name)
	    (note-lossage "Function previously called with unknown argument keyword ~S."
		  name)))))))

;;;; ASSERT-DEFINITION-TYPE

;;; Intersect Lambda's var types with Types, giving a warning if there
;;; is a mismatch. If all intersections are non-null, we return lists
;;; of the variables and intersections, otherwise we return NIL, NIL.
(defun try-type-intersections (vars types where)
  (declare (list vars types) (string where))
  (collect ((res))
    (mapc #'(lambda (var type)
	      (let* ((vtype (leaf-type var))
		     (int (type-intersection vtype type)))
		(cond
		 ((eq int *empty-type*)
		  (note-lossage
		   "Definition's declared type for variable ~A:~%  ~S~@
		   conflicts with this type from ~A:~%  ~S"
		   (leaf-name var) (type-specifier vtype)
		   where (type-specifier type))
		  (return-from try-type-intersections (values nil nil)))
		 (t
		  (res int)))))
	  vars types)
    (values vars (res))))

;;; Check that the optional-dispatch OD conforms to Type. We return
;;; the values of TRY-TYPE-INTERSECTIONS if there are no syntax
;;; problems, otherwise NIL, NIL.
;;;
;;; Note that the variables in the returned list are the actual
;;; original variables (extracted from the optional dispatch arglist),
;;; rather than the variables that are arguments to the main entry.
;;; This difference is significant only for keyword args with hairy
;;; defaults. Returning the actual vars allows us to use the right
;;; variable name in warnings.
;;;
;;; A slightly subtle point: with keywords and optionals, the type in
;;; the function type is only an assertion on calls --- it doesn't
;;; constrain the type of default values. So we have to union in the
;;; type of the default. With optionals, we can't do any assertion
;;; unless the default is constant.
;;;
;;; With keywords, we exploit our knowledge about how hairy keyword
;;; defaulting is done when computing the type assertion to put on the
;;; main-entry argument. In the case of hairy keywords, the default
;;; has been clobbered with NIL, which is the value of the main-entry
;;; arg in the unsupplied case, whatever the actual default value is.
;;; So we can just assume the default is constant, effectively
;;; unioning in NULL, and not totally blow off doing any type
;;; assertion.
(defun find-optional-dispatch-types (od type where)
  (declare (type optional-dispatch od) (type function-type type)
	   (string where))
  (let* ((min (optional-dispatch-min-args od))
	 (req (function-type-required type))
	 (opt (function-type-optional type)))
    (flet ((frob (x y what)
	     (unless (= x y)
	       (note-lossage
		"Definition has ~R ~A arg~P, but ~A has ~R."
		x what x where y))))
      (frob min (length req) "fixed")
      (frob (- (optional-dispatch-max-args od) min) (length opt) "optional"))
    (flet ((frob (x y what)
	     (unless (eq x y)
	       (note-lossage
		"Definition ~:[doesn't have~;has~] ~A, but ~
		~A ~:[doesn't~;does~]."
		x what where y))))
      (frob (optional-dispatch-keyp od) (function-type-keyp type)
	    "keyword args")
      (unless (optional-dispatch-keyp od)
	(frob (not (null (optional-dispatch-more-entry od)))
	      (not (null (function-type-rest type)))
	      "rest args"))
      (frob (optional-dispatch-allowp od) (function-type-allowp type)
	    "&allow-other-keys"))

    (when *lossage-detected*
      (return-from find-optional-dispatch-types (values nil nil)))

    (collect ((res)
	      (vars))
      (let ((keys (function-type-keywords type))
	    (arglist (optional-dispatch-arglist od)))
	(dolist (arg arglist)
	  (cond
	   ((lambda-var-arg-info arg)
	    (let* ((info (lambda-var-arg-info arg))
		   (default (arg-info-default info))
		   (def-type (when (constantp default)
			       (ctype-of (eval default)))))
	      (ecase (arg-info-kind info)
		(:keyword
		 (let* ((key (arg-info-keyword info))
			(kinfo (find key keys :key #'key-info-name)))
		   (cond
		    (kinfo
		     (res (type-union (key-info-type kinfo)
				      (or def-type (specifier-type 'null)))))
		    (t
		     (note-lossage
		      "Defining a ~S keyword not present in ~A."
		      key where)
		     (res *universal-type*)))))
		(:required (res (pop req)))
		(:optional
		 (res (type-union (pop opt) (or def-type *universal-type*))))
		(:rest
		 (when (function-type-rest type)
		   (res (specifier-type 'list))))
		(:more-context
		 (when (function-type-rest type)
		   (res *universal-type*)))
		(:more-count
		 (when (function-type-rest type)
		   (res (specifier-type 'fixnum)))))
	      (vars arg)
	      (when (arg-info-supplied-p info)
		(res *universal-type*)
		(vars (arg-info-supplied-p info)))))
	   (t
	    (res (pop req))
	    (vars arg))))

	(dolist (key keys)
	  (unless (find (key-info-name key) arglist
			:key #'(lambda (x)
				 (let ((info (lambda-var-arg-info x)))
				   (when info
				     (arg-info-keyword info)))))
	    (note-lossage
	     "Definition lacks the ~S keyword present in ~A."
	     (key-info-name key) where))))

      (try-type-intersections (vars) (res) where))))

;;; Check that Type doesn't specify any funny args, and do the
;;; intersection.
(defun find-lambda-types (lambda type where)
  (declare (type clambda lambda) (type function-type type) (string where))
  (flet ((frob (x what)
	   (when x
	     (note-lossage
	      "Definition has no ~A, but the ~A did."
	      what where))))
    (frob (function-type-optional type) "optional args")
    (frob (function-type-keyp type) "keyword args")
    (frob (function-type-rest type) "rest arg"))
  (let* ((vars (lambda-vars lambda))
	 (nvars (length vars))
	 (req (function-type-required type))
	 (nreq (length req)))
    (unless (= nvars nreq)
      (note-lossage "Definition has ~R arg~:P, but the ~A has ~R."
		    nvars where nreq))
    (if *lossage-detected*
	(values nil nil)
	(try-type-intersections vars req where))))

;;; Check for syntactic and type conformance between the definition
;;; Functional and the specified Function-Type. If they are compatible
;;; and Really-Assert is T, then add type assertions to the definition
;;; from the Function-Type.
;;;
;;; If there is a syntactic or type problem, then we call
;;; Error-Function with an error message using Where as context
;;; describing where Function-Type came from.
;;;
;;; If there is no problem, we return T (even if Really-Assert was
;;; false). If there was a problem, we return NIL.
(defun assert-definition-type
       (functional type &key (really-assert t)
		   ((:error-function *error-function*) #'compiler-warning)
		   warning-function
		   (where "previous declaration"))
  (declare (type functional functional)
	   (type function *error-function*)
	   (string where))
  (unless (function-type-p type) (return-from assert-definition-type t))
  (let ((*lossage-detected* nil))
    (multiple-value-bind (vars types)
	(if (function-type-wild-args type)
	    (values nil nil)
	    (etypecase functional
	      (optional-dispatch
	       (find-optional-dispatch-types functional type where))
	      (clambda
	       (find-lambda-types functional type where))))
      (let* ((type-returns (function-type-returns type))
	     (return (lambda-return (main-entry functional)))
	     (atype (when return
		      (continuation-asserted-type (return-result return)))))
	(cond
	 ((and atype (not (values-types-intersect atype type-returns)))
	  (note-lossage
	   "The result type from ~A:~%  ~S~@
	   conflicts with the definition's result type assertion:~%  ~S"
	   where (type-specifier type-returns) (type-specifier atype))
	  nil)
	 (*lossage-detected* nil)
	 ((not really-assert) t)
	 (t
	  (when atype
	    (assert-continuation-type (return-result return) atype))
	  (loop for var in vars and type in types do
	    (cond ((basic-var-sets var)
		   (when (and warning-function
			      (not (csubtypep (leaf-type var) type)))
		     (funcall warning-function
			      "Assignment to argument: ~S~%  ~
			       prevents use of assertion from function ~
			       type ~A:~%  ~S~%"
			      (leaf-name var) where (type-specifier type))))
		  (t
		   (setf (leaf-type var) type)
		   (dolist (ref (leaf-refs var))
		     (derive-node-type ref type)))))
	  t))))))

;;;; the PARSE-DEFMACRO function and related code

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!KERNEL")

;;; variables for accumulating the results of parsing a DEFMACRO. (Declarations
;;; in DEFMACRO are the reason this isn't as easy as it sounds.)
(defvar *arg-tests* nil) ; tests that do argument counting at expansion time
(declaim (type list *arg-tests*))
(defvar *system-lets* nil) ; LET bindings done to allow lambda-list parsing
(declaim (type list *system-lets*))
(defvar *user-lets* nil) ; LET bindings that the user has explicitly supplied
(declaim (type list *user-lets*))

;; the default default for unsupplied optional and keyword args
(defvar *default-default* nil)

;;; temps that we introduce and might not reference
(defvar *ignorable-vars*)
(declaim (type list *ignorable-vars*))

;;; Return, as multiple-values, a body, possibly a declare form to put where
;;; this code is inserted, the documentation for the parsed body, and bounds
;;; on the number of arguments.
(defun parse-defmacro (lambda-list arg-list-name body name error-kind
				   &key
				   (anonymousp nil)
				   (doc-string-allowed t)
				   ((:environment env-arg-name))
				   ((:default-default *default-default*))
				   (error-fun 'error))
  (multiple-value-bind (forms declarations documentation)
      (parse-body body doc-string-allowed)
    (let ((*arg-tests* ())
	  (*user-lets* ())
	  (*system-lets* ())
	  (*ignorable-vars* ()))
      (multiple-value-bind (env-arg-used minimum maximum)
	  (parse-defmacro-lambda-list lambda-list arg-list-name name
				      error-kind error-fun (not anonymousp)
				      nil env-arg-name)
	(values `(let* ,(nreverse *system-lets*)
		   ,@(when *ignorable-vars*
		       `((declare (ignorable ,@*ignorable-vars*))))
		   ,@*arg-tests*
		   (let* ,(nreverse *user-lets*)
		     ,@declarations
		     ,@forms))
		`(,@(when (and env-arg-name (not env-arg-used))
		      `((declare (ignore ,env-arg-name)))))
		documentation
		minimum
		maximum)))))

;;; partial reverse-engineered documentation:
;;;   TOP-LEVEL is true for calls through PARSE-DEFMACRO from DEFSETF and
;;;     DESTRUCTURING-BIND, false otherwise.
;;; -- WHN 19990620
(defun parse-defmacro-lambda-list (possibly-dotted-lambda-list
				   arg-list-name
				   name
				   error-kind
				   error-fun
				   &optional
				   top-level
				   env-illegal
				   env-arg-name)
  (let* (;; PATH is a sort of pointer into the part of the lambda list we're
	 ;; considering at this point in the code. PATH-0 is the root of the
	 ;; lambda list, which is the initial value of PATH.
	 (path-0 (if top-level
		   `(cdr ,arg-list-name)
		   arg-list-name))
	 (path path-0) ; (will change below)
	 (now-processing :required)
	 (maximum 0)
	 (minimum 0)
	 (keys ())
	 ;; ANSI specifies that dotted lists are "treated exactly as if the
	 ;; parameter name that ends the list had appeared preceded by &rest."
	 ;; We force this behavior by transforming dotted lists into ordinary
	 ;; lists with explicit &REST elements.
	 (lambda-list (do ((in-pdll possibly-dotted-lambda-list (cdr in-pdll))
			   (reversed-result nil))
			  ((atom in-pdll)
			   (nreverse (if in-pdll
				       (list* in-pdll '&rest reversed-result)
				       reversed-result)))
			(push (car in-pdll) reversed-result)))
	 rest-name restp allow-other-keys-p env-arg-used)
    (when (member '&whole (rest lambda-list))
      (error "&WHOLE may only appear first in ~S lambda-list." error-kind))
    (do ((rest-of-args lambda-list (cdr rest-of-args)))
	((null rest-of-args))
      (let ((var (car rest-of-args)))
	(cond ((eq var '&whole)
	       (cond ((and (cdr rest-of-args) (symbolp (cadr rest-of-args)))
		      (setq rest-of-args (cdr rest-of-args))
		      (push-let-binding (car rest-of-args) arg-list-name nil))
		     (t
		      (defmacro-error "&WHOLE" error-kind name))))
	      ((eq var '&environment)
	       (cond (env-illegal
		      (error "&ENVIRONMENT is not valid with ~S." error-kind))
		     ((not top-level)
		      (error "&ENVIRONMENT is only valid at top level of ~
			      lambda-list.")))
	       (cond ((and (cdr rest-of-args) (symbolp (cadr rest-of-args)))
		      (setq rest-of-args (cdr rest-of-args))
		      (push-let-binding (car rest-of-args) env-arg-name nil)
		      (setq env-arg-used t))
		     (t
		      (defmacro-error "&ENVIRONMENT" error-kind name))))
	      ((or (eq var '&rest)
		   (eq var '&body))
	       (cond ((and (cdr rest-of-args) (symbolp (cadr rest-of-args)))
		      (setq rest-of-args (cdr rest-of-args))
		      (setq restp t)
		      (push-let-binding (car rest-of-args) path nil))
		     (t
		      (defmacro-error (symbol-name var) error-kind name))))
	      ((eq var '&optional)
	       (setq now-processing :optionals))
	      ((eq var '&key)
	       (setq now-processing :keywords)
	       (setq rest-name (gensym "KEYWORDS-"))
	       (push rest-name *ignorable-vars*)
	       (setq restp t)
	       (push-let-binding rest-name path t))
	      ((eq var '&allow-other-keys)
	       (setq allow-other-keys-p t))
	      ((eq var '&aux)
	       (setq now-processing :auxs))
	      ((listp var)
	       (cond ; (since it's too early to use CASE)
		 ((eq now-processing :required)
		  (let ((sub-list-name (gensym "SUBLIST-")))
		    (push-sub-list-binding sub-list-name `(car ,path) var
					   name error-kind error-fun)
		    (parse-defmacro-lambda-list var sub-list-name name
						error-kind error-fun))
		  (setq path `(cdr ,path)
			minimum (1+ minimum)
			maximum (1+ maximum)))
		 ((eq now-processing :optionals)
		  (when (> (length var) 3)
		    (cerror "Ignore extra noise."
			    "more than variable, initform, and suppliedp ~
			    in &optional binding: ~S"
			    var))
		  (push-optional-binding (car var) (cadr var) (caddr var)
					 `(not (null ,path)) `(car ,path)
					 name error-kind error-fun)
		  (setq path `(cdr ,path)
			maximum (1+ maximum)))
		 ((eq now-processing :keywords)
		  (let* ((keyword-given (consp (car var)))
			 (variable (if keyword-given
				       (cadar var)
				       (car var)))
			 (keyword (if keyword-given
				      (caar var)
				      (keywordicate variable)))
			 (supplied-p (caddr var)))
		    (push-optional-binding variable (cadr var) supplied-p
					   `(keyword-supplied-p ',keyword
								,rest-name)
					   `(lookup-keyword ',keyword
							    ,rest-name)
					   name error-kind error-fun)
		    (push keyword keys)))
		 ((eq now-processing :auxs)
		  (push-let-binding (car var) (cadr var) nil))))
	      ((symbolp var)
	       (cond ; (too early in bootstrapping to use CASE)
		;; FIXME: ^ This "too early in bootstrapping" is no
		;; longer an issue in current SBCL bootstrapping.
		 ((eq now-processing :required)
		  (push-let-binding var `(car ,path) nil)
		  (setq minimum (1+ minimum)
			maximum (1+ maximum)
			path `(cdr ,path)))
		 ((eq now-processing :optionals)
		  (push-let-binding var `(car ,path) nil `(not (null ,path)))
		  (setq path `(cdr ,path)
			maximum (1+ maximum)))
		 ((eq now-processing :keywords)
		  (let ((key (keywordicate var)))
		    (push-let-binding var
				      `(lookup-keyword ,key ,rest-name)
				      nil)
		    (push key keys)))
		 ((eq now-processing :auxs)
		  (push-let-binding var nil nil))))
	      (t
	       (error "non-symbol in lambda-list: ~S" var)))))
    (push `(unless ,(if restp
			;; (If RESTP, then the argument list might be
			;; dotted, in which case ordinary LENGTH won't
			;; work.)
			`(list-of-length-at-least-p ,path-0 ,minimum)
			`(proper-list-of-length-p ,path-0 ,minimum ,maximum))
	     ,(if (eq error-fun 'error)
		  `(do-arg-count-error ',error-kind ',name ,path-0
				       ',lambda-list ,minimum
				       ,(unless restp maximum))
		  `(,error-fun 'defmacro-ll-arg-count-error
			       :kind ',error-kind
			       ,@(when name `(:name ',name))
			       :argument ,path-0
			       :lambda-list ',lambda-list
			       :minimum ,minimum
			       ,@(unless restp
				   `(:maximum ,maximum)))))
	  *arg-tests*)
    (when keys
      (let ((problem (gensym "KEY-PROBLEM-"))
	    (info (gensym "INFO-")))
	(push `(multiple-value-bind (,problem ,info)
		   (verify-keywords ,rest-name
				    ',keys
				    ',allow-other-keys-p)
		 (when ,problem
		   (,error-fun
		    'defmacro-ll-broken-key-list-error
		    :kind ',error-kind
		    ,@(when name `(:name ',name))
		    :problem ,problem
		    :info ,info)))
	      *arg-tests*)))
    (values env-arg-used minimum (if (null restp) maximum nil))))

(defun push-sub-list-binding (variable path object name error-kind error-fun)
  (let ((var (gensym "TEMP-")))
    (push `(,variable
	    (let ((,var ,path))
	      (if (listp ,var)
		,var
		(,error-fun 'defmacro-bogus-sublist-error
			    :kind ',error-kind
			    ,@(when name `(:name ',name))
			    :object ,var
			    :lambda-list ',object))))
	  *system-lets*)))

(defun push-let-binding (variable path systemp &optional condition
				  (init-form *default-default*))
  (let ((let-form (if condition
		      `(,variable (if ,condition ,path ,init-form))
		      `(,variable ,path))))
    (if systemp
      (push let-form *system-lets*)
      (push let-form *user-lets*))))

(defun push-optional-binding (value-var init-form supplied-var condition path
					name error-kind error-fun)
  (unless supplied-var
    (setq supplied-var (gensym "SUPPLIEDP-")))
  (push-let-binding supplied-var condition t)
  (cond ((consp value-var)
	 (let ((whole-thing (gensym "OPTIONAL-SUBLIST-")))
	   (push-sub-list-binding whole-thing
				  `(if ,supplied-var ,path ,init-form)
				  value-var name error-kind error-fun)
	   (parse-defmacro-lambda-list value-var whole-thing name
				       error-kind error-fun)))
	((symbolp value-var)
	 (push-let-binding value-var path nil supplied-var init-form))
	(t
	 (error "Illegal optional variable name: ~S" value-var))))

(defun defmacro-error (problem kind name)
  (error "Illegal or ill-formed ~A argument in ~A~@[ ~S~]."
	 problem kind name))

;;; Determine whether KEY-LIST is a valid list of keyword/value pairs. Do not
;;; signal the error directly, 'cause we don't know how it should be signaled.
(defun verify-keywords (key-list valid-keys allow-other-keys)
  (do ((already-processed nil)
       (unknown-keyword nil)
       (remaining key-list (cddr remaining)))
      ((null remaining)
       (if (and unknown-keyword
		(not allow-other-keys)
		(not (lookup-keyword :allow-other-keys key-list)))
	   (values :unknown-keyword (list unknown-keyword valid-keys))
	   (values nil nil)))
    (cond ((not (and (consp remaining) (listp (cdr remaining))))
	   (return (values :dotted-list key-list)))
	  ((null (cdr remaining))
	   (return (values :odd-length key-list)))
	  ((member (car remaining) already-processed)
	   (return (values :duplicate (car remaining))))
	  ((or (eq (car remaining) :allow-other-keys)
	       (member (car remaining) valid-keys))
	   (push (car remaining) already-processed))
	  (t
	   (setq unknown-keyword (car remaining))))))

(defun lookup-keyword (keyword key-list)
  (do ((remaining key-list (cddr remaining)))
      ((endp remaining))
    (when (eq keyword (car remaining))
      (return (cadr remaining)))))

(defun keyword-supplied-p (keyword key-list)
  (do ((remaining key-list (cddr remaining)))
      ((endp remaining))
    (when (eq keyword (car remaining))
      (return t))))

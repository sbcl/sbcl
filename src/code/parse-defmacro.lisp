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
(defvar *env-var* nil) ; &ENVIRONMENT variable name

;; the default default for unsupplied &OPTIONAL and &KEY args
(defvar *default-default* nil)

;;; temps that we introduce and might not reference
(defvar *ignorable-vars*)
(declaim (type list *ignorable-vars*))

;;; Return, as multiple values, a body, possibly a DECLARE form to put
;;; where this code is inserted, the documentation for the parsed
;;; body, and bounds on the number of arguments.
(defun parse-defmacro (lambda-list arg-list-name body name error-kind
				   &key
				   (anonymousp nil)
				   (doc-string-allowed t)
				   ((:environment env-arg-name))
				   ((:default-default *default-default*))
				   (error-fun 'error)
                                   (wrap-block t))
  (multiple-value-bind (forms declarations documentation)
      (parse-body body :doc-string-allowed doc-string-allowed)
    (let ((*arg-tests* ())
	  (*user-lets* ())
	  (*system-lets* ())
	  (*ignorable-vars* ())
          (*env-var* nil))
      (multiple-value-bind (env-arg-used minimum maximum)
	  (parse-defmacro-lambda-list lambda-list arg-list-name name
				      error-kind error-fun (not anonymousp)
				      nil)
	(values `(let* (,@(when env-arg-used
                            `((,*env-var* ,env-arg-name)))
                        ,@(nreverse *system-lets*))
		   ,@(when *ignorable-vars*
		       `((declare (ignorable ,@*ignorable-vars*))))
		   ,@*arg-tests*
		   (let* ,(nreverse *user-lets*)
		     ,@declarations
                     ,@(if wrap-block
                           `((block ,(fun-name-block-name name)
                               ,@forms))
                           forms)))
		`(,@(when (and env-arg-name (not env-arg-used))
                      `((declare (ignore ,env-arg-name)))))
		documentation
		minimum
		maximum)))))

;;; partial reverse-engineered documentation:
;;;   TOPLEVEL is true for calls through PARSE-DEFMACRO from DEFSETF and
;;;     DESTRUCTURING-BIND, false otherwise.
;;; -- WHN 19990620
(defun parse-defmacro-lambda-list (possibly-dotted-lambda-list
				   arg-list-name
				   name
				   error-kind
				   error-fun
				   &optional
				   toplevel
				   env-illegal)
  (let* (;; PATH is a sort of pointer into the part of the lambda list we're
	 ;; considering at this point in the code. PATH-0 is the root of the
	 ;; lambda list, which is the initial value of PATH.
	 (path-0 (if toplevel
                     `(cdr ,arg-list-name)
                     arg-list-name))
	 (path path-0) ; (will change below)
	 (now-processing :required)
	 (maximum 0)
	 (minimum 0)
	 (keys ())
	 (key-seen nil)
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
      (macrolet ((process-sublist (var sublist-name path)
                   (once-only ((var var))
                     `(if (listp ,var)
                          (let ((sub-list-name (gensym ,sublist-name)))
                            (push-sub-list-binding sub-list-name ,path ,var
                                                   name error-kind error-fun)
                            (parse-defmacro-lambda-list ,var sub-list-name name
                                                        error-kind error-fun))
                          (push-let-binding ,var ,path nil)))))
        (let ((var (car rest-of-args)))
          (typecase var
            (list
             (case now-processing
               ((:required)
                (when restp
                  (defmacro-error "required argument after &REST/&BODY"
                      error-kind name))
                (process-sublist var "SUBLIST-" `(car ,path))
                (setq path `(cdr ,path)
                      minimum (1+ minimum)
                      maximum (1+ maximum)))
               ((:optionals)
                (destructuring-bind (varname &optional initform supplied-p)
                    var
                  (push-optional-binding varname initform supplied-p
                                         `(not (null ,path)) `(car ,path)
                                         name error-kind error-fun))
                (setq path `(cdr ,path)
                      maximum (1+ maximum)))
               ((:keywords)
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
               ((:auxs)
                (push-let-binding (car var) (cadr var) nil))))
            ((and symbol (not (eql nil)))
             (case var
               (&whole
                (cond ((cdr rest-of-args)
                       (setq rest-of-args (cdr rest-of-args))
                       (process-sublist (car rest-of-args)
                                        "WHOLE-LIST-" arg-list-name))
                      (t
                       (defmacro-error "&WHOLE" error-kind name))))
               (&environment
                (cond (env-illegal
                       (error "&ENVIRONMENT is not valid with ~S." error-kind))
                      ((not toplevel)
                       (error "&ENVIRONMENT is only valid at top level of ~
                             lambda-list."))
                      (env-arg-used
                       (error "Repeated &ENVIRONMENT.")))
                (cond ((and (cdr rest-of-args) (symbolp (cadr rest-of-args)))
                       (setq rest-of-args (cdr rest-of-args))
                       (check-defmacro-arg (car rest-of-args))
                       (setq *env-var* (car rest-of-args))
                       (setq env-arg-used t))
                      (t
                       (defmacro-error "&ENVIRONMENT" error-kind name))))
               ((&rest &body)
                (cond ((and (not restp) (cdr rest-of-args))
                       (setq rest-of-args (cdr rest-of-args))
                       (setq restp t)
                       (process-sublist (car rest-of-args) "REST-LIST-" path))
                      (t
                       (defmacro-error (symbol-name var) error-kind name))))
               (&optional
                (setq now-processing :optionals))
               (&key
                (setq now-processing :keywords)
                (setq rest-name (gensym "KEYWORDS-"))
                (push rest-name *ignorable-vars*)
                (setq restp t)
		(setq key-seen t)
                (push-let-binding rest-name path t))
               (&allow-other-keys
                (setq allow-other-keys-p t))
               (&aux
                (setq now-processing :auxs))
               ;; FIXME: Other lambda list keywords.
               (t
                (case now-processing
                  ((:required)
                   (when restp
                     (defmacro-error "required argument after &REST/&BODY"
                         error-kind name))
                   (push-let-binding var `(car ,path) nil)
                   (setq minimum (1+ minimum)
                         maximum (1+ maximum)
                         path `(cdr ,path)))
                  ((:optionals)
                   (push-let-binding var `(car ,path) nil `(not (null ,path)))
                   (setq path `(cdr ,path)
                         maximum (1+ maximum)))
                  ((:keywords)
                   (let ((key (keywordicate var)))
                     (push-let-binding var
                                       `(lookup-keyword ,key ,rest-name)
                                       nil)
                     (push key keys)))
                  ((:auxs)
                   (push-let-binding var nil nil))))))
            (t
             (error "non-symbol in lambda-list: ~S" var))))))
    (let (;; common subexpression, suitable for passing to functions
	  ;; which expect a MAXIMUM argument regardless of whether
	  ;; there actually is a maximum number of arguments
	  ;; (expecting MAXIMUM=NIL when there is no maximum)
	  (explicit-maximum (and (not restp) maximum)))
      (unless (and restp (zerop minimum))
        (push `(unless ,(if restp
                            ;; (If RESTP, then the argument list might be
                            ;; dotted, in which case ordinary LENGTH won't
                            ;; work.)
                            `(list-of-length-at-least-p ,path-0 ,minimum)
                            `(proper-list-of-length-p ,path-0 ,minimum ,maximum))
                 ,(if (eq error-fun 'error)
                      `(arg-count-error ',error-kind ',name ,path-0
                                        ',lambda-list ,minimum
                                        ,explicit-maximum)
                      `(,error-fun 'arg-count-error
                                   :kind ',error-kind
                                   ,@(when name `(:name ',name))
                                   :args ,path-0
                                   :lambda-list ',lambda-list
                                   :minimum ,minimum
                                   :maximum ,explicit-maximum)))
              *arg-tests*))
      (when key-seen
	(let ((problem (gensym "KEY-PROBLEM-"))
	      (info (gensym "INFO-")))
	  (push `(multiple-value-bind (,problem ,info)
		     (verify-keywords ,rest-name
				      ',keys
				      ',allow-other-keys-p)
		   (when ,problem
		     (,error-fun
		      'defmacro-lambda-list-broken-key-list-error
		      :kind ',error-kind
		      ,@(when name `(:name ',name))
		      :problem ,problem
		      :info ,info)))
		*arg-tests*)))
      (values env-arg-used minimum explicit-maximum))))

;;; We save space in macro definitions by calling this function.
(defun arg-count-error (error-kind name args lambda-list minimum maximum)
  (let (#-sb-xc-host
	(sb!debug:*stack-top-hint* (nth-value 1 (find-caller-name-and-frame))))
    (error 'arg-count-error
	   :kind error-kind
	   :name name
	   :args args
	   :lambda-list lambda-list
	   :minimum minimum
	   :maximum maximum)))

(defun push-sub-list-binding (variable path object name error-kind error-fun)
  (check-defmacro-arg variable)
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
  (check-defmacro-arg variable)
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
	 (error "illegal optional variable name: ~S" value-var))))

(defun defmacro-error (problem kind name)
  (error "illegal or ill-formed ~A argument in ~A~@[ ~S~]"
	 problem kind name))

(defun check-defmacro-arg (arg)
  (when (or (and *env-var* (eq arg *env-var*))
            (member arg *system-lets* :key #'car)
            (member arg *user-lets* :key #'car))
    (error "variable ~S occurs more than once" arg)))

;;; Determine whether KEY-LIST is a valid list of keyword/value pairs.
;;; Do not signal the error directly, 'cause we don't know how it
;;; should be signaled.
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

;;;; that part of the condition system which can or should come early
;;;; (mostly macro-related)

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!KERNEL")

;;;; restarts

;;; a list of lists of restarts
(defvar *restart-clusters* '())

;;;  An ALIST (condition . restarts) which records the restarts currently
;;; associated with Condition.
(defvar *condition-restarts* ())

(defun compute-restarts (&optional condition)
  #!+sb-doc
  "Return a list of all the currently active restarts ordered from most
   recently established to less recently established. If Condition is
   specified, then only restarts associated with Condition (or with no
   condition) will be returned."
  (let ((associated ())
	(other ()))
    (dolist (alist *condition-restarts*)
      (if (eq (car alist) condition)
	  (setq associated (cdr alist))
	  (setq other (append (cdr alist) other))))
    (collect ((res))
      (dolist (restart-cluster *restart-clusters*)
	(dolist (restart restart-cluster)
	  (when (and (or (not condition)
			 (member restart associated)
			 (not (member restart other)))
		     (funcall (restart-test-function restart) condition))
	    (res restart))))
      (res))))

(defstruct (restart (:copier nil))
  name
  function
  report-function
  interactive-function
  (test-function #'(lambda (cond) (declare (ignore cond)) t)))
(def!method print-object ((restart restart) stream)
  (if *print-escape*
      (print-unreadable-object (restart stream :type t :identity t))
      (restart-report restart stream)))

#!+sb-doc
(setf (fdocumentation 'restart-name 'function)
      "Returns the name of the given restart object.")

(defun restart-report (restart stream)
  (funcall (or (restart-report-function restart)
	       (let ((name (restart-name restart)))
		 #'(lambda (stream)
		     (if name (format stream "~S" name)
			      (format stream "~S" restart)))))
	   stream))

(defmacro with-condition-restarts (condition-form restarts-form &body body)
  #!+sb-doc
  "WITH-CONDITION-RESTARTS Condition-Form Restarts-Form Form*
   Evaluates the Forms in a dynamic environment where the restarts in the list
   Restarts-Form are associated with the condition returned by Condition-Form.
   This allows FIND-RESTART, etc., to recognize restarts that are not related
   to the error currently being debugged. See also RESTART-CASE."
  (let ((n-cond (gensym)))
    `(let ((*condition-restarts*
	    (cons (let ((,n-cond ,condition-form))
		    (cons ,n-cond
			  (append ,restarts-form
				  (cdr (assoc ,n-cond *condition-restarts*)))))
		  *condition-restarts*)))
       ,@body)))

(defmacro restart-bind (bindings &body forms)
  #!+sb-doc
  "Executes forms in a dynamic context where the given restart bindings are
   in effect. Users probably want to use RESTART-CASE. When clauses contain
   the same restart name, FIND-RESTART will find the first such clause."
  `(let ((*restart-clusters*
	  (cons (list
		 ,@(mapcar #'(lambda (binding)
			       (unless (or (car binding)
					   (member :report-function
						   binding
						   :test #'eq))
				 (warn "Unnamed restart does not have a ~
					report function: ~S"
				       binding))
			       `(make-restart
				 :name ',(car binding)
				 :function ,(cadr binding)
				 ,@(cddr binding)))
			       bindings))
		*restart-clusters*)))
     ,@forms))

(defun find-restart (name &optional condition)
  #!+sb-doc
  "Returns the first restart named name. If name is a restart, it is returned
   if it is currently active. If no such restart is found, nil is returned.
   It is an error to supply nil as a name. If Condition is specified and not
   NIL, then only restarts associated with that condition (or with no
   condition) will be returned."
  (find-if #'(lambda (x)
	       (or (eq x name)
		   (eq (restart-name x) name)))
	   (compute-restarts condition)))

(defun invoke-restart (restart &rest values)
  #!+sb-doc
  "Calls the function associated with the given restart, passing any given
   arguments. If the argument restart is not a restart or a currently active
   non-nil restart name, then a control-error is signalled."
  (let ((real-restart (find-restart restart)))
    (unless real-restart
      (error 'simple-control-error
	     :format-control "Restart ~S is not active."
	     :format-arguments (list restart)))
    (apply (restart-function real-restart) values)))

(defun invoke-restart-interactively (restart)
  #!+sb-doc
  "Calls the function associated with the given restart, prompting for any
   necessary arguments. If the argument restart is not a restart or a
   currently active non-nil restart name, then a control-error is signalled."
  (let ((real-restart (find-restart restart)))
    (unless real-restart
      (error 'simple-control-error
	     :format-control "Restart ~S is not active."
	     :format-arguments (list restart)))
    (apply (restart-function real-restart)
	   (let ((interactive-function
		  (restart-interactive-function real-restart)))
	     (if interactive-function
		 (funcall interactive-function)
		 '())))))

(eval-when (:compile-toplevel :load-toplevel :execute)
;;; Wrap the RESTART-CASE expression in a WITH-CONDITION-RESTARTS if
;;; appropriate. Gross, but it's what the book seems to say...
(defun munge-restart-case-expression (expression data)
  (let ((exp (macroexpand expression)))
    (if (consp exp)
	(let* ((name (car exp))
	       (args (if (eq name 'cerror) (cddr exp) (cdr exp))))
	  (if (member name '(signal error cerror warn))
	      (once-only ((n-cond `(coerce-to-condition
				    ,(first args)
				    (list ,@(rest args))
				    ',(case name
					(warn 'simple-warning)
					(signal 'simple-condition)
					(t 'simple-error))
				    ',name)))
		`(with-condition-restarts
		     ,n-cond
		     (list ,@(mapcar (lambda (da)
				       `(find-restart ',(nth 0 da)))
				     data))
		   ,(if (eq name 'cerror)
			`(cerror ,(second expression) ,n-cond)
			`(,name ,n-cond))))
	      expression))
	expression)))
) ; EVAL-WHEN

;;; FIXME: I did a fair amount of rearrangement of this code in order to
;;; get WITH-KEYWORD-PAIRS to work cleanly. This code should be tested..
(defmacro restart-case (expression &body clauses)
  #!+sb-doc
  "(RESTART-CASE form
   {(case-name arg-list {keyword value}* body)}*)
   The form is evaluated in a dynamic context where the clauses have special
   meanings as points to which control may be transferred (see INVOKE-RESTART).
   When clauses contain the same case-name, FIND-RESTART will find the first
   such clause. If Expression is a call to SIGNAL, ERROR, CERROR or WARN (or
   macroexpands into such) then the signalled condition will be associated with
   the new restarts."
  (flet ((transform-keywords (&key report interactive test)
	   (let ((result '()))
	     (when report
	       (setq result (list* (if (stringp report)
				       `#'(lambda (stream)
					    (write-string ,report stream))
				       `#',report)
				   :report-function
				   result)))
	     (when interactive
	       (setq result (list* `#',interactive
				   :interactive-function
				   result)))
	     (when test
	       (setq result (list* `#',test
				   :test-function
				   result)))
	     (nreverse result)))
	 (parse-keyword-pairs (list keys)
	   (do ((l list (cddr l))
		(k '() (list* (cadr l) (car l) k)))
	       ((or (null l) (not (member (car l) keys)))
		(values (nreverse k) l)))))
    (let ((block-tag (gensym))
	  (temp-var (gensym))
	  (data
	   (macrolet (;; KLUDGE: This started as an old DEFMACRO
		      ;; WITH-KEYWORD-PAIRS general utility, which was used
		      ;; only in this one place in the code. It was translated
		      ;; literally into this MACROLET in order to avoid some
		      ;; cross-compilation bootstrap problems. It would almost
		      ;; certainly be clearer, and it would certainly be more
		      ;; concise, to do a more idiomatic translation, merging
		      ;; this with the TRANSFORM-KEYWORDS logic above.
		      ;;   -- WHN 19990925
		      (with-keyword-pairs ((names expression) &body forms)
			(let ((temp (member '&rest names)))
			  (unless (= (length temp) 2)
			    (error "&REST keyword is ~:[missing~;misplaced~]."
				   temp))
			  (let* ((key-vars (ldiff names temp))
				 (keywords (mapcar #'keywordicate key-vars))
				 (key-var (gensym))
				 (rest-var (cadr temp)))
			    `(multiple-value-bind (,key-var ,rest-var)
				 (parse-keyword-pairs ,expression ',keywords)
			       (let ,(mapcar (lambda (var keyword)
					       `(,var (getf ,key-var
							    ,keyword)))
					     key-vars keywords)
				 ,@forms))))))
	     (mapcar (lambda (clause)
		       (with-keyword-pairs ((report interactive test
						    &rest forms)
					    (cddr clause))
			 (list (car clause) ;name=0
			       (gensym) ;tag=1
			       (transform-keywords :report report ;keywords=2
						   :interactive interactive
						   :test test)
			       (cadr clause) ;bvl=3
			       forms))) ;body=4
		   clauses))))
      `(block ,block-tag
	 (let ((,temp-var nil))
	   (tagbody
	    (restart-bind
		,(mapcar #'(lambda (datum)
			     (let ((name (nth 0 datum))
				   (tag  (nth 1 datum))
				   (keys (nth 2 datum)))
			       `(,name #'(lambda (&rest temp)
					   (setq ,temp-var temp)
					   (go ,tag))
				       ,@keys)))
			 data)
	      (return-from ,block-tag
			   ,(munge-restart-case-expression expression data)))
	    ,@(mapcan #'(lambda (datum)
			  (let ((tag  (nth 1 datum))
				(bvl  (nth 3 datum))
				(body (nth 4 datum)))
			    (list tag
				  `(return-from ,block-tag
						(apply #'(lambda ,bvl ,@body)
						       ,temp-var)))))
		      data)))))))

(defmacro with-simple-restart ((restart-name format-string
					     &rest format-arguments)
			       &body forms)
  #!+sb-doc
  "(WITH-SIMPLE-RESTART (restart-name format-string format-arguments)
   body)
   If restart-name is not invoked, then all values returned by forms are
   returned. If control is transferred to this restart, it immediately
   returns the values nil and t."
  `(restart-case
       ;; If there's just one body form, then don't use PROGN. This allows
       ;; RESTART-CASE to "see" calls to ERROR, etc.
       ,(if (= (length forms) 1) (car forms) `(progn ,@forms))
     (,restart-name ()
	:report (lambda (stream)
		  (format stream ,format-string ,@format-arguments))
      (values nil t))))

;;;; HANDLER-BIND

(defvar *handler-clusters* nil)

(defmacro handler-bind (bindings &body forms)
  #!+sb-doc
  "(HANDLER-BIND ( {(type handler)}* )  body)
   Executes body in a dynamic context where the given handler bindings are
   in effect. Each handler must take the condition being signalled as an
   argument. The bindings are searched first to last in the event of a
   signalled condition."
  (let ((member-if (member-if (lambda (x)
				(not (proper-list-of-length-p x 2)))
			      bindings)))
    (when member-if
      (error "ill-formed handler binding: ~S" (first member-if))))
  `(let ((*handler-clusters*
	  (cons (list ,@(mapcar #'(lambda (x) `(cons ',(car x) ,(cadr x)))
				bindings))
		*handler-clusters*)))
     (multiple-value-prog1
	 (progn
	   ,@forms)
       ;; Wait for any float exceptions.
       #!+x86 (float-wait))))

;;;; HANDLER-CASE and IGNORE-ERRORS

(defmacro handler-case (form &rest cases)
  "(HANDLER-CASE form
   { (type ([var]) body) }* )
   Execute FORM in a context with handlers established for the condition
   types. A peculiar property allows type to be :no-error. If such a clause
   occurs, and form returns normally, all its values are passed to this clause
   as if by MULTIPLE-VALUE-CALL.  The :NO-ERROR clause accepts more than one
   var specification."

  ;; FIXME: This old SBCL code uses multiple nested THROW/CATCH
  ;; operations, which seems like an ugly way to handle lexical
  ;; nonlocal exit. MNA sbcl-devel 2001-07-17 provided a patch
  ;; (included below this form, but #+NIL'ed out) to switch over to
  ;; RETURN-FROM, which seems like basically a better idea.
  ;; Unfortunately when using his patch, this reasonable code
  ;;	(DEFUN FOO1I ()
  ;;	  (IF (NOT (IGNORE-ERRORS
  ;;		     (MAKE-PATHNAME :HOST "FOO"
  ;;				    :DIRECTORY "!BLA"
  ;;				    :NAME "BAR")))
  ;;	      (PRINT "OK")
  ;;	      (ERROR "NOTUNLESSNOT")))
  ;; fails (doing ERROR "NOTUNLESSNOT" when it should PRINT "OK"
  ;; instead). I think this may not be a bug in MNA's patch, but 
  ;; instead in the rest of the compiler (e.g. handling of RETURN-FROM)
  ;; but whatever the reason. (I noticed this problem in
  ;; sbcl-0.pre7.14.flaky4.11, and reverted to the old code at that point.
  ;; The problem also occurs at least in sbcl-0.6.12.59 and
  ;; sbcl-0.6.13.) -- WHN
  (let ((no-error-clause (assoc ':no-error cases)))
    (if no-error-clause
        (let ((normal-return (make-symbol "normal-return"))
              (error-return  (make-symbol "error-return")))
          `(block ,error-return
             (multiple-value-call #'(lambda ,@(cdr no-error-clause))
               (block ,normal-return
                 (return-from ,error-return
                   (handler-case (return-from ,normal-return ,form)
                     ,@(remove no-error-clause cases)))))))
        (let ((var (gensym))
              (outer-tag (gensym))
              (inner-tag (gensym))
              (tag-var (gensym))
              (annotated-cases (mapcar #'(lambda (case) (cons (gensym) case))
                                       cases)))
          `(let ((,outer-tag (cons nil nil))
                 (,inner-tag (cons nil nil))
                 ,var ,tag-var)
             ;; FIXME: should be (DECLARE (IGNORABLE ,VAR))
             ,var                       ;ignoreable
             (catch ,outer-tag
               (catch ,inner-tag
                 (throw ,outer-tag
                        (handler-bind
                            ,(mapcar #'(lambda (annotated-case)
                                         `(,(cadr annotated-case)
                                           #'(lambda (temp)
                                               ,(if (caddr annotated-case)
                                                    `(setq ,var temp)
                                                    '(declare (ignore temp)))
                                               (setf ,tag-var
                                                     ',(car annotated-case))
                                               (throw ,inner-tag nil))))
                                     annotated-cases)
                          ,form)))
               (case ,tag-var
                 ,@(mapcar #'(lambda (annotated-case)
                               (let ((body (cdddr annotated-case))
                                     (varp (caddr annotated-case)))
                                 `(,(car annotated-case)
                                   ,@(if varp
                                         `((let ((,(car varp) ,var))
                                             ,@body))
                                         body))))
                           annotated-cases)))))))
  #+nil ; MNA's patched version -- see FIXME above
  (let ((no-error-clause (assoc ':no-error cases)))
    (if no-error-clause
	(let ((normal-return (make-symbol "normal-return"))
	      (error-return  (make-symbol "error-return")))
	  `(block ,error-return
	     (multiple-value-call (lambda ,@(cdr no-error-clause))
	       (block ,normal-return
		 (return-from ,error-return
		   (handler-case (return-from ,normal-return ,form)
		     ,@(remove no-error-clause cases)))))))
	(let ((tag (gensym))
	      (var (gensym))
	      (annotated-cases (mapcar (lambda (case) (cons (gensym) case))
				       cases)))
	  `(block ,tag
	     (let ((,var nil))
	       (declare (ignorable ,var))
	       (tagbody
		(handler-bind
		    ,(mapcar (lambda (annotated-case)
			       (list (cadr annotated-case)
				     `(lambda (temp)
					,(if (caddr annotated-case)
					     `(setq ,var temp)
					     '(declare (ignore temp)))
					(go ,(car annotated-case)))))
			     annotated-cases)
		  (return-from ,tag
		    #!-x86 ,form
		    #!+x86 (multiple-value-prog1 ,form
			     ;; Need to catch FP errors here!
			     (float-wait))))
		,@(mapcan
		   (lambda (annotated-case)
		     (list (car annotated-case)
			   (let ((body (cdddr annotated-case)))
			     `(return-from
				  ,tag
				,(cond ((caddr annotated-case)
					`(let ((,(caaddr annotated-case)
						,var))
					   ,@body))
				       ((not (cdr body))
					(car body))
				       (t
					`(progn ,@body)))))))
		   annotated-cases))))))))

(defmacro ignore-errors (&rest forms)
  #!+sb-doc
  "Execute FORMS handling ERROR conditions, returning the result of the last
  form, or (VALUES NIL the-ERROR-that-was-caught) if an ERROR was handled."
  `(handler-case (progn ,@forms)
     (error (condition) (values nil condition))))

;;;; helper functions for restartable error handling which couldn't be
;;;; defined 'til now 'cause they use the RESTART-CASE macro

(defun assert-error (assertion places datum &rest arguments)
  (let ((cond (if datum
		(coerce-to-condition datum
						    arguments
						    'simple-error
						    'error)
		(make-condition 'simple-error
				:format-control "The assertion ~S failed."
				:format-arguments (list assertion)))))
    (restart-case
	(error cond)
      (continue ()
		:report (lambda (stream)
			  (format stream "Retry assertion")
			  (if places
			      (format stream
				      " with new value~P for ~{~S~^, ~}."
				      (length places)
				      places)
			      (format stream ".")))
		nil))))

;;; READ-EVALUATED-FORM is used as the interactive method for restart cases
;;; setup by the Common Lisp "casing" (e.g., CCASE and CTYPECASE) macros
;;; and by CHECK-TYPE.
(defun read-evaluated-form ()
  (format *query-io* "~&Type a form to be evaluated:~%")
  (list (eval (read *query-io*))))

(defun check-type-error (place place-value type type-string)
  (let ((cond (if type-string
		  (make-condition 'simple-type-error
				  :datum place
				  :expected-type type
				  :format-control
				  "The value of ~S is ~S, which is not ~A."
				  :format-arguments (list place
							  place-value
							  type-string))
		  (make-condition 'simple-type-error
				  :datum place
				  :expected-type type
				  :format-control
			  "The value of ~S is ~S, which is not of type ~S."
				  :format-arguments (list place
							  place-value
							  type)))))
    (restart-case (error cond)
      (store-value (value)
	:report (lambda (stream)
		  (format stream "Supply a new value for ~S." place))
	:interactive read-evaluated-form
	value))))

(defun case-body-error (name keyform keyform-value expected-type keys)
  (restart-case
      (error 'case-failure
	     :name name
	     :datum keyform-value
	     :expected-type expected-type
	     :possibilities keys)
    (store-value (value)
      :report (lambda (stream)
		(format stream "Supply a new value for ~S." keyform))
      :interactive read-evaluated-form
      value)))

;;;; EVAL and friends

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

;;; general case of EVAL (except in that it can't handle toplevel
;;; EVAL-WHEN magic properly): Delegate to #'COMPILE.
(defun %eval (expr lexenv)
  (funcall (sb!c:compile-in-lexenv
            (gensym "EVAL-TMPFUN-")
            `(lambda ()
               ,expr)
            lexenv)))

;;; Handle PROGN and implicit PROGN.
(defun eval-progn-body (progn-body lexenv)
  (unless (list-with-length-p progn-body)
    (let ((*print-circle* t))
      (error 'simple-program-error
	     :format-control
	     "~@<not a proper list in PROGN or implicit PROGN: ~2I~_~S~:>"
	     :format-arguments (list progn-body))))
  ;; Note:
  ;;   * We can't just use (MAP NIL #'EVAL PROGN-BODY) here, because we
  ;;     need to take care to return all the values of the final EVAL.
  ;;   * It's left as an exercise to the reader to verify that this
  ;;     gives the right result when PROGN-BODY is NIL, because
  ;;     (FIRST NIL) = (REST NIL) = NIL.
  (do* ((i progn-body rest-i)
	(rest-i (rest i) (rest i)))
      (nil)
    (if rest-i ; if not last element of list
	(eval-in-lexenv (first i) lexenv)
	(return (eval-in-lexenv (first i) lexenv)))))

(defun eval (original-exp)
  #!+sb-doc
  "Evaluate the argument in a null lexical environment, returning the
  result or results."
  (eval-in-lexenv original-exp (make-null-lexenv)))

;;; Pick off a few easy cases, and the various top level EVAL-WHEN
;;; magical cases, and call %EVAL for the rest.
(defun eval-in-lexenv (original-exp lexenv)
  (declare (optimize (safety 1)))
  ;; (aver (lexenv-simple-p lexenv))
  (let ((exp (macroexpand original-exp lexenv)))
    (typecase exp
      (symbol
       (ecase (info :variable :kind exp)
	 (:constant
	  (values (info :variable :constant-value exp)))
	 ((:special :global)
	  (symbol-value exp))
	 ;; FIXME: This special case here is a symptom of non-ANSI
	 ;; weirdness in SBCL's ALIEN implementation, which could
	 ;; cause problems for e.g. code walkers. It'd probably be
	 ;; good to ANSIfy it by making alien variable accessors into
	 ;; ordinary forms, e.g. (SB-UNIX:ENV) and (SETF SB-UNIX:ENV),
	 ;; instead of magical symbols, e.g. plain SB-UNIX:ENV. Then
	 ;; if the old magical-symbol syntax is to be retained for
	 ;; compatibility, it can be implemented with
	 ;; DEFINE-SYMBOL-MACRO, keeping the code walkers happy.
	 (:alien
	  (%eval original-exp lexenv))))
      (list
       (let ((name (first exp))
	     (n-args (1- (length exp))))
	 (case name
	   ((function)
	    (unless (= n-args 1)
	      (error "wrong number of args to FUNCTION:~% ~S" exp))
	    (let ((name (second exp)))
	      (if (and (or (atom name)
                           (and (consp name)
                                (eq (car name) 'setf)))
                       (not (consp (let ((sb!c:*lexenv* lexenv))
                                     (sb!c:lexenv-find name funs)))))
		  (fdefinition name)
		  (%eval original-exp lexenv))))
	   ((quote)
	    (unless (= n-args 1)
	      (error "wrong number of args to QUOTE:~% ~S" exp))
	    (second exp))
	   (setq
	    (unless (evenp n-args)
	      (error "odd number of args to SETQ:~% ~S" exp))
	    (unless (zerop n-args)
	      (do ((name (cdr exp) (cddr name)))
		  ((null name)
		   (do ((args (cdr exp) (cddr args)))
		       ((null (cddr args))
			;; We duplicate the call to SET so that the
			;; correct value gets returned.
			(set (first args) (eval (second args))))
		     (set (first args) (eval (second args)))))
		(let ((symbol (first name)))
		  (case (info :variable :kind symbol)
		    ;; FIXME: I took out the *TOPLEVEL-AUTO-DECLARE*
		    ;; test here, and removed the *TOPLEVEL-AUTO-DECLARE*
		    ;; variable; the code should now act as though that
		    ;; variable is NIL. This should be tested..
		    (:special)
		    (t (return (%eval original-exp lexenv))))))))
	   ((progn)
	    (eval-progn-body (rest exp) lexenv))
	   ((eval-when)
	    ;; FIXME: DESTRUCTURING-BIND returns ARG-COUNT-ERROR
	    ;; instead of PROGRAM-ERROR when there's something wrong
	    ;; with the syntax here (e.g. missing SITUATIONS). This
	    ;; could be fixed by hand-crafting clauses to catch and
	    ;; report each possibility, but it would probably be
	    ;; cleaner to write a new macro
	    ;; DESTRUCTURING-BIND-PROGRAM-SYNTAX which does
	    ;; DESTRUCTURING-BIND and promotes any mismatch to
	    ;; PROGRAM-ERROR, then to use it here and in (probably
	    ;; dozens of) other places where the same problem arises.
	    (destructuring-bind (eval-when situations &rest body) exp
	      (declare (ignore eval-when))
	      (multiple-value-bind (ct lt e)
		  (sb!c:parse-eval-when-situations situations)
		;; CLHS 3.8 - Special Operator EVAL-WHEN: The use of
		;; the situation :EXECUTE (or EVAL) controls whether
		;; evaluation occurs for other EVAL-WHEN forms; that
		;; is, those that are not top level forms, or those in
		;; code processed by EVAL or COMPILE. If the :EXECUTE
		;; situation is specified in such a form, then the
		;; body forms are processed as an implicit PROGN;
		;; otherwise, the EVAL-WHEN form returns NIL.
		(declare (ignore ct lt))
		(when e
		  (eval-progn-body body lexenv)))))
	   ((locally)
	    (multiple-value-bind (body decls) (parse-body (rest exp) nil)
	      (let ((lexenv
		     ;; KLUDGE: Uh, yeah.  I'm not anticipating
		     ;; winning any prizes for this code, which was
		     ;; written on a "let's get it to work" basis.
		     ;; These seem to be the variables that need
		     ;; bindings for PROCESS-DECLS to work
		     ;; (*FREE-FUNS* and *FREE-VARS* so that
		     ;; references to free functions and variables in
		     ;; the declarations can be noted;
		     ;; *UNDEFINED-WARNINGS* so that warnings about
		     ;; undefined things can be accumulated [and then
		     ;; thrown away, as it happens]). -- CSR, 2002-10-24
		     (let ((sb!c:*lexenv* lexenv)
			   (sb!c::*free-funs* (make-hash-table :test 'equal))
			   (sb!c::*free-vars* (make-hash-table :test 'eq))
			   (sb!c::*undefined-warnings* nil))
		       (sb!c::process-decls decls
					    nil nil
					    (sb!c::make-continuation)
					    lexenv))))
		(eval-progn-body body lexenv))))
	   ((macrolet)
	    (destructuring-bind (definitions &rest body)
		(rest exp)
	      ;; FIXME: shared code with FUNCALL-IN-FOOMACROLET-LEXENV
	      (declare (type list definitions))
	      (unless (= (length definitions)
			 (length (remove-duplicates definitions :key #'first)))
		(style-warn "duplicate definitions in ~S" definitions))
	      (let ((lexenv
		     (sb!c::make-lexenv
		      :default lexenv
		      :funs (mapcar
			     (sb!c::macrolet-definitionize-fun
			      :eval
			      ;; I'm not sure that this is the correct
			      ;; LEXENV to be compiling local macros
			      ;; in...
			      lexenv)
			     definitions))))
		(eval-in-lexenv `(locally ,@body) lexenv))))
	   ((symbol-macrolet)
	    (destructuring-bind (definitions &rest body)
		(rest exp)
	      ;; FIXME: shared code with FUNCALL-IN-FOOMACROLET-LEXENV
	      (declare (type list definitions))
	      (unless (= (length definitions)
			 (length (remove-duplicates definitions :key #'first)))
		(style-warn "duplicate definitions in ~S" definitions))
	      (let ((lexenv
		     (sb!c::make-lexenv
		      :default lexenv
		      :vars (mapcar
			     (sb!c::symbol-macrolet-definitionize-fun
			      :eval)
			     definitions))))
		(eval-in-lexenv `(locally ,@body) lexenv))))
	   (t
	    (if (and (symbolp name)
		     (eq (info :function :kind name) :function))
		(collect ((args))
                         (dolist (arg (rest exp))
                           (args (eval-in-lexenv arg lexenv)))
                         (apply (symbol-function name) (args)))
		(%eval original-exp lexenv))))))
      (t
       exp))))

;;; miscellaneous full function definitions of things which are
;;; ordinarily handled magically by the compiler

(defun apply (function arg &rest arguments)
  #!+sb-doc
  "Apply FUNCTION to a list of arguments produced by evaluating ARGUMENTS in
  the manner of LIST*. That is, a list is made of the values of all but the
  last argument, appended to the value of the last argument, which must be a
  list."
  (cond ((atom arguments)
	 (apply function arg))
	((atom (cdr arguments))
	 (apply function (cons arg (car arguments))))
	(t (do* ((a1 arguments a2)
		 (a2 (cdr arguments) (cdr a2)))
		((atom (cdr a2))
		 (rplacd a1 (car a2))
		 (apply function (cons arg arguments)))))))

(defun funcall (function &rest arguments)
  #!+sb-doc
  "Call FUNCTION with the given ARGUMENTS."
  (apply function arguments))

(defun values (&rest values)
  #!+sb-doc
  "Return all arguments, in order, as values."
  (values-list values))

(defun values-list (list)
  #!+sb-doc
  "Return all of the elements of LIST, in order, as values."
  (values-list list))

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
(defun %eval (expr)
  (funcall (compile (gensym "EVAL-TMPFUN-")
		    `(lambda ()

		       ;; The user can reasonably expect that the
		       ;; interpreter will be safe.
		       (declare (optimize (safety 3)))

		       ;; It's also good if the interpreter doesn't
		       ;; spend too long thinking about each input
		       ;; form, since if the user'd wanted the
		       ;; tradeoff to favor quality of compiled code
		       ;; over compilation speed, he'd've explicitly
		       ;; asked for compilation.
		       (declare (optimize (compilation-speed 2)))

		       ;; Other properties are relatively unimportant.
		       (declare (optimize (speed 1) (debug 1) (space 1)))

		       ,expr))))

;;; Handle PROGN and implicit PROGN.
(defun eval-progn-body (progn-body)
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
	(eval (first i))
	(return (eval (first i))))))

;;; Pick off a few easy cases, and the various top-level EVAL-WHEN
;;; magical cases, and call %EVAL for the rest. 
(defun eval (original-exp)
  #!+sb-doc
  "Evaluate the argument in a null lexical environment, returning the
  result or results."
  (declare (optimize (safety 1)))
  (let ((exp (macroexpand original-exp)))
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
	  (%eval original-exp))))
      (list
       (let ((name (first exp))
	     (n-args (1- (length exp))))
	 (case name
	   (function
	    (unless (= n-args 1)
	      (error "wrong number of args to FUNCTION:~% ~S" exp))
	    (let ((name (second exp)))
	      (if (or (atom name)
		      (and (consp name)
			   (eq (car name) 'setf)))
		  (fdefinition name)
		  (%eval original-exp))))
	   (quote
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
		    ;; FIXME: I took out the *TOP-LEVEL-AUTO-DECLARE*
		    ;; test here, and removed the *TOP-LEVEL-AUTO-DECLARE*
		    ;; variable; the code should now act as though that
		    ;; variable is NIL. This should be tested..
		    (:special)
		    (t (return (%eval original-exp))))))))
	   ((progn)
	    (eval-progn-body (rest exp)))
	   ((eval-when)
	    ;; FIXME: DESTRUCTURING-BIND returns
	    ;; DEFMACRO-LL-ARG-COUNT-ERROR instead of PROGRAM-ERROR
	    ;; when there's something wrong with the syntax here (e.g.
	    ;; missing SITUATIONS). This could be fixed by
	    ;; hand-crafting clauses to catch and report each
	    ;; possibility, but it would probably be cleaner to write
	    ;; a new macro DESTRUCTURING-BIND-PROGRAM-SYNTAX which
	    ;; does DESTRUCTURING-BIND and promotes any mismatch to
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
		  (eval-progn-body body)))))
	   (t
	    (if (and (symbolp name)
		     (eq (info :function :kind name) :function))
		(collect ((args))
		  (dolist (arg (rest exp))
		    (args (eval arg)))
		  (apply (symbol-function name) (args)))
		(%eval original-exp))))))
      (t
       exp))))

(defun function-lambda-expression (fun)
  "Return (VALUES DEFINING-LAMBDA-EXPRESSION CLOSURE-P NAME), where
  DEFINING-LAMBDA-EXPRESSION is NIL if unknown, or a suitable argument
  to COMPILE otherwise, CLOSURE-P is non-NIL if the function's definition
  might have been enclosed in some non-null lexical environment, and
  NAME is some name (for debugging only) or NIL if there is no name."
    (declare (type function fun))
    (let* ((fun (%simple-fun-self fun))
	   (name (%simple-fun-name fun))
	   (code (sb!di::fun-code-header fun))
	   (info (sb!kernel:%code-debug-info code)))
      (if info
        (let ((source (first (sb!c::compiled-debug-info-source info))))
          (cond ((and (eq (sb!c::debug-source-from source) :lisp)
                      (eq (sb!c::debug-source-info source) fun))
                 (values (second (svref (sb!c::debug-source-name source) 0))
                         nil name))
                ((stringp name)
                 (values nil t name))
                (t
                 (let ((exp (fun-name-inline-expansion name)))
                   (if exp
                       (values exp nil name)
                       (values nil t name))))))
        (values nil t name))))

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

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!EVAL")

;;; This is defined here so that the printer etc. can call
;;; INTERPRETED-FUNCTION-P before the full interpreter is loaded.

;;; an interpreted function
(defstruct (interpreted-function
	    (:alternate-metaclass sb!kernel:funcallable-instance
				  sb!kernel:funcallable-structure-class
				  sb!kernel:make-funcallable-structure-class)
	    (:type sb!kernel:funcallable-structure)
	    (:constructor %make-interpreted-function)
	    (:copier nil)
	    ;; FIXME: Binding PRINT-OBJECT isn't going to help unless
	    ;; we fix the print-a-funcallable-instance code so that
	    ;; it calls PRINT-OBJECT in this case.
	    (:print-object
	     (lambda (x stream)
	       (print-unreadable-object (x stream :identity t)
		 (sb!impl::output-interpreted-function x stream)))))
  ;; The name of this interpreted function, or NIL if none specified.
  (%name nil)
  ;; This function's debug arglist.
  (arglist nil)
  ;; A lambda that can be converted to get the definition.
  (lambda nil)
  ;; If this function has been converted, then this is the XEP. If this is
  ;; false, then the function is not in the cache (or is in the process of
  ;; being removed.)
  (definition nil :type (or sb!c::clambda null))
  ;; The number of consecutive GCs that this function has been unused.
  ;; This is used to control cache replacement.
  (gcs 0 :type sb!c::index)
  ;; True if Lambda has been converted at least once, and thus warnings should
  ;; be suppressed on additional conversions.
  (converted-once nil)
  ;; For a closure, the closure date vector.
  (closure nil :type (or null simple-vector)))

(in-package "SB!IMPL")

;;;; One of the steps in building a nice debuggable macro is changing
;;;; its MACRO-FUNCTION to print as e.g.
;;;;   #<Interpreted Function "DEFMACRO BAR" {9166351}>
;;;; instead of some weird internal representation showing the
;;;; environment argument and stuff. This function is called in order
;;;; to try to make that happen.
;;;;
;;;; When we're running in the target SBCL, we own the
;;;; INTERPRETED-FUNCTION definition, and we can do this; that's what
;;;; the definition below does. When we're a Python cross-compiler
;;;; running in some arbitrary ANSI Common Lisp, we can't do this (and
;;;; we don't care that much about making nice debuggable macros
;;;; anyway). In that environment, a stub no-op version of this
;;;; function is used.
(defun try-to-rename-interpreted-function-as-macro (f name lambda-list)
  (aver (sb!eval:interpreted-function-p f))
  (setf (sb!eval:interpreted-function-name f)
	(format nil "DEFMACRO ~S" name)
	(sb!eval:interpreted-function-arglist f)
	lambda-list)
  (values))

;;;; EVAL and friends

;;; This needs to be initialized in the cold load, since the top-level
;;; catcher will always restore the initial value.
(defvar *eval-stack-top* 0)

;;; Pick off a few easy cases, and call INTERNAL-EVAL for the rest. If
;;; *ALREADY-EVALED-THIS* is true, then we bind it to NIL before doing
;;; a call so that the effect is confined to the lexical scope of the
;;; EVAL-WHEN.
(defun eval (original-exp)
  #!+sb-doc
  "Evaluates its single argument in a null lexical environment, returns the
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
	 (:alien
	  (sb!eval:internal-eval original-exp))))
      (list
       (let ((name (first exp))
	     (args (1- (length exp))))
	 (case name
	   (function
	    (unless (= args 1)
	      (error "wrong number of args to FUNCTION:~% ~S" exp))
	    (let ((name (second exp)))
	      (if (or (atom name)
		      (and (consp name)
			   (eq (car name) 'setf)))
		  (fdefinition name)
		  (sb!eval:make-interpreted-function name))))
	   (quote
	    (unless (= args 1)
	      (error "wrong number of args to QUOTE:~% ~S" exp))
	    (second exp))
	   (setq
	    (unless (evenp args)
	      (error "odd number of args to SETQ:~% ~S" exp))
	    (unless (zerop args)
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
		    (t (return (sb!eval:internal-eval original-exp))))))))
	   ((progn)
	    (when (> args 0)
	      (dolist (x (butlast (rest exp)) (eval (car (last exp))))
		(eval x))))
	   ((eval-when)
	    (if (and (> args 0)
		     (or (member 'eval (second exp))
			 (member :execute (second exp))))
		(when (> args 1)
		  (dolist (x (butlast (cddr exp)) (eval (car (last exp))))
		    (eval x)))
		(sb!eval:internal-eval original-exp)))
	   (t
	    (if (and (symbolp name)
		     (eq (info :function :kind name) :function))
		(collect ((args))
		  (dolist (arg (rest exp))
		    (args (eval arg)))
		  (if sb!eval::*already-evaled-this*
		      (let ((sb!eval::*already-evaled-this* nil))
			(apply (symbol-function name) (args)))
		      (apply (symbol-function name) (args))))
		(sb!eval:internal-eval original-exp))))))
      (t
       exp))))

;;; Given a function, return three values:
;;; 1] A lambda expression that could be used to define the function,
;;;    or NIL if the definition isn't available.
;;; 2] NIL if the function was definitely defined in a null lexical
;;;    environment, and T otherwise.
;;; 3] Some object that \"names\" the function. Although this is
;;;    allowed to be any object, CMU CL always returns a valid
;;;    function name or a string.
;;;
;;; If interpreted, use the interpreter interface. Otherwise, see
;;; whether it was compiled with COMPILE. If that fails, check for an
;;; inline expansion.
(defun function-lambda-expression (fun)
  (declare (type function fun))
  (if (sb!eval:interpreted-function-p fun)
      (sb!eval:interpreted-function-lambda-expression fun)
      (let* ((fun (%function-self fun))
	     (name (%function-name fun))
	     (code (sb!di::function-code-header fun))
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
		     (let ((exp (info :function :inline-expansion name)))
		       (if exp
			   (values exp nil name)
			   (values nil t name))))))
	    (values nil t name)))))

;;; Like FIND-IF, only we do it on a compiled closure's environment.
(defun find-if-in-closure (test fun)
  (dotimes (index (1- (get-closure-length fun)))
    (let ((elt (%closure-index-ref fun index)))
      (when (funcall test elt)
	(return elt)))))

;;; function invocation

(defun apply (function arg &rest args)
  #!+sb-doc
  "Applies FUNCTION to a list of arguments produced by evaluating ARGS in
  the manner of LIST*. That is, a list is made of the values of all but the
  last argument, appended to the value of the last argument, which must be a
  list."
  (cond ((atom args)
	 (apply function arg))
	((atom (cdr args))
	 (apply function (cons arg (car args))))
	(t (do* ((a1 args a2)
		 (a2 (cdr args) (cdr a2)))
		((atom (cdr a2))
		 (rplacd a1 (car a2))
		 (apply function (cons arg args)))))))

(defun funcall (function &rest arguments)
  #!+sb-doc
  "Calls Function with the given Arguments."
  (apply function arguments))

;;; multiple-value forms

(defun values (&rest values)
  #!+sb-doc
  "Returns all arguments, in order, as values."
  (values-list values))

(defun values-list (list)
  #!+sb-doc
  "Returns all of the elements of LIST, in order, as values."
  (values-list list))

;;;; EVAL and friends

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!BYTECODE")

;;; Note: This is defined here, but it's visible in SB-KERNEL, since
;;; various magical things need to happen to it, e.g. initialization
;;; early in cold load, and save/restore in nonlocal exit logic.
(defvar *eval-stack-top* 0)

;;; general case of EVAL (except in that it can't handle toplevel
;;; EVAL-WHEN magic properly): Delegate to the byte compiler.
(defun %eval (expr)
  (funcall (compile (gensym "EVAL-TMPFUN-")
		    `(lambda ()

		       ;; SPEED=0,DEBUG=1 => byte-compile
		       (declare (optimize (speed 0) (debug 1))) 

		       ;; Other than that, basically we care about
		       ;; compilation speed, compilation speed, and
		       ;; compilation speed. (There are cases where
		       ;; the user wants something else, but we don't
		       ;; know enough to guess that; and if he is
		       ;; unhappy about our guessed emphasis, he
		       ;; should explicitly compile his code, with
		       ;; explicit declarations to tell us what to
		       ;; emphasize.)
		       (declare (optimize (space 1) (safety 1)))
		       (declare (optimize (compilation-speed 3)))

		       ,expr))))

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
		  (%eval original-exp))))
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
		    (t (return (%eval original-exp))))))))
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
		(%eval original-exp)))
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

;;;; bootstrapping fundamental machinery (e.g. DEFUN, DEFCONSTANT,
;;;; DEFVAR) from special forms and primitive functions
;;;;
;;;; KLUDGE: The bootstrapping aspect of this is now obsolete. It was
;;;; originally intended that this file file would be loaded into a
;;;; Lisp image which had Common Lisp primitives defined, and DEFMACRO
;;;; defined, and little else. Since then that approach has been
;;;; dropped and this file has been modified somewhat to make it work
;;;; more cleanly when used to predefine macros at
;;;; build-the-cross-compiler time.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

;;;; IN-PACKAGE

(defmacro-mundanely in-package (package-designator)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setq *package* (find-undeleted-package-or-lose ',package-designator))))

;;;; MULTIPLE-VALUE-FOO

(defun list-of-symbols-p (x)
  (and (listp x)
       (every #'symbolp x)))

(defmacro-mundanely multiple-value-bind (vars value-form &body body)
  (if (list-of-symbols-p vars)
    ;; It's unclear why it would be important to special-case the LENGTH=1 case
    ;; at this level, but the CMU CL code did it, so.. -- WHN 19990411
    (if (= (length vars) 1)
      `(let ((,(car vars) ,value-form))
	 ,@body)
      (let ((ignore (gensym)))
	`(multiple-value-call #'(lambda (&optional ,@vars &rest ,ignore)
				  (declare (ignore ,ignore))
				  ,@body)
			      ,value-form)))
    (error "Vars is not a list of symbols: ~S" vars)))

(defmacro-mundanely multiple-value-setq (vars value-form)
  (cond ((null vars)
	 ;; The ANSI spec says that the primary value of VALUE-FORM must be
	 ;; returned. The general-case-handling code below doesn't do this
	 ;; correctly in the special case when there are no vars bound, so we
	 ;; handle this special case separately here.
	 (let ((g (gensym)))
	   `(multiple-value-bind (,g) ,value-form
	      ,g)))
	((list-of-symbols-p vars)
	 (let ((temps (make-gensym-list (length vars))))
	   `(multiple-value-bind ,temps ,value-form
	      ,@(mapcar #'(lambda (var temp)
			    `(setq ,var ,temp))
			vars temps)
	      ,(car temps))))
	(t (error "Vars is not a list of symbols: ~S" vars))))

(defmacro-mundanely multiple-value-list (value-form)
  `(multiple-value-call #'list ,value-form))

;;;; various conditional constructs

;;; COND defined in terms of IF
(defmacro-mundanely cond (&rest clauses)
  (if (endp clauses)
    nil
    (let ((clause (first clauses)))
      (if (atom clause)
	(error "Cond clause is not a list: ~S" clause)
	(let ((test (first clause))
	      (forms (rest clause)))
	  (if (endp forms)
	    (let ((n-result (gensym)))
	      `(let ((,n-result ,test))
		 (if ,n-result
		   ,n-result
		   (cond ,@(rest clauses)))))
	    `(if ,test
	       (progn ,@forms)
	       (cond ,@(rest clauses)))))))))

;;; other things defined in terms of COND
(defmacro-mundanely when (test &body forms)
  #!+sb-doc
  "If the first argument is true, the rest of the forms are
  evaluated as a PROGN."
  `(cond (,test nil ,@forms)))
(defmacro-mundanely unless (test &body forms)
  #!+sb-doc
  "If the first argument is not true, the rest of the forms are
  evaluated as a PROGN."
  `(cond ((not ,test) nil ,@forms)))
(defmacro-mundanely and (&rest forms)
  (cond ((endp forms) t)
	((endp (rest forms)) (first forms))
	(t
	 `(if ,(first forms)
	      (and ,@(rest forms))
	      nil))))
(defmacro-mundanely or (&rest forms)
  (cond ((endp forms) nil)
	((endp (rest forms)) (first forms))
	(t
	 (let ((n-result (gensym)))
	   `(let ((,n-result ,(first forms)))
	      (if ,n-result
		  ,n-result
		  (or ,@(rest forms))))))))

;;;; various sequencing constructs

(defmacro-mundanely prog (varlist &body body-decls)
  (multiple-value-bind (body decls) (parse-body body-decls nil)
    `(block nil
       (let ,varlist
	 ,@decls
	 (tagbody ,@body)))))

(defmacro-mundanely prog* (varlist &body body-decls)
  (multiple-value-bind (body decls) (parse-body body-decls nil)
    `(block nil
       (let* ,varlist
	 ,@decls
	 (tagbody ,@body)))))

(defmacro-mundanely prog1 (result &body body)
  (let ((n-result (gensym)))
    `(let ((,n-result ,result))
       ,@body
       ,n-result)))

(defmacro-mundanely prog2 (form1 result &body body)
  `(prog1 (progn ,form1 ,result) ,@body))

;;; Now that we have the definition of MULTIPLE-VALUE-BIND, we can make a
;;; reasonably readable definition of DEFUN.
;;;
;;; DEFUN expands into %DEFUN which is a function that is treated
;;; magically by the compiler (through an IR1 transform) in order to
;;; handle stuff like inlining. After the compiler has gotten the
;;; information it wants out of macro definition, it compiles a call
;;; to %%DEFUN which happens at load time.
(defmacro-mundanely defun (&whole whole name args &body body)
  (multiple-value-bind (forms decls doc) (parse-body body)
    (let ((def `(lambda ,args
		  ,@decls
		  (block ,(function-name-block-name name)
		    ,@forms))))
      `(sb!c::%defun ',name #',def ,doc ',whole))))
#+sb-xc-host (/show "before PROCLAIM" (sb!c::info :function :kind 'sb!c::%%defun))
#+sb-xc-host (sb!xc:proclaim '(ftype function sb!c::%%defun)) ; to avoid
					; undefined function warnings
#+sb-xc-host (/show "after PROCLAIM" (sb!c::info :function :kind 'sb!c::%%defun))
(defun sb!c::%%defun (name def doc &optional inline-expansion)
  (when (fboundp name)
    (style-warn "redefining ~S in DEFUN" name))
  (setf (sb!xc:fdefinition name) def)
  (when doc
    ;; FIXME: This should use shared SETF-name parsing logic.
    (if (and (consp name) (eq (first name) 'setf))
	(setf (fdocumentation (second name) 'setf) doc)
	(setf (fdocumentation name 'function) doc)))
  (become-defined-function-name name)
  (when (or inline-expansion
	    (info :function :inline-expansion name))
    (setf (info :function :inline-expansion name)
	  inline-expansion))
  name)
;;; Ordinarily this definition of SB!C:%DEFUN as an ordinary function is not
;;; used: the parallel (but different) definition as an IR1 transform takes
;;; precedence. However, it's still good to define this in order to keep the
;;; interpreter happy. We define it here (instead of alongside the parallel
;;; IR1 transform) because while the IR1 transform is needed and appropriate
;;; in the cross-compiler running in the host Common Lisp, this parallel
;;; ordinary function definition is only appropriate in the target Lisp.
(defun sb!c::%defun (name def doc source)
  (declare (ignore source))
  #!+sb-interpreter (setf (sb!eval:interpreted-function-name def) name)
  (ecase (info :function :where-from name)
    (:assumed
      (setf (info :function :where-from name) :defined)
      (setf (info :function :type name)
              (extract-function-type def))
      (when (info :function :assumed-type name)
        (setf (info :function :assumed-type name) nil)))
    (:declared)
    (:defined
     (setf (info :function :type name)
	   (extract-function-type def))
     ;; We shouldn't need to clear this here because it should be clear
     ;; already (cleared when the last definition was processed).
     (aver (null (info :function :assumed-type name)))))
  (sb!c::%%defun name def doc))

;;;; DEFVAR and DEFPARAMETER

(defmacro-mundanely defvar (var &optional (val nil valp) (doc nil docp))
  #!+sb-doc
  "For defining global variables at top level. Declares the variable
  SPECIAL and, optionally, initializes it. If the variable already has a
  value, the old value is not clobbered. The third argument is an optional
  documentation string for the variable."
  `(progn
     (declaim (special ,var))
     ,@(when valp
	 `((unless (boundp ',var)
	     (setq ,var ,val))))
     ,@(when docp
	 `((funcall #'(setf fdocumentation) ',doc ',var 'variable)))
     ',var))

(defmacro-mundanely defparameter (var val &optional (doc nil docp))
  #!+sb-doc
  "Defines a parameter that is not normally changed by the program,
  but that may be changed without causing an error. Declares the
  variable special and sets its value to VAL. The third argument is
  an optional documentation string for the parameter."
  `(progn
     (declaim (special ,var))
     (setq ,var ,val)
     ,@(when docp
	 ;; FIXME: The various FUNCALL #'(SETF FDOCUMENTATION) and
	 ;; other FUNCALL #'(SETF FOO) forms in the code should
	 ;; unbogobootstrapized back to ordinary SETF forms.
	 `((funcall #'(setf fdocumentation) ',doc ',var 'variable)))
     ',var))

;;;; iteration constructs

;;; (These macros are defined in terms of a function DO-DO-BODY which is also
;;; used by SB!INT:DO-ANONYMOUS. Since these macros should not be loaded
;;; on the cross-compilation host, but SB!INT:DO-ANONYMOUS and DO-DO-BODY
;;; should be, these macros can't conveniently be in the same file as
;;; DO-DO-BODY.)
(defmacro-mundanely do (varlist endlist &body body)
  #!+sb-doc
  "DO ({(Var [Init] [Step])}*) (Test Exit-Form*) Declaration* Form*
  Iteration construct. Each Var is initialized in parallel to the value of the
  specified Init form. On subsequent iterations, the Vars are assigned the
  value of the Step form (if any) in parallel. The Test is evaluated before
  each evaluation of the body Forms. When the Test is true, the Exit-Forms
  are evaluated as a PROGN, with the result being the value of the DO. A block
  named NIL is established around the entire expansion, allowing RETURN to be
  used as an alternate exit mechanism."
  (do-do-body varlist endlist body 'let 'psetq 'do nil))
(defmacro-mundanely do* (varlist endlist &body body)
  #!+sb-doc
  "DO* ({(Var [Init] [Step])}*) (Test Exit-Form*) Declaration* Form*
  Iteration construct. Each Var is initialized sequentially (like LET*) to the
  value of the specified Init form. On subsequent iterations, the Vars are
  sequentially assigned the value of the Step form (if any). The Test is
  evaluated before each evaluation of the body Forms. When the Test is true,
  the Exit-Forms are evaluated as a PROGN, with the result being the value
  of the DO. A block named NIL is established around the entire expansion,
  allowing RETURN to be used as an laternate exit mechanism."
  (do-do-body varlist endlist body 'let* 'setq 'do* nil))

;;; DOTIMES and DOLIST could be defined more concisely using
;;; destructuring macro lambda lists or DESTRUCTURING-BIND, but then
;;; it'd be tricky to use them before those things were defined.
;;; They're used enough times before destructuring mechanisms are
;;; defined that it looks as though it's worth just implementing them
;;; ASAP, at the cost of being unable to use the standard
;;; destructuring mechanisms.
(defmacro-mundanely dotimes (var-count-result &body body)
  (multiple-value-bind ; to roll our own destructuring
      (var count result)
      (apply (lambda (var count &optional (result nil))
	       (values var count result))
	     var-count-result)
    (cond ((numberp count)
	   `(do ((,var 0 (1+ ,var)))
		((>= ,var ,count) ,result)
	      (declare (type unsigned-byte ,var))
	      ,@body))
	  (t (let ((v1 (gensym)))
	       `(do ((,var 0 (1+ ,var)) (,v1 ,count))
		    ((>= ,var ,v1) ,result)
		  (declare (type unsigned-byte ,var))
		  ,@body))))))
(defmacro-mundanely dolist (var-list-result &body body)
  (multiple-value-bind ; to roll our own destructuring
      (var list result)
      (apply (lambda (var list &optional (result nil))
	       (values var list result))
	     var-list-result)
    ;; We repeatedly bind the var instead of setting it so that we
    ;; never have to give the var an arbitrary value such as NIL
    ;; (which might conflict with a declaration). If there is a result
    ;; form, we introduce a gratuitous binding of the variable to NIL
    ;; without the declarations, then evaluate the result form in that
    ;; environment. We spuriously reference the gratuitous variable,
    ;; since we don't want to use IGNORABLE on what might be a special
    ;; var.
    (let ((n-list (gensym)))
      `(do ((,n-list ,list (cdr ,n-list)))
	   ((endp ,n-list)
	    ,@(if result
		`((let ((,var nil))
		    ,var
		    ,result))
		'(nil)))
	 (let ((,var (car ,n-list)))
	   ,@body)))))

;;;; miscellaneous

(defmacro-mundanely return (&optional (value nil))
  `(return-from nil ,value))

(defmacro-mundanely psetq (&rest pairs)
  #!+sb-doc
  "SETQ {var value}*
   Set the variables to the values, like SETQ, except that assignments
   happen in parallel, i.e. no assignments take place until all the
   forms have been evaluated."
  ;; (This macro is used in the definition of DO, so we can't use DO in the
  ;; definition of this macro without getting into confusing bootstrap issues.)
  (prog ((lets nil)
	 (setqs nil)
	 (pairs pairs))
    :again
    (when (atom (cdr pairs))
      (return `(let ,(nreverse lets)
		 (setq ,@(nreverse setqs))
		 nil)))
    (let ((gen (gensym)))
      (setq lets (cons `(,gen ,(cadr pairs)) lets)
	    setqs (list* gen (car pairs) setqs)
	    pairs (cddr pairs)))
    (go :again)))

(defmacro-mundanely lambda (&whole whole args &body body)
  (declare (ignore args body))
  `#',whole)

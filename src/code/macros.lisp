;;;; lots of basic macros for the target SBCL

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

;;;; ASSERT and CHECK-TYPE

;;; ASSERT is written this way, to call ASSERT-ERROR, because of how
;;; closures are compiled. RESTART-CASE has forms with closures that
;;; the compiler causes to be generated at the top of any function
;;; using RESTART-CASE, regardless of whether they are needed. Thus if
;;; we just wrapped a RESTART-CASE around the call to ERROR, we'd have
;;; to do a significant amount of work at runtime allocating and
;;; deallocating the closures regardless of whether they were ever
;;; needed.
;;;
;;; ASSERT-ERROR isn't defined until a later file because it uses the
;;; macro RESTART-CASE, which isn't defined until a later file.
(defmacro-mundanely assert (test-form &optional places datum &rest arguments)
  #!+sb-doc
  "Signals an error if the value of test-form is nil. Continuing from this
   error using the CONTINUE restart will allow the user to alter the value of
   some locations known to SETF, starting over with test-form. Returns nil."
  `(do () (,test-form)
     (assert-error ',test-form ',places ,datum ,@arguments)
     ,@(mapcar #'(lambda (place)
		   `(setf ,place (assert-prompt ',place ,place)))
	       places)))

(defun assert-prompt (name value)
  (cond ((y-or-n-p "The old value of ~S is ~S.~
		  ~%Do you want to supply a new value? "
		   name value)
	 (format *query-io* "~&Type a form to be evaluated:~%")
	 (flet ((read-it () (eval (read *query-io*))))
	   (if (symbolp name) ;help user debug lexical variables
	       (progv (list name) (list value) (read-it))
	       (read-it))))
	(t value)))

;;; CHECK-TYPE is written this way, to call CHECK-TYPE-ERROR, because
;;; of how closures are compiled. RESTART-CASE has forms with closures
;;; that the compiler causes to be generated at the top of any
;;; function using RESTART-CASE, regardless of whether they are
;;; needed. Because it would be nice if CHECK-TYPE were cheap to use,
;;; and some things (e.g., READ-CHAR) can't afford this excessive
;;; consing, we bend backwards a little.
;;;
;;; FIXME: In reality, this restart cruft is needed hardly anywhere in
;;; the system. Write NEED and NEED-TYPE to replace ASSERT and
;;; CHECK-TYPE inside the system.
;;;
;;; CHECK-TYPE-ERROR isn't defined until a later file because it uses
;;; the macro RESTART-CASE, which isn't defined until a later file.
(defmacro-mundanely check-type (place type &optional type-string)
  #!+sb-doc
  "Signals a restartable error of type TYPE-ERROR if the value of PLACE is
  not of the specified type. If an error is signalled and the restart is
  used to return, the
  return if the
   STORE-VALUE is invoked. It will store into PLACE and start over."
  (let ((place-value (gensym)))
    `(do ((,place-value ,place))
	 ((typep ,place-value ',type))
       (setf ,place
	     (check-type-error ',place ,place-value ',type ,type-string)))))

#!+high-security-support
(defmacro-mundanely check-type-var (place type-var &optional type-string)
  #!+sb-doc
  "Signals an error of type type-error if the contents of place are not of the
   specified type to which the type-var evaluates. If an error is signaled,
   this can only return if STORE-VALUE is invoked. It will store into place
   and start over."
  (let ((place-value (gensym))
	(type-value (gensym)))
    `(do ((,place-value ,place)
	  (,type-value  ,type-var))
	 ((typep ,place-value ,type-value))
       (setf ,place
	     (check-type-error ',place ,place-value ,type-value ,type-string)))))

;;;; DEFCONSTANT

(defmacro-mundanely defconstant (var val &optional doc)
  #!+sb-doc
  "For defining global constants at top level. The DEFCONSTANT says that the
  value is constant and may be compiled into code. If the variable already has
  a value, and this is not equal to the init, an error is signalled. The third
  argument is an optional documentation string for the variable."
  `(sb!c::%defconstant ',var ,val ',doc))

;;; These are like the other %MUMBLEs except that we currently
;;; actually do something interesting at load time, namely checking
;;; whether the constant is being redefined.
(defun sb!c::%defconstant (name value doc)
  (sb!c::%%defconstant name value doc))
#+sb-xc-host (sb!xc:proclaim '(ftype function sb!c::%%defconstant)) ; to avoid
					; undefined function warnings
(defun sb!c::%%defconstant (name value doc)
  (when doc
    (setf (fdocumentation name 'variable) doc))
  (when (boundp name)
    (unless (equalp (symbol-value name) value)
      (cerror "Go ahead and change the value."
	      "The constant ~S is being redefined."
	      name)))
  (setf (symbol-value name) value)
  (setf (info :variable :kind name) :constant)
  (clear-info :variable :constant-value name)
  name)

;;;; DEFINE-COMPILER-MACRO

;;; FIXME: The logic here for handling compiler macros named (SETF
;;; FOO) was added after the fork from SBCL, is not well tested, and
;;; may conflict with subtleties of the ANSI standard. E.g. section
;;; "3.2.2.1 Compiler Macros" says that creating a lexical binding for
;;; a function name shadows a compiler macro, and it's not clear that
;;; that works with this version. It should be tested.
(defmacro-mundanely define-compiler-macro (name lambda-list &body body)
  #!+sb-doc
  "Define a compiler-macro for NAME."
  (let ((whole (gensym "WHOLE-"))
	(environment (gensym "ENV-")))
    (multiple-value-bind (body local-decs doc)
	(parse-defmacro lambda-list whole body name 'define-compiler-macro
			:environment environment)
      (let ((def `(lambda (,whole ,environment)
		    ,@local-decs
		    (block ,(function-name-block-name name)
		      ,body))))
	`(sb!c::%define-compiler-macro ',name #',def ',lambda-list ,doc)))))
(defun sb!c::%define-compiler-macro (name definition lambda-list doc)
  ;; FIXME: Why does this have to be an interpreted function? Shouldn't
  ;; it get compiled?
  (assert (sb!eval:interpreted-function-p definition))
  (setf (sb!eval:interpreted-function-name definition)
	(format nil "DEFINE-COMPILER-MACRO ~S" name))
  (setf (sb!eval:interpreted-function-arglist definition) lambda-list)
  (sb!c::%%define-compiler-macro name definition doc))
(defun sb!c::%%define-compiler-macro (name definition doc)
  (setf (sb!xc:compiler-macro-function name) definition)
  ;; FIXME: Add support for (SETF FDOCUMENTATION) when object is a list
  ;; and type is COMPILER-MACRO. (Until then, we have to discard any
  ;; compiler macro documentation for (SETF FOO).)
  (unless (listp name)
    (setf (fdocumentation name 'compiler-macro) doc))
  name)

;;;; CASE, TYPECASE, and friends

(eval-when (:compile-toplevel :load-toplevel :execute)

;;; CASE-BODY (interface)
;;;
;;; CASE-BODY returns code for all the standard "case" macros. Name is
;;; the macro name, and keyform is the thing to case on. Multi-p
;;; indicates whether a branch may fire off a list of keys; otherwise,
;;; a key that is a list is interpreted in some way as a single key.
;;; When multi-p, test is applied to the value of keyform and each key
;;; for a given branch; otherwise, test is applied to the value of
;;; keyform and the entire first element, instead of each part, of the
;;; case branch. When errorp, no t or otherwise branch is permitted,
;;; and an ERROR form is generated. When proceedp, it is an error to
;;; omit errorp, and the ERROR form generated is executed within a
;;; RESTART-CASE allowing keyform to be set and retested.
(defun case-body (name keyform cases multi-p test errorp proceedp needcasesp)
  (unless (or cases (not needcasesp))
    (warn "no clauses in ~S" name))
  (let ((keyform-value (gensym))
	(clauses ())
	(keys ()))
    (dolist (case cases)
      (cond ((atom case)
	     (error "~S -- Bad clause in ~S." case name))
	    ((memq (car case) '(t otherwise))
	     (if errorp
		 (error 'simple-program-error
			:format-control "No default clause is allowed in ~S: ~S"
			:format-arguments (list name case))
		 (push `(t nil ,@(rest case)) clauses)))
	    ((and multi-p (listp (first case)))
	     (setf keys (append (first case) keys))
	     (push `((or ,@(mapcar #'(lambda (key)
				       `(,test ,keyform-value ',key))
				   (first case)))
		     nil ,@(rest case))
		   clauses))
	    (t
	     (push (first case) keys)
	     (push `((,test ,keyform-value
			    ',(first case)) nil ,@(rest case)) clauses))))
    (case-body-aux name keyform keyform-value clauses keys errorp proceedp
		   `(,(if multi-p 'member 'or) ,@keys))))

;;; CASE-BODY-AUX provides the expansion once CASE-BODY has groveled
;;; all the cases. Note: it is not necessary that the resulting code
;;; signal case-failure conditions, but that's what KMP's prototype
;;; code did. We call CASE-BODY-ERROR, because of how closures are
;;; compiled. RESTART-CASE has forms with closures that the compiler
;;; causes to be generated at the top of any function using the case
;;; macros, regardless of whether they are needed.
;;;
;;; The CASE-BODY-ERROR function is defined later, when the
;;; RESTART-CASE macro has been defined.
(defun case-body-aux (name keyform keyform-value clauses keys
		      errorp proceedp expected-type)
  (if proceedp
      (let ((block (gensym))
	    (again (gensym)))
	`(let ((,keyform-value ,keyform))
	   (block ,block
	     (tagbody
	      ,again
	      (return-from
	       ,block
	       (cond ,@(nreverse clauses)
		     (t
		      (setf ,keyform-value
			    (setf ,keyform
				  (case-body-error
				   ',name ',keyform ,keyform-value
				   ',expected-type ',keys)))
		      (go ,again))))))))
      `(let ((,keyform-value ,keyform))
	 (declare (ignorable ,keyform-value)) ; e.g. (CASE KEY (T))
	 (cond
	  ,@(nreverse clauses)
	  ,@(if errorp
		`((t (error 'sb!conditions::case-failure
			    :name ',name
			    :datum ,keyform-value
			    :expected-type ',expected-type
			    :possibilities ',keys))))))))
) ; EVAL-WHEN

(defmacro-mundanely case (keyform &body cases)
  #!+sb-doc
  "CASE Keyform {({(Key*) | Key} Form*)}*
  Evaluates the Forms in the first clause with a Key EQL to the value of
  Keyform. If a singleton key is T then the clause is a default clause."
  (case-body 'case keyform cases t 'eql nil nil nil))

(defmacro-mundanely ccase (keyform &body cases)
  #!+sb-doc
  "CCASE Keyform {({(Key*) | Key} Form*)}*
  Evaluates the Forms in the first clause with a Key EQL to the value of
  Keyform. If none of the keys matches then a correctable error is
  signalled."
  (case-body 'ccase keyform cases t 'eql t t t))

(defmacro-mundanely ecase (keyform &body cases)
  #!+sb-doc
  "ECASE Keyform {({(Key*) | Key} Form*)}*
  Evaluates the Forms in the first clause with a Key EQL to the value of
  Keyform. If none of the keys matches then an error is signalled."
  (case-body 'ecase keyform cases t 'eql t nil t))

(defmacro-mundanely typecase (keyform &body cases)
  #!+sb-doc
  "TYPECASE Keyform {(Type Form*)}*
  Evaluates the Forms in the first clause for which TYPEP of Keyform and Type
  is true."
  (case-body 'typecase keyform cases nil 'typep nil nil nil))

(defmacro-mundanely ctypecase (keyform &body cases)
  #!+sb-doc
  "CTYPECASE Keyform {(Type Form*)}*
  Evaluates the Forms in the first clause for which TYPEP of Keyform and Type
  is true. If no form is satisfied then a correctable error is signalled."
  (case-body 'ctypecase keyform cases nil 'typep t t t))

(defmacro-mundanely etypecase (keyform &body cases)
  #!+sb-doc
  "ETYPECASE Keyform {(Type Form*)}*
  Evaluates the Forms in the first clause for which TYPEP of Keyform and Type
  is true. If no form is satisfied then an error is signalled."
  (case-body 'etypecase keyform cases nil 'typep t nil t))

;;;; WITH-FOO i/o-related macros

(defmacro-mundanely with-open-stream ((var stream) &body forms-decls)
  (multiple-value-bind (forms decls) (parse-body forms-decls nil)
    (let ((abortp (gensym)))
      `(let ((,var ,stream)
	     (,abortp t))
	 ,@decls
	 (unwind-protect
	     (multiple-value-prog1
	      (progn ,@forms)
	      (setq ,abortp nil))
	   (when ,var
	     (close ,var :abort ,abortp)))))))

(defmacro-mundanely with-open-file ((stream filespec &rest options)
				    &body body)
  `(with-open-stream (,stream (open ,filespec ,@options))
     ,@body))

(defmacro-mundanely with-input-from-string ((var string &key index start end)
					    &body forms-decls)
  (multiple-value-bind (forms decls) (parse-body forms-decls nil)
    ;; The ONCE-ONLY inhibits compiler note for unreachable code when
    ;; END is true.
    (once-only ((string string))
      `(let ((,var
	      ,(cond ((null end)
		      `(make-string-input-stream ,string ,(or start 0)))
		     ((symbolp end)
		      `(if ,end
			   (make-string-input-stream ,string
						     ,(or start 0)
						     ,end)
			   (make-string-input-stream ,string
						     ,(or start 0))))
		     (t
		      `(make-string-input-stream ,string
						 ,(or start 0)
						 ,end)))))
	 ,@decls
	 (unwind-protect
	     (progn ,@forms)
	   (close ,var)
	   ,@(when index
	       `((setf ,index (string-input-stream-current ,var)))))))))

(defmacro-mundanely with-output-to-string ((var &optional string)
					   &body forms-decls)
  (multiple-value-bind (forms decls) (parse-body forms-decls nil)
    (if string
      `(let ((,var (make-fill-pointer-output-stream ,string)))
  	 ,@decls
  	 (unwind-protect
	     (progn ,@forms)
  	   (close ,var)))
      `(let ((,var (make-string-output-stream)))
  	 ,@decls
  	 (unwind-protect
	     (progn ,@forms)
  	   (close ,var))
  	 (get-output-stream-string ,var)))))

;;;; miscellaneous macros

(defmacro-mundanely nth-value (n form)
  #!+sb-doc
  "Evaluates FORM and returns the Nth value (zero based). This involves no
  consing when N is a trivial constant integer."
  (if (integerp n)
      (let ((dummy-list nil)
	    (keeper (gensym "KEEPER-")))
	;; We build DUMMY-LIST, a list of variables to bind to useless
	;; values, then we explicitly IGNORE those bindings and return
	;; KEEPER, the only thing we're really interested in right now.
	(dotimes (i n)
	  (push (gensym "IGNORE-") dummy-list))
	`(multiple-value-bind (,@dummy-list ,keeper) ,form
	   (declare (ignore ,@dummy-list))
	   ,keeper))
      (once-only ((n n))
	`(case (the fixnum ,n)
	   (0 (nth-value 0 ,form))
	   (1 (nth-value 1 ,form))
	   (2 (nth-value 2 ,form))
	   (t (nth (the fixnum ,n) (multiple-value-list ,form)))))))

(defmacro-mundanely declaim (&rest specs)
  #!+sb-doc
  "DECLAIM Declaration*
  Do a declaration or declarations for the global environment."
  #-sb-xc-host
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@(mapcar #'(lambda (x)
		   `(sb!xc:proclaim ',x))
	       specs))
  ;; KLUDGE: The definition above doesn't work in the cross-compiler,
  ;; because UNCROSS translates SB!XC:PROCLAIM into CL:PROCLAIM before
  ;; the form gets executed. Instead, we have to explicitly do the
  ;; proclamation at macroexpansion time. -- WHN ca. 19990810
  ;;
  ;; FIXME: Maybe we don't need this special treatment any more now
  ;; that we're using DEFMACRO-MUNDANELY instead of DEFMACRO?
  #+sb-xc-host (progn
		 (mapcar #'sb!xc:proclaim specs)
		 `(progn
		    ,@(mapcar #'(lambda (x)
				  `(sb!xc:proclaim ',x))
			      specs))))

(defmacro-mundanely print-unreadable-object ((object stream
					      &key type identity)
					     &body body)
  `(%print-unreadable-object ,object ,stream ,type ,identity
			     ,(if body
				  `#'(lambda () ,@body)
				  nil)))

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
   some locations known to SETF, starting over with test-form. Returns NIL."
  `(do () (,test-form)
     (assert-error ',test-form ',places ,datum ,@arguments)
     ,@(mapcar (lambda (place)
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
;;; CHECK-TYPE inside the system. (CL:CHECK-TYPE must still be
;;; defined, since it's specified by ANSI and it is sometimes nice for
;;; whipping up little things. But as far as I can tell it's not
;;; usually very helpful deep inside the guts of a complex system like
;;; SBCL.)
;;;
;;; CHECK-TYPE-ERROR isn't defined until a later file because it uses
;;; the macro RESTART-CASE, which isn't defined until a later file.
(defmacro-mundanely check-type (place type &optional type-string)
  #!+sb-doc
  "Signal a restartable error of type TYPE-ERROR if the value of PLACE is
  not of the specified type. If an error is signalled and the restart is
  used to return, this can only return if the STORE-VALUE restart is
  invoked. In that case it will store into PLACE and start over."
  (let ((place-value (gensym)))
    `(do ((,place-value ,place ,place))
	 ((typep ,place-value ',type))
       (setf ,place
	     (check-type-error ',place ,place-value ',type ,type-string)))))

;;;; DEFINE-SYMBOL-MACRO

(defmacro-mundanely define-symbol-macro (name expansion)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
    (sb!c::%define-symbol-macro ',name ',expansion)))

(defun sb!c::%define-symbol-macro (name expansion)
  (unless (symbolp name)
    (error 'simple-type-error :datum name :expected-type 'symbol
	   :format-control "Symbol macro name is not a symbol: ~S."
	   :format-arguments (list name)))
  (ecase (info :variable :kind name)
    ((:macro :global nil)
     (setf (info :variable :kind name) :macro)
     (setf (info :variable :macro-expansion name) expansion))
    (:special
     (error 'simple-program-error
	    :format-control "Symbol macro name already declared special: ~S."
	    :format-arguments (list name)))
    (:constant
     (error 'simple-program-error
	    :format-control "Symbol macro name already declared constant: ~S."
	    :format-arguments (list name))))
  name)

;;;; DEFINE-COMPILER-MACRO

(defmacro-mundanely define-compiler-macro (name lambda-list &body body)
  #!+sb-doc
  "Define a compiler-macro for NAME."
  (legal-fun-name-or-type-error name)
  (when (consp name)
    ;; It's fairly clear that the user intends the compiler macro to
    ;; expand when he does (SETF (FOO ...) X). And that's even a
    ;; useful and reasonable thing to want. Unfortunately,
    ;; (SETF (FOO ...) X) macroexpands into (FUNCALL (SETF FOO) X ...),
    ;; and it's not at all clear that it's valid to expand a FUNCALL form,
    ;; and the ANSI standard doesn't seem to say anything else which
    ;; would justify us expanding the compiler macro the way the user
    ;; wants. So instead we rely on 3.2.2.1.3 "When Compiler Macros Are
    ;; Used" which says they never have to be used, so by ignoring such
    ;; macros we're erring on the safe side. But any user who does
    ;; (DEFINE-COMPILER-MACRO (SETF FOO) ...) could easily be surprised
    ;; by this way of complying with a rather screwy aspect of the ANSI
    ;; spec, so at least we can warn him...
    (sb!c::compiler-style-warn
     "defining compiler macro of (SETF ...), which will not be expanded"))
  (when (and (symbolp name) (special-operator-p name))
    (error 'simple-program-error
	   :format-control "cannot define a compiler-macro for a special operator: ~S"
	   :format-arguments (list name)))
  (with-unique-names (whole environment)
    (multiple-value-bind (body local-decs doc)
	(parse-defmacro lambda-list whole body name 'define-compiler-macro
			:environment environment)
      (let ((def `(lambda (,whole ,environment)
		    ,@local-decs
		    ,body))
	    (debug-name (debug-namify "DEFINE-COMPILER-MACRO ~S" name)))
	`(eval-when (:compile-toplevel :load-toplevel :execute)
	  (sb!c::%define-compiler-macro ',name
					#',def
					',lambda-list
					,doc
					,debug-name))))))

;;; FIXME: This will look remarkably similar to those who have already
;;; seen the code for %DEFMACRO in src/code/defmacro.lisp.  Various
;;; bits of logic should be shared (notably arglist setting).
(macrolet
    ((def (times set-p)
	 `(eval-when (,@times)
	   (defun sb!c::%define-compiler-macro
	       (name definition lambda-list doc debug-name)
	     ,@(unless set-p
		 '((declare (ignore lambda-list debug-name))))
	     ;; FIXME: warn about incompatible lambda list with
	     ;; respect to parent function?
	     (setf (sb!xc:compiler-macro-function name) definition)
	     ;; FIXME: Add support for (SETF FDOCUMENTATION) when
	     ;; object is a list and type is COMPILER-MACRO. (Until
	     ;; then, we have to discard any compiler macro
	     ;; documentation for (SETF FOO).)
	     (unless (listp name)
	       (setf (fdocumentation name 'compiler-macro) doc))
	     ,(when set-p
		    `(case (widetag-of definition)
                      (#.sb!vm:closure-header-widetag
                       (setf (%simple-fun-arglist (%closure-fun definition))
                             lambda-list
			     (%simple-fun-name (%closure-fun definition))
			     debug-name))
                      ((#.sb!vm:simple-fun-header-widetag
                        #.sb!vm:closure-fun-header-widetag)
                       (setf (%simple-fun-arglist definition) lambda-list
			     (%simple-fun-name definition) debug-name))))
	     name))))
  (progn
    (def (:load-toplevel :execute) #-sb-xc-host t #+sb-xc-host nil)
    #-sb-xc (def (:compile-toplevel) nil)))

;;;; CASE, TYPECASE, and friends

(eval-when (#-sb-xc :compile-toplevel :load-toplevel :execute)

;;; CASE-BODY returns code for all the standard "case" macros. NAME is
;;; the macro name, and KEYFORM is the thing to case on. MULTI-P
;;; indicates whether a branch may fire off a list of keys; otherwise,
;;; a key that is a list is interpreted in some way as a single key.
;;; When MULTI-P, TEST is applied to the value of KEYFORM and each key
;;; for a given branch; otherwise, TEST is applied to the value of
;;; KEYFORM and the entire first element, instead of each part, of the
;;; case branch. When ERRORP, no T or OTHERWISE branch is permitted,
;;; and an ERROR form is generated. When PROCEEDP, it is an error to
;;; omit ERRORP, and the ERROR form generated is executed within a
;;; RESTART-CASE allowing KEYFORM to be set and retested.
(defun case-body (name keyform cases multi-p test errorp proceedp needcasesp)
  (unless (or cases (not needcasesp))
    (warn "no clauses in ~S" name))
  (let ((keyform-value (gensym))
	(clauses ())
	(keys ()))
    (do* ((cases cases (cdr cases))
	  (case (car cases) (car cases)))
	 ((null cases) nil)
      (unless (list-of-length-at-least-p case 1)
	(error "~S -- bad clause in ~S" case name))
      (destructuring-bind (keyoid &rest forms) case
	(cond ((and (memq keyoid '(t otherwise))
		    (null (cdr cases)))
	       (if errorp
		   (progn
		     (style-warn "~@<Treating bare ~A in ~A as introducing a ~
                                  normal-clause, not an otherwise-clause~@:>"
				 keyoid name)
		     (push keyoid keys)
		     (push `((,test ,keyform-value ',keyoid) nil ,@forms)
			   clauses))
		   (push `(t nil ,@forms) clauses)))
	      ((and multi-p (listp keyoid))
	       (setf keys (append keyoid keys))
	       (push `((or ,@(mapcar (lambda (key)
				       `(,test ,keyform-value ',key))
				     keyoid))
		       nil
		       ,@forms)
		     clauses))
	      (t
	       (push keyoid keys)
	       (push `((,test ,keyform-value ',keyoid)
		       nil
		       ,@forms)
		     clauses)))))
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
		`((t (error 'case-failure
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
  "Evaluate FORM and return the Nth value (zero based). This involves no
  consing when N is a trivial constant integer."
  ;; FIXME: The above is true, if slightly misleading.  The
  ;; MULTIPLE-VALUE-BIND idiom [ as opposed to MULTIPLE-VALUE-CALL
  ;; (LAMBDA (&REST VALUES) (NTH N VALUES)) ] does indeed not cons at
  ;; runtime.  However, for large N (say N = 200), COMPILE on such a
  ;; form will take longer than can be described as adequate, as the
  ;; optional dispatch mechanism for the M-V-B gets increasingly
  ;; hairy.
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
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@(mapcar (lambda (spec) `(sb!xc:proclaim ',spec))
	       specs)))

(defmacro-mundanely print-unreadable-object ((object stream &key type identity)
					     &body body)
  "Output OBJECT to STREAM with \"#<\" prefix, \">\" suffix, optionally
  with object-type prefix and object-identity suffix, and executing the
  code in BODY to provide possible further output."
  `(%print-unreadable-object ,object ,stream ,type ,identity
			     ,(if body
				  `(lambda () ,@body)
				  nil)))

(defmacro-mundanely ignore-errors (&rest forms)
  #!+sb-doc
  "Execute FORMS handling ERROR conditions, returning the result of the last
  form, or (VALUES NIL the-ERROR-that-was-caught) if an ERROR was handled."
  `(handler-case (progn ,@forms)
     (error (condition) (values nil condition))))

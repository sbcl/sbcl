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

;;;; DEFCONSTANT

(defmacro-mundanely defconstant (name value &optional documentation)
  #!+sb-doc
  "Define a global constant, saying that the value is constant and may be
  compiled into code. If the variable already has a value, and this is not
  EQL to the new value, the code is not portable (undefined behavior). The
  third argument is an optional documentation string for the variable."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (sb!c::%defconstant ',name ,value ',documentation)))

;;; the guts of DEFCONSTANT
(defun sb!c::%defconstant (name value doc)
  (unless (symbolp name)
    (error "The constant name is not a symbol: ~S" name))
  (about-to-modify name)
  (when (looks-like-name-of-special-var-p name)
    (style-warn "defining ~S as a constant, even though the name follows~@
the usual naming convention (names like *FOO*) for special variables"
		name))
  (let ((kind (info :variable :kind name)))
    (case kind
      (:constant
       ;; Note: This behavior (discouraging any non-EQL modification)
       ;; is unpopular, but it is specified by ANSI (i.e. ANSI says a
       ;; non-EQL change has undefined consequences). If people really
       ;; want bindings which are constant in some sense other than
       ;; EQL, I suggest either just using DEFVAR (which is usually
       ;; appropriate, despite the un-mnemonic name), or defining
       ;; something like SB-INT:DEFCONSTANT-EQX (which is occasionally
       ;; more appropriate). -- WHN 2000-11-03
       (unless (eql value
		    (info :variable :constant-value name))
	 (cerror "Go ahead and change the value."
		 "The constant ~S is being redefined."
		 name)))
      (:global
       ;; (This is OK -- undefined variables are of this kind. So we
       ;; don't warn or error or anything, just fall through.)
       )
      (t (warn "redefining ~(~A~) ~S to be a constant" kind name))))
  (when doc
    (setf (fdocumentation name 'variable) doc))

  ;; We want to set the cross-compilation host's symbol value, not just
  ;; the cross-compiler's (INFO :VARIABLE :CONSTANT-VALUE NAME), so
  ;; that code like
  ;;   (defconstant max-entries 61)
  ;;   (deftype entry-index () `(mod ,max-entries))
  ;; will be cross-compiled correctly.
  #-sb-xc-host (setf (symbol-value name) value)
  #+sb-xc-host (progn
		 (/show (symbol-package name))
		 ;; Redefining our cross-compilation host's CL symbols
		 ;; would be poor form.
		 ;;
		 ;; FIXME: Having to check this and then not treat it
		 ;; as a fatal error seems like a symptom of things
		 ;; being pretty broken. It's also a problem in and of
		 ;; itself, since it makes it too easy for cases of
		 ;; using the cross-compilation host Lisp's CL
		 ;; constant values in the target Lisp to slip by. I
		 ;; got backed into this because the cross-compiler
		 ;; translates DEFCONSTANT SB!XC:FOO into DEFCONSTANT
		 ;; CL:FOO. It would be good to unscrew the
		 ;; cross-compilation package hacks so that that
		 ;; translation doesn't happen. Perhaps:
		 ;;   * Replace SB-XC with SB-CL. SB-CL exports all the 
		 ;;     symbols which ANSI requires to be exported from CL.
		 ;;   * Make a nickname SB!CL which behaves like SB!XC.
		 ;;   * Go through the loaded-on-the-host code making
		 ;;     every target definition be in SB-CL. E.g.
		 ;;     DEFMACRO-MUNDANELY DEFCONSTANT becomes
		 ;;     DEFMACRO-MUNDANELY SB!CL:DEFCONSTANT.
		 ;;   * Make IN-TARGET-COMPILATION-MODE do 
		 ;;     UNUSE-PACKAGE CL and USE-PACKAGE SB-CL in each
		 ;;     of the target packages (then undo it on exit).
		 ;;   * Make the cross-compiler's implementation of
		 ;;     EVAL-WHEN (:COMPILE-TOPLEVEL) do UNCROSS.
		 ;;     (This may not require any change.)
		 ;;   * Hack GENESIS as necessary so that it outputs
		 ;;     SB-CL stuff as COMMON-LISP stuff.
		 ;;   * Now the code here can assert that the symbol
		 ;;     being defined isn't in the cross-compilation
		 ;;     host's CL package.
		 (unless (eql (find-symbol (symbol-name name) :cl) name)
		   ;; KLUDGE: In the cross-compiler, we use the
		   ;; cross-compilation host's DEFCONSTANT macro
		   ;; instead of just (SETF SYMBOL-VALUE), in order to
		   ;; get whatever blessing the cross-compilation host
		   ;; may expect for a global (SETF SYMBOL-VALUE).
		   ;; (CMU CL, at least around 2.4.19, generated full
		   ;; WARNINGs for code -- e.g. DEFTYPE expanders --
		   ;; which referred to symbols which had been set by
		   ;; (SETF SYMBOL-VALUE). I doubt such warnings are
		   ;; ANSI-compliant, but I'm not sure, so I've
		   ;; written this in a way that CMU CL will tolerate
		   ;; and which ought to work elsewhere too.) -- WHN
		   ;; 2001-03-24
		   (eval `(defconstant ,name ',value))))

  (setf (info :variable :kind name) :constant)
  (setf (info :variable :constant-value name) value)
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
  (declare (ignore lambda-list))
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
    (dolist (case cases)
      (unless (list-of-length-at-least-p case 1)
	(error "~S -- bad clause in ~S" case name))
      (destructuring-bind (keyoid &rest forms) case
	(cond ((memq keyoid '(t otherwise))
	       (if errorp
		   (error 'simple-program-error
			  :format-control
			  "No default clause is allowed in ~S: ~S"
			  :format-arguments (list name case))
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

(defmacro-mundanely print-unreadable-object ((object stream &key type identity)
					     &body body)
  "Output OBJECT to STREAM with \"#<\" prefix, \">\" suffix, optionally
  with object-type prefix and object-identity suffix, and executing the
  code in BODY to provide possible further output."
  `(%print-unreadable-object ,object ,stream ,type ,identity
			     ,(if body
				  `#'(lambda () ,@body)
				  nil)))

(defmacro-mundanely ignore-errors (&rest forms)
  #!+sb-doc
  "Execute FORMS handling ERROR conditions, returning the result of the last
  form, or (VALUES NIL the-ERROR-that-was-caught) if an ERROR was handled."
  `(handler-case (progn ,@forms)
     (error (condition) (values nil condition))))

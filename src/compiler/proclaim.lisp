;;;; This file contains load-time support for declaration processing.
;;;; In CMU CL it was split off from the compiler so that the compiler
;;;; doesn't have to be in the cold load, but in SBCL the compiler is
;;;; in the cold load again, so this might not be valuable.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

;;; A list of UNDEFINED-WARNING structures representing references to unknown
;;; stuff which came up in a compilation unit.
(defvar *undefined-warnings*)
(declaim (list *undefined-warnings*))

;;; Check that NAME is a valid function name, returning the name if
;;; OK, and doing an error if not. In addition to checking for basic
;;; well-formedness, we also check that symbol names are not NIL or
;;; the name of a special form.
(defun check-function-name (name)
  (typecase name
    (list
     (unless (and (consp name) (consp (cdr name))
		  (null (cddr name)) (eq (car name) 'setf)
		  (symbolp (cadr name)))
       (compiler-error "illegal function name: ~S" name))
     name)
    (symbol
     (when (eq (info :function :kind name) :special-form)
       (compiler-error "Special form is an illegal function name: ~S" name))
     name)
    (t
     (compiler-error "illegal function name: ~S" name))))

;;; This is called to do something about SETF functions that overlap
;;; with SETF macros. Perhaps we should interact with the user to see
;;; whether the macro should be blown away, but for now just give a
;;; warning. Due to the weak semantics of the (SETF FUNCTION) name, we
;;; can't assume that they aren't just naming a function (SETF FOO)
;;; for the heck of it. NAME is already known to be well-formed.
(defun note-if-setf-function-and-macro (name)
  (when (consp name)
    (when (or (info :setf :inverse name)
	      (info :setf :expander name))
      (compiler-style-warning
       "defining as a SETF function a name that already has a SETF macro:~
       ~%  ~S"
       name)))
  (values))

;;; Look up some symbols in *FREE-VARIABLES*, returning the var
;;; structures for any which exist. If any of the names aren't
;;; symbols, we complain.
(declaim (ftype (function (list) list) get-old-vars))
(defun get-old-vars (names)
  (collect ((vars))
    (dolist (name names (vars))
      (unless (symbolp name)
	(compiler-error "The name ~S is not a symbol." name))
      (let ((old (gethash name *free-variables*)))
	(when old (vars old))))))

;;; Return a new POLICY containing the policy information represented
;;; by the optimize declaration SPEC. Any parameters not specified are
;;; defaulted from the POLICY argument.
(declaim (ftype (function (list policy) policy) process-optimize-decl))
(defun process-optimize-decl (spec policy)
  (let ((result policy)) ; may have new entries pushed on it below
    (dolist (q-and-v-or-just-q (cdr spec))
      (multiple-value-bind (quality raw-value)
	  (if (atom q-and-v-or-just-q)
	      (values q-and-v-or-just-q 3)
	      (destructuring-bind (quality raw-value) q-and-v-or-just-q
		(values quality raw-value)))
	(cond ((not (policy-quality-name-p quality))
	       (compiler-warning "ignoring unknown optimization quality ~
                                 ~S in ~S"
				 quality spec))
	      ((not (and (typep raw-value 'real) (<= 0 raw-value 3)))
	       (compiler-warning "ignoring bad optimization value ~S in ~S"
				 raw-value spec))
	      (t
	       (push (cons quality (rational raw-value))
		     result)))))
    result))

;;; ANSI defines the declaration (FOO X Y) to be equivalent to
;;; (TYPE FOO X Y) when FOO is a type specifier. This function
;;; implements that by converting (FOO X Y) to (TYPE FOO X Y).
(defun canonized-decl-spec (decl-spec)
  (let ((id (first decl-spec)))
    (unless (symbolp id)
      (error "The declaration identifier is not a symbol: ~S" what))
    (let ((id-is-type (info :type :kind id))
	  (id-is-declared-decl (info :declaration :recognized id)))
      (cond ((and id-is-type id-is-declared-decl)
	     (compiler-error
	      "ambiguous declaration ~S:~%  ~
              ~S was declared as a DECLARATION, but is also a type name."
	      decl-spec id))
	    (id-is-type
	     (cons 'type decl-spec))
	    (t
	     decl-spec)))))

(defun sb!xc:proclaim (raw-form)
  (let* ((form (canonized-decl-spec raw-form))
	 (kind (first form))
	 (args (rest form)))
    (case kind
      (special
       (dolist (name args)
	 (unless (symbolp name)
	   (error "can't declare a non-symbol as SPECIAL: ~S" name))
	 (when (constantp name)
	   (error "can't declare a constant as SPECIAL: ~S" name))
	 (clear-info :variable :constant-value name)
	 (setf (info :variable :kind name) :special)))
      (type
       (when *type-system-initialized*
	 (let ((type (specifier-type (first args))))
	   (dolist (name (rest args))
	     (unless (symbolp name)
	       (error "can't declare TYPE of a non-symbol: ~S" name))
	     (when (eq (info :variable :where-from name) :declared)
	       (let ((old-type (info :variable :type name)))
		 (when (type/= type old-type)
		   (style-warn "The new TYPE proclamation~%  ~S~@
                               for ~S does not match the old TYPE~@
                               proclamation ~S"
			       type name old-type))))
	     (setf (info :variable :type name) type)
	     (setf (info :variable :where-from name) :declared)))))
      (ftype
       ;; FIXME: Since currently *TYPE-SYSTEM-INITIALIZED* is not set
       ;; until many toplevel forms have run, this condition on
       ;; PROCLAIM (FTYPE ..) (and on PROCLAIM (TYPE ..), above) means
       ;; that valid PROCLAIMs in cold code could get lost. Probably
       ;; the cleanest way to deal with this would be to initialize
       ;; the type system completely in special cold init forms,
       ;; before any ordinary toplevel forms run. Failing that, we
       ;; could queue up PROCLAIMs to be done after the type system is
       ;; initialized. Failing that, we could at least issue a warning
       ;; when we have to ignore a PROCLAIM because the type system is
       ;; uninitialized.
       (when *type-system-initialized*
	 (let ((type (specifier-type (first args))))
	   (unless (csubtypep type (specifier-type 'function))
	     (error "not a function type: ~S" (first args)))
	   (dolist (name (rest args))
	     (cond ((info :function :accessor-for name)
		    ;; FIXME: This used to be a WARNING, which was
		    ;; clearly wrong, since it would cause warnings to
		    ;; be issued for conforming code, which is really
		    ;; annoying for people who use Lisp code to build
		    ;; Lisp systems (and check the return values from
		    ;; COMPILE and COMPILE-FILE). Changing it to a
		    ;; compiler note is somewhat better, since it's
		    ;; after all news about a limitation of the
		    ;; compiler, not a problem in the code. But even
		    ;; better would be to handle FTYPE proclamations
		    ;; for slot accessors, and since in the long run
		    ;; slot accessors should become more like other
		    ;; functions, this should eventually become
		    ;; straightforward.
		    (maybe-compiler-note
		     "~@<ignoring FTYPE proclamation for ~
                      slot accessor (currently unsupported): ~2I~_~S~:>"
		     name))
		   (t

		    ;; KLUDGE: Something like the commented-out TYPE/=
		    ;; check here would be nice, but it has been
		    ;; commented out because TYPE/= doesn't support
		    ;; function types. It could probably be made to do
		    ;; so, but it might take some time, since function
		    ;; types involve values types, which aren't
		    ;; supported, and since the SUBTYPEP operator for
		    ;; FUNCTION types is rather broken, e.g.
		    ;;   (SUBTYPEP '(FUNCTION (T BOOLEAN) NIL)
		    ;;             '(FUNCTION (FIXNUM FIXNUM) NIL)) => T, T
		    ;; -- WHN 20000229
		    #+nil
		    (when (eq (info :function :where-from name) :declared)
		      (let ((old-type (info :function :type name)))
			(when (type/= type old-type)
			  (style-warn "new FTYPE proclamation~@
                                       ~S~@
                                       for ~S does not match old FTYPE proclamation~@
                                       ~S"
				      (list type name old-type)))))

		    (proclaim-as-function-name name)
		    (note-name-defined name :function)
		    (setf (info :function :type name) type
			  (info :function :where-from name) :declared)))))))
      (freeze-type
       (dolist (type args)
	 (let ((class (specifier-type type)))
	   (when (typep class 'class)
	     (setf (class-state class) :sealed)
	     (let ((subclasses (class-subclasses class)))
	       (when subclasses
		 (dohash (subclass layout subclasses)
		   (declare (ignore layout))
		   (setf (class-state subclass) :sealed))))))))
      (optimize
       (setq *default-policy* (process-optimize-decl form *default-policy*)))
      (optimize-interface
       (setq *default-interface-policy*
	     (process-optimize-decl form *default-interface-policy*)))
      ((inline notinline maybe-inline)
       (dolist (name args)
	 (proclaim-as-function-name name)
	 (setf (info :function :inlinep name)
	       (case kind
		 (inline :inline)
		 (notinline :notinline)
		 (maybe-inline :maybe-inline)))))
      (declaration
       (dolist (decl args)
	 (unless (symbolp decl)
	   (error "In~%  ~S~%the declaration to be recognized is not a ~
                  symbol:~%  ~S"
		  form decl))
	 (setf (info :declaration :recognized decl) t)))
      (t
       (unless (info :declaration :recognized kind)
	 (compiler-warning "unrecognized declaration ~S" raw-form)))))
  (values))

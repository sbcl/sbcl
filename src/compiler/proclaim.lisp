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

;;; Look up some symbols in *FREE-VARS*, returning the var
;;; structures for any which exist. If any of the names aren't
;;; symbols, we complain.
(declaim (ftype (function (list) list) get-old-vars))
(defun get-old-vars (names)
  (collect ((vars))
    (dolist (name names (vars))
      (unless (symbolp name)
	(compiler-error "The name ~S is not a symbol." name))
      (let ((old (gethash name *free-vars*)))
	(when old (vars old))))))

;;; Return a new POLICY containing the policy information represented
;;; by the optimize declaration SPEC. Any parameters not specified are
;;; defaulted from the POLICY argument.
(declaim (ftype (function (list policy) policy) process-optimize-decl))
(defun process-optimize-decl (spec policy)
  (let ((result nil))
    ;; Add new entries from SPEC.
    (dolist (q-and-v-or-just-q (cdr spec))
      (multiple-value-bind (quality raw-value)
	  (if (atom q-and-v-or-just-q)
	      (values q-and-v-or-just-q 3)
	      (destructuring-bind (quality raw-value) q-and-v-or-just-q
		(values quality raw-value)))
	(cond ((not (policy-quality-name-p quality))
	       (compiler-warn "ignoring unknown optimization quality ~
                               ~S in ~S"
			       quality spec))
	      ((not (typep raw-value 'policy-quality))
	       (compiler-warn "ignoring bad optimization value ~S in ~S"
			      raw-value spec))
	      (t
	       (push (cons quality raw-value)
		     result)))))
    ;; Add any nonredundant entries from old POLICY.
    (dolist (old-entry policy)
      (unless (assq (car old-entry) result)
	(push old-entry result)))
    ;; Voila.
    result))

;;; ANSI defines the declaration (FOO X Y) to be equivalent to
;;; (TYPE FOO X Y) when FOO is a type specifier. This function
;;; implements that by converting (FOO X Y) to (TYPE FOO X Y).
(defun canonized-decl-spec (decl-spec)
  (let ((id (first decl-spec)))
    (unless (symbolp id)
      (error "The declaration identifier is not a symbol: ~S" id))
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
  #+sb-xc (/show0 "entering PROCLAIM, RAW-FORM=..")
  #+sb-xc (/hexstr raw-form)
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
	 (let ((ctype (specifier-type (first args))))
	   (unless (csubtypep ctype (specifier-type 'function))
	     (error "not a function type: ~S" (first args)))
	   (dolist (name (rest args))

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
	     #|
	     (when (eq (info :function :where-from name) :declared)
	       (let ((old-type (info :function :type name)))
		 (when (type/= ctype old-type)
		   (style-warn
		    "new FTYPE proclamation~@
                     ~S~@
                     for ~S does not match old FTYPE proclamation~@
                     ~S"
		    (list ctype name old-type)))))
             |#

	     ;; Now references to this function shouldn't be warned
	     ;; about as undefined, since even if we haven't seen a
	     ;; definition yet, we know one is planned. 
	     ;;
	     ;; Other consequences of we-know-you're-a-function-now
	     ;; are appropriate too, e.g. any MACRO-FUNCTION goes away.
	     (proclaim-as-fun-name name)
	     (note-name-defined name :function)

	     ;; the actual type declaration
	     (setf (info :function :type name) ctype
		   (info :function :where-from name) :declared)))))
      (freeze-type
       (dolist (type args)
	 (let ((class (specifier-type type)))
	   (when (typep class 'sb!xc:class)
	     (setf (class-state class) :sealed)
	     (let ((subclasses (class-subclasses class)))
	       (when subclasses
		 (dohash (subclass layout subclasses)
		   (declare (ignore layout))
		   (setf (class-state subclass) :sealed))))))))
      (optimize
       (setq *policy* (process-optimize-decl form *policy*)))
      ((inline notinline maybe-inline)
       (dolist (name args)
	 (proclaim-as-fun-name name) ; since implicitly it is a function
	 (setf (info :function :inlinep name)
	       (ecase kind
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
	 (compiler-warn "unrecognized declaration ~S" raw-form)))))
  #+sb-xc (/show0 "returning from PROCLAIM")
  (values))

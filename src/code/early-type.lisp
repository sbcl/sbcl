;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!KERNEL")

(!begin-collecting-cold-init-forms)

;;; Has the type system been properly initialized? (I.e. is it OK to
;;; use it?)
(defvar *type-system-initialized* #+sb-xc-host nil) ; (set in cold load)

;;; Use experimental type functionality?
;;;
;;; REMOVEME: Eventually the new type functionality should be stable
;;; enough that nothing depends on this, and we can remove it again.
(defvar *xtype?*)
(!cold-init-forms (setf *xtype?* nil))

;;; Return the type structure corresponding to a type specifier. We
;;; pick off structure types as a special case.
;;;
;;; Note: VALUES-SPECIFIER-TYPE-CACHE-CLEAR must be called whenever a
;;; type is defined (or redefined).
(defun-cached (values-specifier-type
	       :hash-function (lambda (x)
				;; FIXME: the THE FIXNUM stuff is
				;; redundant in SBCL (or modern CMU
				;; CL) because of type inference.
				(the fixnum
				     (logand (the fixnum (sxhash x))
					     #x3FF)))
	       :hash-bits 10
	       :init-wrapper !cold-init-forms)
	      ((orig eq))
  (let ((u (uncross orig)))
    (or (info :type :builtin u)
	(let ((spec (type-expand u)))
	  (cond
	   ((and (not (eq spec u))
		 (info :type :builtin spec)))
	   ((eq (info :type :kind spec) :instance)
	    (sb!xc:find-class spec))
	   ((typep spec 'class)
	    ;; There doesn't seem to be any way to translate
	    ;; (TYPEP SPEC 'BUILT-IN-CLASS) into something which can be
	    ;; executed on the host Common Lisp at cross-compilation time.
	    #+sb-xc-host (error
			  "stub: (TYPEP SPEC 'BUILT-IN-CLASS) on xc host")
	    (if (typep spec 'built-in-class)
		(or (built-in-class-translation spec) spec)
		spec))
	   (t
	    (let* (;; FIXME: This automatic promotion of FOO-style
		   ;; specs to (FOO)-style specs violates the ANSI
		   ;; standard. Unfortunately, we can't fix the
		   ;; problem just by removing it, since then things
		   ;; downstream should break. But at some point we
		   ;; should fix this and the things downstream too.
		   (lspec (if (atom spec) (list spec) spec))
		   (fun (info :type :translator (car lspec))))
	      (cond (fun
		     (funcall fun lspec))
		    ((or (and (consp spec) (symbolp (car spec)))
			 (symbolp spec))
		     (when *type-system-initialized*
		       (signal 'parse-unknown-type :specifier spec))
		     ;; (The RETURN-FROM here inhibits caching.)
		     (return-from values-specifier-type
		       (make-unknown-type :specifier spec)))
		    (t
		     (error "bad thing to be a type specifier: ~S"
			    spec))))))))))

;;; Like VALUES-SPECIFIER-TYPE, except that we guarantee to never
;;; return a VALUES type.
(defun specifier-type (x)
  (let ((res (values-specifier-type x)))
    (when (values-type-p res)
      (error "VALUES type illegal in this context:~%  ~S" x))
    res))

;;; Similar to MACROEXPAND, but expands DEFTYPEs. We don't bother
;;; returning a second value.
(defun type-expand (form)
  (let ((def (cond ((symbolp form)
                    (info :type :expander form))
                   ((and (consp form) (symbolp (car form)))
                    (info :type :expander (car form)))
                   (t nil))))
    (if def
        (type-expand (funcall def (if (consp form) form (list form))))
        form)))

;;; A HAIRY-TYPE represents anything too weird to be described
;;; reasonably or to be useful, such as NOT, SATISFIES, unknown types,
;;; and unreasonably complicated types involving AND. We just remember
;;; the original type spec.
(defstruct (hairy-type (:include ctype
				 (class-info (type-class-or-lose 'hairy))
				 (enumerable t))
		       (:copier nil)
		       #!+cmu (:pure nil))
  ;; the Common Lisp type-specifier
  (specifier nil :type t))

(!define-type-class hairy)

;;; An UNKNOWN-TYPE is a type not known to the type system (not yet
;;; defined). We make this distinction since we don't want to complain
;;; about types that are hairy but defined.
(defstruct (unknown-type (:include hairy-type)
			 (:copier nil)))

;;; ARGS-TYPE objects are used both to represent VALUES types and
;;; to represent FUNCTION types.
(defstruct (args-type (:include ctype)
		      (:constructor nil)
		      (:copier nil))
  ;; Lists of the type for each required and optional argument.
  (required nil :type list)
  (optional nil :type list)
  ;; The type for the rest arg. NIL if there is no rest arg.
  (rest nil :type (or ctype null))
  ;; true if &KEY arguments are specified
  (keyp nil :type boolean)
  ;; list of KEY-INFO structures describing the &KEY arguments
  (keywords nil :type list)
  ;; true if other &KEY arguments are allowed
  (allowp nil :type boolean))

(defstruct (values-type
	    (:include args-type
		      (class-info (type-class-or-lose 'values)))
	    (:copier nil)))

(!define-type-class values)

(defstruct (function-type
	    (:include args-type
		      (class-info (type-class-or-lose 'function))))
  ;; True if the arguments are unrestrictive, i.e. *.
  (wild-args nil :type boolean)
  ;; Type describing the return values. This is a values type
  ;; when multiple values were specified for the return.
  (returns (required-argument) :type ctype))

;;; The CONSTANT-TYPE structure represents a use of the
;;; CONSTANT-ARGUMENT "type specifier", which is only meaningful in
;;; function argument type specifiers used within the compiler. (It
;;; represents something that the compiler knows to be a constant.)
(defstruct (constant-type
	    (:include ctype
		      (class-info (type-class-or-lose 'constant)))
	    (:copier nil))
  ;; The type which the argument must be a constant instance of for this type
  ;; specifier to win.
  (type (required-argument) :type ctype))

;;; The NAMED-TYPE is used to represent *, T and NIL. These types must be
;;; super- or sub-types of all types, not just classes and * and NIL aren't
;;; classes anyway, so it wouldn't make much sense to make them built-in
;;; classes.
(defstruct (named-type (:include ctype
				 (class-info (type-class-or-lose 'named)))
		       (:copier nil))
  (name nil :type symbol))

;;; a list of all the float "formats" (i.e. internal representations;
;;; nothing to do with #'FORMAT), in order of decreasing precision
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *float-formats*
    '(long-float double-float single-float short-float)))

;;; The type of a float format.
(deftype float-format () `(member ,@*float-formats*))

;;; A NUMERIC-TYPE represents any numeric type, including things
;;; such as FIXNUM.
(defstruct (numeric-type (:include ctype
				   (class-info (type-class-or-lose
						'number)))
			 #!+negative-zero-is-not-zero
			 (:constructor %make-numeric-type))
  ;; the kind of numeric type we have, or NIL if not specified (just
  ;; NUMBER or COMPLEX)
  ;;
  ;; KLUDGE: A slot named CLASS for a non-CLASS value is bad.
  ;; Especially when a CLASS value *is* stored in another slot (called
  ;; CLASS-INFO:-). Perhaps this should be called CLASS-NAME? Also
  ;; weird that comment above says "Numeric-Type is used to represent
  ;; all numeric types" but this slot doesn't allow COMPLEX as an
  ;; option.. how does this fall into "not specified" NIL case above?
  (class nil :type (member integer rational float nil))
  ;; "format" for a float type (i.e. type specifier for a CPU
  ;; representation of floating point, e.g. 'SINGLE-FLOAT -- nothing
  ;; to do with #'FORMAT), or NIL if not specified or not a float.
  ;; Formats which don't exist in a given implementation don't appear
  ;; here.
  (format nil :type (or float-format null))
  ;; Is this a complex numeric type?  Null if unknown (only in NUMBER).
  ;;
  ;; FIXME: I'm bewildered by FOO-P names for things not intended to
  ;; interpreted as truth values. Perhaps rename this COMPLEXNESS?
  (complexp :real :type (member :real :complex nil))
  ;; The upper and lower bounds on the value, or NIL if there is no
  ;; bound. If a list of a number, the bound is exclusive. Integer
  ;; types never have exclusive bounds.
  (low nil :type (or number cons null))
  (high nil :type (or number cons null)))

;;; An ARRAY-TYPE is used to represent any array type, including
;;; things such as SIMPLE-STRING.
(defstruct (array-type (:include ctype
				 (class-info (type-class-or-lose 'array)))
		       (:copier nil))
  ;; the dimensions of the array, or * if unspecified. If a dimension
  ;; is unspecified, it is *.
  (dimensions '* :type (or list (member *)))
  ;; Is this not a simple array type? (:MAYBE means that we don't know.)
  (complexp :maybe :type (member t nil :maybe))
  ;; the element type as originally specified
  (element-type (required-argument) :type ctype)
  ;; the element type as it is specialized in this implementation
  (specialized-element-type *wild-type* :type ctype))

;;; A MEMBER-TYPE represent a use of the MEMBER type specifier. We
;;; bother with this at this level because MEMBER types are fairly
;;; important and union and intersection are well defined.
(defstruct (member-type (:include ctype
				  (class-info (type-class-or-lose 'member))
				  (enumerable t))
			(:copier nil)
			#-sb-xc-host (:pure nil))
  ;; the things in the set, with no duplications
  (members nil :type list))

;;; A COMPOUND-TYPE is a type defined out of a set of types, the
;;; common parent of UNION-TYPE and INTERSECTION-TYPE.
(defstruct (compound-type (:include ctype)
			  (:constructor nil)
			  (:copier nil))
  (types nil :type list :read-only t))

;;; A UNION-TYPE represents a use of the OR type specifier which we
;;; couldn't canonicalize to something simpler. Canonical form:
;;;   1. All possible pairwise simplifications (using the UNION2 type
;;;      methods) have been performed. Thus e.g. there is never more
;;;      than one MEMBER-TYPE component. FIXME: As of sbcl-0.6.11.13,
;;;      this hadn't been fully implemented yet.
;;;   2. There are never any UNION-TYPE components.
(defstruct (union-type (:include compound-type
				 (class-info (type-class-or-lose 'union)))
		       (:constructor %make-union-type (enumerable types))
		       (:copier nil)))

;;; An INTERSECTION-TYPE represents a use of the AND type specifier
;;; which we couldn't canonicalize to something simpler. Canonical form:
;;;   1. All possible pairwise simplifications (using the INTERSECTION2
;;;      type methods) have been performed. Thus e.g. there is never more
;;;      than one MEMBER-TYPE component.
;;;   2. There are never any INTERSECTION-TYPE components: we've
;;;      flattened everything into a single INTERSECTION-TYPE object.
;;;   3. There are never any UNION-TYPE components. Either we should
;;;      use the distributive rule to rearrange things so that
;;;      unions contain intersections and not vice versa, or we
;;;      should just punt to using a HAIRY-TYPE.
(defstruct (intersection-type (:include compound-type
					(class-info (type-class-or-lose
						     'intersection)))
			      (:constructor %make-intersection-type
					    (enumerable types))
			      (:copier nil)))

;;; Return TYPE converted to canonical form for a situation where the
;;; "type" '* (which SBCL still represents as a type even though ANSI
;;; CL defines it as a related but different kind of placeholder) is
;;; equivalent to type T.
(defun type-*-to-t (type)
  (if (type= type *wild-type*)
      *universal-type*
      type))

;;; A CONS-TYPE is used to represent a CONS type.
(defstruct (cons-type (:include ctype (:class-info (type-class-or-lose 'cons)))
		      (:constructor
		       ;; ANSI says that for CAR and CDR subtype
		       ;; specifiers '* is equivalent to T. In order
		       ;; to avoid special cases in SUBTYPEP and
		       ;; possibly elsewhere, we slam all CONS-TYPE
		       ;; objects into canonical form w.r.t. this
		       ;; equivalence at creation time.
		       make-cons-type (car-raw-type
				       cdr-raw-type
				       &aux
				       (car-type (type-*-to-t car-raw-type))
				       (cdr-type (type-*-to-t cdr-raw-type))))
		      (:copier nil))
  ;; the CAR and CDR element types (to support ANSI (CONS FOO BAR) types)
  ;;
  ;; FIXME: Most or all other type structure slots could also be :READ-ONLY.
  (car-type (required-argument) :type ctype :read-only t)
  (cdr-type (required-argument) :type ctype :read-only t))

;;; Note that the type NAME has been (re)defined, updating the
;;; undefined warnings and VALUES-SPECIFIER-TYPE cache.
(defun %note-type-defined (name)
  (declare (symbol name))
  (note-name-defined name :type)
  (when (boundp 'sb!kernel::*values-specifier-type-cache-vector*)
    (values-specifier-type-cache-clear))
  (values))

(!defun-from-collected-cold-init-forms !early-type-cold-init)

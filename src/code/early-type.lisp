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
	    (let* ((lspec (if (atom spec) (list spec) spec))
		   (fun (info :type :translator (car lspec))))
	      (cond (fun (funcall fun lspec))
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
;;; reasonably or to be useful, such as AND, NOT and SATISFIES and
;;; unknown types. We just remember the original type spec.
(defstruct (hairy-type (:include ctype
				 (class-info (type-class-or-lose 'hairy))
				 (enumerable t))
		       #!+cmu (:pure nil))
  ;; the Common Lisp type-specifier
  (specifier nil :type t))

(define-type-class hairy)

;;; An UNKNOWN-TYPE is a type not known to the type system (not yet
;;; defined). We make this distinction since we don't want to complain
;;; about types that are hairy but defined.
(defstruct (unknown-type (:include hairy-type)))

;;; ARGS-TYPE objects are used both to represent VALUES types and
;;; to represent FUNCTION types.
(defstruct (args-type (:include ctype)
		      (:constructor nil))
  ;; Lists of the type for each required and optional argument.
  (required nil :type list)
  (optional nil :type list)
  ;; The type for the rest arg. NIL if there is no rest arg.
  (rest nil :type (or ctype null))
  ;; True if keyword arguments are specified.
  (keyp nil :type boolean)
  ;; List of key-info structures describing the keyword arguments.
  (keywords nil :type list)
  ;; True if other keywords are allowed.
  (allowp nil :type boolean))

(defstruct (values-type
	    (:include args-type
		      (class-info (type-class-or-lose 'values)))))

(define-type-class values)

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
		      (class-info (type-class-or-lose 'constant))))
  ;; The type which the argument must be a constant instance of for this type
  ;; specifier to win.
  (type (required-argument) :type ctype))

;;; The NAMED-TYPE is used to represent *, T and NIL. These types must be
;;; super or sub types of all types, not just classes and * & NIL aren't
;;; classes anyway, so it wouldn't make much sense to make them built-in
;;; classes.
(defstruct (named-type (:include ctype
				 (class-info (type-class-or-lose 'named))))
  (name nil :type symbol))

;;; The Numeric-Type is used to represent all numeric types, including things
;;; such as FIXNUM.
(defstruct (numeric-type (:include ctype
				   (class-info (type-class-or-lose
						'number)))
			 #!+negative-zero-is-not-zero
			 (:constructor %make-numeric-type))
  ;; The kind of numeric type we have. NIL if not specified (just NUMBER or
  ;; COMPLEX).
  ;;
  ;; KLUDGE: A slot named CLASS for a non-CLASS value is bad.
  ;; Especially when a CLASS value *is* stored in another slot (called
  ;; CLASS-INFO:-). Perhaps this should be called CLASS-NAME? Also
  ;; weird that comment above says "Numeric-Type is used to represent
  ;; all numeric types" but this slot doesn't allow COMPLEX as an
  ;; option.. how does this fall into "not specified" NIL case above?
  (class nil :type (member integer rational float nil))
  ;; Format for a float type. NIL if not specified or not a float. Formats
  ;; which don't exist in a given implementation don't appear here.
  (format nil :type (or float-format null))
  ;; Is this a complex numeric type?  Null if unknown (only in NUMBER.)
  ;;
  ;; FIXME: I'm bewildered by FOO-P names for things not intended to
  ;; interpreted as truth values. Perhaps rename this COMPLEXNESS?
  (complexp :real :type (member :real :complex nil))
  ;; The upper and lower bounds on the value. If null, there is no bound. If
  ;; a list of a number, the bound is exclusive. Integer types never have
  ;; exclusive bounds.
  (low nil :type (or number cons null))
  (high nil :type (or number cons null)))

;;; The Array-Type is used to represent all array types, including
;;; things such as SIMPLE-STRING.
(defstruct (array-type (:include ctype
				 (class-info (type-class-or-lose 'array))))
  ;; The dimensions of the array. * if unspecified. If a dimension is
  ;; unspecified, it is *.
  (dimensions '* :type (or list (member *)))
  ;; Is this not a simple array type? (:MAYBE means that we don't know.)
  (complexp :maybe :type (member t nil :maybe))
  ;; The element type as originally specified.
  (element-type (required-argument) :type ctype)
  ;; The element type as it is specialized in this implementation.
  (specialized-element-type *wild-type* :type ctype))

;;; The Member-Type represents uses of the MEMBER type specifier. We
;;; bother with this at this level because MEMBER types are fairly
;;; important and union and intersection are well defined.
(defstruct (member-type (:include ctype
				  (class-info (type-class-or-lose 'member))
				  (enumerable t))
			#-sb-xc-host (:pure nil))
  ;; The things in the set, with no duplications.
  (members nil :type list))

;;; A UNION-TYPE represents a use of the OR type specifier which can't
;;; be canonicalized to something simpler. Canonical form:
;;;   1. There is never more than one Member-Type component.
;;;   2. There are never any Union-Type components.
(defstruct (union-type (:include ctype
				 (class-info (type-class-or-lose 'union)))
		       (:constructor %make-union-type (enumerable types)))
  ;; The types in the union.
  (types nil :type list))

;;; Note that the type Name has been (re)defined, updating the
;;; undefined warnings and VALUES-SPECIFIER-TYPE cache.
(defun %note-type-defined (name)
  (declare (symbol name))
  (note-name-defined name :type)
  (when (boundp 'sb!kernel::*values-specifier-type-cache-vector*)
    (values-specifier-type-cache-clear))
  (values))


;;; MNA: cons compound-type patch
;;; FIXIT: all commented out
;;;; Cons types:
 
;;; The Cons-Type is used to represent cons types.
;;;
;; (defstruct (cons-type (:include ctype
;;  				(:class-info (type-class-or-lose 'cons)))
;;                              (:print-function %print-type))
;;   ;;
;;   ;; The car element type.
;;   (car-type *wild-type* :type ctype)
;;   ;;
;;   ;; The cdr element type.
;;   (cdr-type *wild-type* :type ctype))

;; (define-type-class cons)

;;;; KLUDGE: not clear this really belongs here, but where?

;;; Is X a fixnum in the target Lisp?
(defun target-fixnump (x)
  (and (integerp x)
       (<= sb!vm:*target-most-negative-fixnum*
	   x
	   sb!vm:*target-most-positive-fixnum*)))

(!defun-from-collected-cold-init-forms !early-type-cold-init)

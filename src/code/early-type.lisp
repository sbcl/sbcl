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

;;;; representations of types

;;; A HAIRY-TYPE represents anything too weird to be described
;;; reasonably or to be useful, such as NOT, SATISFIES, unknown types,
;;; and unreasonably complicated types involving AND. We just remember
;;; the original type spec.
(defstruct (hairy-type (:include ctype
				 (class-info (type-class-or-lose 'hairy))
				 (enumerable t)
				 (might-contain-other-types-p t))
		       (:copier nil)
		       #!+cmu (:pure nil))
  ;; the Common Lisp type-specifier of the type we represent
  (specifier nil :type t))

(!define-type-class hairy)

;;; An UNKNOWN-TYPE is a type not known to the type system (not yet
;;; defined). We make this distinction since we don't want to complain
;;; about types that are hairy but defined.
(defstruct (unknown-type (:include hairy-type)
			 (:copier nil)))

(defstruct (negation-type (:include ctype
				    (class-info (type-class-or-lose 'negation))
				    ;; FIXME: is this right?  It's
				    ;; what they had before, anyway
				    (enumerable t)
				    (might-contain-other-types-p t))
			  (:copier nil)
			  #!+cmu (:pure nil))
  (type (missing-arg) :type ctype))

(!define-type-class negation)

;;; ARGS-TYPE objects are used both to represent VALUES types and
;;; to represent FUNCTION types.
(defstruct (args-type (:include ctype)
		      (:constructor nil)
		      (:copier nil))
  ;; Lists of the type for each required and optional argument.
  (required nil :type list)
  (optional nil :type list)
  ;; The type for the rest arg. NIL if there is no &REST arg.
  (rest nil :type (or ctype null))
  ;; true if &KEY arguments are specified
  (keyp nil :type boolean)
  ;; list of KEY-INFO structures describing the &KEY arguments
  (keywords nil :type list)
  ;; true if other &KEY arguments are allowed
  (allowp nil :type boolean))

(defun canonicalize-args-type-args (required optional rest)
  (when (eq rest *empty-type*)
    ;; or vice-versa?
    (setq rest nil))
  (loop with last-not-rest = nil
        for i from 0
        for opt in optional
        do (cond ((eq opt *empty-type*)
                  (return (values required (subseq optional i) rest)))
                 ((neq opt rest)
                  (setq last-not-rest i)))
        finally (return (values required
                                (if last-not-rest
                                    (subseq optional 0 (1+ last-not-rest))
                                    nil)
                                rest))))

(defun args-types (lambda-list-like-thing)
  (multiple-value-bind
	(required optional restp rest keyp keys allowp auxp aux
                  morep more-context more-count llk-p)
      (parse-lambda-list-like-thing lambda-list-like-thing)
    (declare (ignore aux morep more-context more-count))
    (when auxp
      (error "&AUX in a FUNCTION or VALUES type: ~S." lambda-list-like-thing))
    (let ((required (mapcar #'single-value-specifier-type required))
	  (optional (mapcar #'single-value-specifier-type optional))
	  (rest (when restp (single-value-specifier-type rest)))
	  (keywords
	   (collect ((key-info))
	     (dolist (key keys)
	       (unless (proper-list-of-length-p key 2)
		 (error "Keyword type description is not a two-list: ~S." key))
	       (let ((kwd (first key)))
		 (when (find kwd (key-info) :key #'key-info-name)
		   (error "~@<repeated keyword ~S in lambda list: ~2I~_~S~:>"
			  kwd lambda-list-like-thing))
		 (key-info
		  (make-key-info
		   :name kwd
		   :type (single-value-specifier-type (second key))))))
	     (key-info))))
      (multiple-value-bind (required optional rest)
	  (canonicalize-args-type-args required optional rest)
	(values required optional rest keyp keywords allowp llk-p)))))

(defstruct (values-type
	    (:include args-type
		      (class-info (type-class-or-lose 'values)))
            (:constructor %make-values-type)
	    (:copier nil)))

(defun-cached (make-values-type-cached
               :hash-bits 8
               :hash-function (lambda (req opt rest allowp)
                                (logand (logxor
                                         (type-list-cache-hash req)
                                         (type-list-cache-hash opt)
                                         (if rest
                                             (type-hash-value rest)
                                             42)
                                         (sxhash allowp))
                                        #xFF)))
    ((required equal-but-no-car-recursion)
     (optional equal-but-no-car-recursion)
     (rest eq)
     (allowp eq))
  (%make-values-type :required required
                     :optional optional
                     :rest rest
                     :allowp allowp))

(defun make-values-type (&key (args nil argsp)
                         required optional rest allowp)
  (if argsp
      (if (eq args '*)
	  *wild-type*
	  (multiple-value-bind (required optional rest keyp keywords allowp
                                llk-p)
	      (args-types args)
            (declare (ignore keywords))
            (when keyp
              (error "&KEY appeared in a VALUES type specifier ~S."
                     `(values ,@args)))
            (if llk-p
                (make-values-type :required required
                                  :optional optional
                                  :rest rest
                                  :allowp allowp)
                (make-short-values-type required))))
      (multiple-value-bind (required optional rest)
          (canonicalize-args-type-args required optional rest)
        (cond ((and (null required)
                    (null optional)
                    (eq rest *universal-type*))
               *wild-type*)
              ((memq *empty-type* required)
               *empty-type*)
              (t (make-values-type-cached required optional
                                          rest allowp))))))

(!define-type-class values)

;;; (SPECIFIER-TYPE 'FUNCTION) and its subtypes
(defstruct (fun-type (:include args-type
			       (class-info (type-class-or-lose 'function)))
                     (:constructor
                      %make-fun-type (&key required optional rest
                                           keyp keywords allowp
                                           wild-args
                                           returns
                                      &aux (rest (if (eq rest *empty-type*)
                                                     nil
                                                     rest)))))
  ;; true if the arguments are unrestrictive, i.e. *
  (wild-args nil :type boolean)
  ;; type describing the return values. This is a values type
  ;; when multiple values were specified for the return.
  (returns (missing-arg) :type ctype))
(defun make-fun-type (&rest initargs
		      &key (args nil argsp) returns &allow-other-keys)
  (if argsp
      (if (eq args '*)
	  (if (eq returns *wild-type*)
	      (specifier-type 'function)
	      (%make-fun-type :wild-args t :returns returns))
	  (multiple-value-bind (required optional rest keyp keywords allowp)
	      (args-types args)
	    (if (and (null required)
		     (null optional)
		     (eq rest *universal-type*)
		     (not keyp))
		(if (eq returns *wild-type*)
		    (specifier-type 'function)
		    (%make-fun-type :wild-args t :returns returns))
		(%make-fun-type :required required
				:optional optional
				:rest rest
				:keyp keyp
				:keywords keywords
				:allowp allowp
				:returns returns))))
      ;; FIXME: are we really sure that we won't make something that
      ;; looks like a completely wild function here?
      (apply #'%make-fun-type initargs)))

;;; The CONSTANT-TYPE structure represents a use of the CONSTANT-ARG
;;; "type specifier", which is only meaningful in function argument
;;; type specifiers used within the compiler. (It represents something
;;; that the compiler knows to be a constant.)
(defstruct (constant-type
	    (:include ctype
		      (class-info (type-class-or-lose 'constant)))
	    (:copier nil))
  ;; The type which the argument must be a constant instance of for this type
  ;; specifier to win.
  (type (missing-arg) :type ctype))

;;; The NAMED-TYPE is used to represent *, T and NIL. These types must
;;; be super- or sub-types of all types, not just classes and * and
;;; NIL aren't classes anyway, so it wouldn't make much sense to make
;;; them built-in classes.
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
				   (class-info (type-class-or-lose 'number)))
			 (:constructor %make-numeric-type)
			 (:copier nil))
  ;; the kind of numeric type we have, or NIL if not specified (just
  ;; NUMBER or COMPLEX)
  ;;
  ;; KLUDGE: A slot named CLASS for a non-CLASS value is bad.
  ;; Especially when a CLASS value *is* stored in another slot (called
  ;; CLASS-INFO:-). Perhaps this should be called CLASS-NAME? Also
  ;; weird that comment above says "Numeric-Type is used to represent
  ;; all numeric types" but this slot doesn't allow COMPLEX as an
  ;; option.. how does this fall into "not specified" NIL case above?
  ;; Perhaps someday we can switch to CLOS and make NUMERIC-TYPE
  ;; be an abstract base class and INTEGER-TYPE, RATIONAL-TYPE, and
  ;; whatnot be concrete subclasses..
  (class nil :type (member integer rational float nil) :read-only t)
  ;; "format" for a float type (i.e. type specifier for a CPU
  ;; representation of floating point, e.g. 'SINGLE-FLOAT -- nothing
  ;; to do with #'FORMAT), or NIL if not specified or not a float.
  ;; Formats which don't exist in a given implementation don't appear
  ;; here.
  (format nil :type (or float-format null) :read-only t)
  ;; Is this a complex numeric type?  Null if unknown (only in NUMBER).
  ;;
  ;; FIXME: I'm bewildered by FOO-P names for things not intended to
  ;; interpreted as truth values. Perhaps rename this COMPLEXNESS?
  (complexp :real :type (member :real :complex nil) :read-only t)
  ;; The upper and lower bounds on the value, or NIL if there is no
  ;; bound. If a list of a number, the bound is exclusive. Integer
  ;; types never have exclusive bounds, i.e. they may have them on
  ;; input, but they're canonicalized to inclusive bounds before we
  ;; store them here.
  (low nil :type (or number cons null) :read-only t)
  (high nil :type (or number cons null) :read-only t))

;;; Impose canonicalization rules for NUMERIC-TYPE. Note that in some
;;; cases, despite the name, we return *EMPTY-TYPE* instead of a
;;; NUMERIC-TYPE.
(defun make-numeric-type (&key class format (complexp :real) low high
			       enumerable)
  ;; if interval is empty
  (if (and low
	   high
	   (if (or (consp low) (consp high)) ; if either bound is exclusive
	       (>= (type-bound-number low) (type-bound-number high))
	       (> low high)))
      *empty-type*
      (multiple-value-bind (canonical-low canonical-high)
	  (case class
	    (integer
	     ;; INTEGER types always have their LOW and HIGH bounds
	     ;; represented as inclusive, not exclusive values.
	     (values (if (consp low)
			 (1+ (type-bound-number low))
			 low)
		     (if (consp high)
			 (1- (type-bound-number high))
			 high)))
	    (t 
	     ;; no canonicalization necessary
	     (values low high)))
	(when (and (eq class 'rational)
		   (integerp canonical-low)
		   (integerp canonical-high)
		   (= canonical-low canonical-high))
	  (setf class 'integer))
	(%make-numeric-type :class class
			    :format format
			    :complexp complexp
			    :low canonical-low
			    :high canonical-high
			    :enumerable enumerable))))

(defun modified-numeric-type (base
			      &key
			      (class      (numeric-type-class      base))
			      (format     (numeric-type-format     base))
			      (complexp   (numeric-type-complexp   base))
			      (low        (numeric-type-low        base))
			      (high       (numeric-type-high       base))
			      (enumerable (numeric-type-enumerable base)))
  (make-numeric-type :class class
		     :format format
		     :complexp complexp
		     :low low
		     :high high
		     :enumerable enumerable))

(defstruct (character-set-type
            (:include ctype
                      (class-info (type-class-or-lose 'character-set)))
            (:constructor %make-character-set-type)
            (:copier nil))
  (pairs (missing-arg) :type list :read-only t))
(defun make-character-set-type (&key pairs)
  (aver (equal (mapcar #'car pairs)
              (sort (mapcar #'car pairs) #'<)))
  (let ((pairs (let (result)
                (do ((pairs pairs (cdr pairs)))
                    ((null pairs) (nreverse result))
                  (destructuring-bind (low . high) (car pairs)
                    (loop for (low1 . high1) in (cdr pairs)
                          if (<= low1 (1+ high))
                          do (progn (setf high (max high high1))
                                    (setf pairs (cdr pairs)))
                          else do (return nil))
                    (cond
                      ((>= low sb!xc:char-code-limit))
                      ((< high 0))
                      (t (push (cons (max 0 low)
                                     (min high (1- sb!xc:char-code-limit)))
                               result))))))))
    (if (null pairs)
       *empty-type*
       (%make-character-set-type :pairs pairs))))

;;; An ARRAY-TYPE is used to represent any array type, including
;;; things such as SIMPLE-BASE-STRING.
(defstruct (array-type (:include ctype
				 (class-info (type-class-or-lose 'array)))
                       (:constructor %make-array-type)
		       (:copier nil))
  ;; the dimensions of the array, or * if unspecified. If a dimension
  ;; is unspecified, it is *.
  (dimensions '* :type (or list (member *)))
  ;; Is this not a simple array type? (:MAYBE means that we don't know.)
  (complexp :maybe :type (member t nil :maybe))
  ;; the element type as originally specified
  (element-type (missing-arg) :type ctype)
  ;; the element type as it is specialized in this implementation
  (specialized-element-type *wild-type* :type ctype))
(define-cached-synonym make-array-type)

;;; A MEMBER-TYPE represent a use of the MEMBER type specifier. We
;;; bother with this at this level because MEMBER types are fairly
;;; important and union and intersection are well defined.
(defstruct (member-type (:include ctype
				  (class-info (type-class-or-lose 'member))
				  (enumerable t))
			(:copier nil)
			(:constructor %make-member-type (members))
			#-sb-xc-host (:pure nil))
  ;; the things in the set, with no duplications
  (members nil :type list))
(defun make-member-type (&key members)
  (declare (type list members))
  ;; make sure that we've removed duplicates
  (aver (= (length members) (length (remove-duplicates members))))
  ;; if we have a pair of zeros (e.g. 0.0d0 and -0.0d0), then we can
  ;; canonicalize to (DOUBLE-FLOAT 0.0d0 0.0d0), because numeric
  ;; ranges are compared by arithmetic operators (while MEMBERship is
  ;; compared by EQL).  -- CSR, 2003-04-23
  (let ((singlep (subsetp `(,(load-time-value (make-unportable-float :single-float-negative-zero)) 0.0f0) members))
	(doublep (subsetp `(,(load-time-value (make-unportable-float :double-float-negative-zero)) 0.0d0) members))
	#!+long-float
	(longp (subsetp `(,(load-time-value (make-unportable-float :long-float-negative-zero)) 0.0l0) members)))
    (if (or singlep doublep #!+long-float longp)
	(let (union-types)
	  (when singlep
	    (push (ctype-of 0.0f0) union-types)
	    (setf members (set-difference members `(,(load-time-value (make-unportable-float :single-float-negative-zero)) 0.0f0))))
	  (when doublep
	    (push (ctype-of 0.0d0) union-types)
	    (setf members (set-difference members `(,(load-time-value (make-unportable-float :double-float-negative-zero)) 0.0d0))))
	  #!+long-float
	  (when longp
	    (push (ctype-of 0.0l0) union-types)
	    (setf members (set-difference members `(,(load-time-value (make-unportable-float :long-float-negative-zero)) 0.0l0))))
	  (aver (not (null union-types)))
	  (make-union-type t
			   (if (null members)
			       union-types
			       (cons (%make-member-type members)
				     union-types))))
	(%make-member-type members))))

;;; A COMPOUND-TYPE is a type defined out of a set of types, the
;;; common parent of UNION-TYPE and INTERSECTION-TYPE.
(defstruct (compound-type (:include ctype
				    (might-contain-other-types-p t))
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
(define-cached-synonym make-union-type)

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
(defstruct (cons-type (:include ctype (class-info (type-class-or-lose 'cons)))
		      (:constructor
		       %make-cons-type (car-type
					cdr-type))
		      (:copier nil))
  ;; the CAR and CDR element types (to support ANSI (CONS FOO BAR) types)
  ;;
  ;; FIXME: Most or all other type structure slots could also be :READ-ONLY.
  (car-type (missing-arg) :type ctype :read-only t)
  (cdr-type (missing-arg) :type ctype :read-only t))
(defun make-cons-type (car-type cdr-type)
  (aver (not (or (eq car-type *wild-type*)
                 (eq cdr-type *wild-type*))))
  (if (or (eq car-type *empty-type*)
	  (eq cdr-type *empty-type*))
      *empty-type*
      (%make-cons-type car-type cdr-type)))

(defun cons-type-length-info (type)
  (declare (type cons-type type))
  (do ((min 1 (1+ min))
       (cdr (cons-type-cdr-type type) (cons-type-cdr-type cdr)))
      ((not (cons-type-p cdr))
       (cond
	 ((csubtypep cdr (specifier-type 'null))
	  (values min t))
	 ((csubtypep *universal-type* cdr)
	  (values min nil))
	 ((type/= (type-intersection (specifier-type 'cons) cdr) *empty-type*)
	  (values min nil))
	 ((type/= (type-intersection (specifier-type 'null) cdr) *empty-type*)
	  (values min t))
	 (t (values min :maybe))))
    ()))
       

;;;; type utilities

;;; Return the type structure corresponding to a type specifier. We
;;; pick off structure types as a special case.
;;;
;;; Note: VALUES-SPECIFIER-TYPE-CACHE-CLEAR must be called whenever a
;;; type is defined (or redefined).
(defun-cached (values-specifier-type
	       :hash-function (lambda (x)
                                (logand (sxhash x) #x3FF))
	       :hash-bits 10
	       :init-wrapper !cold-init-forms)
	      ((orig equal-but-no-car-recursion))
  (let ((u (uncross orig)))
    (or (info :type :builtin u)
	(let ((spec (type-expand u)))
	  (cond
	   ((and (not (eq spec u))
		 (info :type :builtin spec)))
	   ((eq (info :type :kind spec) :instance)
	    (find-classoid spec))
	   ((typep spec 'classoid)
	    ;; There doesn't seem to be any way to translate
	    ;; (TYPEP SPEC 'BUILT-IN-CLASS) into something which can be
	    ;; executed on the host Common Lisp at cross-compilation time.
	    #+sb-xc-host (error
			  "stub: (TYPEP SPEC 'BUILT-IN-CLASS) on xc host")
	    (if (typep spec 'built-in-classoid)
		(or (built-in-classoid-translation spec) spec)
		spec))
	   (t
	    (when (and (atom spec)
		       (member spec '(and or not member eql satisfies values)))
	      (error "The symbol ~S is not valid as a type specifier." spec))
	    (let* ((lspec (if (atom spec) (list spec) spec))
		   (fun (info :type :translator (car lspec))))
	      (cond (fun
		     (funcall fun lspec))
		    ((or (and (consp spec) (symbolp (car spec))
			      (not (info :type :builtin (car spec))))
			 (and (symbolp spec) (not (info :type :builtin spec))))
		     (when (and *type-system-initialized*
                                (not (eq (info :type :kind spec)
                                         :forthcoming-defclass-type)))
		       (signal 'parse-unknown-type :specifier spec))
		     ;; (The RETURN-FROM here inhibits caching.)
		     (return-from values-specifier-type
		       (make-unknown-type :specifier spec)))
		    (t
		     (error "bad thing to be a type specifier: ~S"
			    spec))))))))))

;;; This is like VALUES-SPECIFIER-TYPE, except that we guarantee to
;;; never return a VALUES type.
(defun specifier-type (x)
  (let ((res (values-specifier-type x)))
    (when (or (values-type-p res)
              ;; bootstrap magic :-(
              (and (named-type-p res)
                   (eq (named-type-name res) '*)))
      (error "VALUES type illegal in this context:~%  ~S" x))
    res))

(defun single-value-specifier-type (x)
  (if (eq x '*)
      *universal-type*
      (specifier-type x)))

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

;;; Note that the type NAME has been (re)defined, updating the
;;; undefined warnings and VALUES-SPECIFIER-TYPE cache.
(defun %note-type-defined (name)
  (declare (symbol name))
  (note-name-defined name :type)
  (when (boundp 'sb!kernel::*values-specifier-type-cache-vector*)
    (values-specifier-type-cache-clear))
  (values))


(!defun-from-collected-cold-init-forms !early-type-cold-init)

;;;; This file contains the definition of non-CLASS types (e.g.
;;;; subtypes of interesting BUILT-IN-CLASSes) and the interfaces to
;;;; the type system. Common Lisp type specifiers are parsed into a
;;;; somewhat canonical internal type representation that supports
;;;; type union, intersection, etc. (Except that ALIEN types have
;;;; moved out..)

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

;;; ### Remaining incorrectnesses:
;;;
;;; TYPE-UNION (and the OR type) doesn't properly canonicalize an
;;; exhaustive partition or coalesce contiguous ranges of numeric
;;; types.
;;;
;;; There are all sorts of nasty problems with open bounds on FLOAT
;;; types (and probably FLOAT types in general.)
;;;
;;; RATIO and BIGNUM are not recognized as numeric types.

;;; FIXME: It seems to me that this should be set to NIL by default,
;;; and perhaps not even optionally set to T.
(defvar *use-implementation-types* t
  #!+sb-doc
  "*USE-IMPLEMENTATION-TYPES* is a semi-public flag which determines how
   restrictive we are in determining type membership. If two types are the
   same in the implementation, then we will consider them them the same when
   this switch is on. When it is off, we try to be as restrictive as the
   language allows, allowing us to detect more errors. Currently, this only
   affects array types.")

(!cold-init-forms (setq *use-implementation-types* t))

;;; These functions are used as method for types which need a complex
;;; subtypep method to handle some superclasses, but cover a subtree
;;; of the type graph (i.e. there is no simple way for any other type
;;; class to be a subtype.) There are always still complex ways,
;;; namely UNION and MEMBER types, so we must give TYPE1's method a
;;; chance to run, instead of immediately returning NIL, T.
(defun delegate-complex-subtypep-arg2 (type1 type2)
  (let ((subtypep-arg1
	 (type-class-complex-subtypep-arg1
	  (type-class-info type1))))
    (if subtypep-arg1
	(funcall subtypep-arg1 type1 type2)
	(values nil t))))
(defun delegate-complex-intersection (type1 type2)
  (let ((method (type-class-complex-intersection (type-class-info type1))))
    (if (and method (not (eq method #'delegate-complex-intersection)))
	(funcall method type2 type1)
	(vanilla-intersection type1 type2))))

;;; This is used by DEFINE-SUPERCLASSES to define the SUBTYPE-ARG1
;;; method. INFO is a list of conses (SUPERCLASS-CLASS .
;;; {GUARD-TYPE-SPECIFIER | NIL}). This will never be called with a
;;; hairy type as TYPE2, since the hairy type TYPE2 method gets first
;;; crack.
;;;
;;; FIXME: Declare this as INLINE, since it's only used in one place.
(defun has-superclasses-complex-subtypep-arg1 (type1 type2 info)
  (values
   (and (sb!xc:typep type2 'sb!xc:class)
	(dolist (x info nil)
	  (when (or (not (cdr x))
		    (csubtypep type1 (specifier-type (cdr x))))
	    (return
	     (or (eq type2 (car x))
		 (let ((inherits (layout-inherits (class-layout (car x)))))
		   (dotimes (i (length inherits) nil)
		     (when (eq type2 (layout-class (svref inherits i)))
		       (return t)))))))))
   t))

;;; This function takes a list of specs, each of the form
;;;    (SUPERCLASS-NAME &OPTIONAL GUARD).
;;; Consider one spec (with no guard): any instance of the named
;;; TYPE-CLASS is also a subtype of the named superclass and of any of
;;; its superclasses. If there are multiple specs, then some will have
;;; guards. We choose the first spec whose guard is a supertype of
;;; TYPE1 and use its superclass. In effect, a sequence of guards
;;;    G0, G1, G2
;;; is actually
;;;    G0,(and G1 (not G0)), (and G2 (not (or G0 G1))).
;;;
;;; WHEN controls when the forms are executed.
(defmacro define-superclasses (type-class-name specs when)
  (let ((type-class (gensym "TYPE-CLASS-"))
	(info (gensym "INFO")))
    `(,when
       (let ((,type-class (type-class-or-lose ',type-class-name))
	     (,info (mapcar (lambda (spec)
			      (destructuring-bind
				  (super &optional guard)
				  spec
				(cons (sb!xc:find-class super) guard)))
			    ',specs)))
	 (setf (type-class-complex-subtypep-arg1 ,type-class)
	       (lambda (type1 type2)
		 (has-superclasses-complex-subtypep-arg1 type1 type2 ,info)))
	 (setf (type-class-complex-subtypep-arg2 ,type-class)
	       #'delegate-complex-subtypep-arg2)
	 (setf (type-class-complex-intersection ,type-class)
	       #'delegate-complex-intersection)))))

;;;; FUNCTION and VALUES types
;;;;
;;;; Pretty much all of the general type operations are illegal on
;;;; VALUES types, since we can't discriminate using them, do
;;;; SUBTYPEP, etc. FUNCTION types are acceptable to the normal type
;;;; operations, but are generally considered to be equivalent to
;;;; FUNCTION. These really aren't true types in any type theoretic
;;;; sense, but we still parse them into CTYPE structures for two
;;;; reasons:

;;;; -- Parsing and unparsing work the same way, and indeed we can't
;;;;    tell whether a type is a function or values type without
;;;;    parsing it.
;;;; -- Many of the places that can be annotated with real types can
;;;;    also be annotated with function or values types.

;;; the description of a keyword argument
(defstruct (key-info #-sb-xc-host (:pure t))
  ;; the keyword
  (name (required-argument) :type keyword)
  ;; the type of the argument value
  (type (required-argument) :type ctype))

(define-type-method (values :simple-subtypep :complex-subtypep-arg1)
		    (type1 type2)
  (declare (ignore type2))
  (error "Subtypep is illegal on this type:~%  ~S" (type-specifier type1)))

(define-type-method (values :complex-subtypep-arg2)
		    (type1 type2)
  (declare (ignore type1))
  (error "Subtypep is illegal on this type:~%  ~S" (type-specifier type2)))

(define-type-method (values :unparse) (type)
  (cons 'values (unparse-args-types type)))

;;; Return true if LIST1 and LIST2 have the same elements in the same
;;; positions according to TYPE=. We return NIL, NIL if there is an
;;; uncertain comparison.
(defun type=-list (list1 list2)
  (declare (list list1 list2))
  (do ((types1 list1 (cdr types1))
       (types2 list2 (cdr types2)))
      ((or (null types1) (null types2))
       (if (or types1 types2)
	   (values nil t)
	   (values t t)))
    (multiple-value-bind (val win)
	(type= (first types1) (first types2))
      (unless win
	(return (values nil nil)))
      (unless val
	(return (values nil t))))))

(define-type-method (values :simple-=) (type1 type2)
  (let ((rest1 (args-type-rest type1))
	(rest2 (args-type-rest type2)))
    (cond ((or (args-type-keyp type1) (args-type-keyp type2)
	       (args-type-allowp type1) (args-type-allowp type2))
	   (values nil nil))
	  ((and rest1 rest2 (type/= rest1 rest2))
	   (type= rest1 rest2))
	  ((or rest1 rest2)
	   (values nil t))
	  (t
	   (multiple-value-bind (req-val req-win)
	       (type=-list (values-type-required type1)
			   (values-type-required type2))
	     (multiple-value-bind (opt-val opt-win)
		 (type=-list (values-type-optional type1)
			     (values-type-optional type2))
	       (values (and req-val opt-val) (and req-win opt-win))))))))

(define-type-class function)

;;; a flag that we can bind to cause complex function types to be
;;; unparsed as FUNCTION. This is useful when we want a type that we
;;; can pass to TYPEP.
(defvar *unparse-function-type-simplify*)
(!cold-init-forms (setq *unparse-function-type-simplify* nil))

(define-type-method (function :unparse) (type)
  (if *unparse-function-type-simplify*
      'function
      (list 'function
	    (if (function-type-wild-args type)
		'*
		(unparse-args-types type))
	    (type-specifier
	     (function-type-returns type)))))

;;; Since all function types are equivalent to FUNCTION, they are all
;;; subtypes of each other.
(define-type-method (function :simple-subtypep) (type1 type2)
  (declare (ignore type1 type2))
  (values t t))

(define-superclasses function ((function)) !cold-init-forms)

;;; The union or intersection of two FUNCTION types is FUNCTION.
(define-type-method (function :simple-union) (type1 type2)
  (declare (ignore type1 type2))
  (specifier-type 'function))
(define-type-method (function :simple-intersection) (type1 type2)
  (declare (ignore type1 type2))
  (values (specifier-type 'function) t))

;;; ### Not very real, but good enough for redefining transforms
;;; according to type:
(define-type-method (function :simple-=) (type1 type2)
  (values (equalp type1 type2) t))

(define-type-class constant :inherits values)

(define-type-method (constant :unparse) (type)
  `(constant-argument ,(type-specifier (constant-type-type type))))

(define-type-method (constant :simple-=) (type1 type2)
  (type= (constant-type-type type1) (constant-type-type type2)))

(def-type-translator constant-argument (type)
  (make-constant-type :type (specifier-type type)))

;;; Given a LAMBDA-LIST-like values type specification and an ARGS-TYPE
;;; structure, fill in the slots in the structure accordingly. This is
;;; used for both FUNCTION and VALUES types.
(declaim (ftype (function (list args-type) (values)) parse-args-types))
(defun parse-args-types (lambda-list result)
  (multiple-value-bind (required optional restp rest keyp keys allowp aux)
      (parse-lambda-list lambda-list)
    (when aux
      (error "&Aux in a FUNCTION or VALUES type: ~S." lambda-list))
    (setf (args-type-required result) (mapcar #'specifier-type required))
    (setf (args-type-optional result) (mapcar #'specifier-type optional))
    (setf (args-type-rest result) (if restp (specifier-type rest) nil))
    (setf (args-type-keyp result) keyp)
    (collect ((key-info))
      (dolist (key keys)
	(unless (proper-list-of-length-p key 2)
	  (error "Keyword type description is not a two-list: ~S." key))
	(let ((kwd (first key)))
	  (when (find kwd (key-info) :key #'key-info-name)
	    (error "Repeated keyword ~S in lambda list: ~S." kwd lambda-list))
	  (key-info (make-key-info :name kwd
				   :type (specifier-type (second key))))))
      (setf (args-type-keywords result) (key-info)))
    (setf (args-type-allowp result) allowp)
    (values)))

;;; Return the lambda-list-like type specification corresponding
;;; to an ARGS-TYPE.
(declaim (ftype (function (args-type) list) unparse-args-types))
(defun unparse-args-types (type)
  (collect ((result))

    (dolist (arg (args-type-required type))
      (result (type-specifier arg)))

    (when (args-type-optional type)
      (result '&optional)
      (dolist (arg (args-type-optional type))
	(result (type-specifier arg))))

    (when (args-type-rest type)
      (result '&rest)
      (result (type-specifier (args-type-rest type))))

    (when (args-type-keyp type)
      (result '&key)
      (dolist (key (args-type-keywords type))
	(result (list (key-info-name key)
		      (type-specifier (key-info-type key))))))

    (when (args-type-allowp type)
      (result '&allow-other-keys))

    (result)))

(def-type-translator function (&optional (args '*) (result '*))
  (let ((res (make-function-type
	      :returns (values-specifier-type result))))
    (if (eq args '*)
	(setf (function-type-wild-args res) t)
	(parse-args-types args res))
    res))

(def-type-translator values (&rest values)
  (let ((res (make-values-type)))
    (parse-args-types values res)
    res))

;;;; VALUES types interfaces
;;;;
;;;; We provide a few special operations that can be meaningfully used
;;;; on VALUES types (as well as on any other type).

;;; Return the type of the first value indicated by Type. This is used
;;; by people who don't want to have to deal with values types.

;;; MNA: fix-instance-typep-call patch
#!-sb-fluid (declaim (freeze-type values-type))
; (inline single-value-type))
(defun single-value-type (type)
  (declare (type ctype type))
  (cond ((values-type-p type)
	 (or (car (args-type-required type))
             (if (args-type-optional type)
                 (type-union (car (args-type-optional type)) (specifier-type 'null)))
	     (args-type-rest type)
             (specifier-type 'null)))
	((eq type *wild-type*)
	 *universal-type*)
	(t
	 type)))

;;; Return the minmum number of arguments that a function can be
;;; called with, and the maximum number or NIL. If not a function
;;; type, return NIL, NIL.
(defun function-type-nargs (type)
  (declare (type ctype type))
  (if (function-type-p type)
      (let ((fixed (length (args-type-required type))))
	(if (or (args-type-rest type)
		(args-type-keyp type)
		(args-type-allowp type))
	    (values fixed nil)
	    (values fixed (+ fixed (length (args-type-optional type))))))
      (values nil nil)))

;;; Determine if Type corresponds to a definite number of values. The
;;; first value is a list of the types for each value, and the second
;;; value is the number of values. If the number of values is not
;;; fixed, then return NIL and :Unknown.
(defun values-types (type)
  (declare (type ctype type))
  (cond ((eq type *wild-type*)
	 (values nil :unknown))
	((not (values-type-p type))
	 (values (list type) 1))
	((or (args-type-optional type)
	     (args-type-rest type)
	     (args-type-keyp type)
	     (args-type-allowp type))
	 (values nil :unknown))
	(t
	 (let ((req (args-type-required type)))
	   (values (mapcar #'single-value-type req) (length req))))))

;;; Return two values:
;;; MNA: fix-instance-typep-call patch
;;; 1. A list of all the positional (fixed and optional) types.
;;; 2. The &REST type (if any). If keywords allowed, *UNIVERSAL-TYPE*.
;;;    If no keywords or &REST, then the DEFAULT-TYPE.
(defun values-type-types (type &optional (default-type *empty-type*))
  (declare (type values-type type))
  (values (append (args-type-required type)
		  (args-type-optional type))
	  (cond ((args-type-keyp type) *universal-type*)
		((args-type-rest type))
		(t
                  ;; MNA: fix-instance-typep-call patch
                  default-type))))

;;; Return a list of OPERATION applied to the types in TYPES1 and
;;; TYPES2, padding with REST2 as needed. TYPES1 must not be shorter
;;; than TYPES2. The second value is T if OPERATION always returned a
;;; true second value.
(defun fixed-values-op (types1 types2 rest2 operation)
  (declare (list types1 types2) (type ctype rest2) (type function operation))
  (let ((exact t))
    (values (mapcar #'(lambda (t1 t2)
			(multiple-value-bind (res win)
			    (funcall operation t1 t2)
			  (unless win
			    (setq exact nil))
			  res))
		    types1
		    (append types2
			    (make-list (- (length types1) (length types2))
				       :initial-element rest2)))
	    exact)))

;;; If Type isn't a values type, then make it into one:
;;;    <type>  ==>  (values type &rest t)
(defun coerce-to-values (type)
  (declare (type ctype type))
  (if (values-type-p type)
      type
      (make-values-type :required (list type) :rest *universal-type*)))

;;; Do the specified OPERATION on TYPE1 and TYPE2, which may be any
;;; type, including VALUES types. With VALUES types such as:
;;;    (VALUES a0 a1)
;;;    (VALUES b0 b1)
;;; we compute the more useful result
;;;    (VALUES (<operation> a0 b0) (<operation> a1 b1))
;;; rather than the precise result
;;;    (<operation> (values a0 a1) (values b0 b1))
;;; This has the virtue of always keeping the VALUES type specifier
;;; outermost, and retains all of the information that is really
;;; useful for static type analysis. We want to know what is always
;;; true of each value independently. It is worthless to know that IF
;;; the first value is B0 then the second will be B1.
;;;
;;; If the VALUES count signatures differ, then we produce a result with
;;; the required VALUE count chosen by NREQ when applied to the number
;;; of required values in TYPE1 and TYPE2. Any &KEY values become
;;; &REST T (anyone who uses keyword values deserves to lose.)
;;;
;;; The second value is true if the result is definitely empty or if
;;; OPERATION returned true as its second value each time we called
;;; it. Since we approximate the intersection of VALUES types, the
;;; second value being true doesn't mean the result is exact.
;;; MNA: fix-instance-typep-call patch
(defun args-type-op (type1 type2 operation nreq default-type)
  ;;; MNA: fix-instance-typep-call patch
  (declare (type ctype type1 type2 default-type)
 	   (type function operation nreq))
  (if (or (values-type-p type1) (values-type-p type2))
      (let ((type1 (coerce-to-values type1))
	    (type2 (coerce-to-values type2)))
	(multiple-value-bind (types1 rest1)
            ;;; MNA: fix-instance-typep-call patch
            (values-type-types type1 default-type)
	  (multiple-value-bind (types2 rest2)
              ;;; MNA: fix-instance-typep-call patch
              (values-type-types type2 default-type)
	    (multiple-value-bind (rest rest-exact)
		(funcall operation rest1 rest2)
	      (multiple-value-bind (res res-exact)
		  (if (< (length types1) (length types2))
		      (fixed-values-op types2 types1 rest1 operation)
		      (fixed-values-op types1 types2 rest2 operation))
		(let* ((req (funcall nreq
				     (length (args-type-required type1))
				     (length (args-type-required type2))))
		       (required (subseq res 0 req))
		       (opt (subseq res req))
		       (opt-last (position rest opt :test-not #'type=
					   :from-end t)))
		  (if (find *empty-type* required :test #'type=)
		      (values *empty-type* t)
		      (values (make-values-type
			       :required required
			       :optional (if opt-last
					     (subseq opt 0 (1+ opt-last))
					     ())
                               ;; MNA fix-instance-typep-call patch
			       :rest (if (eq rest default-type) nil rest))
			      (and rest-exact res-exact)))))))))
      (funcall operation type1 type2)))

;;; Do a union or intersection operation on types that might be values
;;; types. The result is optimized for utility rather than exactness,
;;; but it is guaranteed that it will be no smaller (more restrictive)
;;; than the precise result.
;;;
;;; The return convention seems to be analogous to
;;; TYPES-INTERSECT. -- WHN 19990910.
(defun-cached (values-type-union :hash-function type-cache-hash
				 :hash-bits 8
				 :default nil
				 :init-wrapper !cold-init-forms)
	      ((type1 eq) (type2 eq))
  (declare (type ctype type1 type2))
  (cond ((or (eq type1 *wild-type*) (eq type2 *wild-type*)) *wild-type*)
	((eq type1 *empty-type*) type2)
	((eq type2 *empty-type*) type1)
	(t
          ;;; MNA: fix-instance-typep-call patch
	 (values (args-type-op type1 type2 #'type-union #'min *empty-type*)))))
;;;
(defun-cached (values-type-intersection :hash-function type-cache-hash
					:hash-bits 8
					:values 2
					:default (values nil :empty)
					:init-wrapper !cold-init-forms)
	      ((type1 eq) (type2 eq))
  (declare (type ctype type1 type2))
  (cond ((eq type1 *wild-type*) (values type2 t))
	((eq type2 *wild-type*) (values type1 t))
	(t
	 (args-type-op type1 type2 #'type-intersection #'max (specifier-type 'null)))))

;;; This is like TYPES-INTERSECT, except that it sort of works on
;;; VALUES types. Note that due to the semantics of
;;; VALUES-TYPE-INTERSECTION, this might return (VALUES T T) when
;;; there isn't really any intersection (?).
;;;
;;; The return convention seems to be analogous to
;;; TYPES-INTERSECT. -- WHN 19990910.
(defun values-types-intersect (type1 type2)
  (cond ((or (eq type1 *empty-type*) (eq type2 *empty-type*))
	 (values 't t))
	((or (values-type-p type1) (values-type-p type2))
	 (multiple-value-bind (res win) (values-type-intersection type1 type2)
	   (values (not (eq res *empty-type*))
		   win)))
	(t
	 (types-intersect type1 type2))))

;;; a SUBTYPEP-like operation that can be used on any types, including
;;; VALUES types
(defun-cached (values-subtypep :hash-function type-cache-hash
			       :hash-bits 8
			       :values 2
			       :default (values nil :empty)
			       :init-wrapper !cold-init-forms)
	      ((type1 eq) (type2 eq))
  (declare (type ctype type1 type2))
  (cond ((eq type2 *wild-type*) (values t t))
	((eq type1 *wild-type*)
	 (values (eq type2 *universal-type*) t))
	((not (values-types-intersect type1 type2))
	 (values nil t))
	(t
	 (if (or (values-type-p type1) (values-type-p type2))
	     (let ((type1 (coerce-to-values type1))
		   (type2 (coerce-to-values type2)))
	       (multiple-value-bind (types1 rest1) (values-type-types type1)
		 (multiple-value-bind (types2 rest2) (values-type-types type2)
		   (cond ((< (length (values-type-required type1))
			     (length (values-type-required type2)))
			  (values nil t))
			 ((< (length types1) (length types2))
			  (values nil nil))
			 ((or (values-type-keyp type1)
			      (values-type-keyp type2))
			  (values nil nil))
			 (t
			  (do ((t1 types1 (rest t1))
			       (t2 types2 (rest t2)))
			      ((null t2)
			       (csubtypep rest1 rest2))
			    (multiple-value-bind (res win-p)
				(csubtypep (first t1) (first t2))
			      (unless win-p
				(return (values nil nil)))
			      (unless res
				(return (values nil t))))))))))
	     (csubtypep type1 type2)))))

;;;; type method interfaces

;;; like SUBTYPEP, only works on CTYPE structures
(defun-cached (csubtypep :hash-function type-cache-hash
			 :hash-bits 8
			 :values 2
			 :default (values nil :empty)
			 :init-wrapper !cold-init-forms)
	      ((type1 eq) (type2 eq))
  (declare (type ctype type1 type2))
  (cond ((or (eq type1 type2)
	     (eq type1 *empty-type*)
	     (eq type2 *wild-type*))
	 (values t t))
	((or (eq type1 *wild-type*)
	     (eq type2 *empty-type*))
	 (values nil t))
	(t
	 (invoke-type-method :simple-subtypep :complex-subtypep-arg2
			     type1 type2
			     :complex-arg1 :complex-subtypep-arg1))))

;;; Just parse the type specifiers and call CSUBTYPE.
(defun sb!xc:subtypep (type1 type2)
  #!+sb-doc
  "Return two values indicating the relationship between type1 and type2.
  If values are T and T, type1 definitely is a subtype of type2.
  If values are NIL and T, type1 definitely is not a subtype of type2.
  If values are NIL and NIL, it couldn't be determined."
  (csubtypep (specifier-type type1) (specifier-type type2)))

;;; If two types are definitely equivalent, return true. The second
;;; value indicates whether the first value is definitely correct.
;;; This should only fail in the presence of HAIRY types.
(defun-cached (type= :hash-function type-cache-hash
		     :hash-bits 8
		     :values 2
		     :default (values nil :empty)
		     :init-wrapper !cold-init-forms)
	      ((type1 eq) (type2 eq))
  (declare (type ctype type1 type2))
  (if (eq type1 type2)
      (values t t)
      (invoke-type-method :simple-= :complex-= type1 type2)))

;;; Not exactly the negation of TYPE=, since when the relationship is
;;; uncertain, we still return NIL, NIL. This is useful in cases where
;;; the conservative assumption is =.
(defun type/= (type1 type2)
  (declare (type ctype type1 type2))
  (multiple-value-bind (res win) (type= type1 type2)
    (if win
	(values (not res) t)
	(values nil nil))))

;;; Find a type which includes both types. Any inexactness is
;;; represented by the fuzzy element types; we return a single value
;;; that is precise to the best of our knowledge. This result is
;;; simplified into the canonical form, thus is not a UNION type
;;; unless there is no other way to represent the result.
(defun-cached (type-union :hash-function type-cache-hash
			  :hash-bits 8
			  :init-wrapper !cold-init-forms)
	      ((type1 eq) (type2 eq))
  (declare (type ctype type1 type2))
  (if (eq type1 type2)
      type1
      (let ((res (invoke-type-method :simple-union :complex-union
				     type1 type2
				     :default :vanilla)))
	(cond ((eq res :vanilla)
	       (or (vanilla-union type1 type2)
		   (make-union-type (list type1 type2))))
	      (res)
	      (t
	       (make-union-type (list type1 type2)))))))

;;; Return as restrictive a type as we can discover that is no more
;;; restrictive than the intersection of Type1 and Type2. The second
;;; value is true if the result is exact. At worst, we randomly return
;;; one of the arguments as the first value (trying not to return a
;;; hairy type).
(defun-cached (type-intersection :hash-function type-cache-hash
				 :hash-bits 8
				 :values 2
				 :default (values nil :empty)
				 :init-wrapper !cold-init-forms)
	      ((type1 eq) (type2 eq))
  (declare (type ctype type1 type2))
  (if (eq type1 type2)
      (values type1 t)
      (invoke-type-method :simple-intersection :complex-intersection
			  type1 type2
			  :default (values *empty-type* t))))

;;; The first value is true unless the types don't intersect. The
;;; second value is true if the first value is definitely correct. NIL
;;; is considered to intersect with any type. If T is a subtype of
;;; either type, then we also return T, T. This way we consider hairy
;;; types to intersect with T.
(defun types-intersect (type1 type2)
  (declare (type ctype type1 type2))
  (if (or (eq type1 *empty-type*) (eq type2 *empty-type*))
      (values t t)
      (multiple-value-bind (val winp) (type-intersection type1 type2)
	(cond ((not winp)
	       (if (or (csubtypep *universal-type* type1)
		       (csubtypep *universal-type* type2))
		   (values t t)
		   (values t nil)))
	      ((eq val *empty-type*) (values nil t))
	      (t (values t t))))))

;;; Return a Common Lisp type specifier corresponding to the TYPE
;;; object.
(defun type-specifier (type)
  (declare (type ctype type))
  (funcall (type-class-unparse (type-class-info type)) type))

;;; (VALUES-SPECIFIER-TYPE and SPECIFIER-TYPE moved from here to
;;; early-type.lisp by WHN ca. 19990201.)

;;; Take a list of type specifiers, compute the translation and define
;;; it as a builtin type.
(declaim (ftype (function (list) (values)) precompute-types))
(defun precompute-types (specs)
  (dolist (spec specs)
    (let ((res (specifier-type spec)))
      (unless (unknown-type-p res)
	(setf (info :type :builtin spec) res)
	(setf (info :type :kind spec) :primitive))))
  (values))

;;;; built-in types

(define-type-class named)

(defvar *wild-type*)
(defvar *empty-type*)
(defvar *universal-type*)

(!cold-init-forms
 (macrolet ((frob (name var)
	      `(progn
		 (setq ,var (make-named-type :name ',name))
		 (setf (info :type :kind ',name) :primitive)
		 (setf (info :type :builtin ',name) ,var))))
   ;; KLUDGE: In ANSI, * isn't really the name of a type, it's just a
   ;; special symbol which can be stuck in some places where an
   ;; ordinary type can go, e.g. (ARRAY * 1) instead of (ARRAY T 1).
   ;; At some point, in order to become more standard, we should
   ;; convert all the classic CMU CL legacy *s and *WILD-TYPE*s into
   ;; Ts and *UNIVERSAL-TYPE*s.
   (frob * *wild-type*)
   (frob nil *empty-type*)
   (frob t *universal-type*)))

(define-type-method (named :simple-=) (type1 type2)
  (values (eq type1 type2) t))

(define-type-method (named :simple-subtypep) (type1 type2)
  (values (or (eq type1 *empty-type*) (eq type2 *wild-type*)) t))

(define-type-method (named :complex-subtypep-arg1) (type1 type2)
  (assert (not (hairy-type-p type2)))
  (values (eq type1 *empty-type*) t))

(define-type-method (named :complex-subtypep-arg2) (type1 type2)
  (if (hairy-type-p type1)
      (values nil nil)
      (values (not (eq type2 *empty-type*)) t)))

(define-type-method (named :complex-intersection) (type1 type2)
  (vanilla-intersection type1 type2))

(define-type-method (named :unparse) (x)
  (named-type-name x))

;;;; hairy and unknown types

(define-type-method (hairy :unparse) (x) (hairy-type-specifier x))

(define-type-method (hairy :simple-subtypep) (type1 type2)
  (let ((hairy-spec1 (hairy-type-specifier type1))
	(hairy-spec2 (hairy-type-specifier type2)))
    (cond ((and (consp hairy-spec1) (eq (car hairy-spec1) 'not)
		(consp hairy-spec2) (eq (car hairy-spec2) 'not))
	   (csubtypep (specifier-type (cadr hairy-spec2))
		      (specifier-type (cadr hairy-spec1))))
	  ((equal hairy-spec1 hairy-spec2)
	   (values t t))
	  (t
	   (values nil nil)))))

(define-type-method (hairy :complex-subtypep-arg2) (type1 type2)
  (let ((hairy-spec (hairy-type-specifier type2)))
    (cond ((and (consp hairy-spec) (eq (car hairy-spec) 'not))
	   (multiple-value-bind (val win)
	       (type-intersection type1 (specifier-type (cadr hairy-spec)))
	     (if win
		 (values (eq val *empty-type*) t)
		 (values nil nil))))
	  (t
	   (values nil nil)))))

(define-type-method (hairy :complex-subtypep-arg1 :complex-=) (type1 type2)
  (declare (ignore type1 type2))
  (values nil nil))

(define-type-method (hairy :simple-intersection :complex-intersection)
		    (type1 type2)
  (declare (ignore type2))
  (values type1 nil))

(define-type-method (hairy :complex-union) (type1 type2)
  (make-union-type (list type1 type2)))

(define-type-method (hairy :simple-=) (type1 type2)
  (if (equal (hairy-type-specifier type1)
	     (hairy-type-specifier type2))
      (values t t)
      (values nil nil)))

(def-type-translator not (&whole whole type)
  (declare (ignore type))
  (make-hairy-type :specifier whole))

(def-type-translator satisfies (&whole whole fun)
  (declare (ignore fun))
  (make-hairy-type :specifier whole))

;;;; numeric types

;;; A list of all the float formats, in order of decreasing precision.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *float-formats*
    '(long-float double-float single-float short-float)))

;;; The type of a float format.
(deftype float-format () `(member ,@*float-formats*))

#!+negative-zero-is-not-zero
(defun make-numeric-type (&key class format (complexp :real) low high
			       enumerable)
  (flet ((canonicalise-low-bound (x)
	   ;; Canonicalise a low bound of (-0.0) to 0.0.
	   (if (and (consp x) (floatp (car x)) (zerop (car x))
		    (minusp (float-sign (car x))))
	       (float 0.0 (car x))
	       x))
	 (canonicalise-high-bound (x)
	   ;; Canonicalise a high bound of (+0.0) to -0.0.
	   (if (and (consp x) (floatp (car x)) (zerop (car x))
		    (plusp (float-sign (car x))))
	       (float -0.0 (car x))
	       x)))
    (%make-numeric-type :class class
			:format format
			:complexp complexp
			:low (canonicalise-low-bound low)
			:high (canonicalise-high-bound high)
			:enumerable enumerable)))

(define-type-class number)

(define-type-method (number :simple-=) (type1 type2)
  (values
   (and (eq (numeric-type-class type1) (numeric-type-class type2))
	(eq (numeric-type-format type1) (numeric-type-format type2))
	(eq (numeric-type-complexp type1) (numeric-type-complexp type2))
	(equal (numeric-type-low type1) (numeric-type-low type2))
	(equal (numeric-type-high type1) (numeric-type-high type2)))
   t))

(define-type-method (number :unparse) (type)
  (let* ((complexp (numeric-type-complexp type))
	 (low (numeric-type-low type))
	 (high (numeric-type-high type))
	 (base (case (numeric-type-class type)
		 (integer 'integer)
		 (rational 'rational)
		 (float (or (numeric-type-format type) 'float))
		 (t 'real))))
    (let ((base+bounds
	   (cond ((and (eq base 'integer) high low)
		  (let ((high-count (logcount high))
			(high-length (integer-length high)))
		    (cond ((= low 0)
			   (cond ((= high 0) '(integer 0 0))
				 ((= high 1) 'bit)
				 ((and (= high-count high-length)
				       (plusp high-length))
				  `(unsigned-byte ,high-length))
				 (t
				  `(mod ,(1+ high)))))
			  ((and (= low sb!vm:*target-most-negative-fixnum*)
				(= high sb!vm:*target-most-positive-fixnum*))
			   'fixnum)
			  ((and (= low (lognot high))
				(= high-count high-length)
				(> high-count 0))
			   `(signed-byte ,(1+ high-length)))
			  (t
			   `(integer ,low ,high)))))
		 (high `(,base ,(or low '*) ,high))
		 (low
		  (if (and (eq base 'integer) (= low 0))
		      'unsigned-byte
		      `(,base ,low)))
		 (t base))))
      (ecase complexp
	(:real
	 base+bounds)
	(:complex
	 (if (eq base+bounds 'real)
	     'complex
	     `(complex ,base+bounds)))
	((nil)
	 (assert (eq base+bounds 'real))
	 'number)))))

;;; Return true if X is "less than or equal" to Y, taking open bounds
;;; into consideration. CLOSED is the predicate used to test the bound
;;; on a closed interval (e.g. <=), and OPEN is the predicate used on
;;; open bounds (e.g. <). Y is considered to be the outside bound, in
;;; the sense that if it is infinite (NIL), then the test succeeds,
;;; whereas if X is infinite, then the test fails (unless Y is also
;;; infinite).
;;;
;;; This is for comparing bounds of the same kind, e.g. upper and
;;; upper. Use NUMERIC-BOUND-TEST* for different kinds of bounds.
#!-negative-zero-is-not-zero
(defmacro numeric-bound-test (x y closed open)
  `(cond ((not ,y) t)
	 ((not ,x) nil)
	 ((consp ,x)
	  (if (consp ,y)
	      (,closed (car ,x) (car ,y))
	      (,closed (car ,x) ,y)))
	 (t
	  (if (consp ,y)
	      (,open ,x (car ,y))
	      (,closed ,x ,y)))))

#!+negative-zero-is-not-zero
(defmacro numeric-bound-test-zero (op x y)
  `(if (and (zerop ,x) (zerop ,y) (floatp ,x) (floatp ,y))
       (,op (float-sign ,x) (float-sign ,y))
       (,op ,x ,y)))

#!+negative-zero-is-not-zero
(defmacro numeric-bound-test (x y closed open)
  `(cond ((not ,y) t)
	 ((not ,x) nil)
	 ((consp ,x)
	  (if (consp ,y)
	      (numeric-bound-test-zero ,closed (car ,x) (car ,y))
	      (numeric-bound-test-zero ,closed (car ,x) ,y)))
	 (t
	  (if (consp ,y)
	      (numeric-bound-test-zero ,open ,x (car ,y))
	      (numeric-bound-test-zero ,closed ,x ,y)))))

;;; This is used to compare upper and lower bounds. This is different
;;; from the same-bound case:
;;; -- Since X = NIL is -infinity, whereas y = NIL is +infinity, we
;;;    return true if *either* arg is NIL.
;;; -- an open inner bound is "greater" and also squeezes the interval,
;;;    causing us to use the OPEN test for those cases as well.
#!-negative-zero-is-not-zero
(defmacro numeric-bound-test* (x y closed open)
  `(cond ((not ,y) t)
	 ((not ,x) t)
	 ((consp ,x)
	  (if (consp ,y)
	      (,open (car ,x) (car ,y))
	      (,open (car ,x) ,y)))
	 (t
	  (if (consp ,y)
	      (,open ,x (car ,y))
	      (,closed ,x ,y)))))

#!+negative-zero-is-not-zero
(defmacro numeric-bound-test* (x y closed open)
  `(cond ((not ,y) t)
	 ((not ,x) t)
	 ((consp ,x)
	  (if (consp ,y)
	      (numeric-bound-test-zero ,open (car ,x) (car ,y))
	      (numeric-bound-test-zero ,open (car ,x) ,y)))
	 (t
	  (if (consp ,y)
	      (numeric-bound-test-zero ,open ,x (car ,y))
	      (numeric-bound-test-zero ,closed ,x ,y)))))

;;; Return whichever of the numeric bounds X and Y is "maximal"
;;; according to the predicates CLOSED (e.g. >=) and OPEN (e.g. >).
;;; This is only meaningful for maximizing like bounds, i.e. upper and
;;; upper. If MAX-P is true, then we return NIL if X or Y is NIL,
;;; otherwise we return the other arg.
(defmacro numeric-bound-max (x y closed open max-p)
  (once-only ((n-x x)
	      (n-y y))
    `(cond ((not ,n-x) ,(if max-p nil n-y))
	   ((not ,n-y) ,(if max-p nil n-x))
	   ((consp ,n-x)
	    (if (consp ,n-y)
		(if (,closed (car ,n-x) (car ,n-y)) ,n-x ,n-y)
		(if (,open (car ,n-x) ,n-y) ,n-x ,n-y)))
	   (t
	    (if (consp ,n-y)
		(if (,open (car ,n-y) ,n-x) ,n-y ,n-x)
		(if (,closed ,n-y ,n-x) ,n-y ,n-x))))))

(define-type-method (number :simple-subtypep) (type1 type2)
  (let ((class1 (numeric-type-class type1))
	(class2 (numeric-type-class type2))
	(complexp2 (numeric-type-complexp type2))
	(format2 (numeric-type-format type2))
	(low1 (numeric-type-low type1))
	(high1 (numeric-type-high type1))
	(low2 (numeric-type-low type2))
	(high2 (numeric-type-high type2)))
    ;; If one is complex and the other isn't, they are disjoint.
    (cond ((not (or (eq (numeric-type-complexp type1) complexp2)
		    (null complexp2)))
	   (values nil t))
	  ;; If the classes are specified and different, the types are
	  ;; disjoint unless type2 is rational and type1 is integer.
	  ((not (or (eq class1 class2)
		    (null class2)
		    (and (eq class1 'integer)
			 (eq class2 'rational))))
	   (values nil t))
	  ;; If the float formats are specified and different, the types
	  ;; are disjoint.
	  ((not (or (eq (numeric-type-format type1) format2)
		    (null format2)))
	   (values nil t))
	  ;; Check the bounds.
	  ((and (numeric-bound-test low1 low2 >= >)
		(numeric-bound-test high1 high2 <= <))
	   (values t t))
	  (t
	   (values nil t)))))

(define-superclasses number ((generic-number)) !cold-init-forms)

;;; If the high bound of LOW is adjacent to the low bound of HIGH,
;;; then return true, otherwise NIL.
(defun numeric-types-adjacent (low high)
  (let ((low-bound (numeric-type-high low))
	(high-bound (numeric-type-low high)))
    (cond ((not (and low-bound high-bound)) nil)
	  ((and (consp low-bound) (consp high-bound)) nil)
	  ((consp low-bound)
	   #!-negative-zero-is-not-zero
	   (let ((low-value (car low-bound)))
	     (or (eql low-value high-bound)
		 (and (eql low-value -0f0) (eql high-bound 0f0))
		 (and (eql low-value 0f0) (eql high-bound -0f0))
		 (and (eql low-value -0d0) (eql high-bound 0d0))
		 (and (eql low-value 0d0) (eql high-bound -0d0))))
	   #!+negative-zero-is-not-zero
	   (eql (car low-bound) high-bound))
	  ((consp high-bound)
	   #!-negative-zero-is-not-zero
	   (let ((high-value (car high-bound)))
	     (or (eql high-value low-bound)
		 (and (eql high-value -0f0) (eql low-bound 0f0))
		 (and (eql high-value 0f0) (eql low-bound -0f0))
		 (and (eql high-value -0d0) (eql low-bound 0d0))
		 (and (eql high-value 0d0) (eql low-bound -0d0))))
	   #!+negative-zero-is-not-zero
	   (eql (car high-bound) low-bound))
	  #!+negative-zero-is-not-zero
	  ((or (and (eql low-bound -0f0) (eql high-bound 0f0))
	       (and (eql low-bound -0d0) (eql high-bound 0d0))))
	  ((and (eq (numeric-type-class low) 'integer)
		(eq (numeric-type-class high) 'integer))
	   (eql (1+ low-bound) high-bound))
	  (t
	   nil))))

;;; Return a numeric type that is a supertype for both TYPE1 and TYPE2.
;;;
;;; ### Note: we give up early, so keep from dropping lots of information on
;;; the floor by returning overly general types.
(define-type-method (number :simple-union) (type1 type2)
  (declare (type numeric-type type1 type2))
  (cond ((csubtypep type1 type2) type2)
	((csubtypep type2 type1) type1)
	(t
	 (let ((class1 (numeric-type-class type1))
	       (format1 (numeric-type-format type1))
	       (complexp1 (numeric-type-complexp type1))
	       (class2 (numeric-type-class type2))
	       (format2 (numeric-type-format type2))
	       (complexp2 (numeric-type-complexp type2)))
	   (when (and (eq class1 class2)
		      (eq format1 format2)
		      (eq complexp1 complexp2)
		      (or (numeric-types-intersect type1 type2)
			  (numeric-types-adjacent type1 type2)
			  (numeric-types-adjacent type2 type1)))
	     (make-numeric-type
	      :class class1
	      :format format1
	      :complexp complexp1
	      :low (numeric-bound-max (numeric-type-low type1)
				      (numeric-type-low type2)
				      <= < t)
	      :high (numeric-bound-max (numeric-type-high type1)
				       (numeric-type-high type2)
				       >= > t)))))))

(!cold-init-forms
  (setf (info :type :kind 'number) :primitive)
  (setf (info :type :builtin 'number)
	(make-numeric-type :complexp nil)))

(def-type-translator complex (&optional (spec '*))
  (if (eq spec '*)
      (make-numeric-type :complexp :complex)
      (let ((type (specifier-type spec)))
	(unless (numeric-type-p type)
	  (error "Component type for Complex is not numeric: ~S." spec))
	(when (eq (numeric-type-complexp type) :complex)
	  (error "Component type for Complex is complex: ~S." spec))
	(let ((res (copy-numeric-type type)))
	  (setf (numeric-type-complexp res) :complex)
	  res))))

;;; If X is *, return NIL, otherwise return the bound, which must be a
;;; member of TYPE or a one-element list of a member of TYPE.
#!-sb-fluid (declaim (inline canonicalized-bound))
(defun canonicalized-bound (bound type)
  (cond ((eq bound '*) nil)
	((or (sb!xc:typep bound type)
	     (and (consp bound)
		  (sb!xc:typep (car bound) type)
		  (null (cdr bound))))
  	  bound)
	(t
	 (error "Bound is not ~S, a ~S or a list of a ~S: ~S"
		'*
		type
		type
		bound))))

(def-type-translator integer (&optional (low '*) (high '*))
  (let* ((l (canonicalized-bound low 'integer))
	 (lb (if (consp l) (1+ (car l)) l))
	 (h (canonicalized-bound high 'integer))
	 (hb (if (consp h) (1- (car h)) h)))
    (when (and hb lb (< hb lb))
      (error "Lower bound ~S is greater than upper bound ~S." l h))
    (make-numeric-type :class 'integer
		       :complexp :real
		       :enumerable (not (null (and l h)))
		       :low lb
		       :high hb)))

(defmacro def-bounded-type (type class format)
  `(def-type-translator ,type (&optional (low '*) (high '*))
     (let ((lb (canonicalized-bound low ',type))
	   (hb (canonicalized-bound high ',type)))
       (unless (numeric-bound-test* lb hb <= <)
	 (error "Lower bound ~S is not less than upper bound ~S." low high))
       (make-numeric-type :class ',class :format ',format :low lb :high hb))))

(def-bounded-type rational rational nil)
(def-bounded-type float float nil)
(def-bounded-type real nil nil)

(defmacro define-float-format (f)
  `(def-bounded-type ,f float ,f))

(define-float-format short-float)
(define-float-format single-float)
(define-float-format double-float)
(define-float-format long-float)

(defun numeric-types-intersect (type1 type2)
  (declare (type numeric-type type1 type2))
  (let* ((class1 (numeric-type-class type1))
	 (class2 (numeric-type-class type2))
	 (complexp1 (numeric-type-complexp type1))
	 (complexp2 (numeric-type-complexp type2))
	 (format1 (numeric-type-format type1))
	 (format2 (numeric-type-format type2))
	 (low1 (numeric-type-low type1))
	 (high1 (numeric-type-high type1))
	 (low2 (numeric-type-low type2))
	 (high2 (numeric-type-high type2)))
    ;; If one is complex and the other isn't, then they are disjoint.
    (cond ((not (or (eq complexp1 complexp2)
		    (null complexp1) (null complexp2)))
	   nil)
	  ;; If either type is a float, then the other must either be
	  ;; specified to be a float or unspecified. Otherwise, they
	  ;; are disjoint.
	  ((and (eq class1 'float)
		(not (member class2 '(float nil)))) nil)
	  ((and (eq class2 'float)
		(not (member class1 '(float nil)))) nil)
	  ;; If the float formats are specified and different, the
	  ;; types are disjoint.
	  ((not (or (eq format1 format2) (null format1) (null format2)))
	   nil)
	  (t
	   ;; Check the bounds. This is a bit odd because we must
	   ;; always have the outer bound of the interval as the
	   ;; second arg.
	   (if (numeric-bound-test high1 high2 <= <)
	       (or (and (numeric-bound-test low1 low2 >= >)
			(numeric-bound-test* low1 high2 <= <))
		   (and (numeric-bound-test low2 low1 >= >)
			(numeric-bound-test* low2 high1 <= <)))
	       (or (and (numeric-bound-test* low2 high1 <= <)
			(numeric-bound-test low2 low1 >= >))
		   (and (numeric-bound-test high2 high1 <= <)
			(numeric-bound-test* high2 low1 >= >))))))))

;;; Take the numeric bound X and convert it into something that can be
;;; used as a bound in a numeric type with the specified CLASS and
;;; FORMAT. If UP-P is true, then we round up as needed, otherwise we
;;; round down. UP-P true implies that X is a lower bound, i.e. (N) > N.
;;;
;;; This is used by NUMERIC-TYPE-INTERSECTION to mash the bound into
;;; the appropriate type number. X may only be a float when CLASS is
;;; FLOAT.
;;;
;;; ### Note: it is possible for the coercion to a float to overflow
;;; or underflow. This happens when the bound doesn't fit in the
;;; specified format. In this case, we should really return the
;;; appropriate {Most | Least}-{Positive | Negative}-XXX-Float float
;;; of desired format. But these conditions aren't currently signalled
;;; in any useful way.
;;;
;;; Also, when converting an open rational bound into a float we
;;; should probably convert it to a closed bound of the closest float
;;; in the specified format. KLUDGE: In general, open float bounds are
;;; screwed up. -- (comment from original CMU CL)
(defun round-numeric-bound (x class format up-p)
  (if x
      (let ((cx (if (consp x) (car x) x)))
	(ecase class
	  ((nil rational) x)
	  (integer
	   (if (and (consp x) (integerp cx))
	       (if up-p (1+ cx) (1- cx))
	       (if up-p (ceiling cx) (floor cx))))
	  (float
	   (let ((res (if format (coerce cx format) (float cx))))
	     (if (consp x) (list res) res)))))
      nil))

;;; Handle the case of TYPE-INTERSECTION on two numeric types. We use
;;; TYPES-INTERSECT to throw out the case of types with no
;;; intersection. If an attribute in TYPE1 is unspecified, then we use
;;; TYPE2's attribute, which must be at least as restrictive. If the
;;; types intersect, then the only attributes that can be specified
;;; and different are the class and the bounds.
;;;
;;; When the class differs, we use the more restrictive class. The
;;; only interesting case is RATIONAL/INTEGER, since RATIONAL includes
;;; INTEGER.
;;;
;;; We make the result lower (upper) bound the maximum (minimum) of
;;; the argument lower (upper) bounds. We convert the bounds into the
;;; appropriate numeric type before maximizing. This avoids possible
;;; confusion due to mixed-type comparisons (but I think the result is
;;; the same).
(define-type-method (number :simple-intersection) (type1 type2)
  (declare (type numeric-type type1 type2))
  (if (numeric-types-intersect type1 type2)
      (let* ((class1 (numeric-type-class type1))
	     (class2 (numeric-type-class type2))
	     (class (ecase class1
		      ((nil) class2)
		      ((integer float) class1)
		      (rational (if (eq class2 'integer)
				       'integer
				       'rational))))
	     (format (or (numeric-type-format type1)
			 (numeric-type-format type2))))
	(values
	 (make-numeric-type
	  :class class
	  :format format
	  :complexp (or (numeric-type-complexp type1)
			(numeric-type-complexp type2))
	  :low (numeric-bound-max
		(round-numeric-bound (numeric-type-low type1)
				     class format t)
		(round-numeric-bound (numeric-type-low type2)
				     class format t)
		> >= nil)
	  :high (numeric-bound-max
		 (round-numeric-bound (numeric-type-high type1)
				      class format nil)
		 (round-numeric-bound (numeric-type-high type2)
				      class format nil)
		 < <= nil))
	 t))
      (values *empty-type* t)))

;;; Given two float formats, return the one with more precision. If
;;; either one is null, return NIL.
(defun float-format-max (f1 f2)
  (when (and f1 f2)
    (dolist (f *float-formats* (error "bad float format: ~S" f1))
      (when (or (eq f f1) (eq f f2))
	(return f)))))

;;; Return the result of an operation on TYPE1 and TYPE2 according to
;;; the rules of numeric contagion. This is always NUMBER, some float
;;; format (possibly complex) or RATIONAL. Due to rational
;;; canonicalization, there isn't much we can do here with integers or
;;; rational complex numbers.
;;;
;;; If either argument is not a NUMERIC-TYPE, then return NUMBER. This
;;; is useful mainly for allowing types that are technically numbers,
;;; but not a NUMERIC-TYPE.
(defun numeric-contagion (type1 type2)
  (if (and (numeric-type-p type1) (numeric-type-p type2))
      (let ((class1 (numeric-type-class type1))
	    (class2 (numeric-type-class type2))
	    (format1 (numeric-type-format type1))
	    (format2 (numeric-type-format type2))
	    (complexp1 (numeric-type-complexp type1))
	    (complexp2 (numeric-type-complexp type2)))
	(cond ((or (null complexp1)
		   (null complexp2))
	       (specifier-type 'number))
	      ((eq class1 'float)
	       (make-numeric-type
		:class 'float
		:format (ecase class2
			  (float (float-format-max format1 format2))
			  ((integer rational) format1)
			  ((nil)
			   ;; A double-float with any real number is a
			   ;; double-float.
			   #!-long-float
			   (if (eq format1 'double-float)
			     'double-float
			     nil)
			   ;; A long-float with any real number is a
			   ;; long-float.
			   #!+long-float
			   (if (eq format1 'long-float)
			     'long-float
			     nil)))
		:complexp (if (or (eq complexp1 :complex)
				  (eq complexp2 :complex))
			      :complex
			      :real)))
	      ((eq class2 'float) (numeric-contagion type2 type1))
	      ((and (eq complexp1 :real) (eq complexp2 :real))
	       (make-numeric-type
		:class (and class1 class2 'rational)
		:complexp :real))
	      (t
	       (specifier-type 'number))))
      (specifier-type 'number)))

;;;; array types

(define-type-class array)

;;; What this does depends on the setting of the
;;; *USE-IMPLEMENTATION-TYPES* switch. If true, return the specialized
;;; element type, otherwise return the original element type.
(defun specialized-element-type-maybe (type)
  (declare (type array-type type))
  (if *use-implementation-types*
      (array-type-specialized-element-type type)
      (array-type-element-type type)))

(define-type-method (array :simple-=) (type1 type2)
  (values (and (equal (array-type-dimensions type1)
		      (array-type-dimensions type2))
	       (eq (array-type-complexp type1)
		   (array-type-complexp type2))
	       (type= (specialized-element-type-maybe type1)
		      (specialized-element-type-maybe type2)))
	  t))

(define-type-method (array :unparse) (type)
  (let ((dims (array-type-dimensions type))
	(eltype (type-specifier (array-type-element-type type)))
	(complexp (array-type-complexp type)))
    (cond ((eq dims '*)
	   (if (eq eltype '*)
	       (if complexp 'array 'simple-array)
	       (if complexp `(array ,eltype) `(simple-array ,eltype))))
	  ((= (length dims) 1)
	   (if complexp
	       (if (eq (car dims) '*)
		   (case eltype
		     (bit 'bit-vector)
		     (base-char 'base-string)
		     (character 'string)
		     (* 'vector)
		     (t `(vector ,eltype)))
		   (case eltype
		     (bit `(bit-vector ,(car dims)))
		     (base-char `(base-string ,(car dims)))
		     (character `(string ,(car dims)))
		     (t `(vector ,eltype ,(car dims)))))
	       (if (eq (car dims) '*)
		   (case eltype
		     (bit 'simple-bit-vector)
		     (base-char 'simple-base-string)
		     (character 'simple-string)
		     ((t) 'simple-vector)
		     (t `(simple-array ,eltype (*))))
		   (case eltype
		     (bit `(simple-bit-vector ,(car dims)))
		     (base-char `(simple-base-string ,(car dims)))
		     (character `(simple-string ,(car dims)))
		     ((t) `(simple-vector ,(car dims)))
		     (t `(simple-array ,eltype ,dims))))))
	  (t
	   (if complexp
	       `(array ,eltype ,dims)
	       `(simple-array ,eltype ,dims))))))

(define-type-method (array :simple-subtypep) (type1 type2)
  (let ((dims1 (array-type-dimensions type1))
	(dims2 (array-type-dimensions type2))
	(complexp2 (array-type-complexp type2)))
    ;; See whether dimensions are compatible.
    (cond ((not (or (eq dims2 '*)
		    (and (not (eq dims1 '*))
			 ;; (sbcl-0.6.4 has trouble figuring out that
			 ;; DIMS1 and DIMS2 must be lists at this
			 ;; point, and knowing that is important to
			 ;; compiling EVERY efficiently.)
			 (= (length (the list dims1))
			    (length (the list dims2)))
			 (every (lambda (x y)
				  (or (eq y '*) (eql x y)))
				(the list dims1)
				(the list dims2)))))
	   (values nil t))
	  ;; See whether complexpness is compatible.
	  ((not (or (eq complexp2 :maybe)
		    (eq (array-type-complexp type1) complexp2)))
	   (values nil t))
	  ;; If the TYPE2 eltype is wild, we win. Otherwise, the types
	  ;; must be identical.
	  ((or (eq (array-type-element-type type2) *wild-type*)
	       (type= (specialized-element-type-maybe type1)
		      (specialized-element-type-maybe type2)))
	   (values t t))
	  (t
	   (values nil t)))))

(define-superclasses array
  ((string string)
   (vector vector)
   (array))
  !cold-init-forms)

(defun array-types-intersect (type1 type2)
  (declare (type array-type type1 type2))
  (let ((dims1 (array-type-dimensions type1))
	(dims2 (array-type-dimensions type2))
	(complexp1 (array-type-complexp type1))
	(complexp2 (array-type-complexp type2)))
    ;; See whether dimensions are compatible.
    (cond ((not (or (eq dims1 '*) (eq dims2 '*)
		    (and (= (length dims1) (length dims2))
			 (every #'(lambda (x y)
				    (or (eq x '*) (eq y '*) (= x y)))
				dims1 dims2))))
	   (values nil t))
	  ;; See whether complexpness is compatible.
	  ((not (or (eq complexp1 :maybe)
		    (eq complexp2 :maybe)
		    (eq complexp1 complexp2)))
	   (values nil t))
	  ;; If either element type is wild, then they intersect.
	  ;; Otherwise, the types must be identical.
	  ((or (eq (array-type-element-type type1) *wild-type*)
	       (eq (array-type-element-type type2) *wild-type*)
	       (type= (specialized-element-type-maybe type1)
		      (specialized-element-type-maybe type2)))

	   (values t t))
	  (t
	   (values nil t)))))

(define-type-method (array :simple-intersection) (type1 type2)
  (declare (type array-type type1 type2))
  (if (array-types-intersect type1 type2)
      (let ((dims1 (array-type-dimensions type1))
	    (dims2 (array-type-dimensions type2))
	    (complexp1 (array-type-complexp type1))
	    (complexp2 (array-type-complexp type2))
	    (eltype1 (array-type-element-type type1))
	    (eltype2 (array-type-element-type type2)))
	(values
	 (specialize-array-type
	  (make-array-type
	   :dimensions (cond ((eq dims1 '*) dims2)
			     ((eq dims2 '*) dims1)
			     (t
			      (mapcar (lambda (x y) (if (eq x '*) y x))
				      dims1 dims2)))
	   :complexp (if (eq complexp1 :maybe) complexp2 complexp1)
	   :element-type (if (eq eltype1 *wild-type*) eltype2 eltype1)))
	 t))
      (values *empty-type* t)))

;;; Check a supplied dimension list to determine whether it is legal,
;;; and return it in canonical form (as either '* or a list).
(defun canonical-array-dimensions (dims)
  (typecase dims
    ((member *) dims)
    (integer
     (when (minusp dims)
       (error "Arrays can't have a negative number of dimensions: ~S" dims))
     (when (>= dims sb!xc:array-rank-limit)
       (error "array type with too many dimensions: ~S" dims))
     (make-list dims :initial-element '*))
    (list
     (when (>= (length dims) sb!xc:array-rank-limit)
       (error "array type with too many dimensions: ~S" dims))
     (dolist (dim dims)
       (unless (eq dim '*)
	 (unless (and (integerp dim)
		      (>= dim 0)
		      (< dim sb!xc:array-dimension-limit))
	   (error "bad dimension in array type: ~S" dim))))
     dims)
    (t
     (error "Array dimensions is not a list, integer or *:~%  ~S" dims))))

;;;; MEMBER types

(define-type-class member)

(define-type-method (member :unparse) (type)
  (let ((members (member-type-members type)))
    (if (equal members '(nil))
	'null
	`(member ,@members))))

(define-type-method (member :simple-subtypep) (type1 type2)
  (values (subsetp (member-type-members type1) (member-type-members type2))
	  t))

(define-type-method (member :complex-subtypep-arg1) (type1 type2)
  (block PUNT
    (values (every-type-op ctypep type2 (member-type-members type1)
			   :list-first t)
	    t)))

;;; We punt if the odd type is enumerable and intersects with the
;;; MEMBER type. If not enumerable, then it is definitely not a
;;; subtype of the MEMBER type.
(define-type-method (member :complex-subtypep-arg2) (type1 type2)
  (cond ((not (type-enumerable type1)) (values nil t))
	((types-intersect type1 type2) (values nil nil))
	(t
	 (values nil t))))

(define-type-method (member :simple-intersection) (type1 type2)
  (let ((mem1 (member-type-members type1))
	(mem2 (member-type-members type2)))
    (values (cond ((subsetp mem1 mem2) type1)
		  ((subsetp mem2 mem1) type2)
		  (t
		   (let ((res (intersection mem1 mem2)))
		     (if res
			 (make-member-type :members res)
			 *empty-type*))))
	    t)))

(define-type-method (member :complex-intersection) (type1 type2)
  (block PUNT
    (collect ((members))
      (let ((mem2 (member-type-members type2)))
	(dolist (member mem2)
	  (multiple-value-bind (val win) (ctypep member type1)
	    (unless win
	      (return-from PUNT (values type2 nil)))
	    (when val (members member))))

	(values (cond ((subsetp mem2 (members)) type2)
		      ((null (members)) *empty-type*)
		      (t
		       (make-member-type :members (members))))
		t)))))

;;; We don't need a :COMPLEX-UNION, since the only interesting case is a union
;;; type, and the member/union interaction is handled by the union type
;;; method.
(define-type-method (member :simple-union) (type1 type2)
  (let ((mem1 (member-type-members type1))
	(mem2 (member-type-members type2)))
    (cond ((subsetp mem1 mem2) type2)
	  ((subsetp mem2 mem1) type1)
	  (t
	   (make-member-type :members (union mem1 mem2))))))

(define-type-method (member :simple-=) (type1 type2)
  (let ((mem1 (member-type-members type1))
	(mem2 (member-type-members type2)))
    (values (and (subsetp mem1 mem2) (subsetp mem2 mem1))
	    t)))

(define-type-method (member :complex-=) (type1 type2)
  (if (type-enumerable type1)
      (multiple-value-bind (val win) (csubtypep type2 type1)
	(if (or val (not win))
	    (values nil nil)
	    (values nil t)))
      (values nil t)))

(def-type-translator member (&rest members)
  (if members
    (make-member-type :members (remove-duplicates members))
    *empty-type*))

;;;; union types

;;; Make a union type from the specifier types, setting ENUMERABLE in
;;; the result if all are enumerable.
(defun make-union-type (types)
  (declare (list types))
  (%make-union-type (every #'type-enumerable types) types))

(define-type-class union)

;;; If LIST, then return that, otherwise the OR of the component types.
(define-type-method (union :unparse) (type)
  (declare (type ctype type))
  (if (type= type (specifier-type 'list))
      'list
      `(or ,@(mapcar #'type-specifier (union-type-types type)))))

;;; Two union types are equal if every type in one is equal to some
;;; type in the other.
(define-type-method (union :simple-=) (type1 type2)
  (block PUNT
    (let ((types1 (union-type-types type1))
	  (types2 (union-type-types type2)))
      (values (and (dolist (type1 types1 t)
		     (unless (any-type-op type= type1 types2)
		       (return nil)))
		   (dolist (type2 types2 t)
		     (unless (any-type-op type= type2 types1)
		       (return nil))))
	      t))))

;;; Similarly, a union type is a subtype of another if every element
;;; of TYPE1 is a subtype of some element of TYPE2.
(define-type-method (union :simple-subtypep) (type1 type2)
  (block PUNT
    (let ((types2 (union-type-types type2)))
      (values (dolist (type1 (union-type-types type1) t)
		(unless (any-type-op csubtypep type1 types2)
		  (return nil)))
	      t))))

(define-type-method (union :complex-subtypep-arg1) (type1 type2)
  (block PUNT
    (values (every-type-op csubtypep type2 (union-type-types type1)
			   :list-first t)
	    t)))

(define-type-method (union :complex-subtypep-arg2) (type1 type2)
  (block PUNT
    (values (any-type-op csubtypep type1 (union-type-types type2)) t)))

(define-type-method (union :complex-union) (type1 type2)
  (let* ((class1 (type-class-info type1)))
    (collect ((res))
      (let ((this-type type1))
	(dolist (type (union-type-types type2)
		      (if (res)
			  (make-union-type (cons this-type (res)))
			  this-type))
	  (cond ((eq (type-class-info type) class1)
		 (let ((union (funcall (type-class-simple-union class1)
				       this-type type)))
		   (if union
		       (setq this-type union)
		       (res type))))
		((csubtypep type this-type))
		((csubtypep type1 type) (return type2))
		(t
		 (res type))))))))

;;; For the union of union types, we let the :COMPLEX-UNION method do
;;; the work.
(define-type-method (union :simple-union) (type1 type2)
  (let ((res type1))
    (dolist (t2 (union-type-types type2) res)
      (setq res (type-union res t2)))))

(define-type-method (union :simple-intersection :complex-intersection)
		    (type1 type2)
  (let ((res *empty-type*)
	(win t))
    (dolist (type (union-type-types type2) (values res win))
      (multiple-value-bind (int w) (type-intersection type1 type)
	(setq res (type-union res int))
	(unless w (setq win nil))))))

(def-type-translator or (&rest types)
  (reduce #'type-union
	  (mapcar #'specifier-type types)
	  :initial-value *empty-type*))

;;; We don't actually have intersection types, since the result of
;;; reasonable type intersections is always describable as a union of
;;; simple types. If something is too hairy to fit this mold, then we
;;; make a hairy type.
(def-type-translator and (&whole spec &rest types)
  (let ((res *wild-type*))
    (dolist (type types res)
      (let ((ctype (specifier-type type)))
	(multiple-value-bind (int win) (type-intersection res ctype)
	  (unless win
	    (return (make-hairy-type :specifier spec)))
	  (setq res int))))))

;;; Return the type that describes all objects that are in X but not
;;; in Y. If we can't determine this type, then return NIL.
;;;
;;; For now, we only are clever dealing with union and member types.
;;; If either type is not a union type, then we pretend that it is a
;;; union of just one type. What we do is remove from X all the types
;;; that are a subtype any type in Y. If any type in X intersects with
;;; a type in Y but is not a subtype, then we give up.
;;;
;;; We must also special-case any member type that appears in the
;;; union. We remove from X's members all objects that are TYPEP to Y.
;;; If Y has any members, we must be careful that none of those
;;; members are CTYPEP to any of Y's non-member types. We give up in
;;; this case, since to compute that difference we would have to break
;;; the type from X into some collection of types that represents the
;;; type without that particular element. This seems too hairy to be
;;; worthwhile, given its low utility.
(defun type-difference (x y)
  (let ((x-types (if (union-type-p x) (union-type-types x) (list x)))
	(y-types (if (union-type-p y) (union-type-types y) (list y))))
    (collect ((res))
      (dolist (x-type x-types)
	(if (member-type-p x-type)
	    (collect ((members))
	      (dolist (mem (member-type-members x-type))
		(multiple-value-bind (val win) (ctypep mem y)
		  (unless win (return-from type-difference nil))
		  (unless val
		    (members mem))))
	      (when (members)
		(res (make-member-type :members (members)))))
	    (dolist (y-type y-types (res x-type))
	      (multiple-value-bind (val win) (csubtypep x-type y-type)
		(unless win (return-from type-difference nil))
		(when val (return))
		(when (types-intersect x-type y-type)
		  (return-from type-difference nil))))))

      (let ((y-mem (find-if #'member-type-p y-types)))
	(when y-mem
	  (let ((members (member-type-members y-mem)))
	    (dolist (x-type x-types)
	      (unless (member-type-p x-type)
		(dolist (member members)
		  (multiple-value-bind (val win) (ctypep member x-type)
		    (when (or (not win) val)
		      (return-from type-difference nil)))))))))

      (cond ((null (res)) *empty-type*)
	    ((null (rest (res))) (first (res)))
	    (t
	     (make-union-type (res)))))))

(def-type-translator array (&optional (element-type '*)
				      (dimensions '*))
  (specialize-array-type
   (make-array-type :dimensions (canonical-array-dimensions dimensions)
		    :element-type (specifier-type element-type))))

(def-type-translator simple-array (&optional (element-type '*)
					     (dimensions '*))
  (specialize-array-type
   (make-array-type :dimensions (canonical-array-dimensions dimensions)
		    :element-type (specifier-type element-type)
		    :complexp nil)))

(!defun-from-collected-cold-init-forms !late-type-cold-init)

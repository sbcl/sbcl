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

(/show0 "late-type.lisp 19")

(!begin-collecting-cold-init-forms)

;;; ### Remaining incorrectnesses:
;;;
;;; There are all sorts of nasty problems with open bounds on FLOAT
;;; types (and probably FLOAT types in general.)

;;; This condition is signalled whenever we make a UNKNOWN-TYPE so that
;;; compiler warnings can be emitted as appropriate.
(define-condition parse-unknown-type (condition)
  ((specifier :reader parse-unknown-type-specifier :initarg :specifier)))

;;; FIXME: This really should go away. Alas, it doesn't seem to be so
;;; simple to make it go away.. (See bug 123 in BUGS file.)
(defvar *use-implementation-types* t ; actually initialized in cold init
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
(defun delegate-complex-intersection2 (type1 type2)
  (let ((method (type-class-complex-intersection2 (type-class-info type1))))
    (if (and method (not (eq method #'delegate-complex-intersection2)))
	(funcall method type2 type1)
	(hierarchical-intersection2 type1 type2))))

;;; This is used by !DEFINE-SUPERCLASSES to define the SUBTYPE-ARG1
;;; method. INFO is a list of conses
;;;   (SUPERCLASS-CLASS . {GUARD-TYPE-SPECIFIER | NIL}).
(defun !has-superclasses-complex-subtypep-arg1 (type1 type2 info)
  ;; If TYPE2 might be concealing something related to our class
  ;; hierarchy
  (if (type-might-contain-other-types-p type2)
      ;; too confusing, gotta punt
      (values nil nil)
      ;; ordinary case expected by old CMU CL code, where the taxonomy
      ;; of TYPE2's representation accurately reflects the taxonomy of
      ;; the underlying set
      (values
       ;; FIXME: This old CMU CL code probably deserves a comment
       ;; explaining to us mere mortals how it works...
       (and (sb!xc:typep type2 'classoid)
	    (dolist (x info nil)
	      (when (or (not (cdr x))
			(csubtypep type1 (specifier-type (cdr x))))
		(return
		 (or (eq type2 (car x))
		     (let ((inherits (layout-inherits
				      (classoid-layout (car x)))))
		       (dotimes (i (length inherits) nil)
			 (when (eq type2 (layout-classoid (svref inherits i)))
			   (return t)))))))))
       t)))

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
(defmacro !define-superclasses (type-class-name specs when)
  (with-unique-names (type-class info)
    `(,when
       (let ((,type-class (type-class-or-lose ',type-class-name))
	     (,info (mapcar (lambda (spec)
			      (destructuring-bind
				  (super &optional guard)
				  spec
				(cons (find-classoid super) guard)))
			    ',specs)))
	 (setf (type-class-complex-subtypep-arg1 ,type-class)
	       (lambda (type1 type2)
		 (!has-superclasses-complex-subtypep-arg1 type1 type2 ,info)))
	 (setf (type-class-complex-subtypep-arg2 ,type-class)
	       #'delegate-complex-subtypep-arg2)
	 (setf (type-class-complex-intersection2 ,type-class)
	       #'delegate-complex-intersection2)))))

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

;;; the description of a &KEY argument
(defstruct (key-info #-sb-xc-host (:pure t)
		     (:copier nil))
  ;; the key (not necessarily a keyword in ANSI Common Lisp)
  (name (missing-arg) :type symbol)
  ;; the type of the argument value
  (type (missing-arg) :type ctype))

(!define-type-method (values :simple-subtypep :complex-subtypep-arg1)
		     (type1 type2)
  (declare (ignore type2))
  ;; FIXME: should be TYPE-ERROR, here and in next method
  (error "SUBTYPEP is illegal on this type:~%  ~S" (type-specifier type1)))

(!define-type-method (values :complex-subtypep-arg2)
		     (type1 type2)
  (declare (ignore type1))
  (error "SUBTYPEP is illegal on this type:~%  ~S" (type-specifier type2)))

(!define-type-method (values :unparse) (type)
  (cons 'values
        (let ((unparsed (unparse-args-types type)))
          (if (or (values-type-optional type)
                  (values-type-rest type)
                  (values-type-allowp type))
              unparsed
              (nconc unparsed '(&optional))))))

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

(!define-type-method (values :simple-=) (type1 type2)
  (let ((rest1 (args-type-rest type1))
	(rest2 (args-type-rest type2)))
    (cond ((and rest1 rest2 (type/= rest1 rest2))
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

(!define-type-class function)

;;; a flag that we can bind to cause complex function types to be
;;; unparsed as FUNCTION. This is useful when we want a type that we
;;; can pass to TYPEP.
(defvar *unparse-fun-type-simplify*)
(!cold-init-forms (setq *unparse-fun-type-simplify* nil))

(!define-type-method (function :unparse) (type)
  (if *unparse-fun-type-simplify*
      'function
      (list 'function
	    (if (fun-type-wild-args type)
		'*
		(unparse-args-types type))
	    (type-specifier
	     (fun-type-returns type)))))

;;; The meaning of this is a little confused. On the one hand, all
;;; function objects are represented the same way regardless of the
;;; arglists and return values, and apps don't get to ask things like
;;; (TYPEP #'FOO (FUNCTION (FIXNUM) *)) in any meaningful way. On the
;;; other hand, Python wants to reason about function types. So...
(!define-type-method (function :simple-subtypep) (type1 type2)
 (flet ((fun-type-simple-p (type)
          (not (or (fun-type-rest type)
                   (fun-type-keyp type))))
        (every-csubtypep (types1 types2)
          (loop
             for a1 in types1
             for a2 in types2
             do (multiple-value-bind (res sure-p)
                    (csubtypep a1 a2)
                  (unless res (return (values res sure-p))))
             finally (return (values t t)))))
   (and/type (values-subtypep (fun-type-returns type1)
                              (fun-type-returns type2))
             (cond ((fun-type-wild-args type2) (values t t))
                   ((fun-type-wild-args type1)
                    (cond ((fun-type-keyp type2) (values nil nil))
                          ((not (fun-type-rest type2)) (values nil t))
                          ((not (null (fun-type-required type2)))
			   (values nil t))
                          (t (and/type (type= *universal-type*
					      (fun-type-rest type2))
                                       (every/type #'type=
						   *universal-type*
                                                   (fun-type-optional
						    type2))))))
                   ((not (and (fun-type-simple-p type1)
                              (fun-type-simple-p type2)))
                    (values nil nil))
                   (t (multiple-value-bind (min1 max1) (fun-type-nargs type1)
                        (multiple-value-bind (min2 max2) (fun-type-nargs type2)
                          (cond ((or (> max1 max2) (< min1 min2))
                                 (values nil t))
                                ((and (= min1 min2) (= max1 max2))
                                 (and/type (every-csubtypep
					    (fun-type-required type1)
					    (fun-type-required type2))
                                           (every-csubtypep
					    (fun-type-optional type1)
					    (fun-type-optional type2))))
                                (t (every-csubtypep
                                    (concatenate 'list
                                                 (fun-type-required type1)
                                                 (fun-type-optional type1))
                                    (concatenate 'list
                                                 (fun-type-required type2)
                                                 (fun-type-optional type2))))))))))))

(!define-superclasses function ((function)) !cold-init-forms)

;;; The union or intersection of two FUNCTION types is FUNCTION.
(!define-type-method (function :simple-union2) (type1 type2)
  (declare (ignore type1 type2))
  (specifier-type 'function))
(!define-type-method (function :simple-intersection2) (type1 type2)
  (declare (ignore type1 type2))
  (specifier-type 'function))

;;; The union or intersection of a subclass of FUNCTION with a
;;; FUNCTION type is somewhat complicated.
(!define-type-method (function :complex-intersection2) (type1 type2)
  (cond
    ((type= type1 (specifier-type 'function)) type2)
    ((csubtypep type1 (specifier-type 'function)) nil)
    (t :call-other-method)))
(!define-type-method (function :complex-union2) (type1 type2)
  (cond
    ((type= type1 (specifier-type 'function)) type1)
    (t nil)))

(!define-type-method (function :simple-=) (type1 type2)
  (macrolet ((compare (comparator field)
               (let ((reader (symbolicate '#:fun-type- field)))
                 `(,comparator (,reader type1) (,reader type2)))))
    (and/type (compare type= returns)
              (cond ((neq (fun-type-wild-args type1) (fun-type-wild-args type2))
                     (values nil t))
                    ((eq (fun-type-wild-args type1) t)
                     (values t t))
                    (t (and/type
                        (cond ((null (fun-type-rest type1))
                               (values (null (fun-type-rest type2)) t))
                              ((null (fun-type-rest type2))
                               (values nil t))
                              (t
                               (compare type= rest)))
                        (labels ((type-list-= (l1 l2)
                                   (cond ((null l1)
                                          (values (null l2) t))
                                         ((null l2)
                                          (values nil t))
                                         (t (multiple-value-bind (res winp)
                                                (type= (first l1) (first l2))
                                              (cond ((not winp)
                                                     (values nil nil))
                                                    ((not res)
                                                     (values nil t))
                                                    (t
                                                     (type-list-= (rest l1)
                                                                  (rest l2)))))))))
                          (and/type (and/type (compare type-list-= required)
                                              (compare type-list-= optional))
                              (if (or (fun-type-keyp type1) (fun-type-keyp type2))
                                  (values nil nil)
                                  (values t t))))))))))

(!define-type-class constant :inherits values)

(!define-type-method (constant :unparse) (type)
  `(constant-arg ,(type-specifier (constant-type-type type))))

(!define-type-method (constant :simple-=) (type1 type2)
  (type= (constant-type-type type1) (constant-type-type type2)))

(!def-type-translator constant-arg (type)
  (make-constant-type :type (single-value-specifier-type type)))

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

(!def-type-translator function (&optional (args '*) (result '*))
  (make-fun-type :args args
                 :returns (coerce-to-values (values-specifier-type result))))

(!def-type-translator values (&rest values)
  (make-values-type :args values))

;;;; VALUES types interfaces
;;;;
;;;; We provide a few special operations that can be meaningfully used
;;;; on VALUES types (as well as on any other type).

(defun type-single-value-p (type)
  (and (values-type-p type)
       (not (values-type-rest type))
       (null (values-type-optional type))
       (singleton-p (values-type-required type))))

;;; Return the type of the first value indicated by TYPE. This is used
;;; by people who don't want to have to deal with VALUES types.
#!-sb-fluid (declaim (freeze-type values-type))
; (inline single-value-type))
(defun single-value-type (type)
  (declare (type ctype type))
  (cond ((eq type *wild-type*)
         *universal-type*)
        ((eq type *empty-type*)
         *empty-type*)
        ((not (values-type-p type))
         type)
        (t (or (car (args-type-required type))
               (car (args-type-optional type))
               (args-type-rest type)
               (specifier-type 'null)))))

;;; Return the minimum number of arguments that a function can be
;;; called with, and the maximum number or NIL. If not a function
;;; type, return NIL, NIL.
(defun fun-type-nargs (type)
  (declare (type ctype type))
  (if (and (fun-type-p type) (not (fun-type-wild-args type)))
      (let ((fixed (length (args-type-required type))))
	(if (or (args-type-rest type)
		(args-type-keyp type)
		(args-type-allowp type))
	    (values fixed nil)
	    (values fixed (+ fixed (length (args-type-optional type))))))
      (values nil nil)))

;;; Determine whether TYPE corresponds to a definite number of values.
;;; The first value is a list of the types for each value, and the
;;; second value is the number of values. If the number of values is
;;; not fixed, then return NIL and :UNKNOWN.
(defun values-types (type)
  (declare (type ctype type))
  (cond ((or (eq type *wild-type*) (eq type *empty-type*))
	 (values nil :unknown))
	((or (args-type-optional type)
	     (args-type-rest type))
	 (values nil :unknown))
	(t
	 (let ((req (args-type-required type)))
	   (values req (length req))))))

;;; Return two values:
;;; 1. A list of all the positional (fixed and optional) types.
;;; 2. The &REST type (if any). If no &REST, then the DEFAULT-TYPE.
(defun values-type-types (type &optional (default-type *empty-type*))
  (declare (type ctype type))
  (if (eq type *wild-type*)
      (values nil *universal-type*)
      (values (append (args-type-required type)
                      (args-type-optional type))
              (cond ((args-type-rest type))
                    (t default-type)))))

;;; If COUNT values are supplied, which types should they have?
(defun values-type-start (type count)
  (declare (type ctype type) (type unsigned-byte count))
  (if (eq type *wild-type*)
      (make-list count :initial-element *universal-type*)
      (collect ((res))
        (flet ((process-types (types)
                 (loop for type in types
                       while (plusp count)
                       do (decf count)
                       do (res type))))
          (process-types (values-type-required type))
          (process-types (values-type-optional type))
          (when (plusp count)
            (loop with rest = (the ctype (values-type-rest type))
                  repeat count
                  do (res rest))))
        (res))))

;;; Return a list of OPERATION applied to the types in TYPES1 and
;;; TYPES2, padding with REST2 as needed. TYPES1 must not be shorter
;;; than TYPES2. The second value is T if OPERATION always returned a
;;; true second value.
(defun fixed-values-op (types1 types2 rest2 operation)
  (declare (list types1 types2) (type ctype rest2) (type function operation))
  (let ((exact t))
    (values (mapcar (lambda (t1 t2)
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

;;; If TYPE isn't a values type, then make it into one.
(defun-cached (%coerce-to-values
               :hash-bits 8
               :hash-function (lambda (type)
                                (logand (type-hash-value type)
                                        #xff)))
    ((type eq))
  (cond ((multiple-value-bind (res sure)
             (csubtypep (specifier-type 'null) type)
           (and (not res) sure))
         ;; FIXME: What should we do with (NOT SURE)?
         (make-values-type :required (list type) :rest *universal-type*))
        (t
         (make-values-type :optional (list type) :rest *universal-type*))))

(defun coerce-to-values (type)
  (declare (type ctype type))
  (cond ((or (eq type *universal-type*)
             (eq type *wild-type*))
         *wild-type*)
        ((values-type-p type)
         type)
        (t (%coerce-to-values type))))

;;; Return type, corresponding to ANSI short form of VALUES type
;;; specifier.
(defun make-short-values-type (types)
  (declare (list types))
  (let ((last-required (position-if
                        (lambda (type)
                          (not/type (csubtypep (specifier-type 'null) type)))
                        types
                        :from-end t)))
    (if last-required
        (make-values-type :required (subseq types 0 (1+ last-required))
                          :optional (subseq types (1+ last-required))
                          :rest *universal-type*)
        (make-values-type :optional types :rest *universal-type*))))

(defun make-single-value-type (type)
  (make-values-type :required (list type)))

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
;;; true of each value independently. It is worthless to know that if
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
(defun args-type-op (type1 type2 operation nreq)
  (declare (type ctype type1 type2)
 	   (type function operation nreq))
  (when (eq type1 type2)
    (values type1 t))
  (multiple-value-bind (types1 rest1)
      (values-type-types type1)
    (multiple-value-bind (types2 rest2)
        (values-type-types type2)
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
                 (opt (subseq res req)))
            (values (make-values-type
                     :required required
                     :optional opt
                     :rest rest)
                    (and rest-exact res-exact))))))))

;;; Do a union or intersection operation on types that might be values
;;; types. The result is optimized for utility rather than exactness,
;;; but it is guaranteed that it will be no smaller (more restrictive)
;;; than the precise result.
;;;
;;; The return convention seems to be analogous to
;;; TYPES-EQUAL-OR-INTERSECT. -- WHN 19990910.
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
         (values (args-type-op type1 type2 #'type-union #'min)))))

(defun-cached (values-type-intersection :hash-function type-cache-hash
					:hash-bits 8
					:values 2
					:default (values nil :empty)
					:init-wrapper !cold-init-forms)
    ((type1 eq) (type2 eq))
  (declare (type ctype type1 type2))
  (cond ((eq type1 *wild-type*) (values (coerce-to-values type2) t))
        ((or (eq type2 *wild-type*) (eq type2 *universal-type*))
         (values type1 t))
        ((or (eq type1 *empty-type*) (eq type2 *empty-type*))
         *empty-type*)
        ((and (not (values-type-p type2))
              (values-type-required type1))
         (let ((req1 (values-type-required type1)))
         (make-values-type :required (cons (type-intersection (first req1) type2)
                                           (rest req1))
                           :optional (values-type-optional type1)
                           :rest (values-type-rest type1)
                           :allowp (values-type-allowp type1))))
        (t
         (args-type-op type1 (coerce-to-values type2)
                       #'type-intersection
                       #'max))))

;;; This is like TYPES-EQUAL-OR-INTERSECT, except that it sort of
;;; works on VALUES types. Note that due to the semantics of
;;; VALUES-TYPE-INTERSECTION, this might return (VALUES T T) when
;;; there isn't really any intersection.
(defun values-types-equal-or-intersect (type1 type2)
  (cond ((or (eq type1 *empty-type*) (eq type2 *empty-type*))
	 (values t t))
        ((or (eq type1 *wild-type*) (eq type2 *wild-type*))
         (values t t))
	(t
	 (multiple-value-bind (res win) (values-type-intersection type1 type2)
	   (values (not (eq res *empty-type*))
		   win)))))

;;; a SUBTYPEP-like operation that can be used on any types, including
;;; VALUES types
(defun-cached (values-subtypep :hash-function type-cache-hash
			       :hash-bits 8
			       :values 2
			       :default (values nil :empty)
			       :init-wrapper !cold-init-forms)
    ((type1 eq) (type2 eq))
  (declare (type ctype type1 type2))
  (cond ((or (eq type2 *wild-type*) (eq type2 *universal-type*)
             (eq type1 *empty-type*))
         (values t t))
        ((eq type1 *wild-type*)
         (values (eq type2 *wild-type*) t))
        ((or (eq type2 *empty-type*)
             (not (values-types-equal-or-intersect type1 type2)))
         (values nil t))
        ((and (not (values-type-p type2))
              (values-type-required type1))
         (csubtypep (first (values-type-required type1))
                    type2))
        (t (setq type2 (coerce-to-values type2))
           (multiple-value-bind (types1 rest1) (values-type-types type1)
             (multiple-value-bind (types2 rest2) (values-type-types type2)
               (cond ((< (length (values-type-required type1))
                         (length (values-type-required type2)))
                      (values nil t))
                     ((< (length types1) (length types2))
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
                            (return (values nil t))))))))))))

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
	     (eq type2 *universal-type*))
	 (values t t))
        #+nil
	((eq type1 *universal-type*)
	 (values nil t))
	(t
	 (!invoke-type-method :simple-subtypep :complex-subtypep-arg2
			      type1 type2
			      :complex-arg1 :complex-subtypep-arg1))))

;;; Just parse the type specifiers and call CSUBTYPE.
(defun sb!xc:subtypep (type1 type2 &optional environment)
  #!+sb-doc
  "Return two values indicating the relationship between type1 and type2.
  If values are T and T, type1 definitely is a subtype of type2.
  If values are NIL and T, type1 definitely is not a subtype of type2.
  If values are NIL and NIL, it couldn't be determined."
  (declare (ignore environment))
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
      (!invoke-type-method :simple-= :complex-= type1 type2)))

;;; Not exactly the negation of TYPE=, since when the relationship is
;;; uncertain, we still return NIL, NIL. This is useful in cases where
;;; the conservative assumption is =.
(defun type/= (type1 type2)
  (declare (type ctype type1 type2))
  (multiple-value-bind (res win) (type= type1 type2)
    (if win
	(values (not res) t)
	(values nil nil))))

;;; the type method dispatch case of TYPE-UNION2
(defun %type-union2 (type1 type2)
  ;; As in %TYPE-INTERSECTION2, it seems to be a good idea to give
  ;; both argument orders a chance at COMPLEX-INTERSECTION2. Unlike
  ;; %TYPE-INTERSECTION2, though, I don't have a specific case which
  ;; demonstrates this is actually necessary. Also unlike
  ;; %TYPE-INTERSECTION2, there seems to be no need to distinguish
  ;; between not finding a method and having a method return NIL.
  (flet ((1way (x y)
	   (!invoke-type-method :simple-union2 :complex-union2
				x y
				:default nil)))
    (declare (inline 1way))
    (or (1way type1 type2)
	(1way type2 type1))))

;;; Find a type which includes both types. Any inexactness is
;;; represented by the fuzzy element types; we return a single value
;;; that is precise to the best of our knowledge. This result is
;;; simplified into the canonical form, thus is not a UNION-TYPE
;;; unless we find no other way to represent the result.
(defun-cached (type-union2 :hash-function type-cache-hash
			   :hash-bits 8
			   :init-wrapper !cold-init-forms)
	      ((type1 eq) (type2 eq))
  ;; KLUDGE: This was generated from TYPE-INTERSECTION2 by Ye Olde Cut And
  ;; Paste technique of programming. If it stays around (as opposed to
  ;; e.g. fading away in favor of some CLOS solution) the shared logic
  ;; should probably become shared code. -- WHN 2001-03-16
  (declare (type ctype type1 type2))
  (cond ((eq type1 type2)
	 type1)
	((csubtypep type1 type2) type2)
	((csubtypep type2 type1) type1)
	((or (union-type-p type1)
	     (union-type-p type2))
	 ;; Unions of UNION-TYPE should have the UNION-TYPE-TYPES
	 ;; values broken out and united separately. The full TYPE-UNION
	 ;; function knows how to do this, so let it handle it.
	 (type-union type1 type2))
	(t
	 ;; the ordinary case: we dispatch to type methods
	 (%type-union2 type1 type2))))

;;; the type method dispatch case of TYPE-INTERSECTION2
(defun %type-intersection2 (type1 type2)
  ;; We want to give both argument orders a chance at
  ;; COMPLEX-INTERSECTION2. Without that, the old CMU CL type
  ;; methods could give noncommutative results, e.g.
  ;;   (TYPE-INTERSECTION2 *EMPTY-TYPE* SOME-HAIRY-TYPE)
  ;;     => NIL, NIL
  ;;   (TYPE-INTERSECTION2 SOME-HAIRY-TYPE *EMPTY-TYPE*)
  ;;     => #<NAMED-TYPE NIL>, T
  ;; We also need to distinguish between the case where we found a
  ;; type method, and it returned NIL, and the case where we fell
  ;; through without finding any type method. An example of the first
  ;; case is the intersection of a HAIRY-TYPE with some ordinary type.
  ;; An example of the second case is the intersection of two
  ;; completely-unrelated types, e.g. CONS and NUMBER, or SYMBOL and
  ;; ARRAY.
  ;;
  ;; (Why yes, CLOS probably *would* be nicer..)
  (flet ((1way (x y)
	   (!invoke-type-method :simple-intersection2 :complex-intersection2
				x y
				:default :call-other-method)))
    (declare (inline 1way))
    (let ((xy (1way type1 type2)))
      (or (and (not (eql xy :call-other-method)) xy)
	  (let ((yx (1way type2 type1)))
	    (or (and (not (eql yx :call-other-method)) yx)
		(cond ((and (eql xy :call-other-method)
			    (eql yx :call-other-method))
		       *empty-type*)
		      (t
		       (aver (and (not xy) (not yx))) ; else handled above
		       nil))))))))

(defun-cached (type-intersection2 :hash-function type-cache-hash
				  :hash-bits 8
				  :values 1
				  :default nil
				  :init-wrapper !cold-init-forms)
	      ((type1 eq) (type2 eq))
  (declare (type ctype type1 type2))
  (cond ((eq type1 type2)
	 ;; FIXME: For some reason, this doesn't catch e.g. type1 =
	 ;; type2 = (SPECIFIER-TYPE
	 ;; 'SOME-UNKNOWN-TYPE). Investigate. - CSR, 2002-04-10
	 type1)
	((or (intersection-type-p type1)
	     (intersection-type-p type2))
	 ;; Intersections of INTERSECTION-TYPE should have the
	 ;; INTERSECTION-TYPE-TYPES values broken out and intersected
	 ;; separately. The full TYPE-INTERSECTION function knows how
	 ;; to do that, so let it handle it.
	 (type-intersection type1 type2))
	(t
	 ;; the ordinary case: we dispatch to type methods
	 (%type-intersection2 type1 type2))))

;;; Return as restrictive and simple a type as we can discover that is
;;; no more restrictive than the intersection of TYPE1 and TYPE2. At
;;; worst, we arbitrarily return one of the arguments as the first
;;; value (trying not to return a hairy type).
(defun type-approx-intersection2 (type1 type2)
  (cond ((type-intersection2 type1 type2))
	((hairy-type-p type1) type2)
	(t type1)))

;;; a test useful for checking whether a derived type matches a
;;; declared type
;;;
;;; The first value is true unless the types don't intersect and
;;; aren't equal. The second value is true if the first value is
;;; definitely correct. NIL is considered to intersect with any type.
;;; If T is a subtype of either type, then we also return T, T. This
;;; way we recognize that hairy types might intersect with T.
(defun types-equal-or-intersect (type1 type2)
  (declare (type ctype type1 type2))
  (if (or (eq type1 *empty-type*) (eq type2 *empty-type*))
      (values t t)
      (let ((intersection2 (type-intersection2 type1 type2)))
	(cond ((not intersection2)
	       (if (or (csubtypep *universal-type* type1)
		       (csubtypep *universal-type* type2))
		   (values t t)
		   (values t nil)))
	      ((eq intersection2 *empty-type*) (values nil t))
	      (t (values t t))))))

;;; Return a Common Lisp type specifier corresponding to the TYPE
;;; object.
(defun type-specifier (type)
  (declare (type ctype type))
  (funcall (type-class-unparse (type-class-info type)) type))

;;; (VALUES-SPECIFIER-TYPE and SPECIFIER-TYPE moved from here to
;;; early-type.lisp by WHN ca. 19990201.)

;;; Take a list of type specifiers, computing the translation of each
;;; specifier and defining it as a builtin type.
(declaim (ftype (function (list) (values)) precompute-types))
(defun precompute-types (specs)
  (dolist (spec specs)
    (let ((res (specifier-type spec)))
      (unless (unknown-type-p res)
	(setf (info :type :builtin spec) res)
	;; KLUDGE: the three copies of this idiom in this file (and
	;; the one in class.lisp as at sbcl-0.7.4.1x) should be
	;; coalesced, or perhaps the error-detecting code that
	;; disallows redefinition of :PRIMITIVE types should be
	;; rewritten to use *TYPE-SYSTEM-FINALIZED* (rather than
	;; *TYPE-SYSTEM-INITIALIZED*). The effect of this is not to
	;; cause redefinition errors when precompute-types is called
	;; for a second time while building the target compiler using
	;; the cross-compiler. -- CSR, trying to explain why this
	;; isn't completely wrong, 2002-06-07
	(setf (info :type :kind spec) #+sb-xc-host :defined #-sb-xc-host :primitive))))
  (values))

;;;; general TYPE-UNION and TYPE-INTERSECTION operations
;;;;
;;;; These are fully general operations on CTYPEs: they'll always
;;;; return a CTYPE representing the result.

;;; shared logic for unions and intersections: Return a vector of
;;; types representing the same types as INPUT-TYPES, but with
;;; COMPOUND-TYPEs satisfying %COMPOUND-TYPE-P broken up into their
;;; component types, and with any SIMPLY2 simplifications applied.
(declaim (inline simplified-compound-types))
(defun simplified-compound-types (input-types %compound-type-p simplify2)
  (declare (function %compound-type-p simplify2))
  (let ((types (make-array (length input-types)
                           :fill-pointer 0
                           :adjustable t
                           :element-type 'ctype)))
    (labels ((accumulate-compound-type (type)
               (if (funcall %compound-type-p type)
                   (dolist (type (compound-type-types type))
                     (accumulate1-compound-type type))
                   (accumulate1-compound-type type)))
             (accumulate1-compound-type (type)
               (declare (type ctype type))
               ;; Any input object satisfying %COMPOUND-TYPE-P should've been
               ;; broken into components before it reached us.
               (aver (not (funcall %compound-type-p type)))
               (dotimes (i (length types) (vector-push-extend type types))
                 (let ((simplified2 (funcall simplify2 type (aref types i))))
                   (when simplified2
                     ;; Discard the old (AREF TYPES I).
                     (setf (aref types i) (vector-pop types))
                     ;; Merge the new SIMPLIFIED2 into TYPES, by tail recursing.
                     ;; (Note that the tail recursion is indirect: we go through
                     ;; ACCUMULATE, not ACCUMULATE1, so that if SIMPLIFIED2 is
                     ;; handled properly if it satisfies %COMPOUND-TYPE-P.)
                     (return (accumulate-compound-type simplified2)))))))
      (dolist (input-type input-types)
        (accumulate-compound-type input-type)))
    types))

;;; shared logic for unions and intersections: Make a COMPOUND-TYPE
;;; object whose components are the types in TYPES, or skip to special
;;; cases when TYPES is short.
(defun make-probably-compound-type (constructor types enumerable identity)
  (declare (type function constructor))
  (declare (type (vector ctype) types))
  (declare (type ctype identity))
  (case (length types)
    (0 identity)
    (1 (aref types 0))
    (t (funcall constructor
		enumerable
		;; FIXME: This should be just (COERCE TYPES 'LIST), but as
		;; of sbcl-0.6.11.17 the COERCE optimizer is really
		;; brain-dead, so that would generate a full call to
		;; SPECIFIER-TYPE at runtime, so we get into bootstrap
		;; problems in cold init because 'LIST is a compound
		;; type, so we need to MAKE-PROBABLY-COMPOUND-TYPE
		;; before we know what 'LIST is. Once the COERCE
		;; optimizer is less brain-dead, we can make this
		;; (COERCE TYPES 'LIST) again.
		#+sb-xc-host (coerce types 'list)
		#-sb-xc-host (coerce-to-list types)))))

(defun maybe-distribute-one-union (union-type types)
  (let* ((intersection (apply #'type-intersection types))
	 (union (mapcar (lambda (x) (type-intersection x intersection))
			(union-type-types union-type))))
    (if (notany (lambda (x) (or (hairy-type-p x)
				(intersection-type-p x)))
		union)
	union
	nil)))

(defun type-intersection (&rest input-types)
  (%type-intersection input-types))
(defun-cached (%type-intersection :hash-bits 8
                                  :hash-function (lambda (x)
                                                   (logand (sxhash x) #xff)))
    ((input-types equal))
  (let ((simplified-types (simplified-compound-types input-types
						     #'intersection-type-p
						     #'type-intersection2)))
    (declare (type (vector ctype) simplified-types))
    ;; We want to have a canonical representation of types (or failing
    ;; that, punt to HAIRY-TYPE). Canonical representation would have
    ;; intersections inside unions but not vice versa, since you can
    ;; always achieve that by the distributive rule. But we don't want
    ;; to just apply the distributive rule, since it would be too easy
    ;; to end up with unreasonably huge type expressions. So instead
    ;; we try to generate a simple type by distributing the union; if
    ;; the type can't be made simple, we punt to HAIRY-TYPE.
    (if (and (> (length simplified-types) 1)
	     (some #'union-type-p simplified-types))
	(let* ((first-union (find-if #'union-type-p simplified-types))
	       (other-types (coerce (remove first-union simplified-types)
				    'list))
	       (distributed (maybe-distribute-one-union first-union
							other-types)))
	  (if distributed
	      (apply #'type-union distributed)
	      (make-hairy-type
	       :specifier `(and ,@(map 'list
				       #'type-specifier
				       simplified-types)))))
	(make-probably-compound-type #'%make-intersection-type
				     simplified-types
				     (some #'type-enumerable
					   simplified-types)
				     *universal-type*))))

(defun type-union (&rest input-types)
  (%type-union input-types))
(defun-cached (%type-union :hash-bits 8
                           :hash-function (lambda (x)
                                            (logand (sxhash x) #xff)))
    ((input-types equal))
  (let ((simplified-types (simplified-compound-types input-types
						     #'union-type-p
						     #'type-union2)))
    (make-probably-compound-type #'make-union-type
				 simplified-types
				 (every #'type-enumerable simplified-types)
				 *empty-type*)))

;;;; built-in types

(!define-type-class named)

(defvar *wild-type*)
(defvar *empty-type*)
(defvar *universal-type*)
(defvar *universal-fun-type*)

(!cold-init-forms
 (macrolet ((frob (name var)
	      `(progn
                 (setq ,var (make-named-type :name ',name))
		 (setf (info :type :kind ',name)
		       #+sb-xc-host :defined #-sb-xc-host :primitive)
		 (setf (info :type :builtin ',name) ,var))))
   ;; KLUDGE: In ANSI, * isn't really the name of a type, it's just a
   ;; special symbol which can be stuck in some places where an
   ;; ordinary type can go, e.g. (ARRAY * 1) instead of (ARRAY T 1).
   ;; In SBCL it also used to denote universal VALUES type.
   (frob * *wild-type*)
   (frob nil *empty-type*)
   (frob t *universal-type*))
 (setf *universal-fun-type*
       (make-fun-type :wild-args t
		      :returns *wild-type*)))

(!define-type-method (named :simple-=) (type1 type2)
  ;;(aver (not (eq type1 *wild-type*))) ; * isn't really a type.
  (values (eq type1 type2) t))

(!define-type-method (named :complex-=) (type1 type2)
  (cond
    ((and (eq type2 *empty-type*)
	  (intersection-type-p type1)
	  ;; not allowed to be unsure on these... FIXME: keep the list
	  ;; of CL types that are intersection types once and only
	  ;; once.
	  (not (or (type= type1 (specifier-type 'ratio))
		   (type= type1 (specifier-type 'keyword)))))
     ;; things like (AND (EQL 0) (SATISFIES ODDP)) or (AND FUNCTION
     ;; STREAM) can get here.  In general, we can't really tell
     ;; whether these are equal to NIL or not, so
     (values nil nil))
    ((type-might-contain-other-types-p type1)
     (invoke-complex-=-other-method type1 type2))
    (t (values nil t))))

(!define-type-method (named :simple-subtypep) (type1 type2)
  (aver (not (eq type1 *wild-type*))) ; * isn't really a type.
  (values (or (eq type1 *empty-type*) (eq type2 *wild-type*)) t))

(!define-type-method (named :complex-subtypep-arg1) (type1 type2)
  ;; This AVER causes problems if we write accurate methods for the
  ;; union (and possibly intersection) types which then delegate to
  ;; us; while a user shouldn't get here, because of the odd status of
  ;; *wild-type* a type-intersection executed by the compiler can. -
  ;; CSR, 2002-04-10
  ;;
  ;; (aver (not (eq type1 *wild-type*))) ; * isn't really a type.
  (cond ((eq type1 *empty-type*)
	 t)
	(;; When TYPE2 might be the universal type in disguise
	 (type-might-contain-other-types-p type2)
	 ;; Now that the UNION and HAIRY COMPLEX-SUBTYPEP-ARG2 methods
	 ;; can delegate to us (more or less as CALL-NEXT-METHOD) when
	 ;; they're uncertain, we can't just barf on COMPOUND-TYPE and
	 ;; HAIRY-TYPEs as we used to. Instead we deal with the
	 ;; problem (where at least part of the problem is cases like
	 ;;   (SUBTYPEP T '(SATISFIES FOO))
	 ;; or
	 ;;   (SUBTYPEP T '(AND (SATISFIES FOO) (SATISFIES BAR)))
	 ;; where the second type is a hairy type like SATISFIES, or
	 ;; is a compound type which might contain a hairy type) by
	 ;; returning uncertainty.
	 (values nil nil))
	(t
	 ;; By elimination, TYPE1 is the universal type.
	 (aver (eq type1 *universal-type*))
	 ;; This case would have been picked off by the SIMPLE-SUBTYPEP
	 ;; method, and so shouldn't appear here.
	 (aver (not (eq type2 *universal-type*)))
	 ;; Since TYPE2 is not EQ *UNIVERSAL-TYPE* and is not the
	 ;; universal type in disguise, TYPE2 is not a superset of TYPE1.
	 (values nil t))))

(!define-type-method (named :complex-subtypep-arg2) (type1 type2)
  (aver (not (eq type2 *wild-type*))) ; * isn't really a type.
  (cond ((eq type2 *universal-type*)
	 (values t t))
	((type-might-contain-other-types-p type1)
	 ;; those types can be *EMPTY-TYPE* or *UNIVERSAL-TYPE* in
	 ;; disguise.  So we'd better delegate.
	 (invoke-complex-subtypep-arg1-method type1 type2))
	(t
	 ;; FIXME: This seems to rely on there only being 2 or 3
	 ;; NAMED-TYPE values, and the exclusion of various
	 ;; possibilities above. It would be good to explain it and/or
	 ;; rewrite it so that it's clearer.
	 (values (not (eq type2 *empty-type*)) t))))

(!define-type-method (named :complex-intersection2) (type1 type2)
  ;; FIXME: This assertion failed when I added it in sbcl-0.6.11.13.
  ;; Perhaps when bug 85 is fixed it can be reenabled.
  ;;(aver (not (eq type2 *wild-type*))) ; * isn't really a type.
  (hierarchical-intersection2 type1 type2))

(!define-type-method (named :complex-union2) (type1 type2)
  ;; Perhaps when bug 85 is fixed this can be reenabled.
  ;;(aver (not (eq type2 *wild-type*))) ; * isn't really a type.
  (hierarchical-union2 type1 type2))

(!define-type-method (named :unparse) (x)
  (named-type-name x))

;;;; hairy and unknown types

(!define-type-method (hairy :unparse) (x)
  (hairy-type-specifier x))

(!define-type-method (hairy :simple-subtypep) (type1 type2)
  (let ((hairy-spec1 (hairy-type-specifier type1))
	(hairy-spec2 (hairy-type-specifier type2)))
    (cond ((equal-but-no-car-recursion hairy-spec1 hairy-spec2)
	   (values t t))
	  (t
	   (values nil nil)))))

(!define-type-method (hairy :complex-subtypep-arg2) (type1 type2)
  (invoke-complex-subtypep-arg1-method type1 type2))

(!define-type-method (hairy :complex-subtypep-arg1) (type1 type2)
  (declare (ignore type1 type2))
  (values nil nil))

(!define-type-method (hairy :complex-=) (type1 type2)
  (if (unknown-type-p type2)
      (let ((type2 (specifier-type (unknown-type-specifier type2))))
        (if (unknown-type-p type2)
            (values nil nil)
            (type= type1 type2)))
  (values nil nil)))

(!define-type-method (hairy :simple-intersection2 :complex-intersection2) 
		     (type1 type2)
  (if (type= type1 type2)
      type1
      nil))

(!define-type-method (hairy :simple-union2) 
		     (type1 type2)
  (if (type= type1 type2)
      type1
      nil))

(!define-type-method (hairy :simple-=) (type1 type2)
  (if (equal-but-no-car-recursion (hairy-type-specifier type1)
				  (hairy-type-specifier type2))
      (values t t)
      (values nil nil)))

(!def-type-translator satisfies (&whole whole fun)
  (declare (ignore fun))
  ;; Check legality of arguments.
  (destructuring-bind (satisfies predicate-name) whole
    (declare (ignore satisfies))
    (unless (symbolp predicate-name)
      (error 'simple-type-error
	     :datum predicate-name
	     :expected-type 'symbol
	     :format-control "The SATISFIES predicate name is not a symbol: ~S"
	     :format-arguments (list predicate-name))))
  ;; Create object.
  (make-hairy-type :specifier whole))

;;;; negation types

(!define-type-method (negation :unparse) (x)
  `(not ,(type-specifier (negation-type-type x))))

(!define-type-method (negation :simple-subtypep) (type1 type2)
  (csubtypep (negation-type-type type2) (negation-type-type type1)))

(!define-type-method (negation :complex-subtypep-arg2) (type1 type2)
  (let* ((complement-type2 (negation-type-type type2))
	 (intersection2 (type-intersection2 type1
					    complement-type2)))
    (if intersection2
	;; FIXME: if uncertain, maybe try arg1?
	(type= intersection2 *empty-type*)
	(invoke-complex-subtypep-arg1-method type1 type2))))

(!define-type-method (negation :complex-subtypep-arg1) (type1 type2)
  ;; "Incrementally extended heuristic algorithms tend inexorably toward the
  ;; incomprehensible." -- http://www.unlambda.com/~james/lambda/lambda.txt
  ;;
  ;; You may not believe this. I couldn't either. But then I sat down
  ;; and drew lots of Venn diagrams. Comments involving a and b refer
  ;; to the call (subtypep '(not a) 'b) -- CSR, 2002-02-27.
  (block nil
    ;; (Several logical truths in this block are true as long as
    ;; b/=T. As of sbcl-0.7.1.28, it seems impossible to construct a
    ;; case with b=T where we actually reach this type method, but
    ;; we'll test for and exclude this case anyway, since future
    ;; maintenance might make it possible for it to end up in this
    ;; code.)
    (multiple-value-bind (equal certain)
	(type= type2 *universal-type*)
      (unless certain
	(return (values nil nil)))
      (when equal
	(return (values t t))))
    (let ((complement-type1 (negation-type-type type1)))
      ;; Do the special cases first, in order to give us a chance if
      ;; subtype/supertype relationships are hairy.
      (multiple-value-bind (equal certain)
	  (type= complement-type1 type2)
	;; If a = b, ~a is not a subtype of b (unless b=T, which was
	;; excluded above).
	(unless certain
	  (return (values nil nil)))
	(when equal
	  (return (values nil t))))
      ;; KLUDGE: ANSI requires that the SUBTYPEP result between any
      ;; two built-in atomic type specifiers never be uncertain. This
      ;; is hard to do cleanly for the built-in types whose
      ;; definitions include (NOT FOO), i.e. CONS and RATIO. However,
      ;; we can do it with this hack, which uses our global knowledge
      ;; that our implementation of the type system uses disjoint
      ;; implementation types to represent disjoint sets (except when
      ;; types are contained in other types).  (This is a KLUDGE
      ;; because it's fragile. Various changes in internal
      ;; representation in the type system could make it start
      ;; confidently returning incorrect results.) -- WHN 2002-03-08
      (unless (or (type-might-contain-other-types-p complement-type1)
		  (type-might-contain-other-types-p type2))
	;; Because of the way our types which don't contain other
	;; types are disjoint subsets of the space of possible values,
	;; (SUBTYPEP '(NOT AA) 'B)=NIL when AA and B are simple (and B
	;; is not T, as checked above).
	(return (values nil t)))
      ;; The old (TYPE= TYPE1 TYPE2) branch would never be taken, as
      ;; TYPE1 and TYPE2 will only be equal if they're both NOT types,
      ;; and then the :SIMPLE-SUBTYPEP method would be used instead.
      ;; But a CSUBTYPEP relationship might still hold:
      (multiple-value-bind (equal certain)
	  (csubtypep complement-type1 type2)
	;; If a is a subtype of b, ~a is not a subtype of b (unless
	;; b=T, which was excluded above).
	(unless certain
	  (return (values nil nil)))
	(when equal
	  (return (values nil t))))
      (multiple-value-bind (equal certain)
	  (csubtypep type2 complement-type1)
	;; If b is a subtype of a, ~a is not a subtype of b.  (FIXME:
	;; That's not true if a=T. Do we know at this point that a is
	;; not T?)
	(unless certain
	  (return (values nil nil)))
	(when equal
	  (return (values nil t))))
      ;; old CSR comment ca. 0.7.2, now obsoleted by the SIMPLE-CTYPE?
      ;; KLUDGE case above: Other cases here would rely on being able
      ;; to catch all possible cases, which the fragility of this type
      ;; system doesn't inspire me; for instance, if a is type= to ~b,
      ;; then we want T, T; if this is not the case and the types are
      ;; disjoint (have an intersection of *empty-type*) then we want
      ;; NIL, T; else if the union of a and b is the *universal-type*
      ;; then we want T, T. So currently we still claim to be unsure
      ;; about e.g. (subtypep '(not fixnum) 'single-float).
      ;;
      ;; OTOH we might still get here:
      (values nil nil))))

(!define-type-method (negation :complex-=) (type1 type2)
  ;; (NOT FOO) isn't equivalent to anything that's not a negation
  ;; type, except possibly a type that might contain it in disguise.
  (declare (ignore type2))
  (if (type-might-contain-other-types-p type1)
      (values nil nil)
      (values nil t)))

(!define-type-method (negation :simple-intersection2) (type1 type2)
  (let ((not1 (negation-type-type type1))
	(not2 (negation-type-type type2)))
    (cond
      ((csubtypep not1 not2) type2)
      ((csubtypep not2 not1) type1)
      ;; Why no analagous clause to the disjoint in the SIMPLE-UNION2
      ;; method, below?  The clause would read
      ;;
      ;; ((EQ (TYPE-UNION NOT1 NOT2) *UNIVERSAL-TYPE*) *EMPTY-TYPE*)
      ;;
      ;; but with proper canonicalization of negation types, there's
      ;; no way of constructing two negation types with union of their
      ;; negations being the universal type.
      (t
       (aver (not (eq (type-union not1 not2) *universal-type*)))
       nil))))

(!define-type-method (negation :complex-intersection2) (type1 type2)
  (cond
    ((csubtypep type1 (negation-type-type type2)) *empty-type*)
    ((eq (type-intersection type1 (negation-type-type type2)) *empty-type*)
     type1)
    (t nil)))

(!define-type-method (negation :simple-union2) (type1 type2)
  (let ((not1 (negation-type-type type1))
	(not2 (negation-type-type type2)))
    (cond
      ((csubtypep not1 not2) type1)
      ((csubtypep not2 not1) type2)
      ((eq (type-intersection not1 not2) *empty-type*)
       *universal-type*)
      (t nil))))

(!define-type-method (negation :complex-union2) (type1 type2)
  (cond
    ((csubtypep (negation-type-type type2) type1) *universal-type*)
    ((eq (type-intersection type1 (negation-type-type type2)) *empty-type*)
     type2)
    (t nil)))

(!define-type-method (negation :simple-=) (type1 type2)
  (type= (negation-type-type type1) (negation-type-type type2)))

(!def-type-translator not (typespec)
  (let* ((not-type (specifier-type typespec))
	 (spec (type-specifier not-type)))
    (cond
      ;; canonicalize (NOT (NOT FOO))
      ((and (listp spec) (eq (car spec) 'not))
       (specifier-type (cadr spec)))
      ;; canonicalize (NOT NIL) and (NOT T)
      ((eq not-type *empty-type*) *universal-type*)
      ((eq not-type *universal-type*) *empty-type*)
      ((and (numeric-type-p not-type)
	    (null (numeric-type-low not-type))
	    (null (numeric-type-high not-type)))
       (make-negation-type :type not-type))
      ((numeric-type-p not-type)
       (type-union
	(make-negation-type
	 :type (modified-numeric-type not-type :low nil :high nil))
	(cond
	  ((null (numeric-type-low not-type))
	   (modified-numeric-type
	    not-type
	    :low (let ((h (numeric-type-high not-type)))
		   (if (consp h) (car h) (list h)))
	    :high nil))
	  ((null (numeric-type-high not-type))
	   (modified-numeric-type
	    not-type
	    :low nil
	    :high (let ((l (numeric-type-low not-type)))
		    (if (consp l) (car l) (list l)))))
	  (t (type-union
	      (modified-numeric-type
	       not-type
	       :low nil
	       :high (let ((l (numeric-type-low not-type)))
		       (if (consp l) (car l) (list l))))
	      (modified-numeric-type
	       not-type
	       :low (let ((h (numeric-type-high not-type)))
		      (if (consp h) (car h) (list h)))
	       :high nil))))))
      ((intersection-type-p not-type)
       (apply #'type-union
	      (mapcar #'(lambda (x)
			  (specifier-type `(not ,(type-specifier x))))
		      (intersection-type-types not-type))))
      ((union-type-p not-type)
       (apply #'type-intersection
	      (mapcar #'(lambda (x)
			  (specifier-type `(not ,(type-specifier x))))
		      (union-type-types not-type))))
      ((member-type-p not-type)
       (let ((members (member-type-members not-type)))
	 (if (some #'floatp members)
	     (let (floats)
	       (dolist (pair `((0.0f0 . ,(load-time-value (make-unportable-float :single-float-negative-zero)))
			       (0.0d0 . ,(load-time-value (make-unportable-float :double-float-negative-zero)))
			       #!+long-float
			       (0.0l0 . ,(load-time-value (make-unportable-float :long-float-negative-zero)))))
		 (when (member (car pair) members)
		   (aver (not (member (cdr pair) members)))
		   (push (cdr pair) floats)
		   (setf members (remove (car pair) members)))
		 (when (member (cdr pair) members)
		   (aver (not (member (car pair) members)))
		   (push (car pair) floats)
		   (setf members (remove (cdr pair) members))))
	       (apply #'type-intersection
		      (if (null members)
			  *universal-type*
			  (make-negation-type
			   :type (make-member-type :members members)))
		      (mapcar
		       (lambda (x)
			 (let ((type (ctype-of x)))
			   (type-union
			    (make-negation-type
			     :type (modified-numeric-type type
							  :low nil :high nil))
			    (modified-numeric-type type
						   :low nil :high (list x))
			    (make-member-type :members (list x))
			    (modified-numeric-type type
						   :low (list x) :high nil))))
		       floats)))
	     (make-negation-type :type not-type))))
      ((and (cons-type-p not-type)
	    (eq (cons-type-car-type not-type) *universal-type*)
	    (eq (cons-type-cdr-type not-type) *universal-type*))
       (make-negation-type :type not-type))
      ((cons-type-p not-type)
       (type-union
	(make-negation-type :type (specifier-type 'cons))
	(cond
	  ((and (not (eq (cons-type-car-type not-type) *universal-type*))
		(not (eq (cons-type-cdr-type not-type) *universal-type*)))
	   (type-union
	    (make-cons-type
	     (specifier-type `(not ,(type-specifier
				     (cons-type-car-type not-type))))
	     *universal-type*)
	    (make-cons-type
	     *universal-type*
	     (specifier-type `(not ,(type-specifier
				     (cons-type-cdr-type not-type)))))))
	  ((not (eq (cons-type-car-type not-type) *universal-type*))
	   (make-cons-type
	    (specifier-type `(not ,(type-specifier
				    (cons-type-car-type not-type))))
	    *universal-type*))
	  ((not (eq (cons-type-cdr-type not-type) *universal-type*))
	   (make-cons-type
	    *universal-type*
	    (specifier-type `(not ,(type-specifier
				    (cons-type-cdr-type not-type))))))
	  (t (bug "Weird CONS type ~S" not-type)))))
      (t (make-negation-type :type not-type)))))

;;;; numeric types

(!define-type-class number)

(!define-type-method (number :simple-=) (type1 type2)
  (values
   (and (eq (numeric-type-class type1) (numeric-type-class type2))
	(eq (numeric-type-format type1) (numeric-type-format type2))
	(eq (numeric-type-complexp type1) (numeric-type-complexp type2))
	(equalp (numeric-type-low type1) (numeric-type-low type2))
	(equalp (numeric-type-high type1) (numeric-type-high type2)))
   t))

(!define-type-method (number :unparse) (type)
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
			  ((and (= low sb!xc:most-negative-fixnum)
				(= high sb!xc:most-positive-fixnum))
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
	 (aver (eq base+bounds 'real))
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

;;; This is used to compare upper and lower bounds. This is different
;;; from the same-bound case:
;;; -- Since X = NIL is -infinity, whereas y = NIL is +infinity, we
;;;    return true if *either* arg is NIL.
;;; -- an open inner bound is "greater" and also squeezes the interval,
;;;    causing us to use the OPEN test for those cases as well.
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

(!define-type-method (number :simple-subtypep) (type1 type2)
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
	  ;; disjoint unless type2 is RATIONAL and type1 is INTEGER.
	  ;; [ or type1 is INTEGER and type2 is of the form (RATIONAL
	  ;; X X) for integral X, but this is dealt with in the
	  ;; canonicalization inside MAKE-NUMERIC-TYPE ]
	  ((not (or (eq class1 class2)
		    (null class2)
		    (and (eq class1 'integer) (eq class2 'rational))))
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

(!define-superclasses number ((number)) !cold-init-forms)

;;; If the high bound of LOW is adjacent to the low bound of HIGH,
;;; then return true, otherwise NIL.
(defun numeric-types-adjacent (low high)
  (let ((low-bound (numeric-type-high low))
	(high-bound (numeric-type-low high)))
    (cond ((not (and low-bound high-bound)) nil)
	  ((and (consp low-bound) (consp high-bound)) nil)
	  ((consp low-bound)
	   (let ((low-value (car low-bound)))
	     (or (eql low-value high-bound)
		 (and (eql low-value
			   (load-time-value (make-unportable-float
					     :single-float-negative-zero)))
		      (eql high-bound 0f0))
		 (and (eql low-value 0f0)
		      (eql high-bound
			   (load-time-value (make-unportable-float
					     :single-float-negative-zero))))
		 (and (eql low-value
			   (load-time-value (make-unportable-float
					     :double-float-negative-zero)))
		      (eql high-bound 0d0))
		 (and (eql low-value 0d0)
		      (eql high-bound
			   (load-time-value (make-unportable-float
					     :double-float-negative-zero)))))))
	  ((consp high-bound)
	   (let ((high-value (car high-bound)))
	     (or (eql high-value low-bound)
		 (and (eql high-value
			   (load-time-value (make-unportable-float
					     :single-float-negative-zero)))
		      (eql low-bound 0f0))
		 (and (eql high-value 0f0)
		      (eql low-bound
			   (load-time-value (make-unportable-float
					     :single-float-negative-zero))))
		 (and (eql high-value
			   (load-time-value (make-unportable-float
					     :double-float-negative-zero)))
		      (eql low-bound 0d0))
		 (and (eql high-value 0d0)
		      (eql low-bound
			   (load-time-value (make-unportable-float
					     :double-float-negative-zero)))))))
	  ((and (eq (numeric-type-class low) 'integer)
		(eq (numeric-type-class high) 'integer))
	   (eql (1+ low-bound) high-bound))
	  (t
	   nil))))

;;; Return a numeric type that is a supertype for both TYPE1 and TYPE2.
;;;
;;; Old comment, probably no longer applicable:
;;;
;;;   ### Note: we give up early to keep from dropping lots of
;;;   information on the floor by returning overly general types.
(!define-type-method (number :simple-union2) (type1 type2)
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
	   (cond
	     ((and (eq class1 class2)
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
					>= > t)))
	     ;; FIXME: These two clauses are almost identical, and the
	     ;; consequents are in fact identical in every respect.
	     ((and (eq class1 'rational)
		   (eq class2 'integer)
		   (eq format1 format2)
		   (eq complexp1 complexp2)
		   (integerp (numeric-type-low type2))
		   (integerp (numeric-type-high type2))
		   (= (numeric-type-low type2) (numeric-type-high type2))
		   (or (numeric-types-adjacent type1 type2)
		       (numeric-types-adjacent type2 type1)))
	      (make-numeric-type
	       :class 'rational
	       :format format1
	       :complexp complexp1
	       :low (numeric-bound-max (numeric-type-low type1)
				       (numeric-type-low type2)
				       <= < t)
	       :high (numeric-bound-max (numeric-type-high type1)
					(numeric-type-high type2)
					>= > t)))
	     ((and (eq class1 'integer)
		   (eq class2 'rational)
		   (eq format1 format2)
		   (eq complexp1 complexp2)
		   (integerp (numeric-type-low type1))
		   (integerp (numeric-type-high type1))
		   (= (numeric-type-low type1) (numeric-type-high type1))
		   (or (numeric-types-adjacent type1 type2)
		       (numeric-types-adjacent type2 type1)))
	      (make-numeric-type
	       :class 'rational
	       :format format1
	       :complexp complexp1
	       :low (numeric-bound-max (numeric-type-low type1)
				       (numeric-type-low type2)
				       <= < t)
	       :high (numeric-bound-max (numeric-type-high type1)
					(numeric-type-high type2)
					>= > t)))
	     (t nil))))))


(!cold-init-forms
  (setf (info :type :kind 'number)
	#+sb-xc-host :defined #-sb-xc-host :primitive)
  (setf (info :type :builtin 'number)
	(make-numeric-type :complexp nil)))

(!def-type-translator complex (&optional (typespec '*))
  (if (eq typespec '*)
      (make-numeric-type :complexp :complex)
      (labels ((not-numeric ()
		 (error "The component type for COMPLEX is not numeric: ~S"
			typespec))
	       (not-real ()
	         (error "The component type for COMPLEX is not real: ~S"
			typespec))
	       (complex1 (component-type)
	         (unless (numeric-type-p component-type)
		   (not-numeric))
		 (when (eq (numeric-type-complexp component-type) :complex)
		   (not-real))
		 (modified-numeric-type component-type :complexp :complex))
	       (complex-union (component)
		 (unless (numberp component)
		   (not-numeric))
		 ;; KLUDGE: This TYPECASE more or less does
		 ;; (UPGRADED-COMPLEX-PART-TYPE (TYPE-OF COMPONENT)),
		 ;; (plus a small hack to treat (EQL COMPONENT 0) specially)
		 ;; but uses logic cut and pasted from the DEFUN of
		 ;; UPGRADED-COMPLEX-PART-TYPE. That's fragile, because
		 ;; changing the definition of UPGRADED-COMPLEX-PART-TYPE
		 ;; would tend to break the code here. Unfortunately,
		 ;; though, reusing UPGRADED-COMPLEX-PART-TYPE here
		 ;; would cause another kind of fragility, because
		 ;; ANSI's definition of TYPE-OF is so weak that e.g.
		 ;; (UPGRADED-COMPLEX-PART-TYPE (TYPE-OF 1/2)) could
		 ;; end up being (UPGRADED-COMPLEX-PART-TYPE 'REAL)
		 ;; instead of (UPGRADED-COMPLEX-PART-TYPE 'RATIONAL).
		 ;; So using TYPE-OF would mean that ANSI-conforming
		 ;; maintenance changes in TYPE-OF could break the code here.
		 ;; It's not clear how best to fix this. -- WHN 2002-01-21,
		 ;; trying to summarize CSR's concerns in his patch
		 (typecase component
		   (complex (error "The component type for COMPLEX (EQL X) ~
                                    is complex: ~S"
				   component))
		   ((eql 0) (specifier-type nil)) ; as required by ANSI
		   (single-float (specifier-type '(complex single-float)))
		   (double-float (specifier-type '(complex double-float)))
		   #!+long-float
		   (long-float (specifier-type '(complex long-float)))
		   (rational (specifier-type '(complex rational)))
		   (t (specifier-type '(complex real))))))
	(let ((ctype (specifier-type typespec)))
	  (typecase ctype
	    (numeric-type (complex1 ctype))
	    (union-type (apply #'type-union
			       ;; FIXME: This code could suffer from
			       ;; (admittedly very obscure) cases of
			       ;; bug 145 e.g. when TYPE is
			       ;;   (OR (AND INTEGER (SATISFIES ODDP))
			       ;;       (AND FLOAT (SATISFIES FOO))
			       ;; and not even report the problem very well.
			       (mapcar #'complex1
				       (union-type-types ctype))))
	    ;; MEMBER-TYPE is almost the same as UNION-TYPE, but
	    ;; there's a gotcha: (COMPLEX (EQL 0)) is, according to
	    ;; ANSI, equal to type NIL, the empty set.
	    (member-type (apply #'type-union
				(mapcar #'complex-union
					(member-type-members ctype))))
	    (t
	     (multiple-value-bind (subtypep certainly)
		 (csubtypep ctype (specifier-type 'real))
	       (if (and (not subtypep) certainly)
		   (not-real)
		   ;; ANSI just says that TYPESPEC is any subtype of
		   ;; type REAL, not necessarily a NUMERIC-TYPE. In
		   ;; particular, at this point TYPESPEC could legally be
		   ;; an intersection type like (AND REAL (SATISFIES ODDP)),
		   ;; in which case we fall through the logic above and
		   ;; end up here, stumped.
		   (bug "~@<(known bug #145): The type ~S is too hairy to be 
                         used for a COMPLEX component.~:@>"
			typespec)))))))))

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

(!def-type-translator integer (&optional (low '*) (high '*))
  (let* ((l (canonicalized-bound low 'integer))
	 (lb (if (consp l) (1+ (car l)) l))
	 (h (canonicalized-bound high 'integer))
	 (hb (if (consp h) (1- (car h)) h)))
    (if (and hb lb (< hb lb))
	*empty-type*
      (make-numeric-type :class 'integer
			 :complexp :real
			 :enumerable (not (null (and l h)))
			 :low lb
			 :high hb))))

(defmacro !def-bounded-type (type class format)
  `(!def-type-translator ,type (&optional (low '*) (high '*))
     (let ((lb (canonicalized-bound low ',type))
	   (hb (canonicalized-bound high ',type)))
       (if (not (numeric-bound-test* lb hb <= <))
	   *empty-type*
	 (make-numeric-type :class ',class
			    :format ',format
			    :low lb
			    :high hb)))))

(!def-bounded-type rational rational nil)

;;; Unlike CMU CL, we represent the types FLOAT and REAL as
;;; UNION-TYPEs of more primitive types, in order to make
;;; type representation more unique, avoiding problems in the
;;; simplification of things like
;;;   (subtypep '(or (single-float -1.0 1.0) (single-float 0.1))
;;;             '(or (real -1 7) (single-float 0.1) (single-float -1.0 1.0)))
;;; When we allowed REAL to remain as a separate NUMERIC-TYPE,
;;; it was too easy for the first argument to be simplified to
;;; '(SINGLE-FLOAT -1.0), and for the second argument to be simplified
;;; to '(OR (REAL -1 7) (SINGLE-FLOAT 0.1)) and then for the
;;; SUBTYPEP to fail (returning NIL,T instead of T,T) because
;;; the first argument can't be seen to be a subtype of any of the
;;; terms in the second argument.
;;;
;;; The old CMU CL way was:
;;;   (!def-bounded-type float float nil)
;;;   (!def-bounded-type real nil nil)
;;;
;;; FIXME: If this new way works for a while with no weird new
;;; problems, we can go back and rip out support for separate FLOAT
;;; and REAL flavors of NUMERIC-TYPE. The new way was added in
;;; sbcl-0.6.11.22, 2001-03-21.
;;;
;;; FIXME: It's probably necessary to do something to fix the
;;; analogous problem with INTEGER and RATIONAL types. Perhaps
;;; bounded RATIONAL types should be represented as (OR RATIO INTEGER).
(defun coerce-bound (bound type inner-coerce-bound-fun)
  (declare (type function inner-coerce-bound-fun))
  (cond ((eql bound '*)
	 bound)
	((consp bound)
	 (destructuring-bind (inner-bound) bound
	   (list (funcall inner-coerce-bound-fun inner-bound type))))
	(t
	 (funcall inner-coerce-bound-fun bound type))))
(defun inner-coerce-real-bound (bound type)
  (ecase type
    (rational (rationalize bound))
    (float (if (floatp bound)
	       bound
	       ;; Coerce to the widest float format available, to
	       ;; avoid unnecessary loss of precision:
	       (coerce bound 'long-float)))))
(defun coerced-real-bound (bound type)
  (coerce-bound bound type #'inner-coerce-real-bound))
(defun coerced-float-bound (bound type)
  (coerce-bound bound type #'coerce))
(!def-type-translator real (&optional (low '*) (high '*))
  (specifier-type `(or (float ,(coerced-real-bound  low 'float)
			      ,(coerced-real-bound high 'float))
		       (rational ,(coerced-real-bound  low 'rational)
				 ,(coerced-real-bound high 'rational)))))
(!def-type-translator float (&optional (low '*) (high '*))
  (specifier-type 
   `(or (single-float ,(coerced-float-bound  low 'single-float)
		      ,(coerced-float-bound high 'single-float))
	(double-float ,(coerced-float-bound  low 'double-float)
		      ,(coerced-float-bound high 'double-float))
	#!+long-float ,(error "stub: no long float support yet"))))

(defmacro !define-float-format (f)
  `(!def-bounded-type ,f float ,f))

(!define-float-format short-float)
(!define-float-format single-float)
(!define-float-format double-float)
(!define-float-format long-float)

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

;;; Handle the case of type intersection on two numeric types. We use
;;; TYPES-EQUAL-OR-INTERSECT to throw out the case of types with no
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
(!define-type-method (number :simple-intersection2) (type1 type2)
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
		< <= nil)))
      *empty-type*))

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

(!define-type-class array)

;;; What this does depends on the setting of the
;;; *USE-IMPLEMENTATION-TYPES* switch. If true, return the specialized
;;; element type, otherwise return the original element type.
(defun specialized-element-type-maybe (type)
  (declare (type array-type type))
  (if *use-implementation-types*
      (array-type-specialized-element-type type)
      (array-type-element-type type)))

(!define-type-method (array :simple-=) (type1 type2)
  (if (or (unknown-type-p (array-type-element-type type1))
	  (unknown-type-p (array-type-element-type type2)))
      (multiple-value-bind (equalp certainp)
	  (type= (array-type-element-type type1)
		 (array-type-element-type type2))
	;; by its nature, the call to TYPE= should never return NIL,
	;; T, as we don't know what the UNKNOWN-TYPE will grow up to
	;; be.  -- CSR, 2002-08-19
	(aver (not (and (not equalp) certainp)))
	(values equalp certainp))
      (values (and (equal (array-type-dimensions type1)
			  (array-type-dimensions type2))
		   (eq (array-type-complexp type1)
		       (array-type-complexp type2))
		   (type= (specialized-element-type-maybe type1)
			  (specialized-element-type-maybe type2)))
	      t)))

(!define-type-method (array :unparse) (type)
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

(!define-type-method (array :simple-subtypep) (type1 type2)
  (let ((dims1 (array-type-dimensions type1))
	(dims2 (array-type-dimensions type2))
	(complexp2 (array-type-complexp type2)))
    (cond (;; not subtypep unless dimensions are compatible
	   (not (or (eq dims2 '*)
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
	  ;; not subtypep unless complexness is compatible
	  ((not (or (eq complexp2 :maybe)
		    (eq (array-type-complexp type1) complexp2)))
	   (values nil t))
	  ;; Since we didn't fail any of the tests above, we win
	  ;; if the TYPE2 element type is wild.
	  ((eq (array-type-element-type type2) *wild-type*)
	   (values t t))
	  (;; Since we didn't match any of the special cases above, we
	   ;; can't give a good answer unless both the element types
	   ;; have been defined.
	   (or (unknown-type-p (array-type-element-type type1))
	       (unknown-type-p (array-type-element-type type2)))
	   (values nil nil))
	  (;; Otherwise, the subtype relationship holds iff the
	   ;; types are equal, and they're equal iff the specialized
	   ;; element types are identical.
	   t
	   (values (type= (specialized-element-type-maybe type1)
			  (specialized-element-type-maybe type2))
		   t)))))

(!define-superclasses array
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
			 (every (lambda (x y)
				  (or (eq x '*) (eq y '*) (= x y)))
				dims1 dims2))))
	   (values nil t))
	  ;; See whether complexpness is compatible.
	  ((not (or (eq complexp1 :maybe)
		    (eq complexp2 :maybe)
		    (eq complexp1 complexp2)))
	   (values nil t))
	  ;; Old comment:
	  ;;
	  ;;   If either element type is wild, then they intersect.
	  ;;   Otherwise, the types must be identical.
	  ;;
	  ;; FIXME: There seems to have been a fair amount of
	  ;; confusion about the distinction between requested element
	  ;; type and specialized element type; here is one of
	  ;; them. If we request an array to hold objects of an
	  ;; unknown type, we can do no better than represent that
	  ;; type as an array specialized on wild-type.  We keep the
	  ;; requested element-type in the -ELEMENT-TYPE slot, and
	  ;; *WILD-TYPE* in the -SPECIALIZED-ELEMENT-TYPE.  So, here,
	  ;; we must test for the SPECIALIZED slot being *WILD-TYPE*,
	  ;; not just the ELEMENT-TYPE slot.  Maybe the return value
	  ;; in that specific case should be T, NIL?  Or maybe this
	  ;; function should really be called
	  ;; ARRAY-TYPES-COULD-POSSIBLY-INTERSECT?  In any case, this
	  ;; was responsible for bug #123, and this whole issue could
	  ;; do with a rethink and/or a rewrite.  -- CSR, 2002-08-21
	  ((or (eq (array-type-specialized-element-type type1) *wild-type*)
	       (eq (array-type-specialized-element-type type2) *wild-type*)
	       (type= (specialized-element-type-maybe type1)
		      (specialized-element-type-maybe type2)))

	   (values t t))
	  (t
	   (values nil t)))))

(!define-type-method (array :simple-intersection2) (type1 type2)
  (declare (type array-type type1 type2))
  (if (array-types-intersect type1 type2)
      (let ((dims1 (array-type-dimensions type1))
	    (dims2 (array-type-dimensions type2))
	    (complexp1 (array-type-complexp type1))
	    (complexp2 (array-type-complexp type2))
	    (eltype1 (array-type-element-type type1))
	    (eltype2 (array-type-element-type type2)))
	(specialize-array-type
	 (make-array-type
	  :dimensions (cond ((eq dims1 '*) dims2)
			    ((eq dims2 '*) dims1)
			    (t
			     (mapcar (lambda (x y) (if (eq x '*) y x))
				     dims1 dims2)))
	  :complexp (if (eq complexp1 :maybe) complexp2 complexp1)
	  :element-type (cond
			  ((eq eltype1 *wild-type*) eltype2)
			  ((eq eltype2 *wild-type*) eltype1)
			  (t (type-intersection eltype1 eltype2))))))
      *empty-type*))

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

(!define-type-class member)

(!define-type-method (member :unparse) (type)
  (let ((members (member-type-members type)))
    (cond
      ((equal members '(nil)) 'null)
      ((type= type (specifier-type 'standard-char)) 'standard-char)
      (t `(member ,@members)))))

(!define-type-method (member :simple-subtypep) (type1 type2)
  (values (subsetp (member-type-members type1) (member-type-members type2))
	  t))

(!define-type-method (member :complex-subtypep-arg1) (type1 type2)
  (every/type (swapped-args-fun #'ctypep)
	      type2
	      (member-type-members type1)))

;;; We punt if the odd type is enumerable and intersects with the
;;; MEMBER type. If not enumerable, then it is definitely not a
;;; subtype of the MEMBER type.
(!define-type-method (member :complex-subtypep-arg2) (type1 type2)
  (cond ((not (type-enumerable type1)) (values nil t))
	((types-equal-or-intersect type1 type2)
	 (invoke-complex-subtypep-arg1-method type1 type2))
	(t (values nil t))))

(!define-type-method (member :simple-intersection2) (type1 type2)
  (let ((mem1 (member-type-members type1))
	(mem2 (member-type-members type2)))
    (cond ((subsetp mem1 mem2) type1)
	  ((subsetp mem2 mem1) type2)
	  (t
	   (let ((res (intersection mem1 mem2)))
	     (if res
		 (make-member-type :members res)
		 *empty-type*))))))

(!define-type-method (member :complex-intersection2) (type1 type2)
  (block punt
    (collect ((members))
      (let ((mem2 (member-type-members type2)))
        (dolist (member mem2)
	  (multiple-value-bind (val win) (ctypep member type1)
	    (unless win
	      (return-from punt nil))
	    (when val (members member))))
	(cond ((subsetp mem2 (members)) type2)
	      ((null (members)) *empty-type*)
	      (t
	       (make-member-type :members (members))))))))

;;; We don't need a :COMPLEX-UNION2, since the only interesting case is
;;; a union type, and the member/union interaction is handled by the
;;; union type method.
(!define-type-method (member :simple-union2) (type1 type2)
  (let ((mem1 (member-type-members type1))
	(mem2 (member-type-members type2)))
    (cond ((subsetp mem1 mem2) type2)
	  ((subsetp mem2 mem1) type1)
	  (t
	   (make-member-type :members (union mem1 mem2))))))

(!define-type-method (member :simple-=) (type1 type2)
  (let ((mem1 (member-type-members type1))
	(mem2 (member-type-members type2)))
    (values (and (subsetp mem1 mem2)
		 (subsetp mem2 mem1))
	    t)))

(!define-type-method (member :complex-=) (type1 type2)
  (if (type-enumerable type1)
      (multiple-value-bind (val win) (csubtypep type2 type1)
	(if (or val (not win))
	    (values nil nil)
	    (values nil t)))
      (values nil t)))

(!def-type-translator member (&rest members)
  (if members
      (let (ms numbers)
	(dolist (m (remove-duplicates members))
	  (typecase m
	    (float (if (zerop m)
		       (push m ms)
		       (push (ctype-of m) numbers)))
	    (number (push (ctype-of m) numbers))
	    (t (push m ms))))
	(apply #'type-union
	       (if ms
		   (make-member-type :members ms)
		   *empty-type*)
	       (nreverse numbers)))
      *empty-type*))

;;;; intersection types
;;;;
;;;; Until version 0.6.10.6, SBCL followed the original CMU CL approach
;;;; of punting on all AND types, not just the unreasonably complicated
;;;; ones. The change was motivated by trying to get the KEYWORD type
;;;; to behave sensibly:
;;;;    ;; reasonable definition
;;;;    (DEFTYPE KEYWORD () '(AND SYMBOL (SATISFIES KEYWORDP)))
;;;;    ;; reasonable behavior
;;;;    (AVER (SUBTYPEP 'KEYWORD 'SYMBOL))
;;;; Without understanding a little about the semantics of AND, we'd
;;;; get (SUBTYPEP 'KEYWORD 'SYMBOL)=>NIL,NIL and, for entirely
;;;; parallel reasons, (SUBTYPEP 'RATIO 'NUMBER)=>NIL,NIL. That's
;;;; not so good..)
;;;;
;;;; We still follow the example of CMU CL to some extent, by punting
;;;; (to the opaque HAIRY-TYPE) on sufficiently complicated types
;;;; involving AND.

(!define-type-class intersection)

;;; A few intersection types have special names. The others just get
;;; mechanically unparsed.
(!define-type-method (intersection :unparse) (type)
  (declare (type ctype type))
  (or (find type '(ratio keyword) :key #'specifier-type :test #'type=)
      `(and ,@(mapcar #'type-specifier (intersection-type-types type)))))

;;; shared machinery for type equality: true if every type in the set
;;; TYPES1 matches a type in the set TYPES2 and vice versa
(defun type=-set (types1 types2)
  (flet ((type<=-set (x y)
	   (declare (type list x y))
	   (every/type (lambda (x y-element)
                         (any/type #'type= y-element x))
                       x y)))
    (and/type (type<=-set types1 types2)
              (type<=-set types2 types1))))

;;; Two intersection types are equal if their subtypes are equal sets.
;;;
;;; FIXME: Might it be better to use
;;;   (AND (SUBTYPEP X Y) (SUBTYPEP Y X))
;;; instead, since SUBTYPEP is the usual relationship that we care
;;; most about, so it would be good to leverage any ingenuity there
;;; in this more obscure method?
(!define-type-method (intersection :simple-=) (type1 type2)
  (type=-set (intersection-type-types type1)
	     (intersection-type-types type2)))

(defun %intersection-complex-subtypep-arg1 (type1 type2)
  (type= type1 (type-intersection type1 type2)))

(defun %intersection-simple-subtypep (type1 type2)
  (every/type #'%intersection-complex-subtypep-arg1
	      type1
	      (intersection-type-types type2)))

(!define-type-method (intersection :simple-subtypep) (type1 type2)
  (%intersection-simple-subtypep type1 type2))
  
(!define-type-method (intersection :complex-subtypep-arg1) (type1 type2)
  (%intersection-complex-subtypep-arg1 type1 type2))

(defun %intersection-complex-subtypep-arg2 (type1 type2)
  (every/type #'csubtypep type1 (intersection-type-types type2)))

(!define-type-method (intersection :complex-subtypep-arg2) (type1 type2)
  (%intersection-complex-subtypep-arg2 type1 type2))

;;; FIXME: This will look eeriely familiar to readers of the UNION
;;; :SIMPLE-INTERSECTION2 :COMPLEX-INTERSECTION2 method.  That's
;;; because it was generated by cut'n'paste methods.  Given that
;;; intersections and unions have all sorts of symmetries known to
;;; mathematics, it shouldn't be beyond the ken of some programmers to
;;; reflect those symmetries in code in a way that ties them together
;;; more strongly than having two independent near-copies :-/
(!define-type-method (intersection :simple-union2 :complex-union2)
 		     (type1 type2)
  ;; Within this method, type2 is guaranteed to be an intersection
  ;; type:
  (aver (intersection-type-p type2))
  ;; Make sure to call only the applicable methods...
  (cond ((and (intersection-type-p type1)
	      (%intersection-simple-subtypep type1 type2)) type2)
	((and (intersection-type-p type1)
	      (%intersection-simple-subtypep type2 type1)) type1)
	((and (not (intersection-type-p type1))
	      (%intersection-complex-subtypep-arg2 type1 type2))
	 type2)
	((and (not (intersection-type-p type1))
	      (%intersection-complex-subtypep-arg1 type2 type1))
	 type1)
	;; KLUDGE: This special (and somewhat hairy) magic is required
	;; to deal with the RATIONAL/INTEGER special case.  The UNION
	;; of (INTEGER * -1) and (AND (RATIONAL * -1/2) (NOT INTEGER))
	;; should be (RATIONAL * -1/2) -- CSR, 2003-02-28
	((and (csubtypep type2 (specifier-type 'ratio))
	      (numeric-type-p type1)
	      (csubtypep type1 (specifier-type 'integer))
	      (csubtypep type2
			 (make-numeric-type
			  :class 'rational
			  :complexp nil
			  :low (if (null (numeric-type-low type1))
				   nil
				   (list (1- (numeric-type-low type1))))
			  :high (if (null (numeric-type-high type1))
				    nil
				    (list (1+ (numeric-type-high type1)))))))
	 (type-union type1
		     (apply #'type-intersection
			    (remove (specifier-type '(not integer))
				    (intersection-type-types type2)
				    :test #'type=))))
	(t
	 (let ((accumulator *universal-type*))
	   (do ((t2s (intersection-type-types type2) (cdr t2s)))
	       ((null t2s) accumulator)
	     (let ((union (type-union type1 (car t2s))))
	       (when (union-type-p union)
		 ;; we have to give up here -- there are all sorts of
		 ;; ordering worries, but it's better than before.
		 ;; Doing exactly the same as in the UNION
		 ;; :SIMPLE/:COMPLEX-INTERSECTION2 method causes stack
		 ;; overflow with the mutual recursion never bottoming
		 ;; out.
		 (if (and (eq accumulator *universal-type*)
			  (null (cdr t2s)))
		     ;; KLUDGE: if we get here, we have a partially
		     ;; simplified result.  While this isn't by any
		     ;; means a universal simplification, including
		     ;; this logic here means that we can get (OR
		     ;; KEYWORD (NOT KEYWORD)) canonicalized to T.
		     (return union)
		     (return nil)))
	       (setf accumulator
		     (type-intersection accumulator union))))))))

(!def-type-translator and (&whole whole &rest type-specifiers)
  (apply #'type-intersection
	 (mapcar #'specifier-type
		 type-specifiers)))

;;;; union types

(!define-type-class union)

;;; The LIST, FLOAT and REAL types have special names.  Other union
;;; types just get mechanically unparsed.
(!define-type-method (union :unparse) (type)
  (declare (type ctype type))
  (cond
    ((type= type (specifier-type 'list)) 'list)
    ((type= type (specifier-type 'float)) 'float)
    ((type= type (specifier-type 'real)) 'real)
    ((type= type (specifier-type 'sequence)) 'sequence)
    ((type= type (specifier-type 'bignum)) 'bignum)
    (t `(or ,@(mapcar #'type-specifier (union-type-types type))))))

;;; Two union types are equal if they are each subtypes of each
;;; other. We need to be this clever because our complex subtypep
;;; methods are now more accurate; we don't get infinite recursion
;;; because the simple-subtypep method delegates to complex-subtypep
;;; of the individual types of type1. - CSR, 2002-04-09
;;;
;;; Previous comment, now obsolete, but worth keeping around because
;;; it is true, though too strong a condition:
;;;
;;; Two union types are equal if their subtypes are equal sets.
(!define-type-method (union :simple-=) (type1 type2)
  (multiple-value-bind (subtype certain?)
      (csubtypep type1 type2)
    (if subtype
	(csubtypep type2 type1)
	;; we might as well become as certain as possible.
	(if certain?
	    (values nil t)
	    (multiple-value-bind (subtype certain?)
		(csubtypep type2 type1)
	      (declare (ignore subtype))
	      (values nil certain?))))))

(!define-type-method (union :complex-=) (type1 type2)
  (declare (ignore type1))
  (if (some #'type-might-contain-other-types-p 
	    (union-type-types type2))
      (values nil nil)
      (values nil t)))

;;; Similarly, a union type is a subtype of another if and only if
;;; every element of TYPE1 is a subtype of TYPE2.
(defun union-simple-subtypep (type1 type2)
  (every/type (swapped-args-fun #'union-complex-subtypep-arg2)
	      type2
	      (union-type-types type1)))

(!define-type-method (union :simple-subtypep) (type1 type2)
  (union-simple-subtypep type1 type2))
  
(defun union-complex-subtypep-arg1 (type1 type2)
  (every/type (swapped-args-fun #'csubtypep)
	      type2
	      (union-type-types type1)))

(!define-type-method (union :complex-subtypep-arg1) (type1 type2)
  (union-complex-subtypep-arg1 type1 type2))

(defun union-complex-subtypep-arg2 (type1 type2)
  (multiple-value-bind (sub-value sub-certain?)
      ;; was: (any/type #'csubtypep type1 (union-type-types type2)),
      ;; which turns out to be too restrictive, causing bug 91.
      ;;
      ;; the following reimplementation might look dodgy.  It is
      ;; dodgy. It depends on the union :complex-= method not doing
      ;; very much work -- certainly, not using subtypep. Reasoning:
      (progn
	;; At this stage, we know that type2 is a union type and type1
	;; isn't. We might as well check this, though:
	(aver (union-type-p type2))
	(aver (not (union-type-p type1)))
	;;     A is a subset of (B1 u B2)
	;; <=> A n (B1 u B2) = A
	;; <=> (A n B1) u (A n B2) = A
	;;
	;; But, we have to be careful not to delegate this type= to
	;; something that could invoke subtypep, which might get us
	;; back here -> stack explosion. We therefore ensure that the
	;; second type (which is the one that's dispatched on) is
	;; either a union type (where we've ensured that the complex-=
	;; method will not call subtypep) or something with no union
	;; types involved, in which case we'll never come back here.
	;;
	;; If we don't do this, then e.g.
	;; (SUBTYPEP '(MEMBER 3) '(OR (SATISFIES FOO) (SATISFIES BAR)))
	;; would loop infinitely, as the member :complex-= method is
	;; implemented in terms of subtypep.
	;;
	;; Ouch. - CSR, 2002-04-10
	(type= type1
	       (apply #'type-union
		      (mapcar (lambda (x) (type-intersection type1 x))
			      (union-type-types type2)))))
    (if sub-certain?
	(values sub-value sub-certain?)
	;; The ANY/TYPE expression above is a sufficient condition for
	;; subsetness, but not a necessary one, so we might get a more
	;; certain answer by this CALL-NEXT-METHOD-ish step when the
	;; ANY/TYPE expression is uncertain.
	(invoke-complex-subtypep-arg1-method type1 type2))))

(!define-type-method (union :complex-subtypep-arg2) (type1 type2)
  (union-complex-subtypep-arg2 type1 type2))

(!define-type-method (union :simple-intersection2 :complex-intersection2)
 		     (type1 type2)
  ;; The CSUBTYPEP clauses here let us simplify e.g.
  ;;   (TYPE-INTERSECTION2 (SPECIFIER-TYPE 'LIST)
  ;;                       (SPECIFIER-TYPE '(OR LIST VECTOR)))
  ;; (where LIST is (OR CONS NULL)).
  ;;
  ;; The tests are more or less (CSUBTYPEP TYPE1 TYPE2) and vice
  ;; versa, but it's important that we pre-expand them into
  ;; specialized operations on individual elements of
  ;; UNION-TYPE-TYPES, instead of using the ordinary call to
  ;; CSUBTYPEP, in order to avoid possibly invoking any methods which
  ;; might in turn invoke (TYPE-INTERSECTION2 TYPE1 TYPE2) and thus
  ;; cause infinite recursion.
  ;;
  ;; Within this method, type2 is guaranteed to be a union type:
  (aver (union-type-p type2))
  ;; Make sure to call only the applicable methods...
  (cond ((and (union-type-p type1)
	      (union-simple-subtypep type1 type2)) type1)
	((and (union-type-p type1)
	      (union-simple-subtypep type2 type1)) type2)
	((and (not (union-type-p type1))
	      (union-complex-subtypep-arg2 type1 type2))
	 type1)
	((and (not (union-type-p type1))
	      (union-complex-subtypep-arg1 type2 type1))
	 type2)
	(t 
	 ;; KLUDGE: This code accumulates a sequence of TYPE-UNION2
	 ;; operations in a particular order, and gives up if any of
	 ;; the sub-unions turn out not to be simple. In other cases
	 ;; ca. sbcl-0.6.11.15, that approach to taking a union was a
	 ;; bad idea, since it can overlook simplifications which
	 ;; might occur if the terms were accumulated in a different
	 ;; order. It's possible that that will be a problem here too.
	 ;; However, I can't think of a good example to demonstrate
	 ;; it, and without an example to demonstrate it I can't write
	 ;; test cases, and without test cases I don't want to
	 ;; complicate the code to address what's still a hypothetical
	 ;; problem. So I punted. -- WHN 2001-03-20
	 (let ((accumulator *empty-type*))
	   (dolist (t2 (union-type-types type2) accumulator)
	     (setf accumulator
		   (type-union accumulator
			       (type-intersection type1 t2))))))))

(!def-type-translator or (&rest type-specifiers)
  (apply #'type-union
	 (mapcar #'specifier-type
		 type-specifiers)))

;;;; CONS types

(!define-type-class cons)

(!def-type-translator cons (&optional (car-type-spec '*) (cdr-type-spec '*))
  (let ((car-type (single-value-specifier-type car-type-spec))
	(cdr-type (single-value-specifier-type cdr-type-spec)))
    (make-cons-type car-type cdr-type)))
 
(!define-type-method (cons :unparse) (type)
  (let ((car-eltype (type-specifier (cons-type-car-type type)))
	(cdr-eltype (type-specifier (cons-type-cdr-type type))))
    (if (and (member car-eltype '(t *))
	     (member cdr-eltype '(t *)))
	'cons
	`(cons ,car-eltype ,cdr-eltype))))
 
(!define-type-method (cons :simple-=) (type1 type2)
  (declare (type cons-type type1 type2))
  (and (type= (cons-type-car-type type1) (cons-type-car-type type2))
       (type= (cons-type-cdr-type type1) (cons-type-cdr-type type2))))
 
(!define-type-method (cons :simple-subtypep) (type1 type2)
  (declare (type cons-type type1 type2))
  (multiple-value-bind (val-car win-car)
      (csubtypep (cons-type-car-type type1) (cons-type-car-type type2))
    (multiple-value-bind (val-cdr win-cdr)
	(csubtypep (cons-type-cdr-type type1) (cons-type-cdr-type type2))
      (if (and val-car val-cdr)
	  (values t (and win-car win-cdr))
	  (values nil (or win-car win-cdr))))))
 
;;; Give up if a precise type is not possible, to avoid returning
;;; overly general types.
(!define-type-method (cons :simple-union2) (type1 type2)
  (declare (type cons-type type1 type2))
  (let ((car-type1 (cons-type-car-type type1))
	(car-type2 (cons-type-car-type type2))
	(cdr-type1 (cons-type-cdr-type type1))
	(cdr-type2 (cons-type-cdr-type type2)))
    ;; UGH.  -- CSR, 2003-02-24
    (macrolet ((frob-car (car1 car2 cdr1 cdr2)
		 `(type-union
		   (make-cons-type ,car1 (type-union ,cdr1 ,cdr2))
		   (make-cons-type
		    (type-intersection ,car2
		     (specifier-type
		      `(not ,(type-specifier ,car1))))
		    ,cdr2))))
      (cond ((type= car-type1 car-type2)
	     (make-cons-type car-type1
			     (type-union cdr-type1 cdr-type2)))
	    ((type= cdr-type1 cdr-type2)
	     (make-cons-type (type-union car-type1 car-type2)
			     cdr-type1))
	    ((csubtypep car-type1 car-type2)
	     (frob-car car-type1 car-type2 cdr-type1 cdr-type2))
	    ((csubtypep car-type2 car-type1)
	     (frob-car car-type2 car-type1 cdr-type2 cdr-type1))
	    ;; Don't put these in -- consider the effect of taking the
	    ;; union of (CONS (INTEGER 0 2) (INTEGER 5 7)) and
	    ;; (CONS (INTEGER 0 3) (INTEGER 5 6)).
	    #+nil
	    ((csubtypep cdr-type1 cdr-type2)
	     (frob-cdr car-type1 car-type2 cdr-type1 cdr-type2))
	    #+nil
	    ((csubtypep cdr-type2 cdr-type1)
	     (frob-cdr car-type2 car-type1 cdr-type2 cdr-type1))))))
	    
(!define-type-method (cons :simple-intersection2) (type1 type2)
  (declare (type cons-type type1 type2))
  (let (car-int2
	cdr-int2)
    (and (setf car-int2 (type-intersection2 (cons-type-car-type type1)
					    (cons-type-car-type type2)))
	 (setf cdr-int2 (type-intersection2 (cons-type-cdr-type type1)
					    (cons-type-cdr-type type2)))
	 (make-cons-type car-int2 cdr-int2))))

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
		(when (types-equal-or-intersect x-type y-type)
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
      (apply #'type-union (res)))))

(!def-type-translator array (&optional (element-type '*)
				       (dimensions '*))
  (specialize-array-type
   (make-array-type :dimensions (canonical-array-dimensions dimensions)
                    :complexp :maybe
		    :element-type (if (eq element-type '*)
                                      *wild-type*
                                      (specifier-type element-type)))))

(!def-type-translator simple-array (&optional (element-type '*)
					      (dimensions '*))
  (specialize-array-type
   (make-array-type :dimensions (canonical-array-dimensions dimensions)
                    :complexp nil
		    :element-type (if (eq element-type '*)
                                      *wild-type*
                                      (specifier-type element-type)))))

;;;; utilities shared between cross-compiler and target system

;;; Does the type derived from compilation of an actual function
;;; definition satisfy declarations of a function's type?
(defun defined-ftype-matches-declared-ftype-p (defined-ftype declared-ftype)
  (declare (type ctype defined-ftype declared-ftype))
  (flet ((is-built-in-class-function-p (ctype)
	   (and (built-in-classoid-p ctype)
		(eq (built-in-classoid-name ctype) 'function))))
    (cond (;; DECLARED-FTYPE could certainly be #<BUILT-IN-CLASS FUNCTION>;
	   ;; that's what happens when we (DECLAIM (FTYPE FUNCTION FOO)).
	   (is-built-in-class-function-p declared-ftype)
	   ;; In that case, any definition satisfies the declaration.
	   t)
	  (;; It's not clear whether or how DEFINED-FTYPE might be
	   ;; #<BUILT-IN-CLASS FUNCTION>, but it's not obviously
	   ;; invalid, so let's handle that case too, just in case.
	   (is-built-in-class-function-p defined-ftype)
	   ;; No matter what DECLARED-FTYPE might be, we can't prove
	   ;; that an object of type FUNCTION doesn't satisfy it, so
	   ;; we return success no matter what.
	   t)
	  (;; Otherwise both of them must be FUN-TYPE objects.
	   t
	   ;; FIXME: For now we only check compatibility of the return
	   ;; type, not argument types, and we don't even check the
	   ;; return type very precisely (as per bug 94a). It would be
	   ;; good to do a better job. Perhaps to check the
	   ;; compatibility of the arguments, we should (1) redo
	   ;; VALUES-TYPES-EQUAL-OR-INTERSECT as
	   ;; ARGS-TYPES-EQUAL-OR-INTERSECT, and then (2) apply it to
	   ;; the ARGS-TYPE slices of the FUN-TYPEs. (ARGS-TYPE
	   ;; is a base class both of VALUES-TYPE and of FUN-TYPE.)
	   (values-types-equal-or-intersect
	    (fun-type-returns defined-ftype)
	    (fun-type-returns declared-ftype))))))
	   
;;; This messy case of CTYPE for NUMBER is shared between the
;;; cross-compiler and the target system.
(defun ctype-of-number (x)
  (let ((num (if (complexp x) (realpart x) x)))
    (multiple-value-bind (complexp low high)
	(if (complexp x)
	    (let ((imag (imagpart x)))
	      (values :complex (min num imag) (max num imag)))
	    (values :real num num))
      (make-numeric-type :class (etypecase num
				  (integer 'integer)
				  (rational 'rational)
				  (float 'float))
			 :format (and (floatp num) (float-format-name num))
			 :complexp complexp
			 :low low
			 :high high))))

(locally
  ;; Why SAFETY 0? To suppress the is-it-the-right-structure-type
  ;; checking for declarations in structure accessors. Otherwise we
  ;; can get caught in a chicken-and-egg bootstrapping problem, whose
  ;; symptom on x86 OpenBSD sbcl-0.pre7.37.flaky5.22 is an illegal
  ;; instruction trap. I haven't tracked it down, but I'm guessing it
  ;; has to do with setting LAYOUTs when the LAYOUT hasn't been set
  ;; yet. -- WHN
  (declare (optimize (safety 0)))
  (!defun-from-collected-cold-init-forms !late-type-cold-init))

(/show0 "late-type.lisp end of file")

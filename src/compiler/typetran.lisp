;;;; This file contains stuff that implements the portable IR1
;;;; semantics of type tests and coercion. The main thing we do is
;;;; convert complex type operations into simpler code that can be
;;;; compiled inline.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!C")

;;;; type predicate translation
;;;;
;;;; We maintain a bidirectional association between type predicates
;;;; and the tested type. The presence of a predicate in this
;;;; association implies that it is desirable to implement tests of
;;;; this type using the predicate. These are either predicates that
;;;; the back end is likely to have special knowledge about, or
;;;; predicates so complex that the only reasonable implentation is
;;;; via function call.
;;;;
;;;; Some standard types (such as SEQUENCE) are best tested by letting
;;;; the TYPEP source transform do its thing with the expansion. These
;;;; types (and corresponding predicates) are not maintained in this
;;;; association. In this case, there need not be any predicate
;;;; function unless it is required by the Common Lisp specification.
;;;;
;;;; The mapping between predicates and type structures is considered
;;;; part of the backend; different backends can support different
;;;; sets of predicates.

(defmacro define-type-predicate (name type)
  #!+sb-doc
  "Define-Type-Predicate Name Type
  Establish an association between the type predicate Name and the
  corresponding Type. This causes the type predicate to be recognized for
  purposes of optimization."
  `(%define-type-predicate ',name ',type))
(defun %define-type-predicate (name specifier)
  (let ((type (specifier-type specifier)))
    (setf (gethash name *backend-predicate-types*) type)
    (setf *backend-type-predicates*
	  (cons (cons type name)
		(remove name *backend-type-predicates*
			:key #'cdr)))
    (%deftransform name '(function (t) *) #'fold-type-predicate)
    name))

;;;; IR1 transforms

;;; If we discover the type argument is constant during IR1
;;; optimization, then give the source transform another chance. The
;;; source transform can't pass, since we give it an explicit
;;; constant. At worst, it will convert to %TYPEP, which will prevent
;;; spurious attempts at transformation (and possible repeated
;;; warnings.)
(deftransform typep ((object type))
  (unless (constant-continuation-p type)
    (give-up-ir1-transform "can't open-code test of non-constant type"))
  `(typep object ',(continuation-value type)))

;;; If the continuation OBJECT definitely is or isn't of the specified
;;; type, then return T or NIL as appropriate. Otherwise quietly
;;; GIVE-UP-IR1-TRANSFORM.
(defun ir1-transform-type-predicate (object type)
  (declare (type continuation object) (type ctype type))
  (let ((otype (continuation-type object)))
    (cond ((not (types-intersect otype type))
	   'nil)
	  ((csubtypep otype type)
	   't)
	  (t
	   (give-up-ir1-transform)))))

;;; Flush %TYPEP tests whose result is known at compile time.
(deftransform %typep ((object type))
  (unless (constant-continuation-p type) (give-up-ir1-transform))
  (ir1-transform-type-predicate
   object
   (specifier-type (continuation-value type))))

;;; This is the IR1 transform for simple type predicates. It checks
;;; whether the single argument is known to (not) be of the
;;; appropriate type, expanding to T or NIL as appropriate.
(deftransform fold-type-predicate ((object) * * :node node :defun-only t)
  (let ((ctype (gethash (leaf-name
			 (ref-leaf
			  (continuation-use
			   (basic-combination-fun node))))
			*backend-predicate-types*)))
    (assert ctype)
    (ir1-transform-type-predicate object ctype)))

;;; If FIND-CLASS is called on a constant class, locate the CLASS-CELL
;;; at load time.
(deftransform find-class ((name) ((constant-argument symbol)) *
			  :when :both)
  (let* ((name (continuation-value name))
	 (cell (find-class-cell name)))
    `(or (class-cell-class ',cell)
	 (error "class not yet defined: ~S" name))))

;;;; standard type predicates

;;; FIXME: needed only at cold load time, can be uninterned afterwards;
;;; or perhaps could just be done at toplevel
(defun define-standard-type-predicates ()
  (define-type-predicate arrayp array)
  ; (The ATOM predicate is handled separately as (NOT CONS).)
  (define-type-predicate bit-vector-p bit-vector)
  (define-type-predicate characterp character)
  (define-type-predicate compiled-function-p compiled-function)
  (define-type-predicate complexp complex)
  (define-type-predicate complex-rational-p (complex rational))
  (define-type-predicate complex-float-p (complex float))
  (define-type-predicate consp cons)
  (define-type-predicate floatp float)
  (define-type-predicate functionp function)
  (define-type-predicate integerp integer)
  (define-type-predicate keywordp keyword)
  (define-type-predicate listp list)
  (define-type-predicate null null)
  (define-type-predicate numberp number)
  (define-type-predicate rationalp rational)
  (define-type-predicate realp real)
  (define-type-predicate simple-bit-vector-p simple-bit-vector)
  (define-type-predicate simple-string-p simple-string)
  (define-type-predicate simple-vector-p simple-vector)
  (define-type-predicate stringp string)
  (define-type-predicate %instancep instance)
  (define-type-predicate funcallable-instance-p funcallable-instance)
  (define-type-predicate symbolp symbol)
  (define-type-predicate vectorp vector))

(define-standard-type-predicates)

;;;; transforms for type predicates not implemented primitively
;;;;
;;;; See also VM dependent transforms.

(def-source-transform atom (x)
  `(not (consp ,x)))

;;;; TYPEP source transform

;;; Return a form that tests the variable N-OBJECT for being in the
;;; binds specified by TYPE. BASE is the name of the base type, for
;;; declaration. We make SAFETY locally 0 to inhibit any checking of
;;; this assertion.
#!-negative-zero-is-not-zero
(defun transform-numeric-bound-test (n-object type base)
  (declare (type numeric-type type))
  (let ((low (numeric-type-low type))
	(high (numeric-type-high type)))
    `(locally
       (declare (optimize (safety 0)))
       (and ,@(when low
		(if (consp low)
		    `((> (the ,base ,n-object) ,(car low)))
		    `((>= (the ,base ,n-object) ,low))))
	    ,@(when high
		(if (consp high)
		    `((< (the ,base ,n-object) ,(car high)))
		    `((<= (the ,base ,n-object) ,high))))))))

#!+negative-zero-is-not-zero
(defun transform-numeric-bound-test (n-object type base)
  (declare (type numeric-type type))
  (let ((low (numeric-type-low type))
	(high (numeric-type-high type))
	(float-type-p (csubtypep type (specifier-type 'float)))
	(x (gensym))
	(y (gensym)))
    `(locally
       (declare (optimize (safety 0)))
       (and ,@(when low
		(if (consp low)
		    `((let ((,x (the ,base ,n-object))
			    (,y ,(car low)))
			,(if (not float-type-p)
			    `(> ,x ,y)
			    `(if (and (zerop ,x) (zerop ,y))
				 (> (float-sign ,x) (float-sign ,y))
				 (> ,x ,y)))))
		    `((let ((,x (the ,base ,n-object))
			    (,y ,low))
			,(if (not float-type-p)
			    `(>= ,x ,y)
			    `(if (and (zerop ,x) (zerop ,y))
				 (>= (float-sign ,x) (float-sign ,y))
				 (>= ,x ,y)))))))
	    ,@(when high
		(if (consp high)
		    `((let ((,x (the ,base ,n-object))
			    (,y ,(car high)))
			,(if (not float-type-p)
			     `(< ,x ,y)
			     `(if (and (zerop ,x) (zerop ,y))
				  (< (float-sign ,x) (float-sign ,y))
				  (< ,x ,y)))))
		    `((let ((,x (the ,base ,n-object))
			    (,y ,high))
			,(if (not float-type-p)
			     `(<= ,x ,y)
			     `(if (and (zerop ,x) (zerop ,y))
				  (<= (float-sign ,x) (float-sign ,y))
				  (<= ,x ,y)))))))))))

;;; Do source transformation of a test of a known numeric type. We can
;;; assume that the type doesn't have a corresponding predicate, since
;;; those types have already been picked off. In particular, CLASS
;;; must be specified, since it is unspecified only in NUMBER and
;;; COMPLEX. Similarly, we assume that COMPLEXP is always specified.
;;;
;;; For non-complex types, we just test that the number belongs to the
;;; base type, and then test that it is in bounds. When CLASS is
;;; INTEGER, we check to see whether the range is no bigger than
;;; FIXNUM. If so, we check for FIXNUM instead of INTEGER. This allows
;;; us to use fixnum comparison to test the bounds.
;;;
;;; For complex types, we must test for complex, then do the above on
;;; both the real and imaginary parts. When CLASS is float, we need
;;; only check the type of the realpart, since the format of the
;;; realpart and the imagpart must be the same.
(defun source-transform-numeric-typep (object type)
  (let* ((class (numeric-type-class type))
	 (base (ecase class
		 (integer (containing-integer-type type))
		 (rational 'rational)
		 (float (or (numeric-type-format type) 'float))
		 ((nil) 'real))))
    (once-only ((n-object object))
      (ecase (numeric-type-complexp type)
	(:real
	 `(and (typep ,n-object ',base)
	       ,(transform-numeric-bound-test n-object type base)))
	(:complex
	 `(and (complexp ,n-object)
	       ,(once-only ((n-real `(realpart (the complex ,n-object)))
			    (n-imag `(imagpart (the complex ,n-object))))
		  `(progn
		     ,n-imag ; ignorable
		     (and (typep ,n-real ',base)
			  ,@(when (eq class 'integer)
			      `((typep ,n-imag ',base)))
			  ,(transform-numeric-bound-test n-real type base)
			  ,(transform-numeric-bound-test n-imag type
							 base))))))))))

;;; Do the source transformation for a test of a hairy type. AND,
;;; SATISFIES and NOT are converted into the obvious code. We convert
;;; unknown types to %TYPEP, emitting an efficiency note if
;;; appropriate.
(defun source-transform-hairy-typep (object type)
  (declare (type hairy-type type))
  (let ((spec (hairy-type-specifier type)))
    (cond ((unknown-type-p type)
	   (when (policy nil (> speed inhibit-warnings))
	     (compiler-note "can't open-code test of unknown type ~S"
			    (type-specifier type)))
	   `(%typep ,object ',spec))
	  (t
	   (ecase (first spec)
	     (satisfies `(if (funcall #',(second spec) ,object) t nil))
	     ((not and)
	      (once-only ((n-obj object))
		`(,(first spec) ,@(mapcar #'(lambda (x)
					      `(typep ,n-obj ',x))
					  (rest spec))))))))))

;;; Do source transformation for TYPEP of a known union type. If a
;;; union type contains LIST, then we pull that out and make it into a
;;; single LISTP call. Note that if SYMBOL is in the union, then LIST
;;; will be a subtype even without there being any (member NIL). We
;;; just drop through to the general code in this case, rather than
;;; trying to optimize it.
(defun source-transform-union-typep (object type)
  (let* ((types (union-type-types type))
	 (ltype (specifier-type 'list))
	 (mtype (find-if #'member-type-p types)))
    (cond ((and mtype (csubtypep ltype type))
	   (let ((members (member-type-members mtype)))
	     (once-only ((n-obj object))
	       `(if (listp ,n-obj)
		    t
		    (typep ,n-obj
			   '(or ,@(mapcar #'type-specifier
					  (remove (specifier-type 'cons)
						  (remove mtype types)))
				(member ,@(remove nil members))))))))
	  (t
	   (once-only ((n-obj object))
	     `(or ,@(mapcar #'(lambda (x)
				`(typep ,n-obj ',(type-specifier x)))
			    types)))))))

;;; Do source transformation for TYPEP of a known intersection type.
(defun source-transform-intersection-typep (object type)
  ;; FIXME: This is just a placeholder; we should define a better
  ;; version by analogy with SOURCE-TRANSFORM-UNION-TYPEP.
  (declare (ignore object type))
  nil)

;;; If necessary recurse to check the cons type.
(defun source-transform-cons-typep (object type)
  (let* ((car-type (cons-type-car-type type))
	 (cdr-type (cons-type-cdr-type type)))
    (let ((car-test-p (not (or (type= car-type *wild-type*)
			       (type= car-type (specifier-type t)))))
	  (cdr-test-p (not (or (type= cdr-type *wild-type*)
			       (type= cdr-type (specifier-type t))))))
      (if (and (not car-test-p) (not cdr-test-p))
	  `(consp ,object)
	  (once-only ((n-obj object))
	    `(and (consp ,n-obj)
		  ,@(if car-test-p
			`((typep (car ,n-obj)
				 ',(type-specifier car-type))))
		  ,@(if cdr-test-p
			`((typep (cdr ,n-obj)
				 ',(type-specifier cdr-type))))))))))
 
;;; Return the predicate and type from the most specific entry in
;;; *TYPE-PREDICATES* that is a supertype of TYPE.
(defun find-supertype-predicate (type)
  (declare (type ctype type))
  (let ((res nil)
	(res-type nil))
    (dolist (x *backend-type-predicates*)
      (let ((stype (car x)))
	(when (and (csubtypep type stype)
		   (or (not res-type)
		       (csubtypep stype res-type)))
	  (setq res-type stype)
	  (setq res (cdr x)))))
    (values res res-type)))

;;; Return forms to test that OBJ has the rank and dimensions
;;; specified by TYPE, where STYPE is the type we have checked against
;;; (which is the same but for dimensions.)
(defun test-array-dimensions (obj type stype)
  (declare (type array-type type stype))
  (let ((obj `(truly-the ,(type-specifier stype) ,obj))
	(dims (array-type-dimensions type)))
    (unless (eq dims '*)
      (collect ((res))
	(when (eq (array-type-dimensions stype) '*)
	  (res `(= (array-rank ,obj) ,(length dims))))
	(do ((i 0 (1+ i))
	     (dim dims (cdr dim)))
	    ((null dim))
	  (let ((dim (car dim)))
	    (unless (eq dim '*)
	      (res `(= (array-dimension ,obj ,i) ,dim)))))
	(res)))))

;;; If we can find a type predicate that tests for the type w/o
;;; dimensions, then use that predicate and test for dimensions.
;;; Otherwise, just do %TYPEP.
(defun source-transform-array-typep (obj type)
  (multiple-value-bind (pred stype) (find-supertype-predicate type)
    (if (and (array-type-p stype)
	     ;; (If the element type hasn't been defined yet, it's
	     ;; not safe to assume here that it will eventually
	     ;; have (UPGRADED-ARRAY-ELEMENT-TYPE type)=T, so punt.)
	     (not (unknown-type-p (array-type-element-type type)))
	     (type= (array-type-specialized-element-type stype)
		    (array-type-specialized-element-type type))
	     (eq (array-type-complexp stype) (array-type-complexp type)))
	(once-only ((n-obj obj))
	  `(and (,pred ,n-obj)
		,@(test-array-dimensions n-obj type stype)))
	`(%typep ,obj ',(type-specifier type)))))

;;; Transform a type test against some instance type. The type test is
;;; flushed if the result is known at compile time. If not properly
;;; named, error. If sealed and has no subclasses, just test for
;;; layout-EQ. If a structure then test for layout-EQ and then a
;;; general test based on layout-inherits. If safety is important,
;;; then we also check whether the layout for the object is invalid
;;; and signal an error if so. Otherwise, look up the indirect
;;; class-cell and call CLASS-CELL-TYPEP at runtime.
;;;
;;; KLUDGE: The :WHEN :BOTH option here is probably a suboptimal
;;; solution to the problem of %INSTANCE-TYPEP forms in byte compiled
;;; code; it'd probably be better just to have %INSTANCE-TYPEP forms
;;; never be generated in byte compiled code, or maybe to have a DEFUN
;;; %INSTANCE-TYPEP somewhere to handle them if they are. But it's not
;;; terribly important because mostly, %INSTANCE-TYPEP forms *aren't*
;;; generated in byte compiled code. (As of sbcl-0.6.5, they could
;;; sometimes be generated when byte compiling inline functions, but
;;; it's quite uncommon.) -- WHN 20000523
(deftransform %instance-typep ((object spec) * * :when :both)
  (assert (constant-continuation-p spec))
  (let* ((spec (continuation-value spec))
	 (class (specifier-type spec))
	 (name (sb!xc:class-name class))
	 (otype (continuation-type object))
	 (layout (let ((res (info :type :compiler-layout name)))
		   (if (and res (not (layout-invalid res)))
		       res
		       nil))))
    (/noshow "entering DEFTRANSFORM %INSTANCE-TYPEP" otype spec class name layout)
    (cond
      ;; Flush tests whose result is known at compile time.
      ((not (types-intersect otype class))
       (/noshow "flushing constant NIL")
       nil)
      ((csubtypep otype class)
       (/noshow "flushing constant T")
       t)
      ;; If not properly named, error.
      ((not (and name (eq (sb!xc:find-class name) class)))
       (compiler-error "can't compile TYPEP of anonymous or undefined ~
			class:~%  ~S"
		       class))
      (t
       ;; Otherwise transform the type test.
       (multiple-value-bind (pred get-layout)
	   (cond
	     ((csubtypep class (specifier-type 'funcallable-instance))
	      (values 'funcallable-instance-p '%funcallable-instance-layout))
	     ((csubtypep class (specifier-type 'instance))
	      (values '%instancep '%instance-layout))
	     (t
	      (values '(lambda (x) (declare (ignore x)) t) 'layout-of)))
	 (/noshow pred get-layout)
	 (cond
	   ((and (eq (class-state class) :sealed) layout
		 (not (class-subclasses class)))
	    ;; Sealed and has no subclasses.
	    (/noshow "sealed and has no subclasses")
	    (let ((n-layout (gensym)))
	      `(and (,pred object)
		    (let ((,n-layout (,get-layout object)))
		      ,@(when (policy nil (>= safety speed))
			      `((when (layout-invalid ,n-layout)
				  (%layout-invalid-error object ',layout))))
		      (eq ,n-layout ',layout)))))
	   ((and (typep class 'basic-structure-class) layout)
	    (/noshow "structure type tests; hierarchical layout depths")
	    ;; structure type tests; hierarchical layout depths
	    (let ((depthoid (layout-depthoid layout))
		  (n-layout (gensym)))
	      `(and (,pred object)
		    (let ((,n-layout (,get-layout object)))
		      ,@(when (policy nil (>= safety speed))
			      `((when (layout-invalid ,n-layout)
				  (%layout-invalid-error object ',layout))))
		      (if (eq ,n-layout ',layout)
			  t
			  (and (> (layout-depthoid ,n-layout)
				  ,depthoid)
			       (locally (declare (optimize (safety 0)))
				 (eq (svref (layout-inherits ,n-layout)
					    ,depthoid)
				     ',layout))))))))
	   (t
	    (/noshow "default case -- ,PRED and CLASS-CELL-TYPEP")
	    `(and (,pred object)
		  (class-cell-typep (,get-layout object)
				    ',(find-class-cell name)
				    object)))))))))

;;; If the specifier argument is a quoted constant, then we consider
;;; converting into a simple predicate or other stuff. If the type is
;;; constant, but we can't transform the call, then we convert to
;;; %TYPEP. We only pass when the type is non-constant. This allows us
;;; to recognize between calls that might later be transformed
;;; successfully when a constant type is discovered. We don't give an
;;; efficiency note when we pass, since the IR1 transform will give
;;; one if necessary and appropriate.
;;;
;;; If the type is TYPE= to a type that has a predicate, then expand
;;; to that predicate. Otherwise, we dispatch off of the type's type.
;;; These transformations can increase space, but it is hard to tell
;;; when, so we ignore policy and always do them. When byte-compiling,
;;; we only do transforms that have potential for control
;;; simplification. Instance type tests are converted to
;;; %INSTANCE-TYPEP to allow type propagation.
(def-source-transform typep (object spec)
  (if (and (consp spec) (eq (car spec) 'quote))
      (let ((type (specifier-type (cadr spec))))
	(or (let ((pred (cdr (assoc type *backend-type-predicates*
				    :test #'type=))))
	      (when pred `(,pred ,object)))
	    (typecase type
	      (hairy-type
	       (source-transform-hairy-typep object type))
	      (union-type
	       (source-transform-union-typep object type))
	      (intersection-type
	       (source-transform-intersection-typep object type))
	      (member-type
	       `(member ,object ',(member-type-members type)))
	      (args-type
	       (compiler-warning "illegal type specifier for TYPEP: ~S"
				 (cadr spec))
	       `(%typep ,object ,spec))
	      (t nil))
	    (and (not (byte-compiling))
		 (typecase type
		   (numeric-type
		    (source-transform-numeric-typep object type))
		   (sb!xc:class
		    `(%instance-typep ,object ,spec))
		   (array-type
		    (source-transform-array-typep object type))
		   (cons-type
		    (source-transform-cons-typep object type))
		   (t nil)))
	    `(%typep ,object ,spec)))
      (values nil t)))

;;;; coercion

;;; old working version
(deftransform coerce ((x type) (* *) * :when :both)
  (unless (constant-continuation-p type)
    (give-up-ir1-transform))
  (let ((tspec (specifier-type (continuation-value type))))
    (if (csubtypep (continuation-type x) tspec)
	'x
	`(the ,(continuation-value type)
	      ,(cond ((csubtypep tspec (specifier-type 'double-float))
		      '(%double-float x))	
		     ;; FIXME: If LONG-FLOAT is to be supported, we
		     ;; need to pick it off here before falling through
		     ;; to %SINGLE-FLOAT.
		     ((csubtypep tspec (specifier-type 'float))
		      '(%single-float x))
		     (t
		      (give-up-ir1-transform)))))))

;;; KLUDGE: new broken version -- 20000504
;;; FIXME: should be fixed or deleted
#+nil
(deftransform coerce ((x type) (* *) * :when :both)
  (unless (constant-continuation-p type)
    (give-up-ir1-transform))
  (let ((tspec (specifier-type (continuation-value type))))
    (if (csubtypep (continuation-type x) tspec)
	'x
	`(if #+nil (typep x type) #-nil nil
	     x
	     (the ,(continuation-value type)
		  ,(cond ((csubtypep tspec (specifier-type 'double-float))
			  '(%double-float x))	
			 ;; FIXME: If LONG-FLOAT is to be supported,
			 ;; we need to pick it off here before falling
			 ;; through to %SINGLE-FLOAT.
			 ((csubtypep tspec (specifier-type 'float))
			  '(%single-float x))
			 #+nil
			 ((csubtypep tspec (specifier-type 'list))
			  '(coerce-to-list x))
			 #+nil
			 ((csubtypep tspec (specifier-type 'string))
			  '(coerce-to-simple-string x))
			 #+nil
			 ((csubtypep tspec (specifier-type 'bit-vector))
			  '(coerce-to-bit-vector x))
			 #+nil
			 ((csubtypep tspec (specifier-type 'vector))
			  '(coerce-to-vector x type))
			 (t
			  (give-up-ir1-transform))))))))

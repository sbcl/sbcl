;;;; stuff related to the TYPE-CLASS structure

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

(defvar *type-classes*)
(!cold-init-forms
  (unless (boundp '*type-classes*) ; FIXME: How could this be bound?
    (setq *type-classes* (make-hash-table :test 'eq))))

(defun type-class-or-lose (name)
  (or (gethash name *type-classes*)
      (error "~S is not a defined type class." name)))

(defun must-supply-this (&rest foo)
  (/show0 "failing in MUST-SUPPLY-THIS")
  (error "missing type method for ~S" foo))

;;; A TYPE-CLASS object represents the "kind" of a type. It mainly contains
;;; functions which are methods on that kind of type, but is also used in EQ
;;; comparisons to determined if two types have the "same kind".
(def!struct (type-class
	     #-no-ansi-print-object
	     (:print-object (lambda (x stream)
			      (print-unreadable-object (x stream :type t)
				(prin1 (type-class-name x) stream)))))
  ;; the name of this type class (used to resolve references at load time)
  (name nil :type symbol) ; FIXME: should perhaps be REQUIRED-ARGUMENT?
  ;; Dyadic type methods. If the classes of the two types are EQ, then
  ;; we call the SIMPLE-xxx method. If the classes are not EQ, and
  ;; either type's class has a COMPLEX-xxx method, then we call it.
  ;;
  ;; Although it is undefined which method will get precedence when
  ;; both types have a complex method, the complex method can assume
  ;; that the second arg always is in its class, and the first always
  ;; is not. The arguments to commutative operations will be swapped
  ;; if the first argument has a complex method.
  ;;
  ;; Since SUBTYPEP is not commutative, we have two complex methods.
  ;; The ARG1 method is only called when the first argument is in its
  ;; class, and the ARG2 method is only called when called when the
  ;; second type is. If either is specified, both must be.
  (simple-subtypep #'must-supply-this :type function)
  (complex-subtypep-arg1 nil :type (or function null))
  (complex-subtypep-arg2 nil :type (or function null))
  ;; SIMPLE-UNION combines two types of the same class into a single
  ;; type of that class. If the result is a two-type union, then
  ;; return NIL. VANILLA-UNION returns whichever argument is a
  ;; supertype of the other, or NIL.
  (simple-union #'vanilla-union :type function)
  (complex-union nil :type (or function null))
  ;; The default intersection methods assume that if one type is a
  ;; subtype of the other, then that type is the intersection.
  (simple-intersection #'vanilla-intersection :type function)
  (complex-intersection nil :type (or function null))
  (simple-= #'must-supply-this :type function)
  (complex-= nil :type (or function null))
  ;; a function which returns a Common Lisp type specifier
  ;; representing this type
  (unparse #'must-supply-this :type function)

  #|
  Not used, and not really right. Probably we want a TYPE= alist for the
  unary operations, since there are lots of interesting unary predicates that
  aren't equivalent to an entire class
  ;; Names of functions used for testing the type of objects in this type
  ;; class. UNARY-PREDICATE takes just the object, whereas PREDICATE gets
  ;; passed both the object and the CTYPE. Normally one or the other will be
  ;; supplied for any type that can be passed to TYPEP; there is no point in
  ;; supplying both.
  (unary-typep nil :type (or symbol null))
  (typep nil :type (or symbol null))
  ;; Like TYPEP, UNARY-TYPEP except these functions coerce objects to this
  ;; type.
  (unary-coerce nil :type (or symbol null))
  (coerce :type (or symbol null))
  |#
  )

(eval-when (:compile-toplevel :load-toplevel :execute)

;;; Copy TYPE-CLASS object X, using only operations which will work early in
;;; cold load. (COPY-STRUCTURE won't work early in cold load, because it needs
;;; RAW-INDEX and RAW-LENGTH information from LAYOUT-INFO, and LAYOUT-INFO
;;; isn't initialized early in cold load.)
;;;
;;; FIXME: It's nasty having to maintain this hand-written copy function. And
;;; it seems intrinsically dain-bramaged to have RAW-INDEX and RAW-LENGTH in
;;; LAYOUT-INFO instead of directly in LAYOUT. We should fix this: * Move
;;; RAW-INDEX and RAW-LENGTH slots into LAYOUT itself. * Rewrite the various
;;; CHECK-LAYOUT-related functions so that they check RAW-INDEX and RAW-LENGTH
;;; too. * Remove this special hacked copy function, just use COPY-STRUCTURE
;;; instead. (For even more improvement, it'd be good to move the raw slots
;;; into the same object as the ordinary slots, instead of having the
;;; unfortunate extra level of indirection. But that'd probably require a lot
;;; of work, including updating the garbage collector to understand it.)
(declaim (ftype (function (type-class) type-class) copy-type-class-coldly))
(defun copy-type-class-coldly (x)
  ;; KLUDGE: If the slots of TYPE-CLASS ever change, the slots here will have
  ;; to be hand-tweaked to match. -- WHN 19991021
  (make-type-class :name (type-class-name x)
		   :simple-subtypep       (type-class-simple-subtypep x)
		   :complex-subtypep-arg1 (type-class-complex-subtypep-arg1 x)
		   :complex-subtypep-arg2 (type-class-complex-subtypep-arg2 x)
		   :simple-union	  (type-class-simple-union x)
		   :complex-union	 (type-class-complex-union x)
		   :simple-intersection   (type-class-simple-intersection x)
		   :complex-intersection  (type-class-complex-intersection x)
		   :simple-=	      (type-class-simple-= x)
		   :complex-=	     (type-class-complex-= x)
		   :unparse	       (type-class-unparse x)))

;;; KLUDGE: If the slots of TYPE-CLASS ever change, the slots here
;;; will have to be tweaked to match. -- WHN 19991021
(defparameter *type-class-function-slots*
  '((:simple-subtypep . type-class-simple-subtypep)
    (:complex-subtypep-arg1 . type-class-complex-subtypep-arg1)
    (:complex-subtypep-arg2 . type-class-complex-subtypep-arg2)
    (:simple-union . type-class-simple-union)
    (:complex-union . type-class-complex-union)
    (:simple-intersection . type-class-simple-intersection)
    (:complex-intersection . type-class-complex-intersection)
    (:simple-= . type-class-simple-=)
    (:complex-= . type-class-complex-=)
    (:unparse . type-class-unparse)))

(defun class-function-slot-or-lose (name)
  (or (cdr (assoc name *type-class-function-slots*))
      (error "~S is not a defined type class method." name)))
;;; FIXME: This seems to be called at runtime by cold init code.
;;; Make sure that it's not being called at runtime anywhere but
;;; one-time toplevel initialization code.

) ; EVAL-WHEN

(defmacro !define-type-method ((class method &rest more-methods)
			       lambda-list &body body)
  (let ((name (symbolicate class "-" method "-TYPE-METHOD")))
    `(progn
       (defun ,name ,lambda-list
	 ,@body)
       (!cold-init-forms
	,@(mapcar (lambda (method)
		    `(setf (,(class-function-slot-or-lose method)
			    (type-class-or-lose ',class))
			   #',name))
		  (cons method more-methods)))
       ',name)))

(defmacro !define-type-class (name &key inherits)
  `(!cold-init-forms
     ,(once-only ((n-class (if inherits
			       `(copy-type-class-coldly (type-class-or-lose
							 ',inherits))
			       '(make-type-class))))
	`(progn
	   (setf (type-class-name ,n-class) ',name)
	   (setf (gethash ',name *type-classes*) ,n-class)
	   ',name))))

;;; Invoke a type method on TYPE1 and TYPE2. If the two types have the
;;; same class, invoke the simple method. Otherwise, invoke any
;;; complex method. If there isn't a distinct COMPLEX-ARG1 method,
;;; then swap the arguments when calling TYPE1's method. If no
;;; applicable method, return DEFAULT.
(defmacro !invoke-type-method (simple complex-arg2 type1 type2 &key
				      (default '(values nil t))
				      (complex-arg1 :foo complex-arg1-p))
  (declare (type keyword simple complex-arg1 complex-arg2))
  `(multiple-value-bind (result-a result-b valid-p)
       (%invoke-type-method ',(class-function-slot-or-lose simple)
			    ',(class-function-slot-or-lose
			       (if complex-arg1-p
				 complex-arg1
				 complex-arg2))
			    ',(class-function-slot-or-lose complex-arg2)
			    ,complex-arg1-p
			    ,type1
			    ,type2)
     (if valid-p
       (values result-a result-b)
       ,default)))

;;; most of the implementation of !INVOKE-TYPE-METHOD
;;;
;;; KLUDGE: This function must be INLINE in order for cold init to
;;; work, because the first three arguments are TYPE-CLASS structure
;;; accessor functions whose calls have to be compiled inline in order
;;; to work in calls to this function early in cold init. So don't
;;; conditionalize this INLINE declaration with #!-SB-FLUID or
;;; anything, unless you also rearrange things to cause the full
;;; function definitions of the relevant structure accessors to be
;;; available sufficiently early in cold init. -- WHN 19991015
(declaim (inline %invoke-type-method))
(defun %invoke-type-method (simple cslot1 cslot2 complex-arg1-p type1 type2)
  (declare (type symbol simple cslot1 cslot2))
  (multiple-value-bind (result-a result-b)
      (let ((class1 (type-class-info type1))
	    (class2 (type-class-info type2)))
	(if (eq class1 class2)
	  (funcall (funcall simple class1) type1 type2)
	  (let ((complex2 (funcall cslot2 class2)))
	    (if complex2
	      (funcall complex2 type1 type2)
	      (let ((complex1 (funcall cslot1 class1)))
		(if complex1
		  (if complex-arg1-p
		    (funcall complex1 type1 type2)
		    (funcall complex1 type2 type1))
		  ;; No meaningful result was found: the caller should
		  ;; use the default value instead.
		  (return-from %invoke-type-method (values nil nil nil))))))))
    ;; If we get to here (without breaking out by calling RETURN-FROM)
    ;; then a meaningful result was found, and we return it.
    (values result-a result-b t)))

(!defun-from-collected-cold-init-forms !type-class-cold-init)

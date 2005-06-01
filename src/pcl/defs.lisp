;;;; This software is part of the SBCL system. See the README file for
;;;; more information.

;;;; This software is derived from software originally released by Xerox
;;;; Corporation. Copyright and release statements follow. Later modifications
;;;; to the software are in the public domain and are provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for more
;;;; information.

;;;; copyright information from original PCL sources:
;;;;
;;;; Copyright (c) 1985, 1986, 1987, 1988, 1989, 1990 Xerox Corporation.
;;;; All rights reserved.
;;;;
;;;; Use and copying of this software and preparation of derivative works based
;;;; upon this software are permitted. Any distribution of this software or
;;;; derivative works must comply with all applicable United States export
;;;; control laws.
;;;;
;;;; This software is made available AS IS, and Xerox Corporation makes no
;;;; warranty about the software, its performance or its conformity to any
;;;; specification.

(in-package "SB-PCL")

;;; (These are left over from the days when PCL was an add-on package
;;; for a pre-CLOS Common Lisp. They shouldn't happen in a normal
;;; build, of course, but they might happen if someone is experimenting
;;; and debugging, and it's probably worth complaining if they do,
;;; so we've left 'em in.)
(when (eq *boot-state* 'complete)
  (error "Trying to load (or compile) PCL in an environment in which it~%~
	  has already been loaded. This doesn't work, you will have to~%~
	  get a fresh lisp (reboot) and then load PCL."))
(when *boot-state*
  (cerror "Try loading (or compiling) PCL anyways."
	  "Trying to load (or compile) PCL in an environment in which it~%~
	   has already been partially loaded. This may not work, you may~%~
	   need to get a fresh lisp (reboot) and then load PCL."))

;;; comments from CMU CL version of PCL:
;;;     This is like fdefinition on the Lispm. If Common Lisp had
;;;   something like function specs I wouldn't need this. On the other
;;;   hand, I don't like the way this really works so maybe function
;;;   specs aren't really right either?
;;;     I also don't understand the real implications of a Lisp-1 on this
;;;   sort of thing. Certainly some of the lossage in all of this is
;;;   because these SPECs name global definitions.
;;;     Note that this implementation is set up so that an implementation
;;;   which has a 'real' function spec mechanism can use that instead
;;;   and in that way get rid of setf generic function names.
(defmacro parse-gspec (spec
		       (non-setf-var . non-setf-case))
  `(let ((,non-setf-var ,spec)) ,@non-setf-case))

;;; If symbol names a function which is traced, return the untraced
;;; definition. This lets us get at the generic function object even
;;; when it is traced.
(defun unencapsulated-fdefinition (symbol)
  (fdefinition symbol))

;;; If symbol names a function which is traced, redefine the `real'
;;; definition without affecting the trace.
(defun fdefine-carefully (name new-definition)
  (progn
    (sb-c::note-name-defined name :function)
    new-definition)
  (setf (fdefinition name) new-definition))

(defun gboundp (spec)
  (parse-gspec spec
    (name (fboundp name))))

(defun gmakunbound (spec)
  (parse-gspec spec
    (name (fmakunbound name))))

(defun gdefinition (spec)
  (parse-gspec spec
    (name (unencapsulated-fdefinition name))))

(defun (setf gdefinition) (new-value spec)
  (parse-gspec spec
    (name (fdefine-carefully name new-value))))

;;;; type specifier hackery

;;; internal to this file
(defun coerce-to-class (class &optional make-forward-referenced-class-p)
  (if (symbolp class)
      (or (find-class class (not make-forward-referenced-class-p))
	  (ensure-class class))
      class))

;;; interface
(defun specializer-from-type (type &aux args)
  (when (consp type)
    (setq args (cdr type) type (car type)))
  (cond ((symbolp type)
	 (or (and (null args) (find-class type))
	     (ecase type
	       (class    (coerce-to-class (car args)))
	       (prototype (make-instance 'class-prototype-specializer
					 :object (coerce-to-class (car args))))
	       (class-eq (class-eq-specializer (coerce-to-class (car args))))
	       (eql      (intern-eql-specializer (car args))))))
	;; FIXME: do we still need this?
	((and (null args) (typep type 'classoid))
	 (or (classoid-pcl-class type)
	     (ensure-non-standard-class (classoid-name type))))
	((specializerp type) type)))

;;; interface
(defun type-from-specializer (specl)
  (cond ((eq specl t)
	 t)
	((consp specl)
	 (unless (member (car specl) '(class prototype class-eq eql))
	   (error "~S is not a legal specializer type." specl))
	 specl)
	((progn
	   (when (symbolp specl)
	     ;;maybe (or (find-class specl nil) (ensure-class specl)) instead?
	     (setq specl (find-class specl)))
	   (or (not (eq *boot-state* 'complete))
	       (specializerp specl)))
	 (specializer-type specl))
	(t
	 (error "~S is neither a type nor a specializer." specl))))

(defun type-class (type)
  (declare (special *the-class-t*))
  (setq type (type-from-specializer type))
  (if (atom type)
      (if (eq type t)
	  *the-class-t*
	  (error "bad argument to TYPE-CLASS"))
      (case (car type)
	(eql (class-of (cadr type)))
	(prototype (class-of (cadr type))) ;?
	(class-eq (cadr type))
	(class (cadr type)))))

(defun class-eq-type (class)
  (specializer-type (class-eq-specializer class)))

;;; internal to this file..
;;;
;;; These functions are a pale imitation of their namesake. They accept
;;; class objects or types where they should.
(defun *normalize-type (type)
  (cond ((consp type)
	 (if (member (car type) '(not and or))
	     `(,(car type) ,@(mapcar #'*normalize-type (cdr type)))
	     (if (null (cdr type))
		 (*normalize-type (car type))
		 type)))
	((symbolp type)
	 (let ((class (find-class type nil)))
	   (if class
	       (let ((type (specializer-type class)))
		 (if (listp type) type `(,type)))
	       `(,type))))
	((or (not (eq *boot-state* 'complete))
	     (specializerp type))
	 (specializer-type type))
	(t
	 (error "~S is not a type." type))))

;;; internal to this file...
(defun convert-to-system-type (type)
  (case (car type)
    ((not and or) `(,(car type) ,@(mapcar #'convert-to-system-type
					  (cdr type))))
    ((class class-eq) ; class-eq is impossible to do right
     (layout-classoid (class-wrapper (cadr type))))
    (eql type)
    (t (if (null (cdr type))
	   (car type)
	   type))))

;;; Writing the missing NOT and AND clauses will improve the quality
;;; of code generated by GENERATE-DISCRIMINATION-NET, but calling
;;; SUBTYPEP in place of just returning (VALUES NIL NIL) can be very
;;; slow. *SUBTYPEP is used by PCL itself, and must be fast.
;;;
;;; FIXME: SB-KERNEL has fast-and-not-quite-precise type code for use
;;; in the compiler. Could we share some of it here? 
(defun *subtypep (type1 type2)
  (if (equal type1 type2)
      (values t t)
      (if (eq *boot-state* 'early)
	  (values (eq type1 type2) t)
	  (let ((*in-precompute-effective-methods-p* t))
	    (declare (special *in-precompute-effective-methods-p*))
	    ;; FIXME: *IN-PRECOMPUTE-EFFECTIVE-METHODS-P* is not a
	    ;; good name. It changes the way
	    ;; CLASS-APPLICABLE-USING-CLASS-P works.
	    (setq type1 (*normalize-type type1))
	    (setq type2 (*normalize-type type2))
	    (case (car type2)
	      (not
	       (values nil nil)) ; XXX We should improve this.
	      (and
	       (values nil nil)) ; XXX We should improve this.
	      ((eql wrapper-eq class-eq class)
	       (multiple-value-bind (app-p maybe-app-p)
		   (specializer-applicable-using-type-p type2 type1)
		 (values app-p (or app-p (not maybe-app-p)))))
	      (t
	       (subtypep (convert-to-system-type type1)
			 (convert-to-system-type type2))))))))

(defvar *built-in-class-symbols* ())
(defvar *built-in-wrapper-symbols* ())

(defun get-built-in-class-symbol (class-name)
  (or (cadr (assq class-name *built-in-class-symbols*))
      (let ((symbol (make-class-symbol class-name)))
	(push (list class-name symbol) *built-in-class-symbols*)
	symbol)))

(defun get-built-in-wrapper-symbol (class-name)
  (or (cadr (assq class-name *built-in-wrapper-symbols*))
      (let ((symbol (make-wrapper-symbol class-name)))
	(push (list class-name symbol) *built-in-wrapper-symbols*)
	symbol)))

(pushnew '%class *var-declarations*)
(pushnew '%variable-rebinding *var-declarations*)

(defun variable-class (var env)
  (caddr (var-declaration 'class var env)))

(defvar *name->class->slotd-table* (make-hash-table))

(defvar *standard-method-combination*)

(defun make-class-predicate-name (name)
  (list 'class-predicate name))
  
(defun plist-value (object name)
  (getf (object-plist object) name))

(defun (setf plist-value) (new-value object name)
  (if new-value
      (setf (getf (object-plist object) name) new-value)
      (progn
	(remf (object-plist object) name)
	nil)))

;;;; built-in classes

;;; Grovel over SB-KERNEL::*BUILT-IN-CLASSES* in order to set
;;; SB-PCL:*BUILT-IN-CLASSES*.
(/show "about to set up SB-PCL::*BUILT-IN-CLASSES*")
(defvar *built-in-classes*
  (labels ((direct-supers (class)
	     (/noshow "entering DIRECT-SUPERS" (classoid-name class))
	     (if (typep class 'built-in-classoid)
		 (built-in-classoid-direct-superclasses class)
		 (let ((inherits (layout-inherits
				  (classoid-layout class))))
		   (/noshow inherits)
		   (list (svref inherits (1- (length inherits)))))))
	   (direct-subs (class)
	     (/noshow "entering DIRECT-SUBS" (classoid-name class))
	     (collect ((res))
	       (let ((subs (classoid-subclasses class)))
		 (/noshow subs)
		 (when subs
		   (dohash (sub v subs)
		     (declare (ignore v))
		     (/noshow sub)
		     (when (member class (direct-supers sub))
		       (res sub)))))
	       (res))))
    (mapcar (lambda (kernel-bic-entry)
	      (/noshow "setting up" kernel-bic-entry)
	      (let* ((name (car kernel-bic-entry))
		     (class (find-classoid name))
		     (prototype-form
		      (getf (cdr kernel-bic-entry) :prototype-form)))
		(/noshow name class)
		`(,name
		  ,(mapcar #'classoid-name (direct-supers class))
		  ,(mapcar #'classoid-name (direct-subs class))
		  ,(map 'list
			(lambda (x)
			  (classoid-name
			   (layout-classoid x)))
			(reverse
			 (layout-inherits
			  (classoid-layout class))))
		  ,(if prototype-form
		       (eval prototype-form)
		       ;; This is the default prototype value which
		       ;; was used, without explanation, by the CMU CL
		       ;; code we're derived from. Evidently it's safe
		       ;; in all relevant cases.
		       42))))
	    (remove-if (lambda (kernel-bic-entry)
			 (member (first kernel-bic-entry)
				 ;; I'm not sure why these are removed from
				 ;; the list, but that's what the original
				 ;; CMU CL code did. -- WHN 20000715
				 '(t instance
				     funcallable-instance
				     function stream 
				     file-stream string-stream)))
		       sb-kernel::*built-in-classes*))))
(/noshow "done setting up SB-PCL::*BUILT-IN-CLASSES*")

;;;; the classes that define the kernel of the metabraid

(defclass t () ()
  (:metaclass built-in-class))

(defclass instance (t) ()
  (:metaclass built-in-class))

(defclass function (t) ()
  (:metaclass built-in-class))

(defclass funcallable-instance (function) ()
  (:metaclass built-in-class))

(defclass stream (instance) ()
  (:metaclass built-in-class))

(defclass file-stream (stream) ()
  (:metaclass built-in-class))

(defclass string-stream (stream) ()
  (:metaclass built-in-class))

(defclass slot-object (t) ()
  (:metaclass slot-class))

(defclass condition (slot-object instance) ()
  (:metaclass condition-class))

(defclass structure-object (slot-object instance) ()
  (:metaclass structure-class))

(defstruct (dead-beef-structure-object
	    (:constructor |STRUCTURE-OBJECT class constructor|)
	    (:copier nil)))

(defclass std-object (slot-object) ()
  (:metaclass std-class))

(defclass standard-object (std-object instance) ())

(defclass funcallable-standard-object (std-object funcallable-instance)
  ()
  (:metaclass funcallable-standard-class))

(defclass specializer (standard-object)
  ((type
    :initform nil
    :reader specializer-type)))

(defclass definition-source-mixin (std-object)
  ((source
    :initform *load-pathname*
    :reader definition-source
    :initarg :definition-source))
  (:metaclass std-class))

(defclass plist-mixin (std-object)
  ((plist
    :initform ()
    :accessor object-plist))
  (:metaclass std-class))

(defclass dependent-update-mixin (plist-mixin)
  ()
  (:metaclass std-class))

;;; The class CLASS is a specified basic class. It is the common
;;; superclass of any kind of class. That is, any class that can be a
;;; metaclass must have the class CLASS in its class precedence list.
(defclass class (dependent-update-mixin
		 definition-source-mixin
		 specializer)
  ((name
    :initform nil
    :initarg  :name
    :accessor class-name)
   (class-eq-specializer
    :initform nil
    :reader class-eq-specializer)
   (direct-superclasses
    :initform ()
    :reader class-direct-superclasses)
   ;; Note: The (CLASS-)DIRECT-SUBCLASSES for STRUCTURE-CLASSes and
   ;; CONDITION-CLASSes are lazily computed whenever the subclass info
   ;; becomes available, i.e. when the PCL class is created.
   (direct-subclasses
    :initform ()
    :reader class-direct-subclasses)
   (direct-methods
    :initform (cons nil nil))
   (predicate-name
    :initform nil
    :reader class-predicate-name)
   (documentation
    :initform nil
    :initarg :documentation)
   (finalized-p
    :initform nil
    :reader class-finalized-p)))

(def!method make-load-form ((class class) &optional env)
  ;; FIXME: should we not instead pass ENV to FIND-CLASS?  Probably
  ;; doesn't matter while all our environments are the same...
  (declare (ignore env))
  (let ((name (class-name class)))
    (unless (and name (eq (find-class name nil) class))
      (error "~@<Can't use anonymous or undefined class as constant: ~S~:@>"
	     class))
    `(find-class ',name)))

;;; The class PCL-CLASS is an implementation-specific common
;;; superclass of all specified subclasses of the class CLASS.
(defclass pcl-class (class)
  ((class-precedence-list
    :reader class-precedence-list)
   ;; KLUDGE: see note in CPL-OR-NIL
   (cpl-available-p
    :reader cpl-available-p
    :initform nil)
   (can-precede-list
    :initform ()
    :reader class-can-precede-list)
   (incompatible-superclass-list
    :initform ()
    :accessor class-incompatible-superclass-list)
   (wrapper
    :initform nil
    :reader class-wrapper)
   (prototype
    :initform nil
    :reader class-prototype)))

(defclass slot-class (pcl-class)
  ((direct-slots
    :initform ()
    :accessor class-direct-slots)
   (slots
    :initform ()
    :accessor class-slots)))

;;; The class STD-CLASS is an implementation-specific common
;;; superclass of the classes STANDARD-CLASS and
;;; FUNCALLABLE-STANDARD-CLASS.
(defclass std-class (slot-class)
  ())

(defclass standard-class (std-class)
  ())

(defclass funcallable-standard-class (std-class)
  ())

(defclass forward-referenced-class (pcl-class) ())

(defclass built-in-class (pcl-class) ())

(defclass condition-class (slot-class) ())

(defclass structure-class (slot-class)
  ((defstruct-form
     :initform ()
     :accessor class-defstruct-form)
   (defstruct-constructor
     :initform nil
     :accessor class-defstruct-constructor)
   (from-defclass-p
    :initform nil
    :initarg :from-defclass-p)))

(defclass specializer-with-object (specializer) ())

(defclass exact-class-specializer (specializer) ())

(defclass class-eq-specializer (exact-class-specializer
				specializer-with-object)
  ((object :initarg :class
	   :reader specializer-class
	   :reader specializer-object)))

(defclass class-prototype-specializer (specializer-with-object)
  ((object :initarg :class
	   :reader specializer-class
	   :reader specializer-object)))

(defclass eql-specializer (exact-class-specializer specializer-with-object)
  ((object :initarg :object :reader specializer-object
	   :reader eql-specializer-object)))

(defvar *eql-specializer-table* (make-hash-table :test 'eql))

(defun intern-eql-specializer (object)
  (or (gethash object *eql-specializer-table*)
      (setf (gethash object *eql-specializer-table*)
	    (make-instance 'eql-specializer :object object))))

;;;; slot definitions

(defclass slot-definition (standard-object)
  ((name
    :initform nil
    :initarg :name
    :accessor slot-definition-name)
   (initform
    :initform nil
    :initarg :initform
    :accessor slot-definition-initform)
   (initfunction
    :initform nil
    :initarg :initfunction
    :accessor slot-definition-initfunction)
   (readers
    :initform nil
    :initarg :readers
    :accessor slot-definition-readers)
   (writers
    :initform nil
    :initarg :writers
    :accessor slot-definition-writers)
   (initargs
    :initform nil
    :initarg :initargs
    :accessor slot-definition-initargs)
   (type
    :initform t
    :initarg :type
    :accessor slot-definition-type)
   (documentation
    :initform nil
    :initarg :documentation)
   (class
    :initform nil
    :initarg :class
    :accessor slot-definition-class)))

(defclass standard-slot-definition (slot-definition)
  ((allocation
    :initform :instance
    :initarg :allocation
    :accessor slot-definition-allocation)
   (allocation-class
    :initform nil
    :initarg :allocation-class
    :accessor slot-definition-allocation-class)))

(defclass condition-slot-definition (slot-definition)
  ((allocation
    :initform :instance
    :initarg :allocation
    :accessor slot-definition-allocation)
   (allocation-class
    :initform nil
    :initarg :allocation-class
    :accessor slot-definition-allocation-class)))

(defclass structure-slot-definition (slot-definition)
  ((defstruct-accessor-symbol
     :initform nil
     :initarg :defstruct-accessor-symbol
     :accessor slot-definition-defstruct-accessor-symbol)
   (internal-reader-function
     :initform nil
     :initarg :internal-reader-function
     :accessor slot-definition-internal-reader-function)
   (internal-writer-function
     :initform nil
     :initarg :internal-writer-function
     :accessor slot-definition-internal-writer-function)))

(defclass direct-slot-definition (slot-definition)
  ())

(defclass effective-slot-definition (slot-definition)
  ((reader-function ; (lambda (object) ...)
    :accessor slot-definition-reader-function)
   (writer-function ; (lambda (new-value object) ...)
    :accessor slot-definition-writer-function)
   (boundp-function ; (lambda (object) ...)
    :accessor slot-definition-boundp-function)
   (accessor-flags
    :initform 0)))

(defclass standard-direct-slot-definition (standard-slot-definition
					   direct-slot-definition)
  ())

(defclass standard-effective-slot-definition (standard-slot-definition
					      effective-slot-definition)
  ((location ; nil, a fixnum, a cons: (slot-name . value)
    :initform nil
    :accessor slot-definition-location)))

(defclass condition-direct-slot-definition (condition-slot-definition
					    direct-slot-definition)
  ())

(defclass condition-effective-slot-definition (condition-slot-definition
					       effective-slot-definition)
  ())

(defclass structure-direct-slot-definition (structure-slot-definition
					    direct-slot-definition)
  ())

(defclass structure-effective-slot-definition (structure-slot-definition
					       effective-slot-definition)
  ())

(defclass method (standard-object) ())

(defclass standard-method (definition-source-mixin plist-mixin method)
  ((generic-function
    :initform nil	
    :accessor method-generic-function)
;;;     (qualifiers
;;;	:initform ()
;;;	:initarg  :qualifiers
;;;	:reader method-qualifiers)
   (specializers
    :initform ()
    :initarg  :specializers
    :reader method-specializers)
   (lambda-list
    :initform ()
    :initarg  :lambda-list
    :reader method-lambda-list)
   (function
    :initform nil
    :initarg :function)			;no writer
   (fast-function
    :initform nil
    :initarg :fast-function		;no writer
    :reader method-fast-function)
   (documentation
    :initform nil
    :initarg :documentation)))

(defclass standard-accessor-method (standard-method)
  ((slot-name :initform nil
	      :initarg :slot-name
	      :reader accessor-method-slot-name)
   (slot-definition :initform nil
		    :initarg :slot-definition
		    :reader accessor-method-slot-definition)))

(defclass standard-reader-method (standard-accessor-method) ())

(defclass standard-writer-method (standard-accessor-method) ())

(defclass standard-boundp-method (standard-accessor-method) ())

(defclass generic-function (dependent-update-mixin
			    definition-source-mixin
			    funcallable-standard-object)
  ((documentation
    :initform nil
    :initarg :documentation)
   ;; We need to make a distinction between the methods initially set
   ;; up by :METHOD options to DEFGENERIC and the ones set up later by
   ;; DEFMETHOD, because ANSI specifies that executing DEFGENERIC on
   ;; an already-DEFGENERICed function clears the methods set by the
   ;; previous DEFGENERIC, but not methods set by DEFMETHOD. (Making
   ;; this distinction seems a little kludgy, but it has the positive
   ;; effect of making it so that loading a file a.lisp containing
   ;; DEFGENERIC, then loading a second file b.lisp containing
   ;; DEFMETHOD, then modifying and reloading a.lisp and/or b.lisp
   ;; tends to leave the generic function in a state consistent with
   ;; the most-recently-loaded state of a.lisp and b.lisp.)
   (initial-methods
    :initform ()
    :accessor generic-function-initial-methods))
  (:metaclass funcallable-standard-class))

(defclass standard-generic-function (generic-function)
  ((name
    :initform nil
    :initarg :name
    :accessor generic-function-name)
   (methods
    :initform ()
    :accessor generic-function-methods
    :type list)
   (method-class
    :initarg :method-class
    :accessor generic-function-method-class)
   (method-combination
    :initarg :method-combination
    :accessor generic-function-method-combination)
   (declarations
    ;; KLUDGE: AMOP specifies :DECLARATIONS, while ANSI specifies
    ;; :DECLARE.  Allow either (but FIXME: maybe a note or a warning
    ;; might be appropriate).
    :initarg :declarations
    :initarg :declare
    :initform ()
    :accessor generic-function-declarations)
   (arg-info
    :initform (make-arg-info)
    :reader gf-arg-info)
   (dfun-state
    :initform ()
    :accessor gf-dfun-state))
  (:metaclass funcallable-standard-class)
  (:default-initargs :method-class *the-class-standard-method*
		     :method-combination *standard-method-combination*))

(defclass method-combination (standard-object)
  ((documentation
    :reader method-combination-documentation
    :initform nil
    :initarg :documentation)))

(defclass standard-method-combination (definition-source-mixin
				       method-combination)
  ((type
    :reader method-combination-type
    :initarg :type)
   (options
    :reader method-combination-options
    :initarg :options)))

(defclass long-method-combination (standard-method-combination)
  ((function
    :initarg :function
    :reader long-method-combination-function)
   (args-lambda-list
    :initarg :args-lambda-list
    :reader long-method-combination-args-lambda-list)))

(defparameter *early-class-predicates*
  '((specializer specializerp)
    (exact-class-specializer exact-class-specializer-p)
    (class-eq-specializer class-eq-specializer-p)
    (eql-specializer eql-specializer-p)
    (class classp)
    (slot-class slot-class-p)
    (std-class std-class-p)
    (standard-class standard-class-p)
    (funcallable-standard-class funcallable-standard-class-p)
    (condition-class condition-class-p)
    (structure-class structure-class-p)
    (forward-referenced-class forward-referenced-class-p)
    (method method-p)
    (standard-method standard-method-p)
    (standard-accessor-method standard-accessor-method-p)
    (standard-reader-method standard-reader-method-p)
    (standard-writer-method standard-writer-method-p)
    (standard-boundp-method standard-boundp-method-p)
    (generic-function generic-function-p)
    (standard-generic-function standard-generic-function-p)
    (method-combination method-combination-p)
    (long-method-combination long-method-combination-p)))


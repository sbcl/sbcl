;;;; bootstrapping the meta-braid
;;;;
;;;; The code in this file takes the early definitions that have been
;;;; saved up and actually builds those class objects. This work is
;;;; largely driven off of those class definitions, but the fact that
;;;; STANDARD-CLASS is the class of all metaclasses in the braid is
;;;; built into this code pretty deeply.

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

(defun allocate-standard-instance (wrapper
				   &optional (slots-init nil slots-init-p))
  (let ((instance (%make-standard-instance nil (get-instance-hash-code)))
	(no-of-slots (wrapper-no-of-instance-slots wrapper)))
    (setf (std-instance-wrapper instance) wrapper)
    (setf (std-instance-slots instance)
	  (cond (slots-init-p
		 ;; Inline the slots vector allocation and initialization.
		 (let ((slots (make-array no-of-slots :initial-element 0)))
		   (do ((rem-slots slots-init (rest rem-slots))
			(i 0 (1+ i)))
		       ((>= i no-of-slots)) ;endp rem-slots))
		     (declare (list rem-slots)
			      (type index i))
		     (setf (aref slots i) (first rem-slots)))
		   slots))
		(t
		 (make-array no-of-slots
			     :initial-element +slot-unbound+))))
    instance))

(defmacro allocate-funcallable-instance-slots (wrapper &optional
						       slots-init-p slots-init)
  `(let ((no-of-slots (wrapper-no-of-instance-slots ,wrapper)))
     ,(if slots-init-p
	  `(if ,slots-init-p
	       (make-array no-of-slots :initial-contents ,slots-init)
	       (make-array no-of-slots :initial-element +slot-unbound+))
	  `(make-array no-of-slots :initial-element +slot-unbound+))))

(defun allocate-funcallable-instance (wrapper &optional
					      (slots-init nil slots-init-p))
  (let ((fin (%make-pcl-funcallable-instance nil nil
					     (get-instance-hash-code))))
    (set-funcallable-instance-fun
     fin
     #'(sb-kernel:instance-lambda (&rest args)
	 (declare (ignore args))
	 (error "The function of the funcallable-instance ~S has not been set."
		fin)))
    (setf (fsc-instance-wrapper fin) wrapper
	  (fsc-instance-slots fin) (allocate-funcallable-instance-slots
				    wrapper slots-init-p slots-init))
    fin))

(defun allocate-structure-instance (wrapper &optional
					    (slots-init nil slots-init-p))
  (let* ((class (wrapper-class wrapper))
	 (constructor (class-defstruct-constructor class)))
    (if constructor
	(let ((instance (funcall constructor))
	      (slots (class-slots class)))
	  (when slots-init-p
	    (dolist (slot slots)
	      (setf (slot-value-using-class class instance slot)
		    (pop slots-init))))
	  instance)
	(error "can't allocate an instance of class ~S" (class-name class)))))

;;;; BOOTSTRAP-META-BRAID
;;;;
;;;; This function builds the base metabraid from the early class definitions.

(defmacro !initial-classes-and-wrappers (&rest classes)
  `(progn
     ,@(mapcar (lambda (class)
		 (let ((wr (intern (format nil "~A-WRAPPER" class)
				   *pcl-package*)))
		   `(setf ,wr ,(if (eq class 'standard-generic-function)
				   '*sgf-wrapper*
				   `(boot-make-wrapper
				     (early-class-size ',class)
				     ',class))
			  ,class (allocate-standard-instance
				  ,(if (eq class 'standard-generic-function)
				       'funcallable-standard-class-wrapper
				       'standard-class-wrapper))
			  (wrapper-class ,wr) ,class
			  (find-class ',class) ,class)))
	       classes)))

(defun !bootstrap-meta-braid ()
  (let* ((*create-classes-from-internal-structure-definitions-p* nil)
	 std-class-wrapper std-class
	 standard-class-wrapper standard-class
	 funcallable-standard-class-wrapper funcallable-standard-class
	 slot-class-wrapper slot-class
	 built-in-class-wrapper built-in-class
	 structure-class-wrapper structure-class
	 standard-direct-slot-definition-wrapper
	 standard-direct-slot-definition
	 standard-effective-slot-definition-wrapper
	 standard-effective-slot-definition
	 class-eq-specializer-wrapper class-eq-specializer
	 standard-generic-function-wrapper standard-generic-function)
    (!initial-classes-and-wrappers
     standard-class funcallable-standard-class
     slot-class built-in-class structure-class std-class
     standard-direct-slot-definition standard-effective-slot-definition
     class-eq-specializer standard-generic-function)
    ;; First, make a class metaobject for each of the early classes. For
    ;; each metaobject we also set its wrapper. Except for the class T,
    ;; the wrapper is always that of STANDARD-CLASS.
    (dolist (definition *early-class-definitions*)
      (let* ((name (ecd-class-name definition))
	     (meta (ecd-metaclass definition))
	     (wrapper (ecase meta
			(slot-class slot-class-wrapper)
			(std-class std-class-wrapper)
			(standard-class standard-class-wrapper)
			(funcallable-standard-class
			 funcallable-standard-class-wrapper)
			(built-in-class built-in-class-wrapper)
			(structure-class structure-class-wrapper)))
	     (class (or (find-class name nil)
			(allocate-standard-instance wrapper))))	
	(setf (find-class name) class)))
    (dolist (definition *early-class-definitions*)
      (let ((name (ecd-class-name definition))
	    (meta (ecd-metaclass definition))
	    (source (ecd-source definition))
	    (direct-supers (ecd-superclass-names definition))
	    (direct-slots  (ecd-canonical-slots definition))
	    (other-initargs (ecd-other-initargs definition)))
	(let ((direct-default-initargs
	       (getf other-initargs :direct-default-initargs)))
	  (multiple-value-bind (slots cpl default-initargs direct-subclasses)
	      (early-collect-inheritance name)
	    (let* ((class (find-class name))
		   (wrapper (cond ((eq class slot-class)
				   slot-class-wrapper)
				  ((eq class std-class)
				   std-class-wrapper)
				  ((eq class standard-class)
				   standard-class-wrapper)
				  ((eq class funcallable-standard-class)
				   funcallable-standard-class-wrapper)
				  ((eq class standard-direct-slot-definition)
				   standard-direct-slot-definition-wrapper)
				  ((eq class
				       standard-effective-slot-definition)
				   standard-effective-slot-definition-wrapper)
				  ((eq class built-in-class)
				   built-in-class-wrapper)
				  ((eq class structure-class)
				   structure-class-wrapper)
				  ((eq class class-eq-specializer)
				   class-eq-specializer-wrapper)
				  ((eq class standard-generic-function)
				   standard-generic-function-wrapper)
				  (t
				   (boot-make-wrapper (length slots) name))))
		   (proto nil))
	      (when (eq name t) (setq *the-wrapper-of-t* wrapper))
	      (set (intern (format nil "*THE-CLASS-~A*" (symbol-name name))
			   *pcl-package*)
		   class)
	      (dolist (slot slots)
		(unless (eq (getf slot :allocation :instance) :instance)
		  (error "Slot allocation ~S is not supported in bootstrap.")))

	      (when (typep wrapper 'wrapper)
		(setf (wrapper-instance-slots-layout wrapper)
		      (mapcar #'canonical-slot-name slots))
		(setf (wrapper-class-slots wrapper)
		      ()))

	      (setq proto (if (eq meta 'funcallable-standard-class)
			      (allocate-funcallable-instance wrapper)
			      (allocate-standard-instance wrapper)))

	      (setq direct-slots
		    (!bootstrap-make-slot-definitions
		     name class direct-slots
		     standard-direct-slot-definition-wrapper nil))
	      (setq slots
		    (!bootstrap-make-slot-definitions
		     name class slots
		     standard-effective-slot-definition-wrapper t))

	      (case meta
		((std-class standard-class funcallable-standard-class)
		 (!bootstrap-initialize-class
		  meta
		  class name class-eq-specializer-wrapper source
		  direct-supers direct-subclasses cpl wrapper proto
		  direct-slots slots direct-default-initargs default-initargs))
		(built-in-class		; *the-class-t*
		 (!bootstrap-initialize-class
		  meta
		  class name class-eq-specializer-wrapper source
		  direct-supers direct-subclasses cpl wrapper proto))
		(slot-class		; *the-class-slot-object*
		 (!bootstrap-initialize-class
		  meta
		  class name class-eq-specializer-wrapper source
		  direct-supers direct-subclasses cpl wrapper proto))
		(structure-class	; *the-class-structure-object*
		 (!bootstrap-initialize-class
		  meta
		  class name class-eq-specializer-wrapper source
		  direct-supers direct-subclasses cpl wrapper))))))))

    (let* ((smc-class (find-class 'standard-method-combination))
	   (smc-wrapper (!bootstrap-get-slot 'standard-class
					     smc-class
					     'wrapper))
	   (smc (allocate-standard-instance smc-wrapper)))
      (flet ((set-slot (name value)
	       (!bootstrap-set-slot 'standard-method-combination
				    smc
				    name
				    value)))
	(set-slot 'source *load-pathname*)
	(set-slot 'type 'standard)
	(set-slot 'documentation "The standard method combination.")
	(set-slot 'options ()))
      (setq *standard-method-combination* smc))))

;;; Initialize a class metaobject.
(defun !bootstrap-initialize-class
       (metaclass-name class name
	class-eq-wrapper source direct-supers direct-subclasses cpl wrapper
	&optional
	proto direct-slots slots direct-default-initargs default-initargs)
  (flet ((classes (names) (mapcar #'find-class names))
	 (set-slot (slot-name value)
	   (!bootstrap-set-slot metaclass-name class slot-name value)))
    (set-slot 'name name)
    (set-slot 'source source)
    (set-slot 'type (if (eq class (find-class t))
			t
			;; FIXME: Could this just be CLASS instead
			;; of `(CLASS ,CLASS)? If not, why not?
			;; (See also similar expression in 
			;; SHARED-INITIALIZE :BEFORE (CLASS).)
			`(class ,class)))
    (set-slot 'class-eq-specializer
	      (let ((spec (allocate-standard-instance class-eq-wrapper)))
		(!bootstrap-set-slot 'class-eq-specializer spec 'type
				     `(class-eq ,class))
		(!bootstrap-set-slot 'class-eq-specializer spec 'object
				     class)
		spec))
    (set-slot 'class-precedence-list (classes cpl))
    (set-slot 'can-precede-list (classes (cdr cpl)))
    (set-slot 'incompatible-superclass-list nil)
    (set-slot 'direct-superclasses (classes direct-supers))
    (set-slot 'direct-subclasses (classes direct-subclasses))
    (set-slot 'direct-methods (cons nil nil))
    (set-slot 'wrapper wrapper)
    (set-slot 'predicate-name (or (cadr (assoc name *early-class-predicates*))
				  (make-class-predicate-name name)))
    (set-slot 'plist
	      `(,@(and direct-default-initargs
		       `(direct-default-initargs ,direct-default-initargs))
		,@(and default-initargs
		       `(default-initargs ,default-initargs))))
    (when (memq metaclass-name '(standard-class funcallable-standard-class
				 structure-class slot-class std-class))
      (set-slot 'direct-slots direct-slots)
      (set-slot 'slots slots)
      (set-slot 'initialize-info nil))
    (if (eq metaclass-name 'structure-class)
	(let ((constructor-sym '|STRUCTURE-OBJECT class constructor|))
	  (set-slot 'predicate-name (or (cadr (assoc name
						     *early-class-predicates*))
					(make-class-predicate-name name)))
	  (set-slot 'defstruct-form
		    `(defstruct (structure-object (:constructor
						   ,constructor-sym)
						  (:copier nil))))
	  (set-slot 'defstruct-constructor constructor-sym)
	  (set-slot 'from-defclass-p t)
	  (set-slot 'plist nil)
	  (set-slot 'prototype (funcall constructor-sym)))
	(set-slot 'prototype (or proto (allocate-standard-instance wrapper))))
    class))

(defun !bootstrap-make-slot-definitions (name class slots wrapper effective-p)
  (let ((index -1))
    (mapcar (lambda (slot)
	      (incf index)
	      (!bootstrap-make-slot-definition
	       name class slot wrapper effective-p index))
	    slots)))

(defun !bootstrap-make-slot-definition
    (name class slot wrapper effective-p index)
  (let* ((slotd-class-name (if effective-p
			       'standard-effective-slot-definition
			       'standard-direct-slot-definition))
	 (slotd (allocate-standard-instance wrapper))
	 (slot-name (getf slot :name)))
    (flet ((get-val (name) (getf slot name))
	   (set-val (name val)
		    (!bootstrap-set-slot slotd-class-name slotd name val)))
      (set-val 'name	     slot-name)
      (set-val 'initform     (get-val :initform))
      (set-val 'initfunction (get-val :initfunction))
      (set-val 'initargs     (get-val :initargs))
      (set-val 'readers      (get-val :readers))
      (set-val 'writers      (get-val :writers))
      (set-val 'allocation   :instance)
      (set-val 'type	     (or (get-val :type) t))
      (set-val 'documentation (or (get-val :documentation) ""))
      (set-val 'class	class)
      (when effective-p
	(set-val 'location index)
	(let ((fsc-p nil))
	  (set-val 'reader-function (make-optimized-std-reader-method-function
				     fsc-p slot-name index))
	  (set-val 'writer-function (make-optimized-std-writer-method-function
				     fsc-p slot-name index))
	  (set-val 'boundp-function (make-optimized-std-boundp-method-function
				     fsc-p slot-name index)))
	(set-val 'accessor-flags 7)
	(let ((table (or (gethash slot-name *name->class->slotd-table*)
			 (setf (gethash slot-name *name->class->slotd-table*)
			       (make-hash-table :test 'eq :size 5)))))
	  (setf (gethash class table) slotd)))
      (when (and (eq name 'standard-class)
		 (eq slot-name 'slots) effective-p)
	(setq *the-eslotd-standard-class-slots* slotd))
      (when (and (eq name 'funcallable-standard-class)
		 (eq slot-name 'slots) effective-p)
	(setq *the-eslotd-funcallable-standard-class-slots* slotd))
      slotd)))

(defun !bootstrap-accessor-definitions (early-p)
  (let ((*early-p* early-p))
    (dolist (definition *early-class-definitions*)
      (let ((name (ecd-class-name definition))
	    (meta (ecd-metaclass definition)))
	(unless (eq meta 'built-in-class)
	  (let ((direct-slots  (ecd-canonical-slots definition)))
	    (dolist (slotd direct-slots)
	      (let ((slot-name (getf slotd :name))
		    (readers (getf slotd :readers))
		    (writers (getf slotd :writers)))
		(!bootstrap-accessor-definitions1
		 name
		 slot-name
		 readers
		 writers
		 nil)
		(!bootstrap-accessor-definitions1
		 'slot-object
		 slot-name
		 (list (slot-reader-symbol slot-name))
		 (list (slot-writer-symbol slot-name))
		 (list (slot-boundp-symbol slot-name)))))))))))

(defun !bootstrap-accessor-definition (class-name accessor-name slot-name type)
  (multiple-value-bind (accessor-class make-method-function arglist specls doc)
      (ecase type
	(reader (values 'standard-reader-method
			#'make-std-reader-method-function
			(list class-name)
			(list class-name)
			"automatically generated reader method"))
	(writer (values 'standard-writer-method
			#'make-std-writer-method-function
			(list 'new-value class-name)
			(list t class-name)
			"automatically generated writer method"))
	(boundp (values 'standard-boundp-method
			#'make-std-boundp-method-function
			(list class-name)
			(list class-name)
			"automatically generated boundp method")))
    (let ((gf (ensure-generic-function accessor-name)))
      (if (find specls (early-gf-methods gf)
		:key #'early-method-specializers
		:test 'equal)
	  (unless (assoc accessor-name *!generic-function-fixups*
			 :test #'equal)
	    (update-dfun gf))
	  (add-method gf
		      (make-a-method accessor-class
				     ()
				     arglist specls
				     (funcall make-method-function
					      class-name slot-name)
				     doc
				     slot-name))))))

(defun !bootstrap-accessor-definitions1 (class-name
					slot-name
					readers
					writers
					boundps)
  (flet ((do-reader-definition (reader)
	   (!bootstrap-accessor-definition class-name
					   reader
					   slot-name
					   'reader))
	 (do-writer-definition (writer)
	   (!bootstrap-accessor-definition class-name
					   writer
					   slot-name
					   'writer))
	 (do-boundp-definition (boundp)
	   (!bootstrap-accessor-definition class-name
					   boundp
					   slot-name
					   'boundp)))
    (dolist (reader readers) (do-reader-definition reader))
    (dolist (writer writers) (do-writer-definition writer))
    (dolist (boundp boundps) (do-boundp-definition boundp))))

(defun !bootstrap-class-predicates (early-p)
  (let ((*early-p* early-p))
    (dolist (definition *early-class-definitions*)
      (let* ((name (ecd-class-name definition))
	     (class (find-class name)))
	(setf (find-class-predicate name)
	      (make-class-predicate class (class-predicate-name class)))))))

(defun !bootstrap-built-in-classes ()

  ;; First make sure that all the supers listed in
  ;; *BUILT-IN-CLASS-LATTICE* are themselves defined by
  ;; *BUILT-IN-CLASS-LATTICE*. This is just to check for typos and
  ;; other sorts of brainos.
  (dolist (e *built-in-classes*)
    (dolist (super (cadr e))
      (unless (or (eq super t)
		  (assq super *built-in-classes*))
	(error "in *BUILT-IN-CLASSES*: ~S has ~S as a super,~%~
		but ~S is not itself a class in *BUILT-IN-CLASSES*."
	       (car e) super super))))

  ;; In the first pass, we create a skeletal object to be bound to the
  ;; class name.
  (let* ((built-in-class (find-class 'built-in-class))
	 (built-in-class-wrapper (class-wrapper built-in-class)))
    (dolist (e *built-in-classes*)
      (let ((class (allocate-standard-instance built-in-class-wrapper)))
	(setf (find-class (car e)) class))))

  ;; In the second pass, we initialize the class objects.
  (let ((class-eq-wrapper (class-wrapper (find-class 'class-eq-specializer))))
    (dolist (e *built-in-classes*)
      (destructuring-bind (name supers subs cpl prototype) e
	(let* ((class (find-class name))
	       (lclass (cl:find-class name))
	       (wrapper (sb-kernel:class-layout lclass)))
	  (set (get-built-in-class-symbol name) class)
	  (set (get-built-in-wrapper-symbol name) wrapper)
	  (setf (sb-kernel:class-pcl-class lclass) class)

	  (!bootstrap-initialize-class 'built-in-class class
				       name class-eq-wrapper nil
				       supers subs
				       (cons name cpl)
				       wrapper prototype)))))

  (dolist (e *built-in-classes*)
    (let* ((name (car e))
	   (class (find-class name)))
      (setf (find-class-predicate name)
	    (make-class-predicate class (class-predicate-name class))))))

(defmacro wrapper-of-macro (x)
  `(sb-kernel:layout-of ,x))

(defun class-of (x)
  (wrapper-class* (wrapper-of-macro x)))

;;; FIXME: We probably don't need both WRAPPER-OF and WRAPPER-OF-MACRO.
#-sb-fluid (declaim (inline wrapper-of))
(defun wrapper-of (x)
  (wrapper-of-macro x))

(defvar *find-structure-class* nil)

(defun eval-form (form)
  (lambda () (eval form)))

(defun slot-initargs-from-structure-slotd (slotd)
  `(:name ,(structure-slotd-name slotd)
    :defstruct-accessor-symbol ,(structure-slotd-accessor-symbol slotd)
    :internal-reader-function ,(structure-slotd-reader-function slotd)
    :internal-writer-function ,(structure-slotd-writer-function slotd)
    :type ,(or (structure-slotd-type slotd) t)
    :initform ,(structure-slotd-init-form slotd)
    :initfunction ,(eval-form (structure-slotd-init-form slotd))))

(defun find-structure-class (symbol)
  (if (structure-type-p symbol)
      (unless (eq *find-structure-class* symbol)
	(let ((*find-structure-class* symbol))
	  (ensure-class symbol
			:metaclass 'structure-class
			:name symbol
			:direct-superclasses
                        (mapcar #'cl:class-name
                                (sb-kernel:class-direct-superclasses
                                 (cl:find-class symbol)))
			:direct-slots
			(mapcar #'slot-initargs-from-structure-slotd
				(structure-type-slot-description-list
				 symbol)))))
      (error "~S is not a legal structure class name." symbol)))

(defun make-class-predicate (class name)
  (let* ((gf (ensure-generic-function name))
	 (mlist (if (eq *boot-state* 'complete)
		    (generic-function-methods gf)
		    (early-gf-methods gf))))
    (unless mlist
      (unless (eq class *the-class-t*)
	(let* ((default-method-function #'constantly-nil)
	       (default-method-initargs (list :function
					      default-method-function))
	       (default-method (make-a-method
				'standard-method
				()
				(list 'object)
				(list *the-class-t*)
				default-method-initargs
				"class predicate default method")))
	  (setf (method-function-get default-method-function :constant-value)
		nil)
	  (add-method gf default-method)))
      (let* ((class-method-function #'constantly-t)
	     (class-method-initargs (list :function
					  class-method-function))
	     (class-method (make-a-method 'standard-method
					  ()
					  (list 'object)
					  (list class)
					  class-method-initargs
					  "class predicate class method")))
	(setf (method-function-get class-method-function :constant-value) t)
	(add-method gf class-method)))
    gf))

;;; Set the inherits from CPL, and register the layout. This actually
;;; installs the class in the Lisp type system.
(defun update-lisp-class-layout (class layout)
  (let ((lclass (sb-kernel:layout-class layout)))
    (unless (eq (sb-kernel:class-layout lclass) layout)
      (setf (sb-kernel:layout-inherits layout)
              (sb-kernel:order-layout-inherits
               (map 'simple-vector #'class-wrapper
                    (reverse (rest (class-precedence-list class))))))
      (sb-kernel:register-layout layout :invalidate t)

      ;; Subclasses of formerly forward-referenced-class may be
      ;; unknown to CL:FIND-CLASS and also anonymous. This
      ;; functionality moved here from (SETF FIND-CLASS).
      (let ((name (class-name class)))
	(setf (cl:find-class name) lclass
	      ;; FIXME: It's nasty to use double colons. Perhaps the
	      ;; best way to fix this is not to export CLASS-%NAME
	      ;; from SB-KERNEL, but instead to move the whole
	      ;; UPDATE-LISP-CLASS-LAYOUT function to SB-KERNEL, and
	      ;; export it. (since it's also nasty for us to be
	      ;; reaching into %KERNEL implementation details my
	      ;; messing with raw CLASS-%NAME)
	      (sb-kernel::class-%name lclass) name)))))

(clrhash *find-class*)
(!bootstrap-meta-braid)
(!bootstrap-accessor-definitions t)
(!bootstrap-class-predicates t)
(!bootstrap-accessor-definitions nil)
(!bootstrap-class-predicates nil)
(!bootstrap-built-in-classes)

(dohash (name x *find-class*)
	(let* ((class (find-class-from-cell name x))
	       (layout (class-wrapper class))
	       (lclass (sb-kernel:layout-class layout))
	       (lclass-pcl-class (sb-kernel:class-pcl-class lclass))
	       (olclass (cl:find-class name nil)))
	  (if lclass-pcl-class
	      (aver (eq class lclass-pcl-class))
	      (setf (sb-kernel:class-pcl-class lclass) class))

	  (update-lisp-class-layout class layout)

	  (cond (olclass
		 (aver (eq lclass olclass)))
		(t
		 (setf (cl:find-class name) lclass)))))

(setq *boot-state* 'braid)

(defmethod no-applicable-method (generic-function &rest args)
  (error "~@<There is no matching method for the generic function ~2I~_~S~
	  ~I~_when called with arguments ~2I~_~S.~:>"
	 generic-function
	 args))

(defmethod no-next-method ((generic-function standard-generic-function)
			   (method standard-method) &rest args)
  (error "~@<There is no next method for the generic function ~2I~_~S~
	  ~I~_when called from method ~2I~_~S~I~_with arguments ~2I~_~S.~:>"
	 generic-function
	 method
	 args))

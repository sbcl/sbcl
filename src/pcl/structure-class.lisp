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

(defmethod initialize-internal-slot-functions :after
	  ((slotd structure-effective-slot-definition))
  (let ((name (slot-definition-name slotd)))
    (initialize-internal-slot-reader-gfs name)
    (initialize-internal-slot-writer-gfs name)
    (initialize-internal-slot-boundp-gfs name)))

(defmethod slot-definition-allocation ((slotd structure-slot-definition))
  :instance)

(defmethod class-prototype ((class structure-class))
  (with-slots (prototype) class
    (or prototype
	(setq prototype (make-class-prototype class)))))

(defmethod make-class-prototype ((class structure-class))
  (with-slots (wrapper defstruct-constructor) class
    (if defstruct-constructor
	(make-instance class)
      (let* ((proto (%allocate-instance--class *empty-vector*)))
	 (shared-initialize proto T :check-initargs-legality-p NIL)
	 (setf (std-instance-wrapper proto) wrapper)
	 proto))))

(defmethod make-direct-slotd ((class structure-class)
			      &rest initargs
			      &key
			      (name (error "Slot needs a name."))
			      (conc-name (class-defstruct-conc-name class))
			      (defstruct-accessor-symbol () acc-sym-p)
			      &allow-other-keys)
  (declare (ignore defstruct-accessor-symbol))
  (declare (type symbol	name)
	   (type simple-string conc-name))
  (let ((initargs (list* :class class :allow-other-keys T initargs)))
    (unless acc-sym-p
      (setf initargs
	    (list* :defstruct-accessor-symbol
		   (intern (concatenate 'simple-string conc-name (symbol-name name))
			   (symbol-package (class-name class)))
		   initargs)))
    (apply #'make-instance (direct-slot-definition-class class initargs) initargs)))

(defun slot-definition-defstruct-slot-description (slot)
  (let ((type (slot-definition-type slot)))
    `(,(slot-definition-name slot) ,(slot-definition-initform slot)
      ,@(unless (eq type t) `(:type ,type)))))

(defmethod shared-initialize :after
      ((class structure-class)
       slot-names
       &key (direct-superclasses nil direct-superclasses-p)
	    (direct-slots nil direct-slots-p)
	    direct-default-initargs
	    (predicate-name   nil predicate-name-p))
  (declare (ignore slot-names direct-default-initargs))
  (let* ((name (class-name class))
	 (from-defclass-p (slot-value class 'from-defclass-p))
	 (defstruct-form (defstruct-form name))
	 (conc-name
	   (or (if defstruct-form (defstruct-form-conc-name defstruct-form))
	       (slot-value class 'defstruct-conc-name)
	       (format nil "~S structure class " name)))
	 (defstruct-predicate
	   (if defstruct-form (defstruct-form-predicate-name defstruct-form)))
	 (pred-name  ;; Predicate name for class
	   (or (if predicate-name-p (car predicate-name))
	       (if defstruct-form defstruct-predicate)
	       (slot-value class 'predicate-name)
	       (make-class-predicate-name name)))
	 (constructor
	   (or (if defstruct-form (defstruct-form-constructor defstruct-form))
	       (slot-value class 'defstruct-constructor)
	       (if from-defclass-p
		   (list (intern (format nil "~Aconstructor" conc-name)
				 (symbol-package name))
			 ())))))
    (declare (type symbol	name defstruct-predicate pred-name)
	     (type boolean       from-defclass-p)
	     (type simple-string conc-name))
    (if direct-superclasses-p
	(setf (slot-value class 'direct-superclasses)
	      (or direct-superclasses
		  (setq direct-superclasses
			(if (eq name 'structure-object)
			    nil
			    (list *the-class-structure-object*)))))
	(setq direct-superclasses (slot-value class 'direct-superclasses)))
    (setq direct-slots
	  (if direct-slots-p
	      (setf (slot-value class 'direct-slots)
		    (mapcar #'(lambda (pl)
				(apply #'make-direct-slotd class
					:conc-name conc-name pl))
			    direct-slots))
	      (slot-value class 'direct-slots)))
    (when from-defclass-p
      (do-defstruct-from-defclass
	class direct-superclasses direct-slots conc-name pred-name constructor))
    (compile-structure-class-internals
	class direct-slots conc-name pred-name constructor)
    (setf (slot-value class 'predicate-name) pred-name)
    (setf (slot-value class 'defstruct-conc-name) conc-name)
    (unless (extract-required-parameters (second constructor))
      (setf (slot-value class 'defstruct-constructor) (car constructor)))
    (when (and defstruct-predicate (not from-defclass-p))
      (setf (symbol-function pred-name) (symbol-function defstruct-predicate)))
    (unless (or from-defclass-p (slot-value class 'documentation))
      (setf (slot-value class 'documentation)
	    (format nil "~S structure class made from Defstruct" name)))
    (setf (find-class name) class)
    (update-structure-class class direct-superclasses direct-slots)))

(defun update-structure-class (class direct-superclasses direct-slots)
  (add-direct-subclasses class direct-superclasses)
  (setf (slot-value class 'class-precedence-list) (compute-class-precedence-list class))
  (let* ((eslotds (compute-slots class))
	 (internal-slotds (mapcar #'slot-definition-internal-slotd eslotds)))
    (setf (slot-value class 'slots) eslotds)
    (setf (slot-value class 'internal-slotds) internal-slotds)
    (setf (slot-value class 'side-effect-internal-slotds) internal-slotds))
  (unless (slot-value class 'wrapper)
    (setf (slot-value class 'finalized-p) T)
    (setf (slot-value class 'wrapper) (make-wrapper class)))
  (unless (slot-boundp class 'prototype)
    (setf (slot-value class 'prototype) nil))
  (setf (slot-value class 'default-initargs) nil)
  (add-slot-accessors class direct-slots))

(defmethod do-defstruct-from-defclass ((class structure-class)
				       direct-superclasses direct-slots
				       conc-name predicate constructor)
  (declare (type simple-string conc-name))
  (let* ((name (class-name class))
	 (original-defstruct-form
	  `(original-defstruct
	      (,name
		 ,@(when direct-superclasses
		   `((:include ,(class-name (car direct-superclasses)))))
		 (:print-function print-std-instance)
		 (:predicate ,predicate)
		 (:conc-name ,(intern conc-name (symbol-package name)))
		 (:constructor ,@constructor))
	    ,@(mapcar #'slot-definition-defstruct-slot-description
		      direct-slots))))
    (eval original-defstruct-form)
    (store-defstruct-form (cdr original-defstruct-form))))

(defmethod compile-structure-class-internals ((class structure-class)
					      direct-slots conc-name
					      predicate-name constructor)
  (declare (type simple-string conc-name))
  (let* ((name    (class-name class))
	 (package (symbol-package name))
	 (direct-slots-needing-internals
	   (if (slot-value class 'from-defclass-p)
	       direct-slots
	       (remove-if #'slot-definition-internal-reader-function
			  direct-slots)))
	 (reader-names
	   (mapcar #'(lambda (slotd)
		       (intern (format nil "~A~A reader" conc-name
				       (slot-definition-name slotd))
				package))
		   direct-slots-needing-internals))
	 (writer-names
	   (mapcar #'(lambda (slotd)
		       (intern (format nil "~A~A writer" conc-name
				       (slot-definition-name slotd))
			       package))
		   direct-slots-needing-internals))
	 (defstruct-accessor-names
	   (mapcar #'slot-definition-defstruct-accessor-symbol
		   direct-slots-needing-internals))
	 (readers-init
	   (mapcar #'(lambda (defstruct-accessor reader-name)
		       `(progn
			  (force-compile ',defstruct-accessor)
			  (defun ,reader-name (obj)
			    (declare (type ,name obj) #.*optimize-speed*)
			    (,defstruct-accessor obj))
			  (force-compile ',reader-name)))
		   defstruct-accessor-names reader-names))
	 (writers-init
	   (mapcar #'(lambda (defstruct-accessor writer-name)
		       `(progn
			  (force-compile ',defstruct-accessor)
			  (defun ,writer-name (nv obj)
			    (declare (type ,name obj) #.*optimize-speed*)
			    (setf (,defstruct-accessor obj) nv))
			  (force-compile ',writer-name)))
		   defstruct-accessor-names writer-names))
	 (defstruct-extras-form
	   `(progn
	      ,@(when (car constructor)
		  `((force-compile ',(car constructor))))
	      ,@(when (fboundp predicate-name)
		  `((force-compile ',predicate-name)))
	      ,@readers-init
	      ,@writers-init)))
    (declare (type package package))
    (eval defstruct-extras-form)
    (mapc #'(lambda (dslotd reader-name writer-name)
	      (setf (slot-value dslotd 'internal-reader-function)
		    (gdefinition reader-name))
	      (setf (slot-value dslotd 'internal-writer-function)
		    (gdefinition writer-name)))
	  direct-slots-needing-internals reader-names writer-names)))

(defmethod reinitialize-instance :after ((class structure-class)
					 &rest initargs
					 &key)
  (map-dependents class
		  #'(lambda (dependent)
		      (apply #'update-dependent class dependent initargs))))

(defmethod direct-slot-definition-class ((class structure-class) initargs)
  (declare (ignore initargs))
  (find-class 'structure-direct-slot-definition))

(defmethod effective-slot-definition-class ((class structure-class) initargs)
  (declare (ignore initargs))
  (find-class 'structure-effective-slot-definition))

(defmethod finalize-inheritance ((class structure-class))
  nil) ; always finalized

(defmethod compute-slots ((class structure-class))
  (mapcan #'(lambda (superclass)
	      (mapcar #'(lambda (dslotd)
			  (compute-effective-slot-definition
			     class (slot-definition-name dslotd) (list dslotd)))
		      (class-direct-slots superclass)))
	  (reverse (slot-value class 'class-precedence-list))))

(defmethod compute-slots :around ((class structure-class))
  (let ((eslotds (call-next-method)))
    (mapc #'initialize-internal-slot-functions eslotds)
    eslotds))

(defmethod compute-effective-slot-definition ((class structure-class)
					      name dslotds)
  (let* ((initargs (compute-effective-slot-definition-initargs class dslotds))
	 (class (effective-slot-definition-class class initargs))
	 (slot-definition (apply #'make-instance class initargs))
	 (internal-slotd
	   (make-internal-slotd
	     :name name
	     :slot-definition slot-definition
	     :initargs	(slot-definition-initargs     slot-definition)
	     :initfunction    (slot-definition-initfunction slot-definition))))
    (setf (fast-slot-value slot-definition 'internal-slotd) internal-slotd)
    slot-definition))

(defmethod compute-effective-slot-definition-initargs :around
    ((class structure-class) direct-slotds)
  (let ((slotd (car direct-slotds)))
    (list* :defstruct-accessor-symbol (slot-definition-defstruct-accessor-symbol slotd)
	   :internal-reader-function (slot-definition-internal-reader-function slotd)
	   :internal-writer-function (slot-definition-internal-writer-function slotd)
	   (call-next-method))))

(defmethod make-optimized-reader-method-function ((class structure-class)
						  generic-function
						  reader-method-prototype
						  slot-name)
  (declare (ignore generic-function reader-method-prototype))
  (make-structure-instance-reader-method-function slot-name))

(defmethod make-optimized-writer-method-function ((class structure-class)
						  generic-function
						  writer-method-prototype
						  slot-name)
  (declare (ignore generic-function writer-method-prototype))
  (make-structure-instance-writer-method-function slot-name))

(defmethod make-optimized-boundp-method-function ((class structure-class)
						  generic-function
						  boundp-method-prototype
						  slot-name)
  (declare (ignore generic-function boundp-method-prototype))
  (make-structure-instance-boundp-method-function slot-name))

(defun make-structure-instance-reader-method-function (slot-name)
  (declare #.*optimize-speed*)
  #'(lambda (instance)
      (structure-instance-slot-value instance slot-name)))

(defun make-structure-instance-writer-method-function (slot-name)
  (declare #.*optimize-speed*)
  #'(lambda (nv instance)
      (setf (structure-instance-slot-value instance slot-name) nv)))

(defun make-structure-instance-boundp-method-function (slot-name)
  (declare #.*optimize-speed*)
  #'(lambda (instance)
      (structure-instance-slot-boundp instance slot-name)))

(defmethod wrapper-fetcher ((class structure-class))
  'wrapper-for-structure)

(defmethod slots-fetcher ((class structure-class))
  nil)

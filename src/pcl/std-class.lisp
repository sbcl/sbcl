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

(defmethod slot-accessor-function ((slotd effective-slot-definition) type)
  (ecase type
    (reader (slot-definition-reader-function slotd))
    (writer (slot-definition-writer-function slotd))
    (boundp (slot-definition-boundp-function slotd))))

(defmethod (setf slot-accessor-function) (function
					  (slotd effective-slot-definition)
					  type)
  (ecase type
    (reader (setf (slot-definition-reader-function slotd) function))
    (writer (setf (slot-definition-writer-function slotd) function))
    (boundp (setf (slot-definition-boundp-function slotd) function))))

(defconstant *slotd-reader-function-std-p* 1)
(defconstant *slotd-writer-function-std-p* 2)
(defconstant *slotd-boundp-function-std-p* 4)
(defconstant *slotd-all-function-std-p* 7)

(defmethod slot-accessor-std-p ((slotd effective-slot-definition) type)
  (let ((flags (slot-value slotd 'accessor-flags)))
    (declare (type fixnum flags))
    (if (eq type 'all)
	(eql *slotd-all-function-std-p* flags)
	(let ((mask (ecase type
		      (reader *slotd-reader-function-std-p*)
		      (writer *slotd-writer-function-std-p*)
		      (boundp *slotd-boundp-function-std-p*))))
	  (declare (type fixnum mask))
	  (not (zerop (the fixnum (logand mask flags))))))))

(defmethod (setf slot-accessor-std-p) (value
				       (slotd effective-slot-definition)
				       type)
  (let ((mask (ecase type
		(reader *slotd-reader-function-std-p*)
		(writer *slotd-writer-function-std-p*)
		(boundp *slotd-boundp-function-std-p*)))
	(flags (slot-value slotd 'accessor-flags)))
    (declare (type fixnum mask flags))
    (setf (slot-value slotd 'accessor-flags)
	  (if value
	      (the fixnum (logior mask flags))
	      (the fixnum (logand (the fixnum (lognot mask)) flags)))))
  value)

(defmethod initialize-internal-slot-functions ((slotd
						effective-slot-definition))
  (let* ((name (slot-value slotd 'name))
	 (class (slot-value slotd 'class)))
    (let ((table (or (gethash name *name->class->slotd-table*)
		     (setf (gethash name *name->class->slotd-table*)
			   (make-hash-table :test 'eq :size 5)))))
      (setf (gethash class table) slotd))
    (dolist (type '(reader writer boundp))
      (let* ((gf-name (ecase type
			      (reader 'slot-value-using-class)
			      (writer '(setf slot-value-using-class))
			      (boundp 'slot-boundp-using-class)))
	     (gf (gdefinition gf-name)))
	(compute-slot-accessor-info slotd type gf)))
    (initialize-internal-slot-gfs name)))

(defmethod compute-slot-accessor-info ((slotd effective-slot-definition)
				       type gf)
  (let* ((name (slot-value slotd 'name))
	 (class (slot-value slotd 'class))
	 (old-slotd (find-slot-definition class name))
	 (old-std-p (and old-slotd (slot-accessor-std-p old-slotd 'all))))
    (multiple-value-bind (function std-p)
	(if (eq *boot-state* 'complete)
	    (get-accessor-method-function gf type class slotd)
	    (get-optimized-std-accessor-method-function class slotd type))
      (setf (slot-accessor-std-p slotd type) std-p)
      (setf (slot-accessor-function slotd type) function))
    (when (and old-slotd (not (eq old-std-p (slot-accessor-std-p slotd 'all))))
      (push (cons class name) *pv-table-cache-update-info*))))

(defmethod slot-definition-allocation ((slotd structure-slot-definition))
  :instance)

(defmethod shared-initialize :after ((object documentation-mixin)
				     slot-names
				     &key (documentation nil documentation-p))
  (declare (ignore slot-names))
  (when documentation-p
    (setf (plist-value object 'documentation) documentation)))

;;; default if DOC-TYPE doesn't match one of the specified types
(defmethod documentation (object doc-type)
  (warn "unsupported DOCUMENTATION: type ~S for object ~S"
	doc-type
	(type-of object))
  nil)

;;; default if DOC-TYPE doesn't match one of the specified types
(defmethod (setf documentation) (new-value object doc-type)
  ;; CMU CL made this an error, but since ANSI says that even for supported
  ;; doc types an implementation is permitted to discard docs at any time
  ;; for any reason, this feels to me more like a warning. -- WHN 19991214
  (warn "discarding unsupported DOCUMENTATION of type ~S for object ~S"
	doc-type
	(type-of object))
  new-value)

(defmethod documentation ((object documentation-mixin) doc-type)
  (declare (ignore doc-type))
  (plist-value object 'documentation))

(defmethod (setf documentation) (new-value
				 (object documentation-mixin)
				 doc-type)
  (declare (ignore doc-type))
  (setf (plist-value object 'documentation) new-value))

(defmethod documentation ((slotd standard-slot-definition) doc-type)
  (declare (ignore doc-type))
  (slot-value slotd 'documentation))

(defmethod (setf documentation) (new-value
				 (slotd standard-slot-definition)
				 doc-type)
  (declare (ignore doc-type))
  (setf (slot-value slotd 'documentation) new-value))

;;;; various class accessors that are a little more complicated than can be
;;;; done with automatically generated reader methods

(defmethod class-finalized-p ((class pcl-class))
  (with-slots (wrapper) class
    (not (null wrapper))))

(defmethod class-prototype ((class std-class))
  (with-slots (prototype) class
    (or prototype (setq prototype (allocate-instance class)))))

(defmethod class-prototype ((class structure-class))
  (with-slots (prototype wrapper defstruct-constructor) class
    (or prototype
	(setq prototype
	      (if defstruct-constructor
		  (allocate-instance class)
		  (allocate-standard-instance wrapper))))))

(defmethod class-direct-default-initargs ((class slot-class))
  (plist-value class 'direct-default-initargs))

(defmethod class-default-initargs ((class slot-class))
  (plist-value class 'default-initargs))

(defmethod class-constructors ((class slot-class))
  (plist-value class 'constructors))

(defmethod class-slot-cells ((class std-class))
  (plist-value class 'class-slot-cells))

;;;; class accessors that are even a little bit more complicated than those
;;;; above. These have a protocol for updating them, we must implement that
;;;; protocol.

;;; Maintaining the direct subclasses backpointers. The update methods are
;;; here, the values are read by an automatically generated reader method.
(defmethod add-direct-subclass ((class class) (subclass class))
  (with-slots (direct-subclasses) class
    (pushnew subclass direct-subclasses)
    subclass))
(defmethod remove-direct-subclass ((class class) (subclass class))
  (with-slots (direct-subclasses) class
    (setq direct-subclasses (remove subclass direct-subclasses))
    subclass))

;;; Maintaining the direct-methods and direct-generic-functions backpointers.
;;;
;;; There are four generic functions involved, each has one method for the
;;; class case and another method for the damned EQL specializers. All of
;;; these are specified methods and appear in their specified place in the
;;; class graph.
;;;
;;;   ADD-DIRECT-METHOD
;;;   REMOVE-DIRECT-METHOD
;;;   SPECIALIZER-DIRECT-METHODS
;;;   SPECIALIZER-DIRECT-GENERIC-FUNCTIONS
;;;
;;; In each case, we maintain one value which is a cons. The car is the list
;;; methods. The cdr is a list of the generic functions. The cdr is always
;;; computed lazily.
(defmethod add-direct-method ((specializer class) (method method))
  (with-slots (direct-methods) specializer
    (setf (car direct-methods) (adjoin method (car direct-methods))	;PUSH
	  (cdr direct-methods) ()))
  method)
(defmethod remove-direct-method ((specializer class) (method method))
  (with-slots (direct-methods) specializer
    (setf (car direct-methods) (remove method (car direct-methods))
	  (cdr direct-methods) ()))
  method)

(defmethod specializer-direct-methods ((specializer class))
  (with-slots (direct-methods) specializer
    (car direct-methods)))

(defmethod specializer-direct-generic-functions ((specializer class))
  (with-slots (direct-methods) specializer
    (or (cdr direct-methods)
	(setf (cdr direct-methods)
	      (gathering1 (collecting-once)
		(dolist (m (car direct-methods))
		  (gather1 (method-generic-function m))))))))

;;; This hash table is used to store the direct methods and direct generic
;;; functions of EQL specializers. Each value in the table is the cons.
(defvar *eql-specializer-methods* (make-hash-table :test 'eql))
(defvar *class-eq-specializer-methods* (make-hash-table :test 'eq))

(defmethod specializer-method-table ((specializer eql-specializer))
  *eql-specializer-methods*)

(defmethod specializer-method-table ((specializer class-eq-specializer))
  *class-eq-specializer-methods*)

(defmethod add-direct-method ((specializer specializer-with-object) (method method))
  (let* ((object (specializer-object specializer))
	 (table (specializer-method-table specializer))
	 (entry (gethash object table)))
    (unless entry
      (setq entry
	    (setf (gethash object table)
		  (cons nil nil))))
    (setf (car entry) (adjoin method (car entry))
	  (cdr entry) ())
    method))

(defmethod remove-direct-method ((specializer specializer-with-object) (method method))
  (let* ((object (specializer-object specializer))
	 (entry (gethash object (specializer-method-table specializer))))
    (when entry
      (setf (car entry) (remove method (car entry))
	    (cdr entry) ()))
    method))

(defmethod specializer-direct-methods ((specializer specializer-with-object))
  (car (gethash (specializer-object specializer)
		(specializer-method-table specializer))))

(defmethod specializer-direct-generic-functions ((specializer specializer-with-object))
  (let* ((object (specializer-object specializer))
	 (entry (gethash object (specializer-method-table specializer))))
    (when entry
      (or (cdr entry)
	  (setf (cdr entry)
		(gathering1 (collecting-once)
		  (dolist (m (car entry))
		    (gather1 (method-generic-function m)))))))))

(defun map-specializers (function)
  (map-all-classes #'(lambda (class)
		       (funcall function (class-eq-specializer class))
		       (funcall function class)))
  (maphash #'(lambda (object methods)
	       (declare (ignore methods))
	       (intern-eql-specializer object))
	   *eql-specializer-methods*)
  (maphash #'(lambda (object specl)
	       (declare (ignore object))
	       (funcall function specl))
	   *eql-specializer-table*)
  nil)

(defun map-all-generic-functions (function)
  (let ((all-generic-functions (make-hash-table :test 'eq)))
    (map-specializers #'(lambda (specl)
			  (dolist (gf (specializer-direct-generic-functions specl))
			    (unless (gethash gf all-generic-functions)
			      (setf (gethash gf all-generic-functions) t)
			      (funcall function gf))))))
  nil)

(defmethod shared-initialize :after ((specl class-eq-specializer) slot-names &key)
  (declare (ignore slot-names))
  (setf (slot-value specl 'type) `(class-eq ,(specializer-class specl))))

(defmethod shared-initialize :after ((specl eql-specializer) slot-names &key)
  (declare (ignore slot-names))
  (setf (slot-value specl 'type) `(eql ,(specializer-object specl))))

(defun real-load-defclass (name metaclass-name supers slots other accessors)
  (do-standard-defsetfs-for-defclass accessors)			;***
  (let ((res (apply #'ensure-class name :metaclass metaclass-name
		    :direct-superclasses supers
		    :direct-slots slots
		    :definition-source `((defclass ,name)
					 ,*load-truename*)
		    other)))
    ;; Defclass of a class with a forward-referenced superclass does not
    ;; have a wrapper. RES is the incomplete PCL class. The Lisp class
    ;; does not yet exist. Maybe should return NIL in that case as RES
    ;; is not useful to the user?
    (and (class-wrapper res) (sb-kernel:layout-class (class-wrapper res)))))

(setf (gdefinition 'load-defclass) #'real-load-defclass)

(defun ensure-class (name &rest all)
  (apply #'ensure-class-using-class name (find-class name nil) all))

(defmethod ensure-class-using-class (name (class null) &rest args &key)
  (multiple-value-bind (meta initargs)
      (ensure-class-values class args)
    (inform-type-system-about-class (class-prototype meta) name);***
    (setf class (apply #'make-instance meta :name name initargs)
	  (find-class name) class)
    (inform-type-system-about-class class name)			;***
    class))

(defmethod ensure-class-using-class (name (class pcl-class) &rest args &key)
  (multiple-value-bind (meta initargs)
      (ensure-class-values class args)
    (unless (eq (class-of class) meta) (change-class class meta))
    (apply #'reinitialize-instance class initargs)
    (setf (find-class name) class)
    (inform-type-system-about-class class name)			;***
    class))

(defmethod class-predicate-name ((class t))
  'function-returning-nil)

(defun ensure-class-values (class args)
  (let* ((initargs (copy-list args))
	 (unsupplied (list 1))
	 (supplied-meta   (getf initargs :metaclass unsupplied))
	 (supplied-supers (getf initargs :direct-superclasses unsupplied))
	 (supplied-slots  (getf initargs :direct-slots unsupplied))
	 (meta
	   (cond ((neq supplied-meta unsupplied)
		  (find-class supplied-meta))
		 ((or (null class)
		      (forward-referenced-class-p class))
		  *the-class-standard-class*)
		 (t
		  (class-of class)))))
    (flet ((fix-super (s)
	     (cond ((classp s) s)
		   ((not (legal-class-name-p s))
		    (error "~S is not a class or a legal class name." s))
		   (t
		    (or (find-class s nil)
			(setf (find-class s)
			      (make-instance 'forward-referenced-class
					     :name s)))))))
      (loop (unless (remf initargs :metaclass) (return)))
      (loop (unless (remf initargs :direct-superclasses) (return)))
      (loop (unless (remf initargs :direct-slots) (return)))
      (values meta
	      (list* :direct-superclasses
		     (and (neq supplied-supers unsupplied)
			  (mapcar #'fix-super supplied-supers))
		     :direct-slots
		     (and (neq supplied-slots unsupplied) supplied-slots)
		     initargs)))))

#|| ; since it doesn't do anything
(defmethod shared-initialize :before ((class std-class)
				      slot-names
				      &key direct-superclasses)
  (declare (ignore slot-names))
  ;; *** error checking
  )
||#

(defmethod shared-initialize :after
	   ((class std-class)
	    slot-names
	    &key (direct-superclasses nil direct-superclasses-p)
		 (direct-slots nil direct-slots-p)
		 (direct-default-initargs nil direct-default-initargs-p)
		 (predicate-name nil predicate-name-p))
  (declare (ignore slot-names))
  (cond (direct-superclasses-p
	 (setq direct-superclasses
	       (or direct-superclasses
		   (list (if (funcallable-standard-class-p class)
			     *the-class-funcallable-standard-object*
			     *the-class-standard-object*))))
	 (dolist (superclass direct-superclasses)
	   (unless (validate-superclass class superclass)
	     (error "The class ~S was specified as a~%
		     super-class of the class ~S;~%~
		     but the meta-classes ~S and~%~S are incompatible.~@
		     Define a method for ~S to avoid this error."
		     superclass class (class-of superclass) (class-of class)
		     'validate-superclass)))
	 (setf (slot-value class 'direct-superclasses) direct-superclasses))
	(t
	 (setq direct-superclasses (slot-value class 'direct-superclasses))))
  (setq direct-slots
	(if direct-slots-p
	    (setf (slot-value class 'direct-slots)
		  (mapcar #'(lambda (pl) (make-direct-slotd class pl)) direct-slots))
	    (slot-value class 'direct-slots)))
  (if direct-default-initargs-p
      (setf (plist-value class 'direct-default-initargs) direct-default-initargs)
      (setq direct-default-initargs (plist-value class 'direct-default-initargs)))
  (setf (plist-value class 'class-slot-cells)
	(gathering1 (collecting)
	  (dolist (dslotd direct-slots)
	    (when (eq (slot-definition-allocation dslotd) class)
	      (let ((initfunction (slot-definition-initfunction dslotd)))
		(gather1 (cons (slot-definition-name dslotd)
			       (if initfunction
				   (funcall initfunction)
				   *slot-unbound*))))))))
  (setq predicate-name (if predicate-name-p
			   (setf (slot-value class 'predicate-name)
				 (car predicate-name))
			   (or (slot-value class 'predicate-name)
			       (setf (slot-value class 'predicate-name)
				     (make-class-predicate-name (class-name class))))))
  (add-direct-subclasses class direct-superclasses)
  (update-class class nil)
  (make-class-predicate class predicate-name)
  (add-slot-accessors class direct-slots))

(defmethod shared-initialize :before ((class class) slot-names &key name)
  (declare (ignore slot-names name))
  (setf (slot-value class 'type) `(class ,class))
  (setf (slot-value class 'class-eq-specializer)
	(make-instance 'class-eq-specializer :class class)))

(defmethod reinitialize-instance :before ((class slot-class) &key)
  (remove-direct-subclasses class (class-direct-superclasses class))
  (remove-slot-accessors    class (class-direct-slots class)))

(defmethod reinitialize-instance :after ((class slot-class)
					 &rest initargs
					 &key)
  (map-dependents class
		  #'(lambda (dependent)
		      (apply #'update-dependent class dependent initargs))))

(defmethod shared-initialize :after
      ((class structure-class)
       slot-names
       &key (direct-superclasses nil direct-superclasses-p)
	    (direct-slots nil direct-slots-p)
	    direct-default-initargs
	    (predicate-name nil predicate-name-p))
  (declare (ignore slot-names direct-default-initargs))
  (if direct-superclasses-p
      (setf (slot-value class 'direct-superclasses)
	    (or direct-superclasses
		(setq direct-superclasses
		      (and (not (eq (class-name class) 'structure-object))
			   (list *the-class-structure-object*)))))
      (setq direct-superclasses (slot-value class 'direct-superclasses)))
  (let* ((name (class-name class))
	 (from-defclass-p (slot-value class 'from-defclass-p))
	 (defstruct-p (or from-defclass-p (not (structure-type-p name)))))
    (if direct-slots-p
	(setf (slot-value class 'direct-slots)
	      (setq direct-slots
		    (mapcar #'(lambda (pl)
				(when defstruct-p
				  (let* ((slot-name (getf pl :name))
					 (acc-name (format nil "~S structure class ~A"
							   name slot-name))
					 (accessor (intern acc-name)))
				    (setq pl (list* :defstruct-accessor-symbol accessor
						    pl))))
				(make-direct-slotd class pl))
			    direct-slots)))
	(setq direct-slots (slot-value class 'direct-slots)))
    (when defstruct-p
      (let* ((include (car (slot-value class 'direct-superclasses)))
	     (conc-name (intern (format nil "~S structure class " name)))
	     (constructor (intern (format nil "~A constructor" conc-name)))
	     (defstruct `(defstruct (,name
				      ,@(when include
					  `((:include ,(class-name include))))
				      (:print-function print-std-instance)
				      (:predicate nil)
				      (:conc-name ,conc-name)
				      (:constructor ,constructor ()))
			   ,@(mapcar #'(lambda (slot)
					 `(,(slot-definition-name slot)
					   *slot-unbound*))
				     direct-slots)))
	     (reader-names (mapcar #'(lambda (slotd)
				       (intern (format nil "~A~A reader" conc-name
						       (slot-definition-name slotd))))
				   direct-slots))
	     (writer-names (mapcar #'(lambda (slotd)
				       (intern (format nil "~A~A writer" conc-name
						       (slot-definition-name slotd))))
				   direct-slots))
	     (readers-init
	      (mapcar #'(lambda (slotd reader-name)
			  (let ((accessor
				 (slot-definition-defstruct-accessor-symbol slotd)))
			    `(defun ,reader-name (obj)
			       (declare (type ,name obj))
			       (,accessor obj))))
		      direct-slots reader-names))
	     (writers-init
	      (mapcar #'(lambda (slotd writer-name)
			  (let ((accessor
				 (slot-definition-defstruct-accessor-symbol slotd)))
			    `(defun ,writer-name (nv obj)
			       (declare (type ,name obj))
			       (setf (,accessor obj) nv))))
		      direct-slots writer-names))
	     (defstruct-form
	       `(progn
		  ,defstruct
		  ,@readers-init ,@writers-init
		  (declare-structure ',name nil nil))))
	(unless (structure-type-p name) (eval defstruct-form))
	(mapc #'(lambda (dslotd reader-name writer-name)
		  (let* ((reader (gdefinition reader-name))
			 (writer (when (gboundp writer-name)
				   (gdefinition writer-name))))
		    (setf (slot-value dslotd 'internal-reader-function) reader)
		    (setf (slot-value dslotd 'internal-writer-function) writer)))
	      direct-slots reader-names writer-names)
	(setf (slot-value class 'defstruct-form) defstruct-form)
	(setf (slot-value class 'defstruct-constructor) constructor))))
  (add-direct-subclasses class direct-superclasses)
  (setf (slot-value class 'class-precedence-list)
	(compute-class-precedence-list class))
  (setf (slot-value class 'slots) (compute-slots class))
  (let ((lclass (cl:find-class (class-name class))))
    (setf (sb-kernel:class-pcl-class lclass) class)
    (setf (slot-value class 'wrapper) (sb-kernel:class-layout lclass)))
  (update-pv-table-cache-info class)
  (setq predicate-name (if predicate-name-p
			   (setf (slot-value class 'predicate-name)
				 (car predicate-name))
			   (or (slot-value class 'predicate-name)
			       (setf (slot-value class 'predicate-name)
				     (make-class-predicate-name (class-name class))))))
  (make-class-predicate class predicate-name)
  (add-slot-accessors class direct-slots))

(defmethod direct-slot-definition-class ((class structure-class) initargs)
  (declare (ignore initargs))
  (find-class 'structure-direct-slot-definition))

(defmethod finalize-inheritance ((class structure-class))
  nil) ; always finalized

(defun add-slot-accessors (class dslotds)
  (fix-slot-accessors class dslotds 'add))

(defun remove-slot-accessors (class dslotds)
  (fix-slot-accessors class dslotds 'remove))

(defun fix-slot-accessors (class dslotds add/remove)
  (flet ((fix (gfspec name r/w)
	   (let ((gf (ensure-generic-function gfspec)))
	     (case r/w
	       (r (if (eq add/remove 'add)
		      (add-reader-method class gf name)
		      (remove-reader-method class gf)))
	       (w (if (eq add/remove 'add)
		      (add-writer-method class gf name)
		      (remove-writer-method class gf)))))))
    (dolist (dslotd dslotds)
      (let ((slot-name (slot-definition-name dslotd)))
	(dolist (r (slot-definition-readers dslotd)) (fix r slot-name 'r))
	(dolist (w (slot-definition-writers dslotd)) (fix w slot-name 'w))))))

(defun add-direct-subclasses (class new)
  (dolist (n new)
    (unless (memq class (class-direct-subclasses class))
      (add-direct-subclass n class))))

(defun remove-direct-subclasses (class new)
  (let ((old (class-direct-superclasses class)))
    (dolist (o (set-difference old new))
      (remove-direct-subclass o class))))

(defmethod finalize-inheritance ((class std-class))
  (update-class class t))

(defun class-has-a-forward-referenced-superclass-p (class)
  (or (forward-referenced-class-p class)
      (some #'class-has-a-forward-referenced-superclass-p
	    (class-direct-superclasses class))))

;;; This is called by :after shared-initialize whenever a class is initialized
;;; or reinitialized. The class may or may not be finalized.
(defun update-class (class finalizep)
  (when (or finalizep (class-finalized-p class)
	    (not (class-has-a-forward-referenced-superclass-p class)))
    (update-cpl class (compute-class-precedence-list class))
    (update-slots class (compute-slots class))
    (update-gfs-of-class class)
    (update-inits class (compute-default-initargs class))
    (update-make-instance-function-table class))
  (unless finalizep
    (dolist (sub (class-direct-subclasses class)) (update-class sub nil))))

(defun update-cpl (class cpl)
  (if (class-finalized-p class)
      (unless (equal (class-precedence-list class) cpl)
	;; comment from the old CMU CL sources:
	;;   Need to have the cpl setup before update-lisp-class-layout
	;;   is called on CMU CL.
	(setf (slot-value class 'class-precedence-list) cpl)
	(force-cache-flushes class))
      (setf (slot-value class 'class-precedence-list) cpl))
  (update-class-can-precede-p cpl))

(defun update-class-can-precede-p (cpl)
  (when cpl
    (let ((first (car cpl)))
      (dolist (c (cdr cpl))
	(pushnew c (slot-value first 'can-precede-list))))
    (update-class-can-precede-p (cdr cpl))))

(defun class-can-precede-p (class1 class2)
  (member class2 (class-can-precede-list class1)))

(defun update-slots (class eslotds)
  (let ((instance-slots ())
	(class-slots    ()))
    (dolist (eslotd eslotds)
      (let ((alloc (slot-definition-allocation eslotd)))
	(cond ((eq alloc :instance) (push eslotd instance-slots))
	      ((classp alloc)       (push eslotd class-slots)))))

    ;; If there is a change in the shape of the instances then the
    ;; old class is now obsolete.
    (let* ((nlayout (mapcar #'slot-definition-name
			    (sort instance-slots #'< :key #'slot-definition-location)))
	   (nslots (length nlayout))
	   (nwrapper-class-slots (compute-class-slots class-slots))
	   (owrapper (class-wrapper class))
	   (olayout (and owrapper (wrapper-instance-slots-layout owrapper)))
	   (owrapper-class-slots (and owrapper (wrapper-class-slots owrapper)))
	   (nwrapper
	    (cond ((null owrapper)
		   (make-wrapper nslots class))
		  ((and (equal nlayout olayout)
			(not
			 (iterate ((o (list-elements owrapper-class-slots))
				   (n (list-elements nwrapper-class-slots)))
				  (unless (eq (car o) (car n)) (return t)))))
		   owrapper)
		  (t
		   ;; This will initialize the new wrapper to have the same
		   ;; state as the old wrapper. We will then have to change
		   ;; that. This may seem like wasted work (it is), but the
		   ;; spec requires that we call make-instances-obsolete.
		   (make-instances-obsolete class)
		   (class-wrapper class)))))

      (with-slots (wrapper slots) class
	(update-lisp-class-layout class nwrapper)
	(setf slots eslotds
	      (wrapper-instance-slots-layout nwrapper) nlayout
	      (wrapper-class-slots nwrapper) nwrapper-class-slots
	      (wrapper-no-of-instance-slots nwrapper) nslots
	      wrapper nwrapper))

      (unless (eq owrapper nwrapper)
	(update-pv-table-cache-info class)))))

(defun compute-class-slots (eslotds)
  (gathering1 (collecting)
    (dolist (eslotd eslotds)
      (gather1
	(assoc (slot-definition-name eslotd)
	       (class-slot-cells (slot-definition-allocation eslotd)))))))

(defun compute-layout (cpl instance-eslotds)
  (let* ((names
	   (gathering1 (collecting)
	     (dolist (eslotd instance-eslotds)
	       (when (eq (slot-definition-allocation eslotd) :instance)
		 (gather1 (slot-definition-name eslotd))))))
	 (order ()))
    (labels ((rwalk (tail)
	       (when tail
		 (rwalk (cdr tail))
		 (dolist (ss (class-slots (car tail)))
		   (let ((n (slot-definition-name ss)))
		     (when (member n names)
		       (setq order (cons n order)
			     names (remove n names))))))))
      (rwalk (if (slot-boundp (car cpl) 'slots)
		 cpl
		 (cdr cpl)))
      (reverse (append names order)))))

(defun update-gfs-of-class (class)
  (when (and (class-finalized-p class)
	     (let ((cpl (class-precedence-list class)))
	       (or (member *the-class-slot-class* cpl)
		   (member *the-class-standard-effective-slot-definition* cpl))))
    (let ((gf-table (make-hash-table :test 'eq)))
      (labels ((collect-gfs (class)
		 (dolist (gf (specializer-direct-generic-functions class))
		   (setf (gethash gf gf-table) t))
		 (mapc #'collect-gfs (class-direct-superclasses class))))
	(collect-gfs class)
	(maphash #'(lambda (gf ignore)
		     (declare (ignore ignore))
		     (update-gf-dfun class gf))
		 gf-table)))))

(defun update-inits (class inits)
  (setf (plist-value class 'default-initargs) inits))

(defmethod compute-default-initargs ((class slot-class))
  (let ((cpl (class-precedence-list class))
	(direct (class-direct-default-initargs class)))
    (labels ((walk (tail)
	       (if (null tail)
		   nil
		   (let ((c (pop tail)))
		     (append (if (eq c class)
				 direct
				 (class-direct-default-initargs c))
			     (walk tail))))))
      (let ((initargs (walk cpl)))
	(delete-duplicates initargs :test #'eq :key #'car :from-end t)))))

;;;; protocols for constructing direct and effective slot definitions

(defmethod direct-slot-definition-class ((class std-class) initargs)
  (declare (ignore initargs))
  (find-class 'standard-direct-slot-definition))

(defun make-direct-slotd (class initargs)
  (let ((initargs (list* :class class initargs)))
    (apply #'make-instance
	   (direct-slot-definition-class class initargs)
	   initargs)))

(defmethod compute-slots ((class std-class))
  ;; As specified, we must call COMPUTE-EFFECTIVE-SLOT-DEFINITION once
  ;; for each different slot name we find in our superclasses. Each
  ;; call receives the class and a list of the dslotds with that name.
  ;; The list is in most-specific-first order.
  (let ((name-dslotds-alist ()))
    (dolist (c (class-precedence-list class))
      (let ((dslotds (class-direct-slots c)))
	(dolist (d dslotds)
	  (let* ((name (slot-definition-name d))
		 (entry (assq name name-dslotds-alist)))
	    (if entry
		(push d (cdr entry))
		(push (list name d) name-dslotds-alist))))))
    (mapcar #'(lambda (direct)
		(compute-effective-slot-definition class
						   (nreverse (cdr direct))))
	    name-dslotds-alist)))

(defmethod compute-slots :around ((class std-class))
  (let ((eslotds (call-next-method))
	(cpl (class-precedence-list class))
	(instance-slots ())
	(class-slots    ()))
    (dolist (eslotd eslotds)
      (let ((alloc (slot-definition-allocation eslotd)))
	(cond ((eq alloc :instance) (push eslotd instance-slots))
	      ((classp alloc)       (push eslotd class-slots)))))
    (let ((nlayout (compute-layout cpl instance-slots)))
      (dolist (eslotd instance-slots)
	(setf (slot-definition-location eslotd)
	      (position (slot-definition-name eslotd) nlayout))))
    (dolist (eslotd class-slots)
      (setf (slot-definition-location eslotd)
	    (assoc (slot-definition-name eslotd)
		   (class-slot-cells (slot-definition-allocation eslotd)))))
    (mapc #'initialize-internal-slot-functions eslotds)
    eslotds))

(defmethod compute-slots ((class structure-class))
  (mapcan #'(lambda (superclass)
	      (mapcar #'(lambda (dslotd)
			  (compute-effective-slot-definition class
							     (list dslotd)))
		      (class-direct-slots superclass)))
	  (reverse (slot-value class 'class-precedence-list))))

(defmethod compute-slots :around ((class structure-class))
  (let ((eslotds (call-next-method)))
    (mapc #'initialize-internal-slot-functions eslotds)
    eslotds))

(defmethod compute-effective-slot-definition ((class slot-class) dslotds)
  (let* ((initargs (compute-effective-slot-definition-initargs class dslotds))
	 (class (effective-slot-definition-class class initargs)))
    (apply #'make-instance class initargs)))

(defmethod effective-slot-definition-class ((class std-class) initargs)
  (declare (ignore initargs))
  (find-class 'standard-effective-slot-definition))

(defmethod effective-slot-definition-class ((class structure-class) initargs)
  (declare (ignore initargs))
  (find-class 'structure-effective-slot-definition))

(defmethod compute-effective-slot-definition-initargs
    ((class slot-class) direct-slotds)
  (let* ((name nil)
	 (initfunction nil)
	 (initform nil)
	 (initargs nil)
	 (allocation nil)
	 (type t)
	 (namep  nil)
	 (initp  nil)
	 (allocp nil))

    (dolist (slotd direct-slotds)
      (when slotd
	(unless namep
	  (setq name (slot-definition-name slotd)
		namep t))
	(unless initp
	  (when (slot-definition-initfunction slotd)
	    (setq initform (slot-definition-initform slotd)
		  initfunction (slot-definition-initfunction slotd)
		  initp t)))
	(unless allocp
	  (setq allocation (slot-definition-allocation slotd)
		allocp t))
	(setq initargs (append (slot-definition-initargs slotd) initargs))
	(let ((slotd-type (slot-definition-type slotd)))
	  (setq type (cond ((eq type 't) slotd-type)
			   ((*subtypep type slotd-type) type)
			   (t `(and ,type ,slotd-type)))))))
    (list :name name
	  :initform initform
	  :initfunction initfunction
	  :initargs initargs
	  :allocation allocation
	  :type type
	  :class class)))

(defmethod compute-effective-slot-definition-initargs :around
    ((class structure-class) direct-slotds)
  (let ((slotd (car direct-slotds)))
    (list* :defstruct-accessor-symbol (slot-definition-defstruct-accessor-symbol slotd)
	   :internal-reader-function (slot-definition-internal-reader-function slotd)
	   :internal-writer-function (slot-definition-internal-writer-function slotd)
	   (call-next-method))))

;;; NOTE: For bootstrapping considerations, these can't use make-instance
;;;       to make the method object. They have to use make-a-method which
;;;       is a specially bootstrapped mechanism for making standard methods.
(defmethod reader-method-class ((class slot-class) direct-slot &rest initargs)
  (declare (ignore direct-slot initargs))
  (find-class 'standard-reader-method))

(defmethod add-reader-method ((class slot-class) generic-function slot-name)
  (add-method generic-function
	      (make-a-method 'standard-reader-method
			     ()
			     (list (or (class-name class) 'object))
			     (list class)
			     (make-reader-method-function class slot-name)
			     "automatically generated reader method"
			     slot-name)))

(defmethod writer-method-class ((class slot-class) direct-slot &rest initargs)
  (declare (ignore direct-slot initargs))
  (find-class 'standard-writer-method))

(defmethod add-writer-method ((class slot-class) generic-function slot-name)
  (add-method generic-function
	      (make-a-method 'standard-writer-method
			     ()
			     (list 'new-value (or (class-name class) 'object))
			     (list *the-class-t* class)
			     (make-writer-method-function class slot-name)
			     "automatically generated writer method"
			     slot-name)))

(defmethod add-boundp-method ((class slot-class) generic-function slot-name)
  (add-method generic-function
	      (make-a-method 'standard-boundp-method
			     ()
			     (list (or (class-name class) 'object))
			     (list class)
			     (make-boundp-method-function class slot-name)
			     "automatically generated boundp method"
			     slot-name)))

(defmethod remove-reader-method ((class slot-class) generic-function)
  (let ((method (get-method generic-function () (list class) nil)))
    (when method (remove-method generic-function method))))

(defmethod remove-writer-method ((class slot-class) generic-function)
  (let ((method
	  (get-method generic-function () (list *the-class-t* class) nil)))
    (when method (remove-method generic-function method))))

(defmethod remove-boundp-method ((class slot-class) generic-function)
  (let ((method (get-method generic-function () (list class) nil)))
    (when method (remove-method generic-function method))))

;;; make-reader-method-function and make-write-method function are NOT part of
;;; the standard protocol. They are however useful, PCL makes uses makes use
;;; of them internally and documents them for PCL users.
;;;
;;; *** This needs work to make type testing by the writer functions which
;;; *** do type testing faster. The idea would be to have one constructor
;;; *** for each possible type test. In order to do this it would be nice
;;; *** to have help from inform-type-system-about-class and friends.
;;;
;;; *** There is a subtle bug here which is going to have to be fixed.
;;; *** Namely, the simplistic use of the template has to be fixed. We
;;; *** have to give the optimize-slot-value method the user might have
;;; *** defined for this metclass a chance to run.

(defmethod make-reader-method-function ((class slot-class) slot-name)
  (make-std-reader-method-function (class-name class) slot-name))

(defmethod make-writer-method-function ((class slot-class) slot-name)
  (make-std-writer-method-function (class-name class) slot-name))

(defmethod make-boundp-method-function ((class slot-class) slot-name)
  (make-std-boundp-method-function (class-name class) slot-name))

;;;; inform-type-system-about-class
;;;; make-type-predicate
;;;
;;; These are NOT part of the standard protocol. They are internal mechanism
;;; which PCL uses to *try* and tell the type system about class definitions.
;;; In a more fully integrated implementation of CLOS, the type system would
;;; know about class objects and class names in a more fundamental way and
;;; the mechanism used to inform the type system about new classes would be
;;; different.
(defmethod inform-type-system-about-class ((class std-class) name)
  (inform-type-system-about-std-class name))

(defmethod compatible-meta-class-change-p (class proto-new-class)
  (eq (class-of class) (class-of proto-new-class)))

(defmethod validate-superclass ((class class) (new-super class))
  (or (eq new-super *the-class-t*)
      (eq (class-of class) (class-of new-super))))

(defmethod validate-superclass ((class standard-class) (new-super std-class))
  (let ((new-super-meta-class (class-of new-super)))
    (or (eq new-super-meta-class *the-class-std-class*)
	(eq (class-of class) new-super-meta-class))))

(defun force-cache-flushes (class)
  (let* ((owrapper (class-wrapper class))
	 (state (wrapper-state owrapper)))
    ;; We only need to do something if the state is still T. If the
    ;; state isn't T, it will be FLUSH or OBSOLETE, and both of those
    ;; will already be doing what we want. In particular, we must be
    ;; sure we never change an OBSOLETE into a FLUSH since OBSOLETE
    ;; means do what FLUSH does and then some.
    (when (eq state 't) ; FIXME: should be done through INVALID-WRAPPER-P
      (let ((nwrapper (make-wrapper (wrapper-no-of-instance-slots owrapper)
				    class)))
	(setf (wrapper-instance-slots-layout nwrapper)
	      (wrapper-instance-slots-layout owrapper))
	(setf (wrapper-class-slots nwrapper)
	      (wrapper-class-slots owrapper))
	(without-interrupts
	  (update-lisp-class-layout class nwrapper)
	  (setf (slot-value class 'wrapper) nwrapper)
	  (invalidate-wrapper owrapper ':flush nwrapper))))))

(defun flush-cache-trap (owrapper nwrapper instance)
  (declare (ignore owrapper))
  (set-wrapper instance nwrapper))

;;; make-instances-obsolete can be called by user code. It will cause the
;;; next access to the instance (as defined in 88-002R) to trap through the
;;; update-instance-for-redefined-class mechanism.
(defmethod make-instances-obsolete ((class std-class))
  (let* ((owrapper (class-wrapper class))
	 (nwrapper (make-wrapper (wrapper-no-of-instance-slots owrapper)
				 class)))
      (setf (wrapper-instance-slots-layout nwrapper)
	    (wrapper-instance-slots-layout owrapper))
      (setf (wrapper-class-slots nwrapper)
	    (wrapper-class-slots owrapper))
      (without-interrupts
	(update-lisp-class-layout class nwrapper)
	(setf (slot-value class 'wrapper) nwrapper)
	(invalidate-wrapper owrapper ':obsolete nwrapper)
	class)))

(defmethod make-instances-obsolete ((class symbol))
  (make-instances-obsolete (find-class class)))

;;; obsolete-instance-trap is the internal trap that is called when we see
;;; an obsolete instance. The times when it is called are:
;;;   - when the instance is involved in method lookup
;;;   - when attempting to access a slot of an instance
;;;
;;; It is not called by class-of, wrapper-of, or any of the low-level instance
;;; access macros.
;;;
;;; Of course these times when it is called are an internal implementation
;;; detail of PCL and are not part of the documented description of when the
;;; obsolete instance update happens. The documented description is as it
;;; appears in 88-002R.
;;;
;;; This has to return the new wrapper, so it counts on all the methods on
;;; obsolete-instance-trap-internal to return the new wrapper. It also does
;;; a little internal error checking to make sure that the traps are only
;;; happening when they should, and that the trap methods are computing
;;; appropriate new wrappers.

;;; obsolete-instance-trap might be called on structure instances
;;; after a structure is redefined. In most cases, obsolete-instance-trap
;;; will not be able to fix the old instance, so it must signal an
;;; error. The hard part of this is that the error system and debugger
;;; might cause obsolete-instance-trap to be called again, so in that
;;; case, we have to return some reasonable wrapper, instead.

(defvar *in-obsolete-instance-trap* nil)
(defvar *the-wrapper-of-structure-object*
  (class-wrapper (find-class 'structure-object)))

(define-condition obsolete-structure (error)
  ((datum :reader obsolete-structure-datum :initarg :datum))
  (:report
   (lambda (condition stream)
     ;; Don't try to print the structure, since it probably won't work.
     (format stream
	     "obsolete structure error in ~S:~@
	      for a structure of type: ~S"
	     (sb-conditions::condition-function-name condition)
	     (type-of (obsolete-structure-datum condition))))))

(defun obsolete-instance-trap (owrapper nwrapper instance)
  (if (not (pcl-instance-p instance))
      (if *in-obsolete-instance-trap*
	  *the-wrapper-of-structure-object*
	   (let ((*in-obsolete-instance-trap* t))
	     (error 'obsolete-structure :datum instance)))
      (let* ((class (wrapper-class* nwrapper))
	     (copy (allocate-instance class)) ;??? allocate-instance ???
	     (olayout (wrapper-instance-slots-layout owrapper))
	     (nlayout (wrapper-instance-slots-layout nwrapper))
	     (oslots (get-slots instance))
	     (nslots (get-slots copy))
	     (oclass-slots (wrapper-class-slots owrapper))
	     (added ())
	     (discarded ())
	     (plist ()))
	;; local  --> local	transfer
	;; local  --> shared       discard
	;; local  -->  --	  discard
	;; shared --> local	transfer
	;; shared --> shared       discard
	;; shared -->  --	  discard
	;;  --    --> local	add
	;;  --    --> shared	--

	;; Go through all the old local slots.
	(iterate ((name (list-elements olayout))
		  (opos (interval :from 0)))
	  (let ((npos (posq name nlayout)))
	    (if npos
		(setf (instance-ref nslots npos) (instance-ref oslots opos))
		(progn
		  (push name discarded)
		  (unless (eq (instance-ref oslots opos) *slot-unbound*)
		    (setf (getf plist name) (instance-ref oslots opos)))))))

	;; Go through all the old shared slots.
	(iterate ((oclass-slot-and-val (list-elements oclass-slots)))
	  (let ((name (car oclass-slot-and-val))
		(val (cdr oclass-slot-and-val)))
	    (let ((npos (posq name nlayout)))
	      (if npos
		  (setf (instance-ref nslots npos) (cdr oclass-slot-and-val))
		  (progn (push name discarded)
			 (unless (eq val *slot-unbound*)
			   (setf (getf plist name) val)))))))

	;; Go through all the new local slots to compute the added slots.
	(dolist (nlocal nlayout)
	  (unless (or (memq nlocal olayout)
		      (assq nlocal oclass-slots))
	    (push nlocal added)))

	(swap-wrappers-and-slots instance copy)

	(update-instance-for-redefined-class instance
					     added
					     discarded
					     plist)
	nwrapper)))

(defmacro copy-instance-internal (instance)
  `(progn
     (let* ((class (class-of instance))
	    (copy (allocate-instance class)))
       (if (std-instance-p ,instance)
	   (setf (std-instance-slots ,instance)
		 (std-instance-slots ,instance))
	 (setf (fsc-instance-slots ,instance)
	       (fsc-instance-slots ,instance)))
       copy)))

(defun change-class-internal (instance new-class)
  (let* ((old-class (class-of instance))
	 (copy (allocate-instance new-class))
	 (new-wrapper (get-wrapper copy))
	 (old-wrapper (class-wrapper old-class))
	 (old-layout (wrapper-instance-slots-layout old-wrapper))
	 (new-layout (wrapper-instance-slots-layout new-wrapper))
	 (old-slots (get-slots instance))
	 (new-slots (get-slots copy))
	 (old-class-slots (wrapper-class-slots old-wrapper)))

    ;; "The values of local slots specified by both the class CTO and
    ;; CFROM are retained. If such a local slot was unbound, it remains
    ;; unbound."
    (iterate ((new-slot (list-elements new-layout))
	      (new-position (interval :from 0)))
      (let ((old-position (posq new-slot old-layout)))
	(when old-position
	  (setf (instance-ref new-slots new-position)
		(instance-ref old-slots old-position)))))

    ;; "The values of slots specified as shared in the class CFROM and
    ;; as local in the class CTO are retained."
    (iterate ((slot-and-val (list-elements old-class-slots)))
      (let ((position (posq (car slot-and-val) new-layout)))
	(when position
	  (setf (instance-ref new-slots position) (cdr slot-and-val)))))

    ;; Make the copy point to the old instance's storage, and make the
    ;; old instance point to the new storage.
    (swap-wrappers-and-slots instance copy)

    (update-instance-for-different-class copy instance)
    instance))

(defmethod change-class ((instance standard-object)
			 (new-class standard-class))
  (change-class-internal instance new-class))

(defmethod change-class ((instance funcallable-standard-object)
			 (new-class funcallable-standard-class))
  (change-class-internal instance new-class))

(defmethod change-class ((instance standard-object)
			 (new-class funcallable-standard-class))
  (error "You can't change the class of ~S to ~S~@
	  because it isn't already an instance with metaclass ~S."
	 instance new-class 'standard-class))

(defmethod change-class ((instance funcallable-standard-object)
			 (new-class standard-class))
  (error "You can't change the class of ~S to ~S~@
	  because it isn't already an instance with metaclass ~S."
	 instance new-class 'funcallable-standard-class))

(defmethod change-class ((instance t) (new-class-name symbol))
  (change-class instance (find-class new-class-name)))

;;;; The metaclass BUILT-IN-CLASS
;;;;
;;;; This metaclass is something of a weird creature. By this point, all
;;;; instances of it which will exist have been created, and no instance
;;;; is ever created by calling MAKE-INSTANCE.
;;;;
;;;; But, there are other parts of the protocol we must follow and those
;;;; definitions appear here.

(defmethod shared-initialize :before
	   ((class built-in-class) slot-names &rest initargs)
  (declare (ignore slot-names initargs))
  (error "attempt to initialize or reinitialize a built in class"))

(defmethod class-direct-slots	    ((class built-in-class)) ())
(defmethod class-slots		   ((class built-in-class)) ())
(defmethod class-direct-default-initargs ((class built-in-class)) ())
(defmethod class-default-initargs	((class built-in-class)) ())

(defmethod validate-superclass ((c class) (s built-in-class))
  (or (eq s *the-class-t*)
      (eq s *the-class-stream*)))

(defmethod validate-superclass ((c slot-class)
				(f forward-referenced-class))
  't)

(defmethod add-dependent ((metaobject dependent-update-mixin) dependent)
  (pushnew dependent (plist-value metaobject 'dependents)))

(defmethod remove-dependent ((metaobject dependent-update-mixin) dependent)
  (setf (plist-value metaobject 'dependents)
	(delete dependent (plist-value metaobject 'dependents))))

(defmethod map-dependents ((metaobject dependent-update-mixin) function)
  (dolist (dependent (plist-value metaobject 'dependents))
    (funcall function dependent)))


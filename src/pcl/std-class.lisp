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

(defconstant +slotd-reader-function-std-p+ 1)
(defconstant +slotd-writer-function-std-p+ 2)
(defconstant +slotd-boundp-function-std-p+ 4)
(defconstant +slotd-all-function-std-p+ 7)

(defmethod slot-accessor-std-p ((slotd effective-slot-definition) type)
  (let ((flags (slot-value slotd 'accessor-flags)))
    (declare (type fixnum flags))
    (if (eq type 'all)
	(eql +slotd-all-function-std-p+ flags)
	(let ((mask (ecase type
		      (reader +slotd-reader-function-std-p+)
		      (writer +slotd-writer-function-std-p+)
		      (boundp +slotd-boundp-function-std-p+))))
	  (declare (type fixnum mask))
	  (not (zerop (the fixnum (logand mask flags))))))))

(defmethod (setf slot-accessor-std-p) (value
				       (slotd effective-slot-definition)
				       type)
  (let ((mask (ecase type
		(reader +slotd-reader-function-std-p+)
		(writer +slotd-writer-function-std-p+)
		(boundp +slotd-boundp-function-std-p+)))
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

;;; CMUCL (Gerd PCL 2003-04-25) comment:
;;;
;;; Compute an effective method for SLOT-VALUE-USING-CLASS, (SETF
;;; SLOT-VALUE-USING-CLASS) or SLOT-BOUNDP-USING-CLASS for reading/
;;; writing/testing effective slot SLOTD.
;;;
;;; TYPE is one of the symbols READER, WRITER or BOUNDP, depending on
;;; GF.  Store the effective method in the effective slot definition
;;; object itself; these GFs have special dispatch functions calling
;;; effective methods directly retrieved from effective slot
;;; definition objects, as an optimization.
;;;
;;; FIXME: Change the function name to COMPUTE-SVUC-SLOTD-FUNCTION,
;;; or some such.
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
	      (let (collect)
		(dolist (m (car direct-methods))
                  ;; the old PCL code used COLLECTING-ONCE which used
                  ;; #'EQ to check for newness
		  (pushnew (method-generic-function m) collect :test #'eq))
                (nreverse collect))))))

;;; This hash table is used to store the direct methods and direct generic
;;; functions of EQL specializers. Each value in the table is the cons.
(defvar *eql-specializer-methods* (make-hash-table :test 'eql))
(defvar *class-eq-specializer-methods* (make-hash-table :test 'eq))

(defmethod specializer-method-table ((specializer eql-specializer))
  *eql-specializer-methods*)

(defmethod specializer-method-table ((specializer class-eq-specializer))
  *class-eq-specializer-methods*)

(defmethod add-direct-method ((specializer specializer-with-object)
			      (method method))
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

(defmethod remove-direct-method ((specializer specializer-with-object)
				 (method method))
  (let* ((object (specializer-object specializer))
	 (entry (gethash object (specializer-method-table specializer))))
    (when entry
      (setf (car entry) (remove method (car entry))
	    (cdr entry) ()))
    method))

(defmethod specializer-direct-methods ((specializer specializer-with-object))
  (car (gethash (specializer-object specializer)
		(specializer-method-table specializer))))

(defmethod specializer-direct-generic-functions ((specializer
						  specializer-with-object))
  (let* ((object (specializer-object specializer))
	 (entry (gethash object (specializer-method-table specializer))))
    (when entry
      (or (cdr entry)
	  (setf (cdr entry)
		(let (collect)
		  (dolist (m (car entry))
		    (pushnew (method-generic-function m) collect :test #'eq))
                  (nreverse collect)))))))

(defun map-specializers (function)
  (map-all-classes (lambda (class)
		     (funcall function (class-eq-specializer class))
		     (funcall function class)))
  (maphash (lambda (object methods)
	     (declare (ignore methods))
	     (intern-eql-specializer object))
	   *eql-specializer-methods*)
  (maphash (lambda (object specl)
	     (declare (ignore object))
	     (funcall function specl))
	   *eql-specializer-table*)
  nil)

(defun map-all-generic-functions (function)
  (let ((all-generic-functions (make-hash-table :test 'eq)))
    (map-specializers (lambda (specl)
			(dolist (gf (specializer-direct-generic-functions
				     specl))
			  (unless (gethash gf all-generic-functions)
			    (setf (gethash gf all-generic-functions) t)
			    (funcall function gf))))))
  nil)

(defmethod shared-initialize :after ((specl class-eq-specializer)
				     slot-names
				     &key)
  (declare (ignore slot-names))
  (setf (slot-value specl 'type) `(class-eq ,(specializer-class specl))))

(defmethod shared-initialize :after ((specl eql-specializer) slot-names &key)
  (declare (ignore slot-names))
  (setf (slot-value specl 'type) `(eql ,(specializer-object specl))))

(defun real-load-defclass (name metaclass-name supers slots other)
  (let ((res (apply #'ensure-class name :metaclass metaclass-name
		    :direct-superclasses supers
		    :direct-slots slots
		    :definition-source `((defclass ,name)
					 ,*load-pathname*)
		    other)))
    res))

(setf (gdefinition 'load-defclass) #'real-load-defclass)

(defun ensure-class (name &rest all)
  (apply #'ensure-class-using-class (find-class name nil) name all))

(defmethod ensure-class-using-class ((class null) name &rest args &key)
  (multiple-value-bind (meta initargs)
      (ensure-class-values class args)
    (set-class-type-translation (class-prototype meta) name)
    (setf class (apply #'make-instance meta :name name initargs)
	  (find-class name) class)
    (set-class-type-translation class name)
    class))

(defmethod ensure-class-using-class ((class pcl-class) name &rest args &key)
  (multiple-value-bind (meta initargs)
      (ensure-class-values class args)
    (unless (eq (class-of class) meta)
      (apply #'change-class class meta initargs))
    (apply #'reinitialize-instance class initargs)
    (setf (find-class name) class)
    (set-class-type-translation class name)
    class))

(defmethod class-predicate-name ((class t))
  'constantly-nil)

(defun fix-super (s)
  (cond ((classp s) s)
        ((not (legal-class-name-p s))
	 (error "~S is not a class or a legal class name." s))
        (t
	 (or (find-class s nil)
	     (make-instance 'forward-referenced-class
			    :name s)))))

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
    ;; KLUDGE: It seemed to me initially that there ought to be a way
    ;; of collecting all the erroneous problems in one go, rather than
    ;; this way of solving the problem of signalling the errors that
    ;; we are required to, which stops at the first bogus input.
    ;; However, after playing around a little, I couldn't find that
    ;; way, so I've left it as is, but if someone does come up with a
    ;; better way... -- CSR, 2002-09-08
    (do ((direct-slots (getf initargs :direct-slots) (cdr direct-slots)))
	((endp direct-slots) nil)
      (destructuring-bind (slot &rest more) direct-slots
	(let ((slot-name (getf slot :name)))
	  (when (some (lambda (s) (eq slot-name (getf s :name))) more)
	    ;; FIXME: It's quite possible that we ought to define an
	    ;; SB-INT:PROGRAM-ERROR function to signal these and other
	    ;; errors throughout the codebase that are required to be
	    ;; of type PROGRAM-ERROR.
	    (error 'simple-program-error
		   :format-control "~@<There is more than one direct slot ~
                                   with name ~S.~:>"
		   :format-arguments (list slot-name)))
	  (do ((stuff slot (cddr stuff)))
	      ((endp stuff) nil)
	    (destructuring-bind (option value &rest more) stuff
	      (cond
		((and (member option '(:allocation :type
				       :initform :documentation))
		      (not (eq unsupplied
			       (getf more option unsupplied))))
		 (error 'simple-program-error
			:format-control "~@<Duplicate slot option ~S for ~
                                        slot named ~S.~:>"
			:format-arguments (list option slot-name)))
		((and (eq option :readers)
		      (notevery #'symbolp value))
		 (error 'simple-program-error
			:format-control "~@<Slot reader names for slot ~
                                        named ~S must be symbols.~:>"
			:format-arguments (list slot-name)))
		((and (eq option :initargs)
		      (notevery #'symbolp value))
		 (error 'simple-program-error
			:format-control "~@<Slot initarg names for slot ~
                                        named ~S must be symbols.~:>"
			:format-arguments (list slot-name)))))))))
    (loop for (initarg . more) on (getf initargs :direct-default-initargs)
	  for name = (car initarg) 
	  when (some (lambda (a) (eq (car a) name)) more) 
	  do (error 'simple-program-error 
		    :format-control "~@<Duplicate initialization argument ~
                                    name ~S in :DEFAULT-INITARGS.~:>"
		    :format-arguments (list name class)))
    (let ((metaclass 0)
	  (default-initargs 0))
      (do ((args initargs (cddr args)))
	  ((endp args) nil)
	(case (car args)
	  (:metaclass
	   (when (> (incf metaclass) 1)
	     (error 'simple-program-error
		    :format-control "~@<More than one :METACLASS ~
                                    option specified.~:>")))
	  (:direct-default-initargs
	   (when (> (incf default-initargs) 1)
	     (error 'simple-program-error
		    :format-control "~@<More than one :DEFAULT-INITARGS ~
                                    option specified.~:>"))))))
    (remf initargs :metaclass)
    (loop (unless (remf initargs :direct-superclasses) (return)))
    (loop (unless (remf initargs :direct-slots) (return)))
    (values
     meta
     (nconc
      (when (neq supplied-supers unsupplied)
	(list :direct-superclasses (mapcar #'fix-super supplied-supers)))
      (when (neq supplied-slots unsupplied)
	(list :direct-slots supplied-slots))
      initargs))))

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
		  (mapcar (lambda (pl) (make-direct-slotd class pl))
			  direct-slots))
	    (slot-value class 'direct-slots)))
  (if direct-default-initargs-p
      (setf (plist-value class 'direct-default-initargs)
	    direct-default-initargs)
      (setq direct-default-initargs
	    (plist-value class 'direct-default-initargs)))
  (setf (plist-value class 'class-slot-cells)
	(let (collect)
	  (dolist (dslotd direct-slots)
	    (when (eq :class (slot-definition-allocation dslotd))
	      (let ((initfunction (slot-definition-initfunction dslotd)))
		(push (cons (slot-definition-name dslotd)
			       (if initfunction
				   (funcall initfunction)
				   +slot-unbound+))
                      collect))))
          (nreverse collect)))
  (setq predicate-name (if predicate-name-p
			   (setf (slot-value class 'predicate-name)
				 (car predicate-name))
			   (or (slot-value class 'predicate-name)
			       (setf (slot-value class 'predicate-name)
				     (make-class-predicate-name (class-name
								 class))))))
  (add-direct-subclasses class direct-superclasses)
  (make-class-predicate class predicate-name)
  (update-class class nil)
  (do* ((slots (slot-value class 'slots) (cdr slots))
	(dupes nil))
       ((null slots) (when dupes
		       (style-warn
			;; FIXME: the indentation request ("~4I")
			;; below appears not to do anything.  Finding
			;; out why would be nice.  -- CSR, 2003-04-24
			"~@<slot names with the same SYMBOL-NAME but ~
                         different SYMBOL-PACKAGE (possible package problem) ~
                         for class ~S:~@:_~4I~<~@{~S~^~:@_~}~:>~@:>"
			class
			dupes)))
    (let* ((slot (car slots))
	   (oslots (remove (slot-definition-name slot) (cdr slots)
			   :test-not #'string= :key #'slot-definition-name)))
      (when oslots
	(pushnew (cons (slot-definition-name slot)
		       (mapcar #'slot-definition-name oslots))
		 dupes
		 :test #'string= :key #'car))))
  (add-slot-accessors class direct-slots)
  (make-preliminary-layout class))

(defmethod shared-initialize :after ((class forward-referenced-class)
				     slot-names &key &allow-other-keys)
  (declare (ignore slot-names))
  (make-preliminary-layout class))

(defvar *allow-forward-referenced-classes-in-cpl-p* nil)

;;; Give CLASS a preliminary layout if it doesn't have one already, to
;;; make it known to the type system.
(defun make-preliminary-layout (class)
  (flet ((compute-preliminary-cpl (root)
	   (let ((*allow-forward-referenced-classes-in-cpl-p* t))
	     (compute-class-precedence-list root))))
    (unless (class-finalized-p class)
      (let ((name (class-name class)))
	(setf (find-class name) class)
	;; KLUDGE: This is fairly horrible.  We need to make a
	;; full-fledged CLASSOID here, not just tell the compiler that
	;; some class is forthcoming, because there are legitimate
	;; questions one can ask of the type system, implemented in
	;; terms of CLASSOIDs, involving forward-referenced classes. So.
	(when (and (eq *boot-state* 'complete)
		   (null (find-classoid name nil)))
	  (setf (find-classoid name)
		(make-standard-classoid :name name)))
	(set-class-type-translation class name)
	(let ((layout (make-wrapper 0 class))
	      (classoid (find-classoid name)))
	  (setf (layout-classoid layout) classoid)
	  (setf (classoid-pcl-class classoid) class)
	  (setf (slot-value class 'wrapper) layout)
	  (let ((cpl (compute-preliminary-cpl class)))
	    (setf (layout-inherits layout)
		  (order-layout-inherits
		   (map 'simple-vector #'class-wrapper
			(reverse (rest cpl))))))
	  (register-layout layout :invalidate t)
	  (setf (classoid-layout classoid) layout)
	  (mapc #'make-preliminary-layout (class-direct-subclasses class)))))))


(defmethod shared-initialize :before ((class class) slot-names &key name)
  (declare (ignore slot-names name))
  ;; FIXME: Could this just be CLASS instead of `(CLASS ,CLASS)? If not,
  ;; why not? (See also similar expression in !BOOTSTRAP-INITIALIZE-CLASS.)
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
		  (lambda (dependent)
		    (apply #'update-dependent class dependent initargs))))

(defmethod shared-initialize :after ((class condition-class) slot-names
				     &key direct-superclasses)
  (declare (ignore slot-names))
  (let ((classoid (find-classoid (class-name class))))
    (with-slots (wrapper class-precedence-list prototype predicate-name
			 (direct-supers direct-superclasses))
	class
      (setf (slot-value class 'finalized-p) t)
      (setf (classoid-pcl-class classoid) class)
      (setq direct-supers direct-superclasses)
      (setq wrapper (classoid-layout classoid))
      (setq class-precedence-list (compute-class-precedence-list class))
      (setq prototype (make-condition (class-name class)))
      (add-direct-subclasses class direct-superclasses)
      (setq predicate-name (make-class-predicate-name (class-name class)))
      (make-class-predicate class predicate-name))))

(defmethod shared-initialize :after
    ((slotd structure-slot-definition) slot-names &key
     (allocation :instance) allocation-class)
  (declare (ignore slot-names allocation-class))
  (unless (eq allocation :instance)
    (error "Structure slots must have :INSTANCE allocation.")))

(defun make-structure-class-defstruct-form (name direct-slots include)
  (let* ((conc-name (intern (format nil "~S structure class " name)))
         (constructor (intern (format nil "~Aconstructor" conc-name)))
         (defstruct `(defstruct (,name
                                 ,@(when include
                                         `((:include ,(class-name include))))
                                 (:predicate nil)
                                 (:conc-name ,conc-name)
                                 (:constructor ,constructor ())
                                 (:copier nil))
                      ,@(mapcar (lambda (slot)
                                  `(,(slot-definition-name slot)
                                    +slot-unbound+))
                                direct-slots)))
         (reader-names (mapcar (lambda (slotd)
                                 (list 'slot-accessor name
				       (slot-definition-name slotd)
				       'reader))
                               direct-slots))
         (writer-names (mapcar (lambda (slotd)
                                 (list 'slot-accessor name
				       (slot-definition-name slotd)
				       'writer))
                               direct-slots))
         (readers-init
           (mapcar (lambda (slotd reader-name)
                     (let ((accessor
                             (slot-definition-defstruct-accessor-symbol
                              slotd)))
                       `(defun ,reader-name (obj)
                         (declare (type ,name obj))
                         (,accessor obj))))
                   direct-slots reader-names))
         (writers-init
           (mapcar (lambda (slotd writer-name)
                     (let ((accessor
                             (slot-definition-defstruct-accessor-symbol
                              slotd)))
                       `(defun ,writer-name (nv obj)
                         (declare (type ,name obj))
                         (setf (,accessor obj) nv))))
                   direct-slots writer-names))
         (defstruct-form
             `(progn
               ,defstruct
               ,@readers-init ,@writers-init
               (cons nil nil))))
    (values defstruct-form constructor reader-names writer-names)))

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
		    (mapcar (lambda (pl)
			      (when defstruct-p
				(let* ((slot-name (getf pl :name))
				       (acc-name
					(format nil
						"~S structure class ~A"
						name slot-name))
				       (accessor (intern acc-name)))
				  (setq pl (list* :defstruct-accessor-symbol
						  accessor pl))))
			      (make-direct-slotd class pl))
			    direct-slots)))
	(setq direct-slots (slot-value class 'direct-slots)))
    (when defstruct-p
      (let ((include (car (slot-value class 'direct-superclasses))))
        (multiple-value-bind (defstruct-form constructor reader-names writer-names)
            (make-structure-class-defstruct-form name direct-slots include)
          (unless (structure-type-p name) (eval defstruct-form))
          (mapc (lambda (dslotd reader-name writer-name)
		  (let* ((reader (gdefinition reader-name))
			 (writer (when (gboundp writer-name)
				   (gdefinition writer-name))))
		    (setf (slot-value dslotd 'internal-reader-function)
			  reader)
		    (setf (slot-value dslotd 'internal-writer-function)
			  writer)))
                direct-slots reader-names writer-names)
          (setf (slot-value class 'defstruct-form) defstruct-form)
          (setf (slot-value class 'defstruct-constructor) constructor))))
    (add-direct-subclasses class direct-superclasses)
    (setf (slot-value class 'class-precedence-list)
            (compute-class-precedence-list class))
    (setf (slot-value class 'slots) (compute-slots class))
    (let ((lclass (find-classoid (class-name class))))
      (setf (classoid-pcl-class lclass) class)
      (setf (slot-value class 'wrapper) (classoid-layout lclass)))
    (setf (slot-value class 'finalized-p) t)
    (update-pv-table-cache-info class)
    (setq predicate-name (if predicate-name-p
			   (setf (slot-value class 'predicate-name)
                                   (car predicate-name))
			   (or (slot-value class 'predicate-name)
			       (setf (slot-value class 'predicate-name)
                                       (make-class-predicate-name
                                        (class-name class))))))
    (make-class-predicate class predicate-name)
    (add-slot-accessors class direct-slots)))

(defmethod direct-slot-definition-class ((class structure-class) &rest initargs)
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

(defun add-direct-subclasses (class supers)
  (dolist (super supers)
    (unless (memq class (class-direct-subclasses class))
      (add-direct-subclass super class))))

(defun remove-direct-subclasses (class supers)
  (let ((old (class-direct-superclasses class)))
    (dolist (o (set-difference old supers))
      (remove-direct-subclass o class))))

(defmethod finalize-inheritance ((class std-class))
  (update-class class t))

(defmethod finalize-inheritance ((class forward-referenced-class))
  ;; FIXME: should we not be thinking a bit about what kinds of error
  ;; we're throwing?  Maybe we need a clos-error type to mix in?  Or
  ;; possibly a forward-referenced-class-error, though that's
  ;; difficult given e.g. class precedence list calculations...
  (error
   "~@<FINALIZE-INHERITANCE was called on a forward referenced class:~
       ~2I~_~S~:>"
   class))


(defun class-has-a-forward-referenced-superclass-p (class)
  (or (forward-referenced-class-p class)
      (some #'class-has-a-forward-referenced-superclass-p
	    (class-direct-superclasses class))))

;;; This is called by :after shared-initialize whenever a class is initialized
;;; or reinitialized. The class may or may not be finalized.
(defun update-class (class finalizep)
  ;; Comment from Gerd Moellmann:
  ;;
  ;; Note that we can't simply delay the finalization when CLASS has
  ;; no forward referenced superclasses because that causes bootstrap
  ;; problems.
  (when (and (not finalizep)
	     (not (class-finalized-p class))
	     (not (class-has-a-forward-referenced-superclass-p class)))
    (finalize-inheritance class)
    (return-from update-class))
  (when (or finalizep (class-finalized-p class)
	    (not (class-has-a-forward-referenced-superclass-p class)))
    (setf (find-class (class-name class)) class)
    (update-cpl class (compute-class-precedence-list class))
    ;; This invocation of UPDATE-SLOTS, in practice, finalizes the
    ;; class.  The hoops above are to ensure that FINALIZE-INHERITANCE
    ;; is called at finalization, so that MOP programmers can hook
    ;; into the system as described in "Class Finalization Protocol"
    ;; (section 5.5.2 of AMOP).
    (update-slots class (compute-slots class))
    (update-gfs-of-class class)
    (update-inits class (compute-default-initargs class))
    (update-ctors 'finalize-inheritance :class class))
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
	(case alloc
          (:instance (push eslotd instance-slots))
          (:class (push eslotd class-slots)))))

    ;; If there is a change in the shape of the instances then the
    ;; old class is now obsolete.
    (let* ((nlayout (mapcar #'slot-definition-name
			    (sort instance-slots #'<
				  :key #'slot-definition-location)))
	   (nslots (length nlayout))
	   (nwrapper-class-slots (compute-class-slots class-slots))
	   (owrapper (when (class-finalized-p class)
		       (class-wrapper class)))
	   (olayout (when owrapper
		      (wrapper-instance-slots-layout owrapper)))
	   (owrapper-class-slots (and owrapper (wrapper-class-slots owrapper)))
	   (nwrapper
	    (cond ((null owrapper)
		   (make-wrapper nslots class))
		  ((and (equal nlayout olayout)
			(not
                         (loop for o in owrapper-class-slots
                               for n in nwrapper-class-slots
                               do (unless (eq (car o) (car n)) (return t)))))
		   owrapper)
		  (t
		   ;; This will initialize the new wrapper to have the
		   ;; same state as the old wrapper. We will then have
		   ;; to change that. This may seem like wasted work
		   ;; (and it is), but the spec requires that we call
		   ;; MAKE-INSTANCES-OBSOLETE.
		   (make-instances-obsolete class)
		   (class-wrapper class)))))

      (with-slots (wrapper slots) class
	(update-lisp-class-layout class nwrapper)
	(setf slots eslotds
	      (wrapper-instance-slots-layout nwrapper) nlayout
	      (wrapper-class-slots nwrapper) nwrapper-class-slots
	      (wrapper-no-of-instance-slots nwrapper) nslots
	      wrapper nwrapper))
      (setf (slot-value class 'finalized-p) t)
      (unless (eq owrapper nwrapper)
	(update-pv-table-cache-info class)))))

(defun compute-class-slots (eslotds)
  (let (collect)
    (dolist (eslotd eslotds)
      (push (assoc (slot-definition-name eslotd)
                   (class-slot-cells (slot-definition-class eslotd)))
            collect))
    (nreverse collect)))

(defun update-gfs-of-class (class)
  (when (and (class-finalized-p class)
	     (let ((cpl (class-precedence-list class)))
	       (or (member *the-class-slot-class* cpl)
		   (member *the-class-standard-effective-slot-definition*
			   cpl))))
    (let ((gf-table (make-hash-table :test 'eq)))
      (labels ((collect-gfs (class)
		 (dolist (gf (specializer-direct-generic-functions class))
		   (setf (gethash gf gf-table) t))
		 (mapc #'collect-gfs (class-direct-superclasses class))))
	(collect-gfs class)
	(maphash (lambda (gf ignore)
		   (declare (ignore ignore))
		   (update-gf-dfun class gf))
		 gf-table)))))

(defun update-inits (class inits)
  (setf (plist-value class 'default-initargs) inits))

(defmethod compute-default-initargs ((class slot-class))
  (let ((initargs (loop for c in (class-precedence-list class)
			append (class-direct-default-initargs c))))
    (delete-duplicates initargs :test #'eq :key #'car :from-end t)))

;;;; protocols for constructing direct and effective slot definitions

(defmethod direct-slot-definition-class ((class std-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'standard-direct-slot-definition))

(defun make-direct-slotd (class initargs)
  (let ((initargs (list* :class class initargs)))
    (apply #'make-instance
	   (apply #'direct-slot-definition-class class initargs)
	   initargs)))

(defmethod compute-slots ((class std-class))
  ;; As specified, we must call COMPUTE-EFFECTIVE-SLOT-DEFINITION once
  ;; for each different slot name we find in our superclasses. Each
  ;; call receives the class and a list of the dslotds with that name.
  ;; The list is in most-specific-first order.
  (let ((name-dslotds-alist ()))
    (dolist (c (class-precedence-list class))
      (dolist (slot (class-direct-slots c))
	(let* ((name (slot-definition-name slot))
	       (entry (assq name name-dslotds-alist)))
	  (if entry
	      (push slot (cdr entry))
	      (push (list name slot) name-dslotds-alist)))))
    (mapcar (lambda (direct)
	      (compute-effective-slot-definition class
						 (car direct)
						 (nreverse (cdr direct))))
	    name-dslotds-alist)))

(defmethod compute-slots ((class standard-class))
  (call-next-method))

(defmethod compute-slots :around ((class standard-class))
  (let ((eslotds (call-next-method))
	(location -1))
    (dolist (eslotd eslotds eslotds)
      (setf (slot-definition-location eslotd)
	    (ecase (slot-definition-allocation eslotd)
	      (:instance
	       (incf location))
	      (:class
	       (let* ((name (slot-definition-name eslotd))
		      (from-class (slot-definition-allocation-class eslotd))
		      (cell (assq name (class-slot-cells from-class))))
		 (aver (consp cell))
		 cell))))
      (initialize-internal-slot-functions eslotd))))

(defmethod compute-slots ((class funcallable-standard-class))
  (call-next-method))

(defmethod compute-slots :around ((class funcallable-standard-class))
  (labels ((instance-slot-names (slotds)
	     (let (collect)
	       (dolist (slotd slotds (nreverse collect))
		 (when (eq (slot-definition-allocation slotd) :instance)
		   (push (slot-definition-name slotd) collect)))))
	   ;; This sorts slots so that slots of classes later in the CPL
           ;; come before slots of other classes.  This is crucial for
           ;; funcallable instances because it ensures that the slots of
           ;; FUNCALLABLE-STANDARD-OBJECT, which includes the slots of
           ;; KERNEL:FUNCALLABLE-INSTANCE, come first, which in turn
           ;; makes it possible to treat FUNCALLABLE-STANDARD-OBJECT as
           ;; a funcallable instance.
	   (compute-layout (eslotds)
	     (let ((first ())
		   (names (instance-slot-names eslotds)))
	       (dolist (class
			(reverse (class-precedence-list class))
			(nreverse (nconc names first)))
		 (dolist (ss (class-slots class))
		   (let ((name (slot-definition-name ss)))
		     (when (member name names)
		       (push name first)
		       (setq names (delete name names)))))))))
    (let ((all-slotds (call-next-method))
	  (instance-slots ())
	  (class-slots ()))
      (dolist (slotd all-slotds)
	(ecase (slot-definition-allocation slotd)
	  (:instance (push slotd instance-slots))
	  (:class (push slotd class-slots))))
      (let ((layout (compute-layout instance-slots)))
	(dolist (slotd instance-slots)
	  (setf (slot-definition-location slotd)
		(position (slot-definition-name slotd) layout))
	  (initialize-internal-slot-functions slotd)))
      (dolist (slotd class-slots)
	(let ((name (slot-definition-name slotd))
	      (from-class (slot-definition-allocation-class slotd)))
	  (setf (slot-definition-location slotd)
		(assoc name (class-slot-cells from-class)))
	  (aver (consp (slot-definition-location slotd)))
	  (initialize-internal-slot-functions slotd)))
      all-slotds)))

(defmethod compute-slots ((class structure-class))
  (mapcan (lambda (superclass)
	    (mapcar (lambda (dslotd)
		      (compute-effective-slot-definition
		       class
		       (slot-definition-name dslotd)
		       (list dslotd)))
		    (class-direct-slots superclass)))
	  (reverse (slot-value class 'class-precedence-list))))

(defmethod compute-slots :around ((class structure-class))
  (let ((eslotds (call-next-method)))
    (mapc #'initialize-internal-slot-functions eslotds)
    eslotds))

(defmethod compute-effective-slot-definition ((class slot-class) name dslotds)
  (declare (ignore name))
  (let* ((initargs (compute-effective-slot-definition-initargs class dslotds))
	 (class (apply #'effective-slot-definition-class class initargs)))
    (apply #'make-instance class initargs)))

(defmethod effective-slot-definition-class ((class std-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'standard-effective-slot-definition))

(defmethod effective-slot-definition-class ((class structure-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'structure-effective-slot-definition))

(defmethod compute-effective-slot-definition-initargs
    ((class slot-class) direct-slotds)
  (let* ((name nil)
	 (initfunction nil)
	 (initform nil)
	 (initargs nil)
	 (allocation nil)
	 (allocation-class nil)
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
		allocation-class (slot-definition-class slotd)
		allocp t))
	(setq initargs (append (slot-definition-initargs slotd) initargs))
	(let ((slotd-type (slot-definition-type slotd)))
	  (setq type (cond ((eq type t) slotd-type)
			   ((*subtypep type slotd-type) type)
			   (t `(and ,type ,slotd-type)))))))
    (list :name name
	  :initform initform
	  :initfunction initfunction
	  :initargs initargs
	  :allocation allocation
	  :allocation-class allocation-class
	  :type type
	  :class class)))

(defmethod compute-effective-slot-definition-initargs :around
    ((class structure-class) direct-slotds)
  (let ((slotd (car direct-slotds)))
    (list* :defstruct-accessor-symbol
	   (slot-definition-defstruct-accessor-symbol slotd)
	   :internal-reader-function
	   (slot-definition-internal-reader-function slotd)
	   :internal-writer-function
	   (slot-definition-internal-writer-function slotd)
	   (call-next-method))))

;;; NOTE: For bootstrapping considerations, these can't use MAKE-INSTANCE
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

;;; MAKE-READER-METHOD-FUNCTION and MAKE-WRITE-METHOD function are NOT
;;; part of the standard protocol. They are however useful, PCL makes
;;; use of them internally and documents them for PCL users.
;;;
;;; *** This needs work to make type testing by the writer functions which
;;; *** do type testing faster. The idea would be to have one constructor
;;; *** for each possible type test.
;;;
;;; *** There is a subtle bug here which is going to have to be fixed.
;;; *** Namely, the simplistic use of the template has to be fixed. We
;;; *** have to give the OPTIMIZE-SLOT-VALUE method the user might have
;;; *** defined for this metaclass a chance to run.

(defmethod make-reader-method-function ((class slot-class) slot-name)
  (make-std-reader-method-function (class-name class) slot-name))

(defmethod make-writer-method-function ((class slot-class) slot-name)
  (make-std-writer-method-function (class-name class) slot-name))

(defmethod make-boundp-method-function ((class slot-class) slot-name)
  (make-std-boundp-method-function (class-name class) slot-name))

(defmethod compatible-meta-class-change-p (class proto-new-class)
  (eq (class-of class) (class-of proto-new-class)))

(defmethod validate-superclass ((class class) (new-super class))
  (or (eq new-super *the-class-t*)
      (eq (class-of class) (class-of new-super))))

(defmethod validate-superclass ((class standard-class) (new-super std-class))
  (let ((new-super-meta-class (class-of new-super)))
    (or (eq new-super-meta-class *the-class-std-class*)
	(eq (class-of class) new-super-meta-class))))

;;; What this does depends on which of the four possible values of
;;; LAYOUT-INVALID the PCL wrapper has; the simplest case is when it
;;; is (:FLUSH <wrapper>) or (:OBSOLETE <wrapper>), when there is
;;; nothing to do, as the new wrapper has already been created.  If
;;; LAYOUT-INVALID returns NIL, then we invalidate it (setting it to
;;; (:FLUSH <wrapper>); UPDATE-SLOTS later gets to choose whether or
;;; not to "upgrade" this to (:OBSOLETE <wrapper>).
;;;
;;; This leaves the case where LAYOUT-INVALID returns T, which happens
;;; when REGISTER-LAYOUT has invalidated a superclass of CLASS (which
;;; invalidated all the subclasses in SB-KERNEL land).  Again, here we
;;; must flush the caches and allow UPDATE-SLOTS to decide whether to
;;; obsolete the wrapper.
;;;
;;; FIXME: either here or in INVALID-WRAPPER-P looks like a good place
;;; for (AVER (NOT (EQ (LAYOUT-INVALID OWRAPPER)
;;;                    :UNINITIALIZED)))
;;;
;;; Thanks to Gerd Moellmann for the explanation.  -- CSR, 2002-10-29
(defun force-cache-flushes (class)
  (let* ((owrapper (class-wrapper class)))
    ;; We only need to do something if the wrapper is still valid. If
    ;; the wrapper isn't valid, state will be FLUSH or OBSOLETE, and
    ;; both of those will already be doing what we want. In
    ;; particular, we must be sure we never change an OBSOLETE into a
    ;; FLUSH since OBSOLETE means do what FLUSH does and then some.
    (when (or (not (invalid-wrapper-p owrapper))
	      ;; KLUDGE: despite the observations above, this remains
	      ;; a violation of locality or what might be considered
	      ;; good style.  There has to be a better way!  -- CSR,
	      ;; 2002-10-29
	      (eq (layout-invalid owrapper) t))
      (let ((nwrapper (make-wrapper (wrapper-no-of-instance-slots owrapper)
				    class)))
	(setf (wrapper-instance-slots-layout nwrapper)
	      (wrapper-instance-slots-layout owrapper))
	(setf (wrapper-class-slots nwrapper)
	      (wrapper-class-slots owrapper))
	(with-pcl-lock
	  (update-lisp-class-layout class nwrapper)
	  (setf (slot-value class 'wrapper) nwrapper)
	  (invalidate-wrapper owrapper :flush nwrapper))))))

(defun flush-cache-trap (owrapper nwrapper instance)
  (declare (ignore owrapper))
  (set-wrapper instance nwrapper))

;;; MAKE-INSTANCES-OBSOLETE can be called by user code. It will cause
;;; the next access to the instance (as defined in 88-002R) to trap
;;; through the UPDATE-INSTANCE-FOR-REDEFINED-CLASS mechanism.
(defmethod make-instances-obsolete ((class std-class))
  (let* ((owrapper (class-wrapper class))
	 (nwrapper (make-wrapper (wrapper-no-of-instance-slots owrapper)
				 class)))
      (setf (wrapper-instance-slots-layout nwrapper)
	    (wrapper-instance-slots-layout owrapper))
      (setf (wrapper-class-slots nwrapper)
	    (wrapper-class-slots owrapper))
      (with-pcl-lock
	(update-lisp-class-layout class nwrapper)
	(setf (slot-value class 'wrapper) nwrapper)
	(invalidate-wrapper owrapper :obsolete nwrapper)
	class)))

(defmethod make-instances-obsolete ((class symbol))
  (make-instances-obsolete (find-class class)))

;;; OBSOLETE-INSTANCE-TRAP is the internal trap that is called when we
;;; see an obsolete instance. The times when it is called are:
;;;   - when the instance is involved in method lookup
;;;   - when attempting to access a slot of an instance
;;;
;;; It is not called by class-of, wrapper-of, or any of the low-level
;;; instance access macros.
;;;
;;; Of course these times when it is called are an internal
;;; implementation detail of PCL and are not part of the documented
;;; description of when the obsolete instance update happens. The
;;; documented description is as it appears in 88-002R.
;;;
;;; This has to return the new wrapper, so it counts on all the
;;; methods on obsolete-instance-trap-internal to return the new
;;; wrapper. It also does a little internal error checking to make
;;; sure that the traps are only happening when they should, and that
;;; the trap methods are computing appropriate new wrappers.

;;; OBSOLETE-INSTANCE-TRAP might be called on structure instances
;;; after a structure is redefined. In most cases,
;;; OBSOLETE-INSTANCE-TRAP will not be able to fix the old instance,
;;; so it must signal an error. The hard part of this is that the
;;; error system and debugger might cause OBSOLETE-INSTANCE-TRAP to be
;;; called again, so in that case, we have to return some reasonable
;;; wrapper, instead.

(defvar *in-obsolete-instance-trap* nil)
(defvar *the-wrapper-of-structure-object*
  (class-wrapper (find-class 'structure-object)))

(define-condition obsolete-structure (error)
  ((datum :reader obsolete-structure-datum :initarg :datum))
  (:report
   (lambda (condition stream)
     ;; Don't try to print the structure, since it probably won't work.
     (format stream
	     "~@<obsolete structure error for a structure of type ~2I~_~S~:>"
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
        (let ((opos 0))
          (dolist (name olayout)
            (let ((npos (posq name nlayout)))
              (if npos
                  (setf (clos-slots-ref nslots npos)
                        (clos-slots-ref oslots opos))
                  (progn
                    (push name discarded)
                    (unless (eq (clos-slots-ref oslots opos) +slot-unbound+)
                      (setf (getf plist name) (clos-slots-ref oslots opos))))))
            (incf opos)))

	;; Go through all the old shared slots.
        (dolist (oclass-slot-and-val oclass-slots)
	  (let ((name (car oclass-slot-and-val))
		(val (cdr oclass-slot-and-val)))
	    (let ((npos (posq name nlayout)))
	      (if npos
		  (setf (clos-slots-ref nslots npos) (cdr oclass-slot-and-val))
		  (progn (push name discarded)
			 (unless (eq val +slot-unbound+)
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

(defun change-class-internal (instance new-class initargs)
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
    ;; CFROM are retained. If such a local slot was unbound, it
    ;; remains unbound."
    (let ((new-position 0))
      (dolist (new-slot new-layout)
        (let ((old-position (posq new-slot old-layout)))
          (when old-position
            (setf (clos-slots-ref new-slots new-position)
                  (clos-slots-ref old-slots old-position))))
	(incf new-position)))

    ;; "The values of slots specified as shared in the class CFROM and
    ;; as local in the class CTO are retained."
    (dolist (slot-and-val old-class-slots)
      (let ((position (posq (car slot-and-val) new-layout)))
	(when position
	  (setf (clos-slots-ref new-slots position) (cdr slot-and-val)))))

    ;; Make the copy point to the old instance's storage, and make the
    ;; old instance point to the new storage.
    (swap-wrappers-and-slots instance copy)

    (apply #'update-instance-for-different-class copy instance initargs)
    instance))

(defmethod change-class ((instance standard-object)
			 (new-class standard-class)
			 &rest initargs)
  (change-class-internal instance new-class initargs))

(defmethod change-class ((instance funcallable-standard-object)
			 (new-class funcallable-standard-class)
			 &rest initargs)
  (change-class-internal instance new-class initargs))

(defmethod change-class ((instance standard-object)
			 (new-class funcallable-standard-class)
			 &rest initargs)
  (declare (ignore initargs))
  (error "You can't change the class of ~S to ~S~@
	  because it isn't already an instance with metaclass ~S."
	 instance new-class 'standard-class))

(defmethod change-class ((instance funcallable-standard-object)
			 (new-class standard-class)
			 &rest initargs)
  (declare (ignore initargs))
  (error "You can't change the class of ~S to ~S~@
	  because it isn't already an instance with metaclass ~S."
	 instance new-class 'funcallable-standard-class))

(defmethod change-class ((instance t) (new-class-name symbol) &rest initargs)
  (apply #'change-class instance (find-class new-class-name) initargs))

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

;;; Some necessary methods for FORWARD-REFERENCED-CLASS
(defmethod class-direct-slots ((class forward-referenced-class)) ())
(defmethod class-direct-default-initargs ((class forward-referenced-class)) ())
(macrolet ((def (method)
             `(defmethod ,method ((class forward-referenced-class))
                (error "~@<~I~S was called on a forward referenced class:~2I~_~S~:>"
                       ',method class))))
  (def class-default-initargs)
  (def class-precedence-list)
  (def class-slots))

(defmethod validate-superclass ((c slot-class)
				(f forward-referenced-class))
  t)

(defmethod add-dependent ((metaobject dependent-update-mixin) dependent)
  (pushnew dependent (plist-value metaobject 'dependents)))

(defmethod remove-dependent ((metaobject dependent-update-mixin) dependent)
  (setf (plist-value metaobject 'dependents)
	(delete dependent (plist-value metaobject 'dependents))))

(defmethod map-dependents ((metaobject dependent-update-mixin) function)
  (dolist (dependent (plist-value metaobject 'dependents))
    (funcall function dependent)))


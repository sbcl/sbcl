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

(defun ensure-accessor (type fun-name slot-name)
  (labels ((slot-missing-fun (slot-name type)
	     (let* ((method-type (ecase type
				   (slot-value 'reader-method)
				   (setf 'writer-method)
				   (slot-boundp 'boundp-method)))
		    (initargs
		     (copy-tree
		      (ecase type
			(slot-value
			 (make-method-function
			  (lambda (obj)
			    (values
			     (slot-missing (class-of obj) obj slot-name
					   'slot-value)))))
			(slot-boundp
			 (make-method-function
			  (lambda (obj)
			    (not (not
				  (slot-missing (class-of obj) obj slot-name
						'slot-boundp))))))
			(setf
			 (make-method-function
			  (lambda (val obj)
			    (slot-missing (class-of obj) obj slot-name
					  'setf val)
			    val)))))))
	       (setf (getf (getf initargs :plist) :slot-name-lists)
		     (list (list nil slot-name)))
	       (setf (getf (getf initargs :plist) :pv-table-symbol)
                     (gensym))
	       (list* :method-spec (list method-type 'slot-object slot-name)
                      initargs)))
	   (add-slot-missing-method (gf slot-name type)
	     (multiple-value-bind (class lambda-list specializers)
                 (ecase type
                   (slot-value
                    (values 'standard-reader-method
                            '(object)
                            (list *the-class-slot-object*)))
                   (slot-boundp
                    (values 'standard-boundp-method
                            '(object)
                            (list *the-class-slot-object*)))
                   (setf
                    (values 'standard-writer-method
                            '(new-value object)
                            (list *the-class-t* *the-class-slot-object*))))
               (add-method gf (make-a-method class
                                             ()
                                             lambda-list
                                             specializers
                                             (slot-missing-fun slot-name type)
                                             "generated slot-missing method"
                                             slot-name)))))
    (unless (fboundp fun-name)
      (let ((gf (ensure-generic-function
		 fun-name
		 :lambda-list (ecase type
				((reader boundp) '(object))
				(writer '(new-value object))))))
        (ecase type
          (reader (add-slot-missing-method gf slot-name 'slot-value))
          (boundp (add-slot-missing-method gf slot-name 'slot-boundp))
          (writer (add-slot-missing-method gf slot-name 'setf)))
        (setf (plist-value gf 'slot-missing-method) t))
      t)))

(defmacro accessor-slot-value (object slot-name)
  (aver (constantp slot-name))
  (let* ((slot-name (eval slot-name))
	 (reader-name (slot-reader-name slot-name)))
    `(let ((.ignore. (load-time-value
		      (ensure-accessor 'reader ',reader-name ',slot-name))))
      (declare (ignore .ignore.))
      (truly-the (values t &optional)
                 (funcall #',reader-name ,object)))))

(defmacro accessor-set-slot-value (object slot-name new-value &environment env)
  (aver (constantp slot-name))
  (setq object (macroexpand object env))
  (setq slot-name (macroexpand slot-name env))
  (let* ((slot-name (eval slot-name))
	 (bindings (unless (or (constantp new-value) (atom new-value))
		     (let ((object-var (gensym)))
		       (prog1 `((,object-var ,object))
			 (setq object object-var)))))
	 (writer-name (slot-writer-name slot-name))
	 (form
	  `(let ((.ignore.
		  (load-time-value
		   (ensure-accessor 'writer ',writer-name ',slot-name)))
		 (.new-value. ,new-value))
	    (declare (ignore .ignore.))
	    (funcall #',writer-name .new-value. ,object)
	    .new-value.)))
    (if bindings
	`(let ,bindings ,form)
	form)))

(defmacro accessor-slot-boundp (object slot-name)
  (aver (constantp slot-name))
  (let* ((slot-name (eval slot-name))
	 (boundp-name (slot-boundp-name slot-name)))
    `(let ((.ignore. (load-time-value
		      (ensure-accessor 'boundp ',boundp-name ',slot-name))))
      (declare (ignore .ignore.))
      (funcall #',boundp-name ,object))))

(defun make-structure-slot-boundp-function (slotd)
  (declare (ignore slotd))
  (lambda (object)
    (declare (ignore object))
    t))

(define-condition instance-structure-protocol-error
    (reference-condition error)
  ((slotd :initarg :slotd :reader instance-structure-protocol-error-slotd)
   (fun :initarg :fun :reader instance-structure-protocol-error-fun))
  (:report
   (lambda (c s)
     (format s "~@<The slot ~S has neither ~S nor ~S ~
                allocation, so it can't be ~A by the default ~
                ~S method.~@:>"
	     (instance-structure-protocol-error-slotd c)
	     :instance :class
	     (cond
	       ((member (instance-structure-protocol-error-fun c)
			'(slot-value-using-class slot-boundp-using-class))
		"read")
	       (t "written"))
	     (instance-structure-protocol-error-fun c)))))

(defun instance-structure-protocol-error (slotd fun)
  (error 'instance-structure-protocol-error
	 :slotd slotd :fun fun
	 :references (list `(:amop :generic-function ,fun)
			   '(:amop :section (5 5 3)))))

(defun get-optimized-std-accessor-method-function (class slotd name)
  (cond
    ((structure-class-p class)
     (ecase name
       (reader (slot-definition-internal-reader-function slotd))
       (writer (slot-definition-internal-writer-function slotd))
       (boundp (make-structure-slot-boundp-function slotd))))
    ((condition-class-p class)
     (ecase name
       (reader (slot-definition-reader-function slotd))
       (writer (slot-definition-writer-function slotd))
       (boundp (slot-definition-boundp-function slotd))))
    (t
     (let* ((fsc-p (cond ((standard-class-p class) nil)
			 ((funcallable-standard-class-p class) t)
			 ((std-class-p class)
			  ;; Shouldn't be using the optimized-std-accessors
			  ;; in this case.
			  #+nil (format t "* warning: ~S ~S~%   ~S~%"
					name slotd class)
			  nil)
			 (t (error "~S is not a STANDARD-CLASS." class))))
	    (slot-name (slot-definition-name slotd))
	    (location (slot-definition-location slotd))
	    (function (ecase name
			(reader #'make-optimized-std-reader-method-function)
			(writer #'make-optimized-std-writer-method-function)
			(boundp #'make-optimized-std-boundp-method-function)))
	    ;; KLUDGE: we need this slightly hacky calling convention
	    ;; for these functions for bootstrapping reasons: see
	    ;; !BOOTSTRAP-MAKE-SLOT-DEFINITION in braid.lisp.  -- CSR,
	    ;; 2004-07-12
	    (value (funcall function fsc-p slotd slot-name location)))
       (declare (type function function))
       (values value (slot-definition-location slotd))))))

(defun make-optimized-std-reader-method-function
    (fsc-p slotd slot-name location)
  (declare #.*optimize-speed*)
  (set-fun-name
   (etypecase location
     (fixnum
      (if fsc-p
	  (lambda (instance)
	    (check-obsolete-instance instance)
	    (let ((value (clos-slots-ref (fsc-instance-slots instance)
					 location)))
	      (if (eq value +slot-unbound+)
		  (values
		   (slot-unbound (class-of instance) instance slot-name))
		  value)))
	  (lambda (instance)
	    (check-obsolete-instance instance)
	    (let ((value (clos-slots-ref (std-instance-slots instance)
					 location)))
	      (if (eq value +slot-unbound+)
		  (values
		   (slot-unbound (class-of instance) instance slot-name))
		  value)))))
     (cons
      (lambda (instance)
	(check-obsolete-instance instance)
	(let ((value (cdr location)))
	  (if (eq value +slot-unbound+)
	      (values (slot-unbound (class-of instance) instance slot-name))
	      value))))
     (null
      (lambda (instance)
	(instance-structure-protocol-error slotd 'slot-value-using-class))))
   `(reader ,slot-name)))

(defun make-optimized-std-writer-method-function
    (fsc-p slotd slot-name location)
  (declare #.*optimize-speed*)
  (set-fun-name
   (etypecase location
     (fixnum (if fsc-p
		 (lambda (nv instance)
		   (check-obsolete-instance instance)
		   (setf (clos-slots-ref (fsc-instance-slots instance)
					 location)
			 nv))
		 (lambda (nv instance)
		   (check-obsolete-instance instance)
		   (setf (clos-slots-ref (std-instance-slots instance)
					 location)
			 nv))))
     (cons (lambda (nv instance)
	     (check-obsolete-instance instance)
	     (setf (cdr location) nv)))
     (null
      (lambda (nv instance)
	(declare (ignore nv))
	(instance-structure-protocol-error slotd
					   '(setf slot-value-using-class)))))
   `(writer ,slot-name)))

(defun make-optimized-std-boundp-method-function
    (fsc-p slotd slot-name location)
  (declare #.*optimize-speed*)
  (set-fun-name
   (etypecase location
     (fixnum (if fsc-p
		 (lambda (instance)
		   (check-obsolete-instance instance)
		   (not (eq (clos-slots-ref (fsc-instance-slots instance)
					    location)
			    +slot-unbound+)))
		 (lambda (instance)
		   (check-obsolete-instance instance)
		   (not (eq (clos-slots-ref (std-instance-slots instance)
					    location)
			    +slot-unbound+)))))
     (cons (lambda (instance)
	     (check-obsolete-instance instance)
	     (not (eq (cdr location) +slot-unbound+))))
     (null
      (lambda (instance)
	(instance-structure-protocol-error slotd 'slot-boundp-using-class))))
   `(boundp ,slot-name)))

(defun make-optimized-structure-slot-value-using-class-method-function
    (function)
  (declare (type function function))
  (lambda (class object slotd)
    (declare (ignore class slotd))
    (funcall function object)))

(defun make-optimized-structure-setf-slot-value-using-class-method-function
    (function)
  (declare (type function function))
  (lambda (nv class object slotd)
    (declare (ignore class slotd))
    (funcall function nv object)))

(defun make-optimized-structure-slot-boundp-using-class-method-function ()
  (lambda (class object slotd)
    (declare (ignore class object slotd))
    t))

(defun get-optimized-std-slot-value-using-class-method-function
    (class slotd name)
  (cond
    ((structure-class-p class)
     (ecase name
       (reader (make-optimized-structure-slot-value-using-class-method-function
		(slot-definition-internal-reader-function slotd)))
       (writer (make-optimized-structure-setf-slot-value-using-class-method-function
		(slot-definition-internal-writer-function slotd)))
       (boundp (make-optimized-structure-slot-boundp-using-class-method-function))))
    ((condition-class-p class)
     (ecase name
       (reader
	(let ((fun (slot-definition-reader-function slotd)))
	  (declare (type function fun))
	  (lambda (class object slotd)
	    (declare (ignore class slotd))
	    (funcall fun object))))
       (writer
	(let ((fun (slot-definition-writer-function slotd)))
	  (declare (type function fun))
	  (lambda (new-value class object slotd)
	    (declare (ignore class slotd))
	    (funcall fun new-value object))))
       (boundp
	(let ((fun (slot-definition-boundp-function slotd)))
	  (declare (type function fun))
	  (lambda (class object slotd)
	    (declare (ignore class slotd))
	    (funcall fun object))))))
    (t
     (let* ((fsc-p (cond ((standard-class-p class) nil)
			 ((funcallable-standard-class-p class) t)
			 (t (error "~S is not a standard-class" class))))
	    (function
	     (ecase name
	       (reader
		#'make-optimized-std-slot-value-using-class-method-function)
	       (writer
		#'make-optimized-std-setf-slot-value-using-class-method-function)
	       (boundp
		#'make-optimized-std-slot-boundp-using-class-method-function))))
       (declare (type function function))
       (values (funcall function fsc-p slotd)
	       (slot-definition-location slotd))))))

(defun make-optimized-std-slot-value-using-class-method-function (fsc-p slotd)
  (declare #.*optimize-speed*)
  (let ((location (slot-definition-location slotd))
	(slot-name (slot-definition-name slotd)))
    (etypecase location
      (fixnum (if fsc-p
		  (lambda (class instance slotd)
		    (declare (ignore slotd))
		    (check-obsolete-instance instance)
		    (let ((value (clos-slots-ref (fsc-instance-slots instance)
						 location)))
		      (if (eq value +slot-unbound+)
			  (values (slot-unbound class instance slot-name))
			  value)))
		  (lambda (class instance slotd)
		    (declare (ignore slotd))
		    (check-obsolete-instance instance)
		    (let ((value (clos-slots-ref (std-instance-slots instance)
						 location)))
		      (if (eq value +slot-unbound+)
			  (values (slot-unbound class instance slot-name))
			  value)))))
      (cons (lambda (class instance slotd)
	      (declare (ignore slotd))
	      (check-obsolete-instance instance)
	      (let ((value (cdr location)))
		(if (eq value +slot-unbound+)
		    (values (slot-unbound class instance slot-name))
		    value))))
      (null
       (lambda (class instance slotd)
	 (declare (ignore class instance))
	 (instance-structure-protocol-error slotd 'slot-value-using-class))))))

(defun make-optimized-std-setf-slot-value-using-class-method-function
    (fsc-p slotd)
  (declare #.*optimize-speed*)
  (let ((location (slot-definition-location slotd)))
    (etypecase location
      (fixnum
       (if fsc-p
	   (lambda (nv class instance slotd)
	     (declare (ignore class slotd))
	     (check-obsolete-instance instance)
	     (setf (clos-slots-ref (fsc-instance-slots instance) location)
		   nv))
	   (lambda (nv class instance slotd)
	     (declare (ignore class slotd))
	     (check-obsolete-instance instance)
	     (setf (clos-slots-ref (std-instance-slots instance) location)
		   nv))))
      (cons (lambda (nv class instance slotd)
	      (declare (ignore class slotd))
	      (check-obsolete-instance instance)
	      (setf (cdr location) nv)))
      (null (lambda (nv class instance slotd)
	      (declare (ignore nv class instance))
	      (instance-structure-protocol-error
	       slotd '(setf slot-value-using-class)))))))

(defun make-optimized-std-slot-boundp-using-class-method-function
    (fsc-p slotd)
  (declare #.*optimize-speed*)
  (let ((location (slot-definition-location slotd)))
    (etypecase location
      (fixnum
       (if fsc-p
	   (lambda (class instance slotd)
	     (declare (ignore class slotd))
	     (check-obsolete-instance instance)
	     (not (eq (clos-slots-ref (fsc-instance-slots instance) location)
		      +slot-unbound+)))
	   (lambda (class instance slotd)
	     (declare (ignore class slotd))
	     (check-obsolete-instance instance)
	     (not (eq (clos-slots-ref (std-instance-slots instance) location)
		      +slot-unbound+)))))
      (cons (lambda (class instance slotd)
	      (declare (ignore class slotd))
	      (check-obsolete-instance instance)
	      (not (eq (cdr location) +slot-unbound+))))
      (null
       (lambda (class instance slotd)
	 (declare (ignore class instance))
	 (instance-structure-protocol-error slotd
					    'slot-boundp-using-class))))))

(defun get-accessor-from-svuc-method-function (class slotd sdfun name)
  (macrolet ((emf-funcall (emf &rest args)
	       `(invoke-effective-method-function ,emf nil ,@args)))
    (set-fun-name
     (case name
       (reader (lambda (instance)
		 (emf-funcall sdfun class instance slotd)))
       (writer (lambda (nv instance)
		 (emf-funcall sdfun nv class instance slotd)))
       (boundp (lambda (instance)
		 (emf-funcall sdfun class instance slotd))))
     `(,name ,(class-name class) ,(slot-definition-name slotd)))))

(defun make-internal-reader-method-function (class-name slot-name)
  (list* :method-spec `(internal-reader-method ,class-name ,slot-name)
	 (make-method-function
	  (lambda (instance)
	    (let ((wrapper (get-instance-wrapper-or-nil instance)))
	      (if wrapper
		  (let* ((class (wrapper-class* wrapper))
			 (index (or (instance-slot-index wrapper slot-name)
				    (assq slot-name
					  (wrapper-class-slots wrapper)))))
		    (typecase index
		      (fixnum 	
		       (let ((value (clos-slots-ref (get-slots instance)
						    index)))
			 (if (eq value +slot-unbound+)
			     (values (slot-unbound (class-of instance)
						   instance
						   slot-name))
			     value)))
		      (cons
		       (let ((value (cdr index)))
			 (if (eq value +slot-unbound+)
			     (values (slot-unbound (class-of instance)
						   instance
						   slot-name))
			     value)))
		      (t
		       (error "~@<The wrapper for class ~S does not have ~
                               the slot ~S~@:>"
			      class slot-name))))
		  (slot-value instance slot-name)))))))

(defun make-std-reader-method-function (class-name slot-name)
  (let* ((pv-table-symbol (gensym))
	 (initargs (copy-tree
		    (make-method-function
		     (lambda (instance)
		       (pv-binding1 (.pv. .calls.
					  (symbol-value pv-table-symbol)
					  (instance) (instance-slots))
			 (instance-read-internal
			  .pv. instance-slots 1
			  (slot-value instance slot-name))))))))
    (setf (getf (getf initargs :plist) :slot-name-lists)
	  (list (list nil slot-name)))
    (setf (getf (getf initargs :plist) :pv-table-symbol) pv-table-symbol)
    (list* :method-spec `(reader-method ,class-name ,slot-name)
	   initargs)))

(defun make-std-writer-method-function (class-name slot-name)
  (let* ((pv-table-symbol (gensym))
	 (initargs (copy-tree
		    (make-method-function
		     (lambda (nv instance)
		       (pv-binding1 (.pv. .calls.
					  (symbol-value pv-table-symbol)
					  (instance) (instance-slots))
			 (instance-write-internal
			  .pv. instance-slots 1 nv
			  (setf (slot-value instance slot-name) nv))))))))
    (setf (getf (getf initargs :plist) :slot-name-lists)
	  (list nil (list nil slot-name)))
    (setf (getf (getf initargs :plist) :pv-table-symbol) pv-table-symbol)
    (list* :method-spec `(writer-method ,class-name ,slot-name)
	   initargs)))

(defun make-std-boundp-method-function (class-name slot-name)
  (let* ((pv-table-symbol (gensym))
	 (initargs (copy-tree
		    (make-method-function
		     (lambda (instance)
		       (pv-binding1 (.pv. .calls.
					  (symbol-value pv-table-symbol)
					  (instance) (instance-slots))
			  (instance-boundp-internal
			   .pv. instance-slots 1
			   (slot-boundp instance slot-name))))))))
    (setf (getf (getf initargs :plist) :slot-name-lists)
	  (list (list nil slot-name)))
    (setf (getf (getf initargs :plist) :pv-table-symbol) pv-table-symbol)
    (list* :method-spec `(boundp-method ,class-name ,slot-name)
	   initargs)))

(defun initialize-internal-slot-gfs (slot-name &optional type)
  (macrolet ((frob (type name-fun add-fun ll)
	       `(when (or (null type) (eq type ',type))
		 (let* ((name (,name-fun slot-name))
			(gf (ensure-generic-function name
						     :lambda-list ',ll))
			(methods (generic-function-methods gf)))
		   (when (or (null methods)
			     (plist-value gf 'slot-missing-method))
		     (setf (plist-value gf 'slot-missing-method) nil)
		     (,add-fun *the-class-slot-object* gf slot-name))))))
    (frob reader slot-reader-name add-reader-method (object))
    (frob writer slot-writer-name add-writer-method (new-value object))
    (frob boundp slot-boundp-name add-boundp-method (object))))

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

(defmacro asv-funcall (sym slot-name type &rest args)
  (declare (ignore type))
  `(if (fboundp ',sym)
       (,sym ,@args)
       (no-slot ',sym ',slot-name)))

(defun no-slot (sym slot-name)
  (error "No class has a slot named ~S (~S has no function binding)."
	 slot-name sym))

(defmacro accessor-slot-value (object slot-name)
  (unless (constantp slot-name)
    (error "~S requires its slot-name argument to be a constant"
	   'accessor-slot-value))
  (let* ((slot-name (eval slot-name))
	 (sym (slot-reader-symbol slot-name)))
    `(asv-funcall ,sym ,slot-name reader ,object)))

(defmacro accessor-set-slot-value (object slot-name new-value &environment env)
  (unless (constantp slot-name)
    (error "~S requires its slot-name argument to be a constant"
	   'accessor-set-slot-value))
  (setq object (macroexpand object env))
  (setq slot-name (macroexpand slot-name env))
  (let* ((slot-name (eval slot-name))
	 (bindings (unless (or (constantp new-value) (atom new-value))
		     (let ((object-var (gensym)))
		       (prog1 `((,object-var ,object))
			 (setq object object-var)))))
	 (sym (slot-writer-symbol slot-name))
	 (form `(asv-funcall ,sym ,slot-name writer ,new-value ,object)))
    (if bindings
	`(let ,bindings ,form)
	form)))

(defmacro accessor-slot-boundp (object slot-name)
  (unless (constantp slot-name)
    (error "~S requires its slot-name argument to be a constant"
	   'accessor-slot-boundp))
  (let ((slot-name (eval slot-name)))
    `(slot-boundp-normal ,object ',slot-name)))

(defun structure-slot-boundp (object)
  (declare (ignore object))
  t)

(defun make-structure-slot-boundp-function (slotd)
  (let* ((reader (slot-definition-internal-reader-function slotd))
	 (fun (lambda (object)
		(not (eq (funcall reader object) +slot-unbound+)))))
    (declare (type function reader))
    fun))

(defun get-optimized-std-accessor-method-function (class slotd name)
  (if (structure-class-p class)
      (ecase name
	(reader (slot-definition-internal-reader-function slotd))
	(writer (slot-definition-internal-writer-function slotd))
	(boundp (make-structure-slot-boundp-function slotd)))
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
	     (index (slot-definition-location slotd))
	     (function (ecase name
			 (reader #'make-optimized-std-reader-method-function)
			 (writer #'make-optimized-std-writer-method-function)
			 (boundp #'make-optimized-std-boundp-method-function)))
	     (value (funcall function fsc-p slot-name index)))
	(declare (type function function))
	(values value index))))

(defun make-optimized-std-reader-method-function (fsc-p slot-name index)
  (declare #.*optimize-speed*)
  (set-fun-name
   (etypecase index
     (fixnum (if fsc-p
		 (lambda (instance)
		   (let ((value (clos-slots-ref (fsc-instance-slots instance)
						index)))
		     (if (eq value +slot-unbound+)
			 (slot-unbound (class-of instance) instance slot-name)
			 value)))
		 (lambda (instance)
		   (let ((value (clos-slots-ref (std-instance-slots instance)
					      index)))
		     (if (eq value +slot-unbound+)
			 (slot-unbound (class-of instance) instance slot-name)
			 value)))))
     (cons   (lambda (instance)
	       (let ((value (cdr index)))
		 (if (eq value +slot-unbound+)
		     (slot-unbound (class-of instance) instance slot-name)
		     value)))))
   `(reader ,slot-name)))

(defun make-optimized-std-writer-method-function (fsc-p slot-name index)
  (declare #.*optimize-speed*)
  (set-fun-name
   (etypecase index
     (fixnum (if fsc-p
		 (lambda (nv instance)
		   (setf (clos-slots-ref (fsc-instance-slots instance) index)
			 nv))
		 (lambda (nv instance)
		   (setf (clos-slots-ref (std-instance-slots instance) index)
			 nv))))
     (cons   (lambda (nv instance)
	       (declare (ignore instance))
	       (setf (cdr index) nv))))
   `(writer ,slot-name)))

(defun make-optimized-std-boundp-method-function (fsc-p slot-name index)
  (declare #.*optimize-speed*)
  (set-fun-name
   (etypecase index
     (fixnum (if fsc-p
		 (lambda (instance)
		   (not (eq (clos-slots-ref (fsc-instance-slots instance)
					    index)
			    +slot-unbound+)))
		 (lambda (instance)
		   (not (eq (clos-slots-ref (std-instance-slots instance)
					    index)
			    +slot-unbound+)))))
     (cons (lambda (instance)
	     (declare (ignore instance))
	     (not (eq (cdr index) +slot-unbound+)))))
   `(boundp ,slot-name)))

(defun make-optimized-structure-slot-value-using-class-method-function (function)
  (declare (type function function))
  (lambda (class object slotd)
    (let ((value (funcall function object)))
      (if (eq value +slot-unbound+)
	  (slot-unbound class object (slot-definition-name slotd))
	  value))))

(defun make-optimized-structure-setf-slot-value-using-class-method-function (function)
  (declare (type function function))
  (lambda (nv class object slotd)
    (declare (ignore class slotd))
    (funcall function nv object)))

(defun make-optimized-structure-slot-boundp-using-class-method-function (function)
  (declare (type function function))
  (lambda (class object slotd)
    (declare (ignore class slotd))
    (not (eq (funcall function object) +slot-unbound+))))

(defun get-optimized-std-slot-value-using-class-method-function (class
								 slotd
								 name)
  (if (structure-class-p class)
      (ecase name
	(reader (make-optimized-structure-slot-value-using-class-method-function
		 (slot-definition-internal-reader-function slotd)))
	(writer (make-optimized-structure-setf-slot-value-using-class-method-function
		 (slot-definition-internal-writer-function slotd)))
	(boundp (make-optimized-structure-slot-boundp-using-class-method-function
		 (slot-definition-internal-writer-function slotd))))
      (let* ((fsc-p (cond ((standard-class-p class) nil)
			  ((funcallable-standard-class-p class) t)
			  (t (error "~S is not a standard-class" class))))
	     (slot-name (slot-definition-name slotd))
	     (index (slot-definition-location slotd))
	     (function
	      (ecase name
		(reader
		 #'make-optimized-std-slot-value-using-class-method-function)
		(writer
		 #'make-optimized-std-setf-slot-value-using-class-method-function)
		(boundp
		 #'make-optimized-std-slot-boundp-using-class-method-function))))
	(declare (type function function))
	(values (funcall function fsc-p slot-name index) index))))

(defun make-optimized-std-slot-value-using-class-method-function
    (fsc-p slot-name index)
  (declare #.*optimize-speed*)
  (etypecase index
    (fixnum (if fsc-p
		(lambda (class instance slotd)
		  (declare (ignore slotd))
		  (unless (fsc-instance-p instance) (error "not fsc"))
		  (let ((value (clos-slots-ref (fsc-instance-slots instance)
					       index)))
		    (if (eq value +slot-unbound+)
			(slot-unbound class instance slot-name)
			value)))
		(lambda (class instance slotd)
		  (declare (ignore slotd))
		  (unless (std-instance-p instance) (error "not std"))
		  (let ((value (clos-slots-ref (std-instance-slots instance)
					       index)))
		    (if (eq value +slot-unbound+)
			(slot-unbound class instance slot-name)
			value)))))
    (cons   (lambda (class instance slotd)
	      (declare (ignore slotd))
	      (let ((value (cdr index)))
		(if (eq value +slot-unbound+)
		    (slot-unbound class instance slot-name)
		    value))))))

(defun make-optimized-std-setf-slot-value-using-class-method-function
    (fsc-p slot-name index)
  (declare #.*optimize-speed*)
  (declare (ignore slot-name))
  (etypecase index
    (fixnum (if fsc-p
		(lambda (nv class instance slotd)
		  (declare (ignore class slotd))
		  (setf (clos-slots-ref (fsc-instance-slots instance) index)
			nv))
		(lambda (nv class instance slotd)
		  (declare (ignore class slotd))
		  (setf (clos-slots-ref (std-instance-slots instance) index)
			nv))))
    (cons  (lambda (nv class instance slotd)
	     (declare (ignore class instance slotd))
	     (setf (cdr index) nv)))))

(defun make-optimized-std-slot-boundp-using-class-method-function
    (fsc-p slot-name index)
  (declare #.*optimize-speed*)
  (declare (ignore slot-name))
  (etypecase index
    (fixnum (if fsc-p
		(lambda (class instance slotd)
		  (declare (ignore class slotd))
		  (not (eq (clos-slots-ref (fsc-instance-slots instance) index)
			   +slot-unbound+)))
		(lambda (class instance slotd)
		  (declare (ignore class slotd))
		  (not (eq (clos-slots-ref (std-instance-slots instance) index)
			   +slot-unbound+)))))
    (cons   (lambda (class instance slotd)
	      (declare (ignore class instance slotd))
	      (not (eq (cdr index) +slot-unbound+))))))

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
			     (slot-unbound (class-of instance)
					   instance
					   slot-name)
			     value)))
		      (cons
		       (let ((value (cdr index)))
			 (if (eq value +slot-unbound+)
			     (slot-unbound (class-of instance)
					   instance
					   slot-name)
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
  (when (or (null type) (eq type 'reader))
    (let* ((name (slot-reader-symbol slot-name))
	   (gf (ensure-generic-function name)))
      (unless (generic-function-methods gf)
	(add-reader-method *the-class-slot-object* gf slot-name))))
  (when (or (null type) (eq type 'writer))
    (let* ((name (slot-writer-symbol slot-name))
	   (gf (ensure-generic-function name)))
      (unless (generic-function-methods gf)
	(add-writer-method *the-class-slot-object* gf slot-name))))
  nil)

(defun initialize-internal-slot-gfs* (readers writers boundps)
  (dolist (reader readers)
    (initialize-internal-slot-gfs reader 'reader))
  (dolist (writer writers)
    (initialize-internal-slot-gfs writer 'writer))
  (dolist (boundp boundps)
    (initialize-internal-slot-gfs boundp 'boundp)))

;;;; This file defines the optimized make-instance functions.

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

(defvar *compile-make-instance-functions-p* nil)

(defun update-make-instance-function-table (&optional (class *the-class-t*))
  (when (symbolp class) (setq class (find-class class)))
    (when (eq class *the-class-t*) (setq class *the-class-slot-object*))
    (when (memq *the-class-slot-object* (class-precedence-list class))
      (map-all-classes #'reset-class-initialize-info class)))

(defun constant-symbol-p (form)
  (and (constantp form)
       (let ((object (eval form)))
	 (and (symbolp object)
	      (symbol-package object)))))

(defvar *make-instance-function-keys* nil)

(defun expand-make-instance-form (form)
  (let ((class (cadr form)) (initargs (cddr form))
	(keys nil)(allow-other-keys-p nil) key value)
    (when (and (constant-symbol-p class)
	       (let ((initargs-tail initargs))
		 (loop (when (null initargs-tail) (return t))
		       (unless (constant-symbol-p (car initargs-tail))
			 (return nil))
		       (setq key (eval (pop initargs-tail)))
		       (setq value (pop initargs-tail))
		       (when (eq ':allow-other-keys key)
			 (setq allow-other-keys-p value))
		       (push key keys))))
      (let* ((class (eval class))
	     (keys (nreverse keys))
	     (key (list class keys allow-other-keys-p))
	     (sym (make-instance-function-symbol key)))
	(push key *make-instance-function-keys*)
	(when sym
	  ;; (famous last words:
	  ;;   1. Don't worry, I know what I'm doing.
	  ;;   2. You and what army?
	  ;;   3. If you were as smart as you think you are, you
	  ;;      wouldn't be a copy.
	  ;; This is case #1.:-) Even if SYM hasn't been defined yet,
	  ;; it must be an implementation function, or we we wouldn't
	  ;; have expanded into it. So declare SYM as defined, so that
	  ;; even if it hasn't been defined yet, the user doesn't get
	  ;; obscure warnings about undefined internal implementation
	  ;; functions like HAIRY-MAKE-instance-name.
	  (sb-kernel:become-defined-fun-name sym)
	  `(,sym ',class (list ,@initargs)))))))

(defmacro expanding-make-instance-top-level (&rest forms &environment env)
  (let* ((*make-instance-function-keys* nil)
	 (form (macroexpand `(expanding-make-instance ,@forms) env)))
    `(progn
       ,@(when *make-instance-function-keys*
	   `((get-make-instance-functions ',*make-instance-function-keys*)))
       ,form)))

(defmacro expanding-make-instance (&rest forms &environment env)
  `(progn
     ,@(mapcar #'(lambda (form)
		   (walk-form form env
			      #'(lambda (subform context env)
				  (declare (ignore env))
				  (or (and (eq context ':eval)
					   (consp subform)
					   (eq (car subform) 'make-instance)
					   (expand-make-instance-form subform))
				      subform))))
	       forms)))

(defmacro defconstructor
	  (name class lambda-list &rest initialization-arguments)
  `(expanding-make-instance-top-level
    (defun ,name ,lambda-list
      (make-instance ',class ,@initialization-arguments))))

(defun get-make-instance-functions (key-list)
  (dolist (key key-list)
    (let* ((cell (find-class-cell (car key)))
	   (make-instance-function-keys
	    (find-class-cell-make-instance-function-keys cell))
	   (mif-key (cons (cadr key) (caddr key))))
      (unless (find mif-key make-instance-function-keys
		    :test #'equal)
	(push mif-key (find-class-cell-make-instance-function-keys cell))
	(let ((class (find-class-cell-class cell)))
	  (when (and class (not (forward-referenced-class-p class)))
	    (update-initialize-info-internal
	     (initialize-info class (car mif-key) nil (cdr mif-key))
	     'make-instance-function)))))))

(defun make-instance-function-symbol (key)
  (let* ((class (car key))
	 (symbolp (symbolp class)))
    (when (or symbolp (classp class))
      (let* ((class-name (if (symbolp class) class (class-name class)))
	     (keys (cadr key))
	     (allow-other-keys-p (caddr key)))
	(when (and (or symbolp
		       (and (symbolp class-name)
			    (eq class (find-class class-name nil))))
		   (symbol-package class-name))
	  (let ((*package* *pcl-package*)
		(*print-length* nil)
		(*print-level* nil)
		(*print-circle* nil)
		(*print-case* :upcase)
		(*print-pretty* nil))
	    (intern (format nil
			    "MAKE-INSTANCE ~S ~S ~S"
			    class-name
			    keys
			    allow-other-keys-p))))))))

(defun make-instance-1 (class initargs)
  (apply #'make-instance class initargs))

(defmacro define-cached-reader (type name trap)
  (let ((reader-name (intern (format nil "~A-~A" type name)))
	(cached-name (intern (format nil "~A-CACHED-~A" type name))))
    `(defmacro ,reader-name (info)
       `(let ((value (,',cached-name ,info)))
	  (if (eq value ':unknown)
	      (progn
		(,',trap ,info ',',name)
		(,',cached-name ,info))
	      value)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defparameter *initialize-info-cached-slots*
  '(valid-p				; t or (:invalid key)
    ri-valid-p
    initargs-form-list
    new-keys
    default-initargs-function
    shared-initialize-t-function
    shared-initialize-nil-function
    constants
    combined-initialize-function ; allocate-instance + shared-initialize
    make-instance-function ; nil means use gf
    make-instance-function-symbol)))

(defmacro define-initialize-info ()
  (let ((cached-slot-names
	 (mapcar #'(lambda (name)
		     (intern (format nil "CACHED-~A" name)))
		 *initialize-info-cached-slots*))
	(cached-names
	 (mapcar #'(lambda (name)
		     (intern (format nil "~A-CACHED-~A"
				     'initialize-info name)))
		 *initialize-info-cached-slots*)))
    `(progn
       (defstruct (initialize-info (:copier nil))
	 key wrapper
	 ,@(mapcar #'(lambda (name)
		       `(,name :unknown))
		   cached-slot-names))
       (defmacro reset-initialize-info-internal (info)
	 `(progn
	    ,@(mapcar #'(lambda (cname)
			  `(setf (,cname ,info) ':unknown))
		      ',cached-names)))
       (defun initialize-info-bound-slots (info)
	 (let ((slots nil))
	   ,@(mapcar #'(lambda (name cached-name)
			 `(unless (eq ':unknown (,cached-name info))
			    (push ',name slots)))
		     *initialize-info-cached-slots* cached-names)
	   slots))
      ,@(mapcar #'(lambda (name)
		    `(define-cached-reader initialize-info ,name
		      update-initialize-info-internal))
		*initialize-info-cached-slots*))))

(define-initialize-info)

(defvar *initialize-info-cache-class* nil)
(defvar *initialize-info-cache-initargs* nil)
(defvar *initialize-info-cache-info* nil)

(defvar *revert-initialize-info-p* nil)

(defun reset-initialize-info (info)
  (setf (initialize-info-wrapper info)
	(class-wrapper (car (initialize-info-key info))))
  (let ((slots-to-revert (if *revert-initialize-info-p*
			     (initialize-info-bound-slots info)
			     '(make-instance-function))))
    (reset-initialize-info-internal info)
    (dolist (slot slots-to-revert)
      (update-initialize-info-internal info slot))
    info))

(defun reset-class-initialize-info (class)
  (reset-class-initialize-info-1 (class-initialize-info class)))

(defun reset-class-initialize-info-1 (cell)
  (when (consp cell)
    (when (car cell)
      (reset-initialize-info (car cell)))
    (let ((alist (cdr cell)))
      (dolist (a alist)
	(reset-class-initialize-info-1 (cdr a))))))

(defun initialize-info (class
			initargs
			&optional
			(plist-p t)
			allow-other-keys-arg)
  (let ((info nil))
    (if (and (eq *initialize-info-cache-class* class)
	     (eq *initialize-info-cache-initargs* initargs))
	(setq info *initialize-info-cache-info*)
	(let ((initargs-tail initargs)
	      (cell (or (class-initialize-info class)
			(setf (class-initialize-info class) (cons nil nil)))))
	  (loop (when (null initargs-tail) (return nil))
		(let ((keyword (pop initargs-tail))
		      (alist-cell cell))
		  (when plist-p
		    (if (eq keyword :allow-other-keys)
			(setq allow-other-keys-arg (pop initargs-tail))
			(pop initargs-tail)))
		  (loop (let ((alist (cdr alist-cell)))
			  (when (null alist)
			    (setq cell (cons nil nil))
			    (setf (cdr alist-cell) (list (cons keyword cell)))
			    (return nil))
			  (when (eql keyword (caar alist))
			    (setq cell (cdar alist))
			    (return nil))
			  (setq alist-cell alist)))))
	  (setq info (or (car cell)
			 (setf (car cell) (make-initialize-info))))))
    (let ((wrapper (initialize-info-wrapper info)))
      (unless (eq wrapper (class-wrapper class))
	(unless wrapper
	  (let* ((initargs-tail initargs)
		 (klist-cell (list nil))
		 (klist-tail klist-cell))
	    (loop (when (null initargs-tail) (return nil))
		  (let ((key (pop initargs-tail)))
		    (setf (cdr klist-tail) (list key)))
		  (setf klist-tail (cdr klist-tail))
		  (when plist-p (pop initargs-tail)))
	    (setf (initialize-info-key info)
		  (list class (cdr klist-cell) allow-other-keys-arg))))
	(reset-initialize-info info)))
    (setq *initialize-info-cache-class* class)
    (setq *initialize-info-cache-initargs* initargs)
    (setq *initialize-info-cache-info* info)
    info))

(defun update-initialize-info-internal (info name)
  (let* ((key (initialize-info-key info))
	 (class (car key))
	 (keys (cadr key))
	 (allow-other-keys-arg (caddr key)))
    (ecase name
      ((initargs-form-list new-keys)
       (multiple-value-bind (initargs-form-list new-keys)
	   (make-default-initargs-form-list class keys)
	 (setf (initialize-info-cached-initargs-form-list info)
	       initargs-form-list)
	 (setf (initialize-info-cached-new-keys info) new-keys)))
      ((default-initargs-function)
       (let ((initargs-form-list (initialize-info-initargs-form-list info)))
	 (setf (initialize-info-cached-default-initargs-function info)
	       (initialize-instance-simple-function
		'default-initargs-function info
		class initargs-form-list))))
      ((valid-p ri-valid-p)
       (flet ((compute-valid-p (methods)
		(or (not (null allow-other-keys-arg))
		    (multiple-value-bind (legal allow-other-keys)
			(check-initargs-values class methods)
		      (or (not (null allow-other-keys))
			  (dolist (key keys t)
			    (unless (member key legal)
			      (return (cons :invalid key)))))))))
	 (let ((proto (class-prototype class)))
	   (setf (initialize-info-cached-valid-p info)
		 (compute-valid-p
		  (list (list* 'allocate-instance class nil)
			(list* 'initialize-instance proto nil)
			(list* 'shared-initialize proto t nil))))
	   (setf (initialize-info-cached-ri-valid-p info)
		 (compute-valid-p
		  (list (list* 'reinitialize-instance proto nil)
			(list* 'shared-initialize proto nil nil)))))))
      ((shared-initialize-t-function)
       (multiple-value-bind (initialize-form-list ignore)
	   (make-shared-initialize-form-list class keys t nil)
	 (declare (ignore ignore))
	 (setf (initialize-info-cached-shared-initialize-t-function info)
	       (initialize-instance-simple-function
		'shared-initialize-t-function info
		class initialize-form-list))))
      ((shared-initialize-nil-function)
       (multiple-value-bind (initialize-form-list ignore)
	   (make-shared-initialize-form-list class keys nil nil)
	 (declare (ignore ignore))
	 (setf (initialize-info-cached-shared-initialize-nil-function info)
	       (initialize-instance-simple-function
		'shared-initialize-nil-function info
		class initialize-form-list))))
      ((constants combined-initialize-function)
       (let ((initargs-form-list (initialize-info-initargs-form-list info))
	     (new-keys (initialize-info-new-keys info)))
	 (multiple-value-bind (initialize-form-list constants)
	     (make-shared-initialize-form-list class new-keys t t)
	   (setf (initialize-info-cached-constants info) constants)
	   (setf (initialize-info-cached-combined-initialize-function info)
		 (initialize-instance-simple-function
		  'combined-initialize-function info
		  class (append initargs-form-list initialize-form-list))))))
      ((make-instance-function-symbol)
       (setf (initialize-info-cached-make-instance-function-symbol info)
	     (make-instance-function-symbol key)))
      ((make-instance-function)
       (let* ((function (get-make-instance-function key))
	      (symbol (initialize-info-make-instance-function-symbol info)))
	 (setf (initialize-info-cached-make-instance-function info) function)
	 (when symbol (setf (gdefinition symbol)
			    (or function #'make-instance-1)))))))
  info)

(defun get-make-instance-function (key)
  (let* ((class (car key))
	 (keys (cadr key)))
    (unless (eq *boot-state* 'complete)
      (return-from get-make-instance-function nil))
    (when (symbolp class)
      (setq class (find-class class)))
    (when (classp class)
      (unless (class-finalized-p class) (finalize-inheritance class)))
    (let* ((initargs (mapcan #'(lambda (key) (list key nil)) keys))
	   (class-and-initargs (list* class initargs))
	   (make-instance (gdefinition 'make-instance))
	   (make-instance-methods
	    (compute-applicable-methods make-instance class-and-initargs))
	   (std-mi-meth (find-standard-ii-method make-instance-methods 'class))
	   (class+initargs (list class initargs))
	   (default-initargs (gdefinition 'default-initargs))
	   (default-initargs-methods
	       (compute-applicable-methods default-initargs class+initargs))
	   (proto (and (classp class) (class-prototype class)))
	   (initialize-instance-methods
	    (when proto
	      (compute-applicable-methods (gdefinition 'initialize-instance)
					  (list* proto initargs))))
	   (shared-initialize-methods
	    (when proto
	      (compute-applicable-methods (gdefinition 'shared-initialize)
					  (list* proto t initargs)))))
      (when (null make-instance-methods)
	(return-from get-make-instance-function
	  #'(lambda (class initargs)
	      (apply #'no-applicable-method make-instance class initargs))))
      (unless (and (null (cdr make-instance-methods))
		   (eq (car make-instance-methods) std-mi-meth)
		   (null (cdr default-initargs-methods))
		   (eq (car (method-specializers
			     (car default-initargs-methods)))
		       *the-class-slot-class*)
		   (flet ((check-meth (meth)
			    (let ((quals (method-qualifiers meth)))
			      (if (null quals)
				  (eq (car (method-specializers meth))
				      *the-class-slot-object*)
				  (and (null (cdr quals))
				       (or (eq (car quals) ':before)
					   (eq (car quals) ':after)))))))
		     (and (every #'check-meth initialize-instance-methods)
			  (every #'check-meth shared-initialize-methods))))
	(return-from get-make-instance-function nil))
      (get-make-instance-function-internal
       class key (default-initargs class initargs)
       initialize-instance-methods shared-initialize-methods))))

(defun get-make-instance-function-internal (class key initargs
						  initialize-instance-methods
						  shared-initialize-methods)
  (let* ((keys (cadr key))
	 (allow-other-keys-p (caddr key))
	 (allocate-instance-methods
	  (compute-applicable-methods (gdefinition 'allocate-instance)
				      (list* class initargs))))
    (unless allow-other-keys-p
      (unless (check-initargs-1
	       class initargs
	       (append allocate-instance-methods
		       initialize-instance-methods
		       shared-initialize-methods)
	       t nil)
	(return-from get-make-instance-function-internal nil)))
    (if (or (cdr allocate-instance-methods)
	    (some #'complicated-instance-creation-method
		  initialize-instance-methods)
	    (some #'complicated-instance-creation-method
		  shared-initialize-methods))
	(make-instance-function-complex
	 key class keys
	 initialize-instance-methods shared-initialize-methods)
	(make-instance-function-simple
	 key class keys
	 initialize-instance-methods shared-initialize-methods))))

(defun complicated-instance-creation-method (m)
  (let ((qual (method-qualifiers m)))
    (if qual
	(not (and (null (cdr qual)) (eq (car qual) ':after)))
	(let ((specl (car (method-specializers m))))
	  (or (not (classp specl))
	      (not (eq 'slot-object (class-name specl))))))))

(defun find-standard-ii-method (methods class-names)
  (dolist (m methods)
    (when (null (method-qualifiers m))
      (let ((specl (car (method-specializers m))))
	(when (and (classp specl)
		   (if (listp class-names)
		       (member (class-name specl) class-names)
		       (eq (class-name specl) class-names)))
	  (return m))))))

(defmacro call-initialize-function (initialize-function instance initargs)
  `(let ((.function. ,initialize-function))
     (if (and (consp .function.)
	      (eq (car .function.) 'call-initialize-instance-simple))
	 (initialize-instance-simple (cadr .function.) (caddr .function.)
				     ,instance ,initargs)
	 (funcall (the function .function.) ,instance ,initargs))))

(defun make-instance-function-simple (key class keys
					  initialize-instance-methods
					  shared-initialize-methods)
  (multiple-value-bind (initialize-function constants)
      (get-simple-initialization-function class keys (caddr key))
    (let* ((wrapper (class-wrapper class))
	   (lwrapper (list wrapper))
	   (allocate-function
	    (cond ((structure-class-p class)
		   #'allocate-structure-instance)
		  ((standard-class-p class)
		   #'allocate-standard-instance)
		  ((funcallable-standard-class-p class)
		   #'allocate-funcallable-instance)
		  (t
		   (error "error in make-instance-function-simple"))))
	   (std-si-meth (find-standard-ii-method shared-initialize-methods
						 'slot-object))
	   (shared-initfns
	    (nreverse (mapcar #'(lambda (method)
				  (make-effective-method-function
				   #'shared-initialize
				   `(call-method ,method nil)
				   nil lwrapper))
			      (remove std-si-meth shared-initialize-methods))))
	   (std-ii-meth (find-standard-ii-method initialize-instance-methods
						 'slot-object))
	   (initialize-initfns
	    (nreverse (mapcar #'(lambda (method)
				  (make-effective-method-function
				   #'initialize-instance
				   `(call-method ,method nil)
				   nil lwrapper))
			      (remove std-ii-meth
				      initialize-instance-methods)))))
      #'(lambda (class1 initargs)
	  (if (not (eq wrapper (class-wrapper class)))
	      (let* ((info (initialize-info class1 initargs))
		     (fn (initialize-info-make-instance-function info)))
		(declare (type function fn))
		(funcall fn class1 initargs))
	      (let* ((instance (funcall allocate-function wrapper constants))
		     (initargs (call-initialize-function initialize-function
							 instance initargs)))
		(dolist (fn shared-initfns)
		  (invoke-effective-method-function fn t instance t initargs))
		(dolist (fn initialize-initfns)
		  (invoke-effective-method-function fn t instance initargs))
		instance))))))

(defun make-instance-function-complex (key class keys
					   initialize-instance-methods
					   shared-initialize-methods)
  (multiple-value-bind (initargs-function initialize-function)
      (get-complex-initialization-functions class keys (caddr key))
    (let* ((wrapper (class-wrapper class))
	   (shared-initialize
	    (get-secondary-dispatch-function
	     #'shared-initialize shared-initialize-methods
	     `((class-eq ,class) t t)
	     `((,(find-standard-ii-method shared-initialize-methods
					  'slot-object)
		,#'(lambda (instance init-type &rest initargs)
		     (declare (ignore init-type))
		     (call-initialize-function initialize-function
					       instance initargs)
		     instance)))
	     (list wrapper *the-wrapper-of-t* *the-wrapper-of-t*)))
	   (initialize-instance
	    (get-secondary-dispatch-function
	     #'initialize-instance initialize-instance-methods
	     `((class-eq ,class) t)
	     `((,(find-standard-ii-method initialize-instance-methods
					  'slot-object)
		,#'(lambda (instance &rest initargs)
		     (invoke-effective-method-function
		      shared-initialize t instance t initargs))))
	     (list wrapper *the-wrapper-of-t*))))
      #'(lambda (class1 initargs)
	  (if (not (eq wrapper (class-wrapper class)))
	      (let* ((info (initialize-info class1 initargs))
		     (fn (initialize-info-make-instance-function info)))
		(declare (type function fn))
		(funcall fn class1 initargs))
	      (let* ((initargs (call-initialize-function initargs-function
							 nil initargs))
		     (instance (apply #'allocate-instance class initargs)))
		(invoke-effective-method-function
		 initialize-instance t instance initargs)
		instance))))))

(defun get-simple-initialization-function (class
					   keys
					   &optional allow-other-keys-arg)
  (let ((info (initialize-info class keys nil allow-other-keys-arg)))
    (values (initialize-info-combined-initialize-function info)
	    (initialize-info-constants info))))

(defun get-complex-initialization-functions (class
					     keys
					     &optional
					     allow-other-keys-arg
					     separate-p)
  (let* ((info (initialize-info class keys nil allow-other-keys-arg))
	 (default-initargs-function (initialize-info-default-initargs-function
				     info)))
    (if separate-p
	(values default-initargs-function
		(initialize-info-shared-initialize-t-function info))
	(values default-initargs-function
		(initialize-info-shared-initialize-t-function
		 (initialize-info class (initialize-info-new-keys info)
				  nil allow-other-keys-arg))))))

(defun add-forms (forms forms-list)
  (when forms
    (setq forms (copy-list forms))
    (if (null (car forms-list))
	(setf (car forms-list) forms)
	(setf (cddr forms-list) forms))
    (setf (cdr forms-list) (last forms)))
  (car forms-list))

(defun make-default-initargs-form-list (class keys &optional (separate-p t))
  (let ((initargs-form-list (cons nil nil))
	(default-initargs (class-default-initargs class))
	(nkeys keys)
	(slots-alist
	 (mapcan #'(lambda (slot)
		     (mapcar #'(lambda (arg)
				 (cons arg slot))
			     (slot-definition-initargs slot)))
		 (class-slots class)))
	(nslots nil))
    (dolist (key nkeys)
      (pushnew (cdr (assoc key slots-alist)) nslots))
    (dolist (default default-initargs)
      (let* ((key (car default))
	     (slot (cdr (assoc key slots-alist)))
	     (function (cadr default)))
	(unless (member slot nslots)
	  (add-forms `((funcall ,function) (push-initarg ,key))
		     initargs-form-list)
	  (push key nkeys)
	  (push slot nslots))))
    (when separate-p
      (add-forms `((update-initialize-info-cache
		    ,class ,(initialize-info class nkeys nil)))
		 initargs-form-list))
    (add-forms `((finish-pushing-initargs))
	       initargs-form-list)
    (values (car initargs-form-list) nkeys)))

(defun make-shared-initialize-form-list (class keys si-slot-names simple-p)
  (let* ((initialize-form-list (cons nil nil))
	 (type (cond ((structure-class-p class)
		      'structure)
		     ((standard-class-p class)
		      'standard)
		     ((funcallable-standard-class-p class)
		      'funcallable)
		     (t (error "error in make-shared-initialize-form-list"))))
	 (wrapper (class-wrapper class))
	 (constants (when simple-p
		      (make-list (wrapper-no-of-instance-slots wrapper)
				 ':initial-element +slot-unbound+)))
	 (slots (class-slots class))
	 (slot-names (mapcar #'slot-definition-name slots))
	 (slots-key (mapcar #'(lambda (slot)
				(let ((index most-positive-fixnum))
				  (dolist (key (slot-definition-initargs slot))
				    (let ((pos (position key keys)))
				      (when pos (setq index (min index pos)))))
				  (cons slot index)))
			    slots))
	 (slots (stable-sort slots-key #'< :key #'cdr)))
    (let ((n-popped 0))
      (dolist (slot+index slots)
	(let* ((slot (car slot+index))
	       (name (slot-definition-name slot))
	       (npop (1+ (- (cdr slot+index) n-popped))))
	  (unless (eql (cdr slot+index) most-positive-fixnum)
	    (let* ((pv-offset (1+ (position name slot-names))))
	      (add-forms `(,@(when (plusp npop)
			       `((pop-initargs ,(* 2 npop))))
			   (instance-set ,pv-offset ,slot))
			 initialize-form-list))
	    (incf n-popped npop)))))
    (dolist (slot+index slots)
      (let* ((slot (car slot+index))
	     (name (slot-definition-name slot)))
	(when (and (eql (cdr slot+index) most-positive-fixnum)
		   (or (eq si-slot-names t)
		       (member name si-slot-names)))
	  (let* ((initform (slot-definition-initform slot))
		 (initfunction (slot-definition-initfunction slot))
		 (location (unless (eq type 'structure)
			     (slot-definition-location slot)))
		 (pv-offset (1+ (position name slot-names)))
		 (forms (cond ((null initfunction)
			       nil)
			      ((constantp initform)
			       (let ((value (funcall initfunction)))
				 (if (and simple-p (integerp location))
				     (progn (setf (nth location constants)
						  value)
					    nil)
				     `((const ,value)
				       (instance-set ,pv-offset ,slot)))))
			      (t
			       `((funcall ,(slot-definition-initfunction slot))
				 (instance-set ,pv-offset ,slot))))))
	    (add-forms `(,@(unless (or simple-p (null forms))
			     `((skip-when-instance-boundp ,pv-offset ,slot
				,(length forms))))
			 ,@forms)
		       initialize-form-list)))))
    (values (car initialize-form-list) constants)))

(defvar *class-pv-table-table* (make-hash-table :test 'eq))

(defun get-pv-cell-for-class (class)
  (let* ((slot-names (mapcar #'slot-definition-name (class-slots class)))
	 (slot-name-lists (list (cons nil slot-names)))
	 (pv-table (gethash class *class-pv-table-table*)))
    (unless (and pv-table
		 (equal slot-name-lists (pv-table-slot-name-lists pv-table)))
      (setq pv-table (intern-pv-table :slot-name-lists slot-name-lists))
      (setf (gethash class *class-pv-table-table*) pv-table))
    (pv-table-lookup pv-table (class-wrapper class))))

(defvar *initialize-instance-simple-alist* nil)
(defvar *note-iis-entry-p* nil)

(defvar *compiled-initialize-instance-simple-functions*
  (make-hash-table :test 'equal))

(defun initialize-instance-simple-function (use info class form-list)
  (let* ((pv-cell (get-pv-cell-for-class class))
	 (key (initialize-info-key info))
	 (sf-key (list* use (class-name (car key)) (cdr key))))
    (if (or *compile-make-instance-functions-p*
	    (gethash sf-key *compiled-initialize-instance-simple-functions*))
	(multiple-value-bind (form args)
	    (form-list-to-lisp pv-cell form-list)
	  (let ((entry (assoc form *initialize-instance-simple-alist*
			      :test #'equal)))
	    (setf (gethash sf-key
			   *compiled-initialize-instance-simple-functions*)
		  t)
	    (if entry
		(setf (cdddr entry) (union (list sf-key) (cdddr entry)
					   :test #'equal))
		(progn
		  (setq entry (list* form nil nil (list sf-key)))
		  (setq *initialize-instance-simple-alist*
			(nconc *initialize-instance-simple-alist*
			       (list entry)))))
	    (unless (or *note-iis-entry-p* (cadr entry))
	      (setf (cadr entry) (compile nil (car entry))))
	    (if (cadr entry)
		(apply (the function (cadr entry)) args)
		`(call-initialize-instance-simple ,pv-cell ,form-list))))
	#||
	#'(lambda (instance initargs)
	    (initialize-instance-simple pv-cell form-list instance initargs))
	||#
	`(call-initialize-instance-simple ,pv-cell ,form-list))))

(defun load-precompiled-iis-entry (form function system uses)
  (let ((entry (assoc form *initialize-instance-simple-alist*
		      :test #'equal)))
    (unless entry
      (setq entry (list* form nil nil nil))
      (setq *initialize-instance-simple-alist*
	    (nconc *initialize-instance-simple-alist*
		   (list entry))))
    (setf (cadr entry) function)
    (setf (caddr entry) system)
    (dolist (use uses)
      (setf (gethash use *compiled-initialize-instance-simple-functions*) t))
    (setf (cdddr entry) (union uses (cdddr entry)
			       :test #'equal))))

(defmacro precompile-iis-functions (&optional system)
  `(progn
    ,@(gathering1 (collecting)
                  (dolist (iis-entry *initialize-instance-simple-alist*)
                    (when (or (null (caddr iis-entry))
                              (eq (caddr iis-entry) system))
                      (when system (setf (caddr iis-entry) system))
                      (gather1
                       `(load-precompiled-iis-entry
                         ',(car iis-entry)
                         #',(car iis-entry)
                         ',system
                         ',(cdddr iis-entry))))))))

(defun compile-iis-functions (after-p)
  (let ((*compile-make-instance-functions-p* t)
	(*revert-initialize-info-p* t)
	(*note-iis-entry-p* (not after-p)))
    (declare (special *compile-make-instance-functions-p*))
    (when (eq *boot-state* 'complete)
      (update-make-instance-function-table))))

;(const const)
;(funcall function)
;(push-initarg const)
;(pop-supplied count) ; a positive odd number
;(instance-set pv-offset slotd)
;(skip-when-instance-boundp pv-offset slotd n)

(defun initialize-instance-simple (pv-cell form-list instance initargs)
  (let ((pv (car pv-cell))
	(initargs-tail initargs)
	(slots (get-slots-or-nil instance))
	(class (class-of instance))
	value)
    (loop (when (null form-list) (return nil))
	  (let ((form (pop form-list)))
	    (ecase (car form)
	      (push-initarg
	       (push value initargs)
	       (push (cadr form) initargs))
	      (const
	       (setq value (cadr form)))
	      (funcall
	       (setq value (funcall (the function (cadr form)))))
	      (pop-initargs
	       (setq initargs-tail (nthcdr (1- (cadr form)) initargs-tail))
	       (setq value (pop initargs-tail)))
	      (instance-set
	       (instance-write-internal
		pv slots (cadr form) value
		(setf (slot-value-using-class class instance (caddr form))
		      value)))
	      (skip-when-instance-boundp
	       (when (instance-boundp-internal
		      pv slots (cadr form)
		      (slot-boundp-using-class class instance (caddr form)))
		 (dotimes-fixnum (i (cadddr form))
		   (pop form-list))))
	      (update-initialize-info-cache
	       (when (consp initargs)
		 (setq initargs (cons (car initargs) (cdr initargs))))
	       (setq *initialize-info-cache-class* (cadr form))
	       (setq *initialize-info-cache-initargs* initargs)
	       (setq *initialize-info-cache-info* (caddr form)))
	      (finish-pushing-initargs
	       (setq initargs-tail initargs)))))
    initargs))

(defun add-to-cvector (cvector constant)
  (or (position constant cvector)
      (prog1 (fill-pointer cvector)
	(vector-push-extend constant cvector))))

(defvar *inline-iis-instance-locations-p* t)

(defun first-form-to-lisp (forms cvector pv)
  (flet ((const (constant)
	   (cond ((or (numberp constant) (characterp constant))
		  constant)
		 ((and (symbolp constant) (symbol-package constant))
		  `',constant)
		 (t
		  `(svref cvector ,(add-to-cvector cvector constant))))))
    (let ((form (pop (car forms))))
      (ecase (car form)
	(push-initarg
	 `((push value initargs)
	   (push ,(const (cadr form)) initargs)))
	(const
	 `((setq value ,(const (cadr form)))))
	(funcall
	 `((setq value (funcall (the function ,(const (cadr form)))))))
	(pop-initargs
	 `((setq initargs-tail (,@(let ((pop (1- (cadr form))))
				    (case pop
				      (1 `(cdr))
				      (3 `(cdddr))
				      (t `(nthcdr ,pop))))
				initargs-tail))
	   (setq value (pop initargs-tail))))
	(instance-set
	 (let* ((pv-offset (cadr form))
		(location (pvref pv pv-offset))
		(default `(setf (slot-value-using-class class instance
							,(const (caddr form)))
				value)))
	   (if *inline-iis-instance-locations-p*
	       (typecase location
		 (fixnum `((and slots
                                (setf (clos-slots-ref slots ,(const location))
				      value))))
		 (cons `((setf (cdr ,(const location)) value)))
		 (t `(,default)))
	       `((instance-write-internal pv slots ,(const pv-offset) value
		  ,default
		  ,(typecase location
		     (fixnum ':instance)
		     (cons ':class)
		     (t ':default)))))))
	(skip-when-instance-boundp
	 (let* ((pv-offset (cadr form))
		(location (pvref pv pv-offset))
		(default `(slot-boundp-using-class class instance
			   ,(const (caddr form)))))
	   `((unless ,(if *inline-iis-instance-locations-p*
			  (typecase location
			    (fixnum `(not (and slots
                                               (eq (clos-slots-ref
						    slots
						    ,(const location))
                                                   +slot-unbound+))))
			    (cons `(not (eq (cdr ,(const location))
					    +slot-unbound+)))
			    (t default))
			  `(instance-boundp-internal
			    pv slots ,(const pv-offset)
			    ,default
			    ,(typecase (pvref pv pv-offset)
			       (fixnum ':instance)
			       (cons ':class)
			       (t ':default))))
	       ,@(let ((sforms (cons nil nil)))
		   (dotimes-fixnum (i (cadddr form) (car sforms))
		     (add-forms (first-form-to-lisp forms cvector pv)
				sforms)))))))
	(update-initialize-info-cache
	 `((when (consp initargs)
	     (setq initargs (cons (car initargs) (cdr initargs))))
	   (setq *initialize-info-cache-class* ,(const (cadr form)))
	   (setq *initialize-info-cache-initargs* initargs)
	   (setq *initialize-info-cache-info* ,(const (caddr form)))))
	(finish-pushing-initargs
	 `((setq initargs-tail initargs)))))))

(defmacro iis-body (&body forms)
  `(let ((initargs-tail initargs)
	 (slots (get-slots-or-nil instance))
	 (class (class-of instance))
	 (pv (car pv-cell))
	 value)
     initargs instance initargs-tail pv cvector slots class value
     ,@forms))

(defun form-list-to-lisp (pv-cell form-list)
  (let* ((forms (list form-list))
	 (cvector (make-array (floor (length form-list) 2)
			      :fill-pointer 0 :adjustable t))
	 (pv (car pv-cell))
	 (body (let ((rforms (cons nil nil)))
		 (loop (when (null (car forms)) (return (car rforms)))
		       (add-forms (first-form-to-lisp forms cvector pv)
				  rforms))))
	 (cvector-type `(simple-vector ,(length cvector))))
    (values
     `(lambda (pv-cell cvector)
	(declare (type ,cvector-type cvector))
	#'(lambda (instance initargs)
	    (declare #.*optimize-speed*)
	    (iis-body ,@body)
	    initargs))
     (list pv-cell (coerce cvector cvector-type)))))

;;; The effect of this is to cause almost all of the overhead of
;;; MAKE-INSTANCE to happen at load time (or maybe at precompile time,
;;; as explained in a previous message) rather than the first time
;;; that MAKE-INSTANCE is called with a given class-name and sequence
;;; of keywords.

;;; This optimization applies only when the first argument and all the
;;; even numbered arguments are constants evaluating to interned
;;; symbols.

(declaim (ftype (function (t) symbol) get-make-instance-function-symbol))

(define-compiler-macro make-instance (&whole form &rest args)
  (declare (ignore args))
  (let* ((*make-instance-function-keys* nil)
	 (expanded-form (expand-make-instance-form form)))
    (if expanded-form
	`(funcall (fdefinition
		   ;; The name is guaranteed to be fbound.
		   ;; Is there a way to declare this?
		   (load-time-value
		    (get-make-instance-function-symbol
		     ',(first *make-instance-function-keys*))))
		  ,@(cdr expanded-form))
	form)))

(defun get-make-instance-function-symbol (key)
  (get-make-instance-functions (list key))
  (make-instance-function-symbol key))

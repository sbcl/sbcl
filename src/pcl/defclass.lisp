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

(sb-int:file-comment
  "$Header$")

;;; MAKE-TOP-LEVEL-FORM is used by all PCL macros that appear `at top-level'.
;;;
;;; The original motiviation for this function was to deal with the bug in
;;; the Genera compiler that prevents lambda expressions in top-level forms
;;; other than DEFUN from being compiled.
;;;
;;; Now this function is used to grab other functionality as well. This
;;; includes:
;;;   - Preventing the grouping of top-level forms. For example, a
;;;     DEFCLASS followed by a DEFMETHOD may not want to be grouped
;;;     into the same top-level form.
;;;   - Telling the programming environment what the pretty version
;;;     of the name of this form is. This is used by WARN.
;;;
;;; FIXME: It's not clear that this adds value any more. Couldn't
;;; we just use EVAL-WHEN?
(defun make-top-level-form (name times form)
  (flet ((definition-name ()
	   (if (and (listp name)
		    (memq (car name)
			  '(defmethod defclass class
			    method method-combination)))
	       (format nil "~A~{ ~S~}"
		       (capitalize-words (car name) ()) (cdr name))
	       (format nil "~S" name))))
    ;; FIXME: It appears that we're just consing up a string and then
    ;; throwing it away?!
    (definition-name)
    (if (or (member 'compile times)
	    (member ':compile-toplevel times))
	`(eval-when ,times ,form)
	form)))

(defun make-progn (&rest forms)
  (let ((progn-form nil))
    (labels ((collect-forms (forms)
	       (unless (null forms)
		 (collect-forms (cdr forms))
		 (if (and (listp (car forms))
			  (eq (caar forms) 'progn))
		     (collect-forms (cdar forms))
		     (push (car forms) progn-form)))))
      (collect-forms forms)
      (cons 'progn progn-form))))

;;; Like the DEFMETHOD macro, the expansion of the DEFCLASS macro is fixed.
;;; DEFCLASS always expands into a call to LOAD-DEFCLASS. Until the meta-
;;; braid is set up, LOAD-DEFCLASS has a special definition which simply
;;; collects all class definitions up, when the metabraid is initialized it
;;; is done from those class definitions.
;;;
;;; After the metabraid has been setup, and the protocol for defining classes
;;; has been defined, the real definition of LOAD-DEFCLASS is installed by the
;;; file defclass.lisp
(defmacro defclass (name direct-superclasses direct-slots &rest options)
  (declare (indentation 2 4 3 1))
  (expand-defclass name direct-superclasses direct-slots options))

(defun expand-defclass (name supers slots options)
  (declare (special *defclass-times* *boot-state* *the-class-structure-class*))
  (setq supers  (copy-tree supers)
	slots   (copy-tree slots)
	options (copy-tree options))
  (let ((metaclass 'standard-class))
    (dolist (option options)
      (if (not (listp option))
	  (error "~S is not a legal defclass option." option)
	  (when (eq (car option) ':metaclass)
	    (unless (legal-class-name-p (cadr option))
	      (error "The value of the :metaclass option (~S) is not a~%~
		      legal class name."
		     (cadr option)))
	    (setq metaclass
		  (case (cadr option)
		    (cl:standard-class 'standard-class)
		    (cl:structure-class 'structure-class)
		    (t (cadr option))))
	    (setf options (remove option options))
	    (return t))))

    (let ((*initfunctions* ())
	  (*accessors* ())		;Truly a crock, but we got
	  (*readers* ())		;to have it to live nicely.
	  (*writers* ()))
      (declare (special *initfunctions* *accessors* *readers* *writers*))
      (let ((canonical-slots
	      (mapcar #'(lambda (spec)
			  (canonicalize-slot-specification name spec))
		      slots))
	    (other-initargs
	      (mapcar #'(lambda (option)
			  (canonicalize-defclass-option name option))
		      options))
	    (defstruct-p (and (eq *boot-state* 'complete)
			      (let ((mclass (find-class metaclass nil)))
				(and mclass
				     (*subtypep mclass
						*the-class-structure-class*))))))
	(do-standard-defsetfs-for-defclass *accessors*)
	(let ((defclass-form
		 (make-top-level-form `(defclass ,name)
		   (if defstruct-p '(:load-toplevel :execute) *defclass-times*)
		   `(progn
		      ,@(mapcar #'(lambda (x)
				    `(declaim (ftype (function (t) t) ,x)))
				*readers*)
		      ,@(mapcar #'(lambda (x)
				    #-setf (when (consp x)
					     (setq x (get-setf-function-name (cadr x))))
				    `(declaim (ftype (function (t t) t) ,x)))
				*writers*)
		      (let ,(mapcar #'cdr *initfunctions*)
			(load-defclass ',name
				       ',metaclass
				       ',supers
				       (list ,@canonical-slots)
				       (list ,@(apply #'append
						      (when defstruct-p
							'(:from-defclass-p t))
						      other-initargs))
				       ',*accessors*))))))
	  (if defstruct-p
	      (progn
		(eval defclass-form) ; define the class now, so that
		`(progn	      ; the defstruct can be compiled.
		   ,(class-defstruct-form (find-class name))
		   ,defclass-form))
	      (progn
		(when (and (eq *boot-state* 'complete)
			   (not (member 'compile *defclass-times*)))
		  (inform-type-system-about-std-class name))
		defclass-form)))))))

(defun make-initfunction (initform)
  (declare (special *initfunctions*))
  (cond ((or (eq initform 't)
	     (equal initform ''t))
	 '(function true))
	((or (eq initform 'nil)
	     (equal initform ''nil))
	 '(function false))
	((or (eql initform '0)
	     (equal initform ''0))
	 '(function zero))
	(t
	 (let ((entry (assoc initform *initfunctions* :test #'equal)))
	   (unless entry
	     (setq entry (list initform
			       (gensym)
			       `(function (lambda () ,initform))))
	     (push entry *initfunctions*))
	   (cadr entry)))))

(defun canonicalize-slot-specification (class-name spec)
  (declare (special *accessors* *readers* *writers*))
  (cond ((and (symbolp spec)
	      (not (keywordp spec))
	      (not (memq spec '(t nil))))
	 `'(:name ,spec))
	((not (consp spec))
	 (error "~S is not a legal slot specification." spec))
	((null (cdr spec))
	 `'(:name ,(car spec)))
	((null (cddr spec))
	 (error "In DEFCLASS ~S, the slot specification ~S is obsolete.~%~
		 Convert it to ~S"
		class-name spec (list (car spec) :initform (cadr spec))))
	(t
	 (let* ((name (pop spec))
		(readers ())
		(writers ())
		(initargs ())
		(unsupplied (list nil))
		(initform (getf spec :initform unsupplied)))
	   (doplist (key val) spec
	     (case key
	       (:accessor (push val *accessors*)
			  (push val readers)
			  (push `(setf ,val) writers))
	       (:reader   (push val readers))
	       (:writer   (push val writers))
	       (:initarg  (push val initargs))))
	   (loop (unless (remf spec :accessor) (return)))
	   (loop (unless (remf spec :reader)   (return)))
	   (loop (unless (remf spec :writer)   (return)))
	   (loop (unless (remf spec :initarg)  (return)))
	   (setq *writers* (append writers *writers*))
	   (setq *readers* (append readers *readers*))
	   (setq spec `(:name     ',name
			:readers  ',readers
			:writers  ',writers
			:initargs ',initargs
			',spec))
	   (if (eq initform unsupplied)
	       `(list* ,@spec)
	       `(list* :initfunction ,(make-initfunction initform) ,@spec))))))
						
(defun canonicalize-defclass-option (class-name option)
  (declare (ignore class-name))
  (case (car option)
    (:default-initargs
      (let ((canonical ()))
	(let (key val (tail (cdr option)))
	  (loop (when (null tail) (return nil))
		(setq key (pop tail)
		      val (pop tail))
		(push ``(,',key ,,(make-initfunction val) ,',val) canonical))
	  `(':direct-default-initargs (list ,@(nreverse canonical))))))
    (:documentation
      `(',(car option) ',(cadr option)))
    (otherwise
     `(',(car option) ',(cdr option)))))

;;; This is the early definition of load-defclass. It just collects up
;;; all the class definitions in a list. Later, in the file
;;; braid1.lisp, these are actually defined.

;;; Each entry in *EARLY-CLASS-DEFINITIONS* is an EARLY-CLASS-DEFINITION.
(defparameter *early-class-definitions* ())

(defun early-class-definition (class-name)
  (or (find class-name *early-class-definitions* :key #'ecd-class-name)
      (error "~S is not a class in *early-class-definitions*." class-name)))

(defun make-early-class-definition
       (name source metaclass
	superclass-names canonical-slots other-initargs)
  (list 'early-class-definition
	name source metaclass
	superclass-names canonical-slots other-initargs))

(defun ecd-class-name        (ecd) (nth 1 ecd))
(defun ecd-source            (ecd) (nth 2 ecd))
(defun ecd-metaclass         (ecd) (nth 3 ecd))
(defun ecd-superclass-names  (ecd) (nth 4 ecd))
(defun ecd-canonical-slots   (ecd) (nth 5 ecd))
(defun ecd-other-initargs    (ecd) (nth 6 ecd))

(defvar *early-class-slots* nil)

(defun canonical-slot-name (canonical-slot)
  (getf canonical-slot :name))

(defun early-class-slots (class-name)
  (cdr (or (assoc class-name *early-class-slots*)
	   (let ((a (cons class-name
			  (mapcar #'canonical-slot-name
				  (early-collect-inheritance class-name)))))
	     (push a *early-class-slots*)
	     a))))

(defun early-class-size (class-name)
  (length (early-class-slots class-name)))

(defun early-collect-inheritance (class-name)
  ;;(declare (values slots cpl default-initargs direct-subclasses))
  (let ((cpl (early-collect-cpl class-name)))
    (values (early-collect-slots cpl)
	    cpl
	    (early-collect-default-initargs cpl)
	    (gathering1 (collecting)
	      (dolist (definition *early-class-definitions*)
		(when (memq class-name (ecd-superclass-names definition))
		  (gather1 (ecd-class-name definition))))))))

(defun early-collect-slots (cpl)
  (let* ((definitions (mapcar #'early-class-definition cpl))
	 (super-slots (mapcar #'ecd-canonical-slots definitions))
	 (slots (apply #'append (reverse super-slots))))
    (dolist (s1 slots)
      (let ((name1 (canonical-slot-name s1)))
	(dolist (s2 (cdr (memq s1 slots)))
	  (when (eq name1 (canonical-slot-name s2))
	    (error "More than one early class defines a slot with the~%~
		    name ~S. This can't work because the bootstrap~%~
		    object system doesn't know how to compute effective~%~
		    slots."
		   name1)))))
    slots))

(defun early-collect-cpl (class-name)
  (labels ((walk (c)
	     (let* ((definition (early-class-definition c))
		    (supers (ecd-superclass-names definition)))
	       (cons c
		     (apply #'append (mapcar #'early-collect-cpl supers))))))
    (remove-duplicates (walk class-name) :from-end nil :test #'eq)))

(defun early-collect-default-initargs (cpl)
  (let ((default-initargs ()))
    (dolist (class-name cpl)
      (let* ((definition (early-class-definition class-name))
	     (others (ecd-other-initargs definition)))
	(loop (when (null others) (return nil))
	      (let ((initarg (pop others)))
		(unless (eq initarg :direct-default-initargs)
		 (error "The defclass option ~S is not supported by the bootstrap~%~
			object system."
			initarg)))
	      (setq default-initargs
		    (nconc default-initargs (reverse (pop others)))))))
    (reverse default-initargs)))

(defun bootstrap-slot-index (class-name slot-name)
  (or (position slot-name (early-class-slots class-name))
      (error "~S not found" slot-name)))

;;; bootstrap-get-slot and bootstrap-set-slot are used to access and change the
;;; values of slots during bootstrapping. During bootstrapping, there are only
;;; two kinds of objects whose slots we need to access, CLASSes and
;;; SLOT-DEFINITIONs. The first argument to these functions tells whether the
;;; object is a CLASS or a SLOT-DEFINITION.
;;;
;;; Note that the way this works it stores the slot in the same place in
;;; memory that the full object system will expect to find it later. This
;;; is critical to the bootstrapping process, the whole changeover to the
;;; full object system is predicated on this.
;;;
;;; One important point is that the layout of standard classes and standard
;;; slots must be computed the same way in this file as it is by the full
;;; object system later.
(defmacro bootstrap-get-slot (type object slot-name)
  `(instance-ref (get-slots ,object) (bootstrap-slot-index ,type ,slot-name)))
(defun bootstrap-set-slot (type object slot-name new-value)
  (setf (bootstrap-get-slot type object slot-name) new-value))

(defun early-class-name (class)
  (bootstrap-get-slot 'class class 'name))

(defun early-class-precedence-list (class)
  (bootstrap-get-slot 'pcl-class class 'class-precedence-list))

(defun early-class-name-of (instance)
  (early-class-name (class-of instance)))

(defun early-class-slotds (class)
  (bootstrap-get-slot 'slot-class class 'slots))

(defun early-slot-definition-name (slotd)
  (bootstrap-get-slot 'standard-effective-slot-definition slotd 'name))

(defun early-slot-definition-location (slotd)
  (bootstrap-get-slot 'standard-effective-slot-definition slotd 'location))

(defun early-accessor-method-slot-name (method)
  (bootstrap-get-slot 'standard-accessor-method method 'slot-name))

(unless (fboundp 'class-name-of)
  (setf (symbol-function 'class-name-of)
	(symbol-function 'early-class-name-of)))
;;; FIXME: Can we then delete EARLY-CLASS-NAME-OF?

(defun early-class-direct-subclasses (class)
  (bootstrap-get-slot 'class class 'direct-subclasses))

(declaim (notinline load-defclass))
(defun load-defclass
       (name metaclass supers canonical-slots canonical-options accessor-names)
  (setq supers  (copy-tree supers)
	canonical-slots   (copy-tree canonical-slots)
	canonical-options (copy-tree canonical-options))
  (do-standard-defsetfs-for-defclass accessor-names)
  (when (eq metaclass 'standard-class)
    (inform-type-system-about-std-class name))
  (let ((ecd
	  (make-early-class-definition name
				       *load-truename*
				       metaclass
				       supers
				       canonical-slots
				       canonical-options))
	(existing
	  (find name *early-class-definitions* :key #'ecd-class-name)))
    (setq *early-class-definitions*
	  (cons ecd (remove existing *early-class-definitions*)))
    ecd))


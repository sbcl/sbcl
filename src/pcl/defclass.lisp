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

;;;; DEFCLASS macro and close personal friends

;;; ANSI says (Macro DEFCLASS, section 7.7) that DEFCLASS, if it
;;; "appears as a top level form, the compiler must make the class
;;; name be recognized as a valid type name in subsequent declarations
;;; (as for deftype) and be recognized as a valid class name for
;;; defmethod parameter specializers and for use as the :metaclass
;;; option of a subsequent defclass."
(defun preinform-compiler-about-class-type (name)
  ;; Unless the type system already has an actual type attached to
  ;; NAME (in which case (1) writing a placeholder value over that
  ;; actual type as a compile-time side-effect would probably be a bad
  ;; idea and (2) anyway we don't need to modify it in order to make
  ;; NAME be recognized as a valid type name)
  (unless (info :type :kind name)
    ;; Tell the compiler to expect a class with the given NAME, by
    ;; writing a kind of minimal placeholder type information. This
    ;; placeholder will be overwritten later when the class is defined.
    (setf (info :type :kind name) :forthcoming-defclass-type))
  (values))

;;; state for the current DEFCLASS expansion
(defvar *initfunctions-for-this-defclass*)
(defvar *readers-for-this-defclass*)
(defvar *writers-for-this-defclass*)
(defvar *slot-names-for-this-defclass*)

;;; Like the DEFMETHOD macro, the expansion of the DEFCLASS macro is
;;; fixed. DEFCLASS always expands into a call to LOAD-DEFCLASS. Until
;;; the meta-braid is set up, LOAD-DEFCLASS has a special definition
;;; which simply collects all class definitions up, when the metabraid
;;; is initialized it is done from those class definitions.
;;;
;;; After the metabraid has been setup, and the protocol for defining
;;; classes has been defined, the real definition of LOAD-DEFCLASS is
;;; installed by the file std-class.lisp
(defmacro defclass (&environment env name %direct-superclasses %direct-slots &rest %options)
  (let ((supers  (copy-tree %direct-superclasses))
	(slots   (copy-tree %direct-slots))
	(options (copy-tree %options)))
    (let ((metaclass 'standard-class))
      (dolist (option options)
        (if (not (listp option))
	  (error "~S is not a legal defclass option." option)
	  (when (eq (car option) :metaclass)
	    (unless (legal-class-name-p (cadr option))
	      (error "The value of the :metaclass option (~S) is not a~%~
		      legal class name."
		     (cadr option)))
	    (setq metaclass (cadr option))
	    (setf options (remove option options))
	    (return t))))

      (let ((*initfunctions-for-this-defclass* ())
            (*readers-for-this-defclass* ()) ;Truly a crock, but we got
            (*writers-for-this-defclass* ()) ;to have it to live nicely.
            (*slot-names-for-this-defclass* ()))
        (let ((canonical-slots
                (mapcar (lambda (spec)
			  (canonicalize-slot-specification name spec env))
                        slots))
              (other-initargs
                (mapcar (lambda (option)
			  (canonicalize-defclass-option name option))
                        options))
              ;; DEFSTRUCT-P should be true if the class is defined
              ;; with a metaclass STRUCTURE-CLASS, so that a DEFSTRUCT
              ;; is compiled for the class.
              (defstruct-p (and (eq *boot-state* 'complete)
                                (let ((mclass (find-class metaclass nil)))
                                  (and mclass
                                       (*subtypep
                                        mclass
                                        *the-class-structure-class*))))))
          (let ((defclass-form
		  `(progn
		     ,@(mapcar (lambda (x)
				 `(declaim (ftype (function (t) t) ,x)))
			       *readers-for-this-defclass*)
		     ,@(mapcar (lambda (x)
				 `(declaim (ftype (function (t t) t) ,x)))
			       *writers-for-this-defclass*)
                     ,@(mapcar (lambda (x)
                                 `(declaim (ftype (function (t) t)
                                                  ,(slot-reader-name x)
                                                  ,(slot-boundp-name x))
                                           (ftype (function (t t) t)
                                                  ,(slot-writer-name x))))
                               *slot-names-for-this-defclass*)
		     (let ,(mapcar #'cdr *initfunctions-for-this-defclass*)
		       (load-defclass ',name
				      ',metaclass
				      ',supers
				      (list ,@canonical-slots)
				      (list ,@(apply #'append
						     (when defstruct-p
						       '(:from-defclass-p t))
						     other-initargs)))))))
            (if defstruct-p
		(progn
		  ;; FIXME: (YUK!) Why do we do this? Because in order
		  ;; to make the defstruct form, we need to know what
		  ;; the accessors for the slots are, so we need
		  ;; already to have hooked into the CLOS machinery.
		  ;;
		  ;; There may be a better way to do this: it would
		  ;; involve knowing enough about PCL to ask "what
		  ;; will my slot names and accessors be"; failing
		  ;; this, we currently just evaluate the whole
		  ;; kaboodle, and then use CLASS-DIRECT-SLOTS. --
		  ;; CSR, 2002-06-07
		  (eval defclass-form)
		  (let* ((include (or (and supers
					   (fix-super (car supers)))
				      (and (not (eq name 'structure-object))
					   *the-class-structure-object*)))
			 (defstruct-form (make-structure-class-defstruct-form
					  name (class-direct-slots (find-class name)) include)))
		    `(progn
		      (eval-when (:compile-toplevel :load-toplevel :execute)
			,defstruct-form) ; really compile the defstruct-form
		      (eval-when (:compile-toplevel :load-toplevel :execute)
			,defclass-form))))
		`(progn
		   ;; By telling the type system at compile time about
		   ;; the existence of a class named NAME, we can avoid
		   ;; various bogus warnings about "type isn't defined yet"
		   ;; for code elsewhere in the same file which uses
		   ;; the name of the type.
		   ;;
		   ;; We only need to do this at compile time, because
		   ;; at load and execute time we write the actual
		   ;; full-blown class, so the "a class of this name is
		   ;; coming" note we write here would be irrelevant.
		   (eval-when (:compile-toplevel)
		     (preinform-compiler-about-class-type ',name))
		   ,defclass-form))))))))

(defun make-initfunction (initform)
  (cond ((or (eq initform t)
	     (equal initform ''t))
	 '(function constantly-t))
	((or (eq initform nil)
	     (equal initform ''nil))
	 '(function constantly-nil))
	((or (eql initform 0)
	     (equal initform ''0))
	 '(function constantly-0))
	(t
	 (let ((entry (assoc initform *initfunctions-for-this-defclass*
			     :test #'equal)))
	   (unless entry
	     (setq entry (list initform
			       (gensym)
			       `(function (lambda () ,initform))))
	     (push entry *initfunctions-for-this-defclass*))
	   (cadr entry)))))

(defun canonicalize-slot-specification (class-name spec env)
  (labels ((slot-name-illegal (reason)
	     (error 'simple-program-error
		    :format-control
		    (format nil "~~@<in DEFCLASS ~~S, the slot name in the ~
                                 specification ~~S is ~A.~~@:>" reason)
		    :format-arguments (list class-name spec)))
	   (check-slot-name-legality (name)
	     (cond
	       ((not (symbolp name))
		(slot-name-illegal "not a symbol"))
	       ((keywordp name)
		(slot-name-illegal "a keyword"))
	       ((constantp name env)
		(slot-name-illegal "a constant")))))
    (cond ((atom spec)
	   (check-slot-name-legality spec)
	   (push spec *slot-names-for-this-defclass*)
	   `'(:name ,spec))
	  ((null (cdr spec))
	   (check-slot-name-legality (car spec))
	   (push (car spec) *slot-names-for-this-defclass*)
	 `'(:name ,(car spec)))
	  ((null (cddr spec))
	   (error 'simple-program-error
		  :format-control
		  "~@<in DEFCLASS ~S, the slot specification ~S is invalid; ~
                   the probable intended meaning may be achieved by ~
                   specifiying ~S instead."
		  :format-arguments
		  (list class-name spec
			`(,(car spec) :initform ,(cadr spec)))))
	  (t
	   (let* ((name (car spec))
		  (spec (cdr spec))
		  (readers ())
		  (writers ())
		  (initargs ())
		  (unsupplied (list nil))
		  (initform (getf spec :initform unsupplied)))
	     (check-slot-name-legality name)
	     (push name *slot-names-for-this-defclass*)
	     (doplist (key val) spec
	       (case key
		 (:accessor (push val readers)
			    (push `(setf ,val) writers))
		 (:reader   (push val readers))
		 (:writer   (push val writers))
		 (:initarg  (push val initargs))))
	     (loop (unless (remf spec :accessor) (return)))
	     (loop (unless (remf spec :reader)   (return)))
	     (loop (unless (remf spec :writer)   (return)))
	     (loop (unless (remf spec :initarg)  (return)))
	     (setq *writers-for-this-defclass*
		   (append writers *writers-for-this-defclass*))
	     (setq *readers-for-this-defclass*
		   (append readers *readers-for-this-defclass*))
	     (setq spec `(:name     ',name
			  :readers  ',readers
			  :writers  ',writers
			  :initargs ',initargs
			  ',spec))
	     (if (eq initform unsupplied)
		 `(list* ,@spec)
		 `(list* :initfunction ,(make-initfunction initform)
		         ,@spec)))))))

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
	  `(:direct-default-initargs (list ,@(nreverse canonical))))))
    (:documentation
      `(',(car option) ',(cadr option)))
    (otherwise
     `(',(car option) ',(cdr option)))))

;;; This is the early definition of LOAD-DEFCLASS. It just collects up
;;; all the class definitions in a list. Later, in braid1.lisp, these
;;; are actually defined.

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
	    (let (collect)
	      (dolist (definition *early-class-definitions*)
		(when (memq class-name (ecd-superclass-names definition))
		  (push (ecd-class-name definition) collect)))
              (nreverse collect)))))

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
		 (error "~@<The defclass option ~S is not supported by ~
			the bootstrap object system.~:@>"
			initarg)))
	      (setq default-initargs
		    (nconc default-initargs (reverse (pop others)))))))
    (reverse default-initargs)))

(defun !bootstrap-slot-index (class-name slot-name)
  (or (position slot-name (early-class-slots class-name))
      (error "~S not found" slot-name)))

;;; !BOOTSTRAP-GET-SLOT and !BOOTSTRAP-SET-SLOT are used to access and
;;; change the values of slots during bootstrapping. During
;;; bootstrapping, there are only two kinds of objects whose slots we
;;; need to access, CLASSes and SLOT-DEFINITIONs. The first argument
;;; to these functions tells whether the object is a CLASS or a
;;; SLOT-DEFINITION.
;;;
;;; Note that the way this works it stores the slot in the same place
;;; in memory that the full object system will expect to find it
;;; later. This is critical to the bootstrapping process, the whole
;;; changeover to the full object system is predicated on this.
;;;
;;; One important point is that the layout of standard classes and
;;; standard slots must be computed the same way in this file as it is
;;; by the full object system later.
(defmacro !bootstrap-get-slot (type object slot-name)
  `(clos-slots-ref (get-slots ,object)
		   (!bootstrap-slot-index ,type ,slot-name)))
(defun !bootstrap-set-slot (type object slot-name new-value)
  (setf (!bootstrap-get-slot type object slot-name) new-value))

(defun early-class-name (class)
  (!bootstrap-get-slot 'class class 'name))

(defun early-class-precedence-list (class)
  (!bootstrap-get-slot 'pcl-class class 'class-precedence-list))

(defun early-class-name-of (instance)
  (early-class-name (class-of instance)))

(defun early-class-slotds (class)
  (!bootstrap-get-slot 'slot-class class 'slots))

(defun early-slot-definition-name (slotd)
  (!bootstrap-get-slot 'standard-effective-slot-definition slotd 'name))

(defun early-slot-definition-location (slotd)
  (!bootstrap-get-slot 'standard-effective-slot-definition slotd 'location))

(defun early-accessor-method-slot-name (method)
  (!bootstrap-get-slot 'standard-accessor-method method 'slot-name))

(unless (fboundp 'class-name-of)
  (setf (symbol-function 'class-name-of)
	(symbol-function 'early-class-name-of)))
(unintern 'early-class-name-of)

(defun early-class-direct-subclasses (class)
  (!bootstrap-get-slot 'class class 'direct-subclasses))

(declaim (notinline load-defclass))
(defun load-defclass (name metaclass supers canonical-slots canonical-options)
  (setq supers  (copy-tree supers)
	canonical-slots   (copy-tree canonical-slots)
	canonical-options (copy-tree canonical-options))
  (let ((ecd
	  (make-early-class-definition name
				       *load-pathname*
				       metaclass
				       supers
				       canonical-slots
				       canonical-options))
	(existing
	  (find name *early-class-definitions* :key #'ecd-class-name)))
    (setq *early-class-definitions*
	  (cons ecd (remove existing *early-class-definitions*)))
    ecd))


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

(eval-when (:compile-toplevel :load-toplevel :execute)

;;; FIXME: These are non-ANSI hacks which it would be nice to get rid of.
(defvar *defclass-times*   '(:load-toplevel :execute)) ; You probably have
					; to change this if you use
					; DEFCONSTRUCTOR.
(defvar *defmethod-times*  '(:load-toplevel :execute))
(defvar *defgeneric-times* '(:load-toplevel :execute))

) ; EVAL-WHEN

(eval-when (:load-toplevel :execute)
  (when (eq *boot-state* 'complete)
    (error "Trying to load (or compile) PCL in an environment in which it~%~
	    has already been loaded. This doesn't work, you will have to~%~
	    get a fresh lisp (reboot) and then load PCL."))
  (when *boot-state*
    (cerror "Try loading (or compiling) PCL anyways."
	    "Trying to load (or compile) PCL in an environment in which it~%~
	     has already been partially loaded. This may not work, you may~%~
	     need to get a fresh lisp (reboot) and then load PCL."))
  ) ; EVAL-WHEN

;;; This is like fdefinition on the Lispm. If Common Lisp had something like
;;; function specs I wouldn't need this. On the other hand, I don't like the
;;; way this really works so maybe function specs aren't really right either?
;;;
;;; I also don't understand the real implications of a Lisp-1 on this sort of
;;; thing. Certainly some of the lossage in all of this is because these
;;; SPECs name global definitions.
;;;
;;; Note that this implementation is set up so that an implementation which
;;; has a 'real' function spec mechanism can use that instead and in that way
;;; get rid of setf generic function names.
(defmacro parse-gspec (spec
		       (non-setf-var . non-setf-case)
		       (setf-var . setf-case))
  (declare (indentation 1 1))
  #+setf (declare (ignore setf-var setf-case))
  (once-only (spec)
    `(cond (#-setf (symbolp ,spec) #+setf t
	    (let ((,non-setf-var ,spec)) ,@non-setf-case))
	   #-setf
	   ((and (listp ,spec)
		 (eq (car ,spec) 'setf)
		 (symbolp (cadr ,spec)))
	    (let ((,setf-var (cadr ,spec))) ,@setf-case))
	   #-setf
	   (t
	    (error
	      "Can't understand ~S as a generic function specifier.~%~
	       It must be either a symbol which can name a function or~%~
	       a list like ~S, where the car is the symbol ~S and the cadr~%~
	       is a symbol which can name a generic function."
	      ,spec '(setf <foo>) 'setf)))))

;;; If symbol names a function which is traced or advised, return the
;;; unadvised, traced etc. definition. This lets me get at the generic
;;; function object even when it is traced.
(defun unencapsulated-fdefinition (symbol)
  (symbol-function symbol))

;;; If symbol names a function which is traced or advised, redefine
;;; the `real' definition without affecting the advise.
(defun fdefine-carefully (name new-definition)
  (progn
    (sb-c::%%defun name new-definition nil)
    (sb-c::note-name-defined name :function)
    new-definition)
  (setf (symbol-function name) new-definition))

(defun gboundp (spec)
  (parse-gspec spec
    (name (fboundp name))
    (name (fboundp (get-setf-function-name name)))))

(defun gmakunbound (spec)
  (parse-gspec spec
    (name (fmakunbound name))
    (name (fmakunbound (get-setf-function-name name)))))

(defun gdefinition (spec)
  (parse-gspec spec
    (name (or #-setf (macro-function name)		;??
	      (unencapsulated-fdefinition name)))
    (name (unencapsulated-fdefinition (get-setf-function-name name)))))

(defun #-setf SETF\ SB-PCL\ GDEFINITION #+setf (setf gdefinition) (new-value
								   spec)
  (parse-gspec spec
    (name (fdefine-carefully name new-value))
    (name (fdefine-carefully (get-setf-function-name name) new-value))))

(declaim (special *the-class-t*
		  *the-class-vector* *the-class-symbol*
		  *the-class-string* *the-class-sequence*
		  *the-class-rational* *the-class-ratio*
		  *the-class-number* *the-class-null* *the-class-list*
		  *the-class-integer* *the-class-float* *the-class-cons*
		  *the-class-complex* *the-class-character*
		  *the-class-bit-vector* *the-class-array*
		  *the-class-stream*

		  *the-class-slot-object*
		  *the-class-structure-object*
		  *the-class-std-object*
		  *the-class-standard-object*
		  *the-class-funcallable-standard-object*
		  *the-class-class*
		  *the-class-generic-function*
		  *the-class-built-in-class*
		  *the-class-slot-class*
		  *the-class-structure-class*
		  *the-class-std-class*
		  *the-class-standard-class*
		  *the-class-funcallable-standard-class*
		  *the-class-method*
		  *the-class-standard-method*
		  *the-class-standard-reader-method*
		  *the-class-standard-writer-method*
		  *the-class-standard-boundp-method*
		  *the-class-standard-generic-function*
		  *the-class-standard-effective-slot-definition*

		  *the-eslotd-standard-class-slots*
		  *the-eslotd-funcallable-standard-class-slots*))

(declaim (special *the-wrapper-of-t*
		  *the-wrapper-of-vector* *the-wrapper-of-symbol*
		  *the-wrapper-of-string* *the-wrapper-of-sequence*
		  *the-wrapper-of-rational* *the-wrapper-of-ratio*
		  *the-wrapper-of-number* *the-wrapper-of-null*
		  *the-wrapper-of-list* *the-wrapper-of-integer*
		  *the-wrapper-of-float* *the-wrapper-of-cons*
		  *the-wrapper-of-complex* *the-wrapper-of-character*
		  *the-wrapper-of-bit-vector* *the-wrapper-of-array*))

;;;; type specifier hackery

;;; internal to this file.
(defun coerce-to-class (class &optional make-forward-referenced-class-p)
  (if (symbolp class)
      (or (find-class class (not make-forward-referenced-class-p))
	  (ensure-class class))
      class))

;;; Interface
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
	((and (null args) (typep type 'cl:class))
	 (or (sb-kernel:class-pcl-class type)
	     (find-structure-class (cl:class-name type))))
	((specializerp type) type)))

;;; interface
(defun type-from-specializer (specl)
  (cond ((eq specl 't)
	 't)
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
      (if (eq type 't)
	  *the-class-t*
	  (error "bad argument to type-class"))
      (case (car type)
	(eql (class-of (cadr type)))
	(prototype (class-of (cadr type))) ;?
	(class-eq (cadr type))
	(class (cadr type)))))

(defun class-eq-type (class)
  (specializer-type (class-eq-specializer class)))

(defun inform-type-system-about-std-class (name)
  (let ((predicate-name (make-type-predicate-name name)))
    (setf (gdefinition predicate-name)
	  (make-type-predicate name))
    (do-satisfies-deftype name predicate-name)))

(defun make-type-predicate (name)
  (let ((cell (find-class-cell name)))
    #'(lambda (x)
	(funcall (the function (find-class-cell-predicate cell)) x))))

;This stuff isn't right. Good thing it isn't used.
;The satisfies predicate has to be a symbol. There is no way to
;construct such a symbol from a class object if class names change.
(defun class-predicate (class)
  (when (symbolp class) (setq class (find-class class)))
  #'(lambda (object) (memq class (class-precedence-list (class-of object)))))

(defun make-class-eq-predicate (class)
  (when (symbolp class) (setq class (find-class class)))
  #'(lambda (object) (eq class (class-of object))))

(defun make-eql-predicate (eql-object)
  #'(lambda (object) (eql eql-object object)))

#|| ; The argument to satisfies must be a symbol.
(deftype class (&optional class)
  (if class
      `(satisfies ,(class-predicate class))
      `(satisfies ,(class-predicate 'class))))

(deftype class-eq (class)
  `(satisfies ,(make-class-eq-predicate class)))
||#

;;; internal to this file
;;;
;;; These functions are a pale imitiation of their namesake. They accept
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

;;; Not used...
#+nil
(defun unparse-type-list (tlist)
  (mapcar #'unparse-type tlist))

;;; Not used...
#+nil
(defun unparse-type (type)
  (if (atom type)
      (if (specializerp type)
	  (unparse-type (specializer-type type))
	  type)
      (case (car type)
	(eql type)
	(class-eq `(class-eq ,(class-name (cadr type))))
	(class (class-name (cadr type)))
	(t `(,(car type) ,@(unparse-type-list (cdr type)))))))

;;; internal to this file...
(defun convert-to-system-type (type)
  (case (car type)
    ((not and or) `(,(car type) ,@(mapcar #'convert-to-system-type
					  (cdr type))))
    ((class class-eq) ; class-eq is impossible to do right
     (sb-kernel:layout-class (class-wrapper (cadr type))))
    (eql type)
    (t (if (null (cdr type))
	   (car type)
	   type))))

;;; not used...
#+nil
(defun *typep (object type)
  (setq type (*normalize-type type))
  (cond ((member (car type) '(eql wrapper-eq class-eq class))
	 (specializer-applicable-using-type-p type `(eql ,object)))
	((eq (car type) 'not)
	 (not (*typep object (cadr type))))
	(t
	 (typep object (convert-to-system-type type)))))

;;; Writing the missing NOT and AND clauses will improve
;;; the quality of code generated by generate-discrimination-net, but
;;; calling subtypep in place of just returning (values nil nil) can be
;;; very slow. *SUBTYPEP is used by PCL itself, and must be fast.
(defun *subtypep (type1 type2)
  (if (equal type1 type2)
      (values t t)
      (if (eq *boot-state* 'early)
	  (values (eq type1 type2) t)
	  (let ((*in-precompute-effective-methods-p* t))
	    (declare (special *in-precompute-effective-methods-p*))
	    ;; *in-precompute-effective-methods-p* is not a good name.
	    ;; It changes the way class-applicable-using-class-p works.
	    (setq type1 (*normalize-type type1))
	    (setq type2 (*normalize-type type2))
	    (case (car type2)
	      (not
	       (values nil nil)) ; Should improve this.
	      (and
	       (values nil nil)) ; Should improve this.
	      ((eql wrapper-eq class-eq class)
	       (multiple-value-bind (app-p maybe-app-p)
		   (specializer-applicable-using-type-p type2 type1)
		 (values app-p (or app-p (not maybe-app-p)))))
	      (t
	       (subtypep (convert-to-system-type type1)
			 (convert-to-system-type type2))))))))

(defun do-satisfies-deftype (name predicate)
  (declare (ignore name predicate)))

(defun make-type-predicate-name (name &optional kind)
  (if (symbol-package name)
      (intern (format nil
		      "~@[~A ~]TYPE-PREDICATE ~A ~A"
		      kind
		      (package-name (symbol-package name))
		      (symbol-name name))
	      *pcl-package*)
      (make-symbol (format nil
			   "~@[~A ~]TYPE-PREDICATE ~A"
			   kind
			   (symbol-name name)))))

(defvar *built-in-class-symbols* ())
(defvar *built-in-wrapper-symbols* ())

(defun get-built-in-class-symbol (class-name)
  (or (cadr (assq class-name *built-in-class-symbols*))
      (let ((symbol (intern (format nil
				    "*THE-CLASS-~A*"
				    (symbol-name class-name))
			    *pcl-package*)))
	(push (list class-name symbol) *built-in-class-symbols*)
	symbol)))

(defun get-built-in-wrapper-symbol (class-name)
  (or (cadr (assq class-name *built-in-wrapper-symbols*))
      (let ((symbol (intern (format nil
				    "*THE-WRAPPER-OF-~A*"
				    (symbol-name class-name))
			    *pcl-package*)))
	(push (list class-name symbol) *built-in-wrapper-symbols*)
	symbol)))

(pushnew 'class *variable-declarations*)
(pushnew 'variable-rebinding *variable-declarations*)

(defun variable-class (var env)
  (caddr (variable-declaration 'class var env)))

(defvar *name->class->slotd-table* (make-hash-table))

;;; This is used by combined methods to communicate the next methods to
;;; the methods they call. This variable is captured by a lexical variable
;;; of the methods to give it the proper lexical scope.
(defvar *next-methods* nil)

(defvar *not-an-eql-specializer* '(not-an-eql-specializer))

(defvar *umi-gfs*)
(defvar *umi-complete-classes*)
(defvar *umi-reorder*)

(defvar *invalidate-discriminating-function-force-p* ())
(defvar *invalid-dfuns-on-stack* ())

(defvar *standard-method-combination*)

(defvar *slotd-unsupplied* (list '*slotd-unsupplied*))	;***

(defmacro define-gf-predicate (predicate-name &rest classes)
  `(progn
     (defmethod ,predicate-name ((x t)) nil)
     ,@(mapcar #'(lambda (c) `(defmethod ,predicate-name ((x ,c)) t))
	       classes)))

(defun make-class-predicate-name (name)
  (intern (format nil "~A::~A class predicate"
		  (package-name (symbol-package name))
		  name)
	  *pcl-package*))

(defun plist-value (object name)
  (getf (object-plist object) name))

(defun #-setf SETF\ SB-PCL\ PLIST-VALUE #+setf (setf plist-value) (new-value
								   object
								   name)
  (if new-value
      (setf (getf (object-plist object) name) new-value)
      (progn
	(remf (object-plist object) name)
	nil)))

;;;; built-in classes

;;; FIXME: This was the portable PCL way of setting up
;;; *BUILT-IN-CLASSES*, but in SBCL (as in CMU CL) it's almost
;;; entirely wasted motion, since it's immediately overwritten by a
;;; result mostly derived from SB-KERNEL::*BUILT-IN-CLASSES*. However,
;;; we can't just delete it, since the fifth element from each entry
;;; (a prototype of the class) is still in the final result. It would
;;; be nice to clean this up so that the other, never-used stuff is
;;; gone, perhaps finding a tidier way to represent examples of each
;;; class, too.
;;;
;;; FIXME: This can probably be blown away after bootstrapping.
;;; And SB-KERNEL::*BUILT-IN-CLASSES*, too..
#|
(defvar *built-in-classes*
  ;; name       supers     subs		     cdr of cpl
  ;; prototype
  '(;(t	 ()	 (number sequence array character symbol) ())
    (number     (t)	(complex float rational) (t))
    (complex    (number)   ()		       (number t)
     #c(1 1))
    (float      (number)   ()		       (number t)
     1.0)
    (rational   (number)   (integer ratio)	  (number t))
    (integer    (rational) ()		       (rational number t)
     1)
    (ratio      (rational) ()		       (rational number t)
     1/2)

    (sequence   (t)	(list vector)	    (t))
    (list       (sequence) (cons null)	      (sequence t))
    (cons       (list)     ()		       (list sequence t)
     (nil))

    (array      (t)	(vector)		 (t)
     #2A((nil)))
    (vector     (array
		 sequence) (string bit-vector)      (array sequence t)
     #())
    (string     (vector)   ()		       (vector array sequence t)
     "")
    (bit-vector (vector)   ()		       (vector array sequence t)
     #*1)
    (character  (t)	()		       (t)
     #\c)

    (symbol     (t)	(null)		   (t)
     symbol)
    (null       (symbol
		 list)     ()		       (symbol list sequence t)
     nil)))
|#

;;; Grovel over SB-KERNEL::*BUILT-IN-CLASSES* in order to set
;;; SB-PCL:*BUILT-IN-CLASSES*.
(sb-int:/show "about to set up SB-PCL::*BUILT-IN-CLASSES*")
(defvar *built-in-classes*
  (labels ((direct-supers (class)
	     (sb-int:/show "entering DIRECT-SUPERS" (sb-kernel::class-name class))
	     (if (typep class 'cl:built-in-class)
		 (sb-kernel:built-in-class-direct-superclasses class)
		 (let ((inherits (sb-kernel:layout-inherits
				  (sb-kernel:class-layout class))))
		   (sb-int:/show inherits)
		   (list (svref inherits (1- (length inherits)))))))
	   (direct-subs (class)
	     (sb-int:/show "entering DIRECT-SUBS" (sb-kernel::class-name class))
	     (sb-int:collect ((res))
	       (let ((subs (sb-kernel:class-subclasses class)))
		 (sb-int:/show subs)
		 (when subs
		   (sb-int:dohash (sub v subs)
		     (declare (ignore v))
		     (sb-int:/show sub)
		     (when (member class (direct-supers sub))
		       (res sub)))))
	       (res)))
	   (prototype (class-name)
	     (let ((assoc (assoc class-name
				 '((complex    . #c(1 1))
				   (float      . 1.0)
				   (integer    . 1)
				   (ratio      . 1/2)
				   (sequence   . nil)
				   (list       . nil)
				   (cons       . (nil))
				   (array      . #2a((nil)))
				   (vector     . #())
				   (string     . "")
				   (bit-vector . #*1)
				   (character  . #\c)
				   (symbol     . symbol)
				   (null       . nil)))))
	       (if assoc
		   (cdr assoc)
		   ;; This is the default prototype value which was
		   ;; used, without explanation, by the CMU CL code
		   ;; we're derived from. Evidently it's safe in all
		   ;; relevant cases.
		   42))))
    (mapcar (lambda (kernel-bic-entry)
	      (sb-int:/show "setting up" kernel-bic-entry)
	      (let* ((name (car kernel-bic-entry))
		     (class (cl:find-class name)))
		(sb-int:/show name class)
		`(,name
		  ,(mapcar #'cl:class-name (direct-supers class))
		  ,(mapcar #'cl:class-name (direct-subs class))
		  ,(map 'list
			(lambda (x)
			  (cl:class-name (sb-kernel:layout-class x)))
			(reverse
			 (sb-kernel:layout-inherits
			  (sb-kernel:class-layout class))))
		  ,(prototype name))))
	    (remove-if (lambda (kernel-bic-entry)
			 (member (first kernel-bic-entry)
				 ;; I'm not sure why these are removed from
				 ;; the list, but that's what the original
				 ;; CMU CL code did. -- WHN 20000715
				 '(t sb-kernel:instance
				     sb-kernel:funcallable-instance
				     function stream)))
		       sb-kernel::*built-in-classes*))))
(sb-int:/show "done setting up SB-PCL::*BUILT-IN-CLASSES*")

;;;; the classes that define the kernel of the metabraid

(defclass t () ()
  (:metaclass built-in-class))

(defclass sb-kernel:instance (t) ()
  (:metaclass built-in-class))

(defclass function (t) ()
  (:metaclass built-in-class))

(defclass sb-kernel:funcallable-instance (function) ()
  (:metaclass built-in-class))

(defclass stream (t) ()
  (:metaclass built-in-class))

(defclass slot-object (t) ()
  (:metaclass slot-class))

(defclass structure-object (slot-object sb-kernel:instance) ()
  (:metaclass structure-class))

(defstruct (dead-beef-structure-object
	    (:constructor |STRUCTURE-OBJECT class constructor|)))

(defclass std-object (slot-object) ()
  (:metaclass std-class))

(defclass standard-object (std-object sb-kernel:instance) ())

(defclass funcallable-standard-object (std-object
				       sb-kernel:funcallable-instance)
     ()
  (:metaclass funcallable-standard-class))

(defclass specializer (standard-object)
     ((type
	:initform nil
	:reader specializer-type)))

(defclass definition-source-mixin (std-object)
     ((source
	:initform *load-truename*
	:reader definition-source
	:initarg :definition-source))
  (:metaclass std-class))

(defclass plist-mixin (std-object)
     ((plist
	:initform ()
	:accessor object-plist))
  (:metaclass std-class))

(defclass documentation-mixin (plist-mixin)
     ()
  (:metaclass std-class))

(defclass dependent-update-mixin (plist-mixin)
    ()
  (:metaclass std-class))

;;; The class CLASS is a specified basic class. It is the common superclass
;;; of any kind of class. That is any class that can be a metaclass must
;;; have the class CLASS in its class precedence list.
(defclass class (documentation-mixin dependent-update-mixin
		 definition-source-mixin specializer)
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
      (direct-subclasses
	:initform ()
	:reader class-direct-subclasses)
      (direct-methods
	:initform (cons nil nil))
      (predicate-name
	:initform nil
	:reader class-predicate-name)))

;;; The class PCL-CLASS is an implementation-specific common superclass of
;;; all specified subclasses of the class CLASS.
(defclass pcl-class (class)
     ((class-precedence-list
	:reader class-precedence-list)
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
	:accessor class-slots)
      (initialize-info
	:initform nil
	:accessor class-initialize-info)))

;;; The class STD-CLASS is an implementation-specific common superclass of
;;; the classes STANDARD-CLASS and FUNCALLABLE-STANDARD-CLASS.
(defclass std-class (slot-class)
  ())

(defclass standard-class (std-class)
  ())

(defclass funcallable-standard-class (std-class)
  ())

(defclass forward-referenced-class (pcl-class) ())

(defclass built-in-class (pcl-class) ())

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
	:initform ""
	:initarg :documentation)
      (class
	:initform nil
	:initarg :class
	:accessor slot-definition-class)))

(defclass standard-slot-definition (slot-definition)
  ((allocation
    :initform :instance
    :initarg :allocation
    :accessor slot-definition-allocation)))

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
  ((reader-function ; #'(lambda (object) ...)
    :accessor slot-definition-reader-function)
   (writer-function ; #'(lambda (new-value object) ...)
    :accessor slot-definition-writer-function)
   (boundp-function ; #'(lambda (object) ...)
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
;     (qualifiers
;	:initform ()
;	:initarg  :qualifiers
;	:reader method-qualifiers)
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
	:initarg :function)		;no writer
      (fast-function
	:initform nil
	:initarg :fast-function		;no writer
	:reader method-fast-function)
;     (documentation
;	:initform nil
;	:initarg  :documentation
;	:reader method-documentation)
      ))

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
			    documentation-mixin
			    funcallable-standard-object)
     ()
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
      (arg-info
	:initform (make-arg-info)
	:reader gf-arg-info)
      (dfun-state
	:initform ()
	:accessor gf-dfun-state)
      (pretty-arglist
	:initform ()
	:accessor gf-pretty-arglist))
  (:metaclass funcallable-standard-class)
  (:default-initargs :method-class *the-class-standard-method*
		     :method-combination *standard-method-combination*))

(defclass method-combination (standard-object) ())

(defclass standard-method-combination
	  (definition-source-mixin method-combination)
     ((type	  :reader method-combination-type
		     :initarg :type)
      (documentation :reader method-combination-documentation
		     :initarg :documentation)
      (options       :reader method-combination-options
		     :initarg :options)))

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
    (method-combination method-combination-p)))


;;;; This file defines the defconstructor and other make-instance optimization
;;;; mechanisms.

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

;;; defconstructor is used to define special purpose functions which just
;;; call make-instance with a symbol as the first argument. The semantics
;;; of defconstructor is that it is equivalent to defining a function which
;;; just calls make-instance. The purpose of defconstructor is to provide
;;; PCL with a way of noticing these calls to make-instance so that it can
;;; optimize them. Specific ports of PCL could just have their compiler
;;; spot these calls to make-instance and then call this code. Having the
;;; special defconstructor facility is the best we can do portably.
;;;
;;; A call to defconstructor like:
;;;
;;;  (defconstructor make-foo foo (a b &rest r) a a :mumble b baz r)
;;;
;;; Is equivalent to a defun like:
;;;
;;;  (defun make-foo (a b &rest r)
;;;    (make-instance 'foo 'a a ':mumble b 'baz r))
;;;
;;; Calls like the following are also legal:
;;;
;;;  (defconstructor make-foo foo ())
;;;  (defconstructor make-bar bar () :x *x* :y *y*)
;;;  (defconstructor make-baz baz (a b c) a-b (list a b) b-c (list b c))
;;;
;;; The general idea of this implementation is that the expansion of the
;;; defconstructor form includes the creation of closure generators which
;;; can be called to create constructor code for the class. The ways that
;;; a constructor can be optimized depends not only on the defconstructor
;;; form, but also on the state of the class and the generic functions in
;;; the initialization protocol. Because of this, the determination of the
;;; form of constructor code to be used is a two part process.
;;;
;;; At compile time, make-constructor-code-generators looks at the actual
;;; defconstructor form and makes a list of appropriate constructor code
;;; generators. All that is really taken into account here is whether
;;; any initargs are supplied in the call to make-instance, and whether
;;; any of those are constant.
;;;
;;; At constructor code generation time (see note about lazy evaluation)
;;; compute-constructor-code calls each of the constructor code generators
;;; to try to get code for this constructor. Each generator looks at the
;;; state of the class and initialization protocol generic functions and
;;; decides whether its type of code is appropriate. This depends on things
;;; like whether there are any applicable methods on initialize-instance,
;;; whether class slots are affected by initialization etc.
;;;
;;; Constructor objects are funcallable instances, the protocol followed to
;;; to compute the constructor code for them is quite similar to the protocol
;;; followed to compute the discriminator code for a generic function. When
;;; the constructor is first loaded, we install as its code a function which
;;; will compute the actual constructor code the first time it is called.
;;;
;;; If there is an update to the class structure which might invalidate the
;;; optimized constructor, the special lazy constructor installer is put back
;;; so that it can compute the appropriate constructor when it is called.
;;; This is the same kind of lazy evaluation update strategy used elswhere
;;; in PCL.
;;;
;;; To allow for flexibility in the PCL implementation and to allow PCL users
;;; to specialize this constructor facility for their own metaclasses, there
;;; is an internal protocol followed by the code which loads and installs
;;; the constructors. This is documented in the comments in the code.
;;;
;;; This code is also designed so that one of its levels, can be used to
;;; implement optimization of calls to make-instance which can't go through
;;; the defconstructor facility. This has not been implemented yet, but the
;;; hooks are there.

(defmacro defconstructor
	  (name class lambda-list &rest initialization-arguments)
  (expand-defconstructor class
			 name
			 lambda-list
			 (copy-list initialization-arguments)))

(defun expand-defconstructor (class-name name lambda-list supplied-initargs)
  (let ((class (find-class class-name nil))
	(supplied-initarg-names
	  (gathering1 (collecting)
	    (iterate ((name (*list-elements supplied-initargs :by #'cddr)))
	      (gather1 name)))))
    (when (null class)
      (error "defconstructor form being compiled (or evaluated) before~@
	      class ~S is defined."
	     class-name))
    `(progn
       ;; comments from PCL code back when it was portable:
       ;;   In order to avoid undefined function warnings, we want to
       ;;   tell the compile time environment that a function with this
       ;;   name and this argument list has been defined. The portable
       ;;   way to do this is with defun:
       ;;   #-cmu (declaim (notinline ,name))
       ;;   #-cmu
       ;;   (defun ,name ,lambda-list
       ;;     (declare (ignore ,@(extract-parameters lambda-list)))
       ;;     (error "Constructor ~S not loaded." ',name))
       ;;   But the derived result type for the above is wrong under CMU CL.
       ;;   So instead:
       (declaim (ftype ,(ftype-declaration-from-lambda-list lambda-list name)
		       ,name))
       ,(make-top-level-form `(defconstructor ,name)
			     '(load eval)
	  `(load-constructor
	     ',class-name
	     ',(class-name (class-of class))
	     ',name
	     ',supplied-initarg-names
	     ;; make-constructor-code-generators is called to return a list
	     ;; of constructor code generators. The actual interpretation
	     ;; of this list is left to compute-constructor-code, but the
	     ;; general idea is that it should be an plist where the keys
	     ;; name a kind of constructor code and the values are generator
	     ;; functions which return the actual constructor code. The
	     ;; constructor code is usually a closures over the arguments
	     ;; to the generator.
	     ,(make-constructor-code-generators class
						name
						lambda-list
						supplied-initarg-names
						supplied-initargs))))))

(defun load-constructor (class-name metaclass-name constructor-name
			 supplied-initarg-names code-generators)
  (let ((class (find-class class-name nil)))
    (cond ((null class)
	   (error "defconstructor form being loaded (or evaluated) before~@
		   class ~S is defined."
		  class-name))
	  ((neq (class-name (class-of class)) metaclass-name)
	   (error "When defconstructor ~S was compiled, the metaclass of the~@
		   class ~S was ~S. The metaclass is now ~S.~@
		   The constructor must be recompiled."
		  constructor-name
		  class-name
		  metaclass-name
		  (class-name (class-of class))))
	  (t
	   (load-constructor-internal class
				      constructor-name
				      supplied-initarg-names
				      code-generators)
	   constructor-name))))

;;; The actual constructor objects.
(defclass constructor (funcallable-standard-object)
     ((class					;The class with which this
	:initarg :class				;constructor is associated.
	:reader constructor-class)		;The actual class object,
						;not the class name.

      (name					;The name of this constructor.
	:initform nil				;This is the symbol in whose
	:initarg :name				;function cell the constructor
	:reader constructor-name)		;usually sits. Of course, this
						;is optional. defconstructor
						;makes named constructors, but
						;it is possible to manipulate
						;anonymous constructors also.

      (code-type				;The type of code currently in
	:initform nil				;use by this constructor. This
	:accessor constructor-code-type)	;is mostly for debugging and
						;analysis purposes.
						;The lazy installer sets this
						;to LAZY. The most basic and
						;least optimized type of code
						;is called FALLBACK.

      (supplied-initarg-names			;The names of the initargs this
	:initarg :supplied-initarg-names	;constructor supplies when it
	:reader					;"calls" make-instance.
	   constructor-supplied-initarg-names)	;

      (code-generators				;Generators for the different
	:initarg :code-generators		;types of code this constructor
	:reader constructor-code-generators))	;could use.
  (:metaclass funcallable-standard-class))

;;; Because the value in the code-type slot should always correspond to the
;;; funcallable-instance-function of the constructor, this function should
;;; always be used to set the both at the same time.
(defun set-constructor-code (constructor code type)
  (set-funcallable-instance-function constructor code)
  (set-function-name constructor (constructor-name constructor))
  (setf (constructor-code-type constructor) type))

(defmethod describe-object ((constructor constructor) stream)
  (format stream
	  "~S is a constructor for the class ~S.~%~
	    The current code type is ~S.~%~
	    Other possible code types are ~S."
	  constructor (constructor-class constructor)
	  (constructor-code-type constructor)
	  (gathering1 (collecting)
	    (doplist (key val) (constructor-code-generators constructor)
	      (gather1 key)))))

;;; I am not in a hairy enough mood to make this implementation be metacircular
;;; enough that it can support a defconstructor for constructor objects.
(defun make-constructor (class name supplied-initarg-names code-generators)
  (make-instance 'constructor
		 :class class
		 :name name
		 :supplied-initarg-names supplied-initarg-names
		 :code-generators code-generators))

; This definition actually appears in std-class.lisp.
;(defmethod class-constructors ((class std-class))
;  (with-slots (plist) class (getf plist 'constructors)))

(defmethod add-constructor ((class slot-class)
			    (constructor constructor))
  (with-slots (plist) class
    (pushnew constructor (getf plist 'constructors))))

(defmethod remove-constructor ((class slot-class)
			       (constructor constructor))
  (with-slots (plist) class
    (setf (getf plist 'constructors)
	  (delete constructor (getf plist 'constructors)))))

(defmethod get-constructor ((class slot-class) name &optional (error-p t))
  (or (dolist (c (class-constructors class))
	(when (eq (constructor-name c) name) (return c)))
      (if error-p
	  (error "Couldn't find a constructor with name ~S for class ~S."
		 name class)
	  ())))

;;; This is called to actually load a defconstructor constructor. It must
;;; install the lazy installer in the function cell of the constructor name,
;;; and also add this constructor to the list of constructors the class has.
(defmethod load-constructor-internal
	   ((class slot-class) name initargs generators)
  (let ((constructor (make-constructor class name initargs generators))
	(old (get-constructor class name nil)))
    (when old (remove-constructor class old))
    (install-lazy-constructor-installer constructor)
    (add-constructor class constructor)
    (setf (gdefinition name) constructor)))

(defmethod install-lazy-constructor-installer ((constructor constructor))
  (let ((class (constructor-class constructor)))
    (set-constructor-code constructor
			  #'(sb-kernel:instance-lambda (&rest args)
			      (multiple-value-bind (code type)
				  (compute-constructor-code class constructor)
				(set-constructor-code constructor code type)
				(apply constructor args)))
			  'lazy)))

;;; The interface to keeping the constructors updated.
;;;
;;; add-method and remove-method (for standard-generic-function and -method),
;;; promise to call maybe-update-constructors on the generic function and
;;; the method.
;;;
;;; The class update code promises to call update-constructors whenever the
;;; class is changed. That is, whenever the supers, slots or options change.
;;; If user defined classes of constructor needs to be updated in more than
;;; these circumstances, they should use the dependent updating mechanism to
;;; make sure update-constructors is called.
;;;
;;; Bootstrapping concerns force the definitions of maybe-update-constructors
;;; and update-constructors to be in the file std-class. For clarity, they
;;; also appear below. Be sure to keep the definition here and there in sync.
;(defvar *initialization-generic-functions*
;	 (list #'make-instance
;	       #'default-initargs
;	       #'allocate-instance
;	       #'initialize-instance
;	       #'shared-initialize))
;
;(defmethod maybe-update-constructors
;	   ((generic-function generic-function)
;	    (method method))
;  (when (memq generic-function *initialization-generic-functions*)
;    (labels ((recurse (class)
;	       (update-constructors class)
;	       (dolist (subclass (class-direct-subclasses class))
;		 (recurse subclass))))
;      (when (classp (car (method-specializers method)))
;	(recurse (car (method-specializers method)))))))
;
;(defmethod update-constructors ((class slot-class))
;  (dolist (cons (class-constructors class))
;    (install-lazy-constructor-installer cons)))
;
;(defmethod update-constructors ((class class))
;  ())

;;; Here is the actual smarts for making the code generators and then trying
;;; each generator to get constructor code. This extensible mechanism allows
;;; new kinds of constructor code types to be added. A programmer defining a
;;; specialization of the constructor class can either use this mechanism to
;;; define new code types, or can override this mechanism by overriding the
;;; methods on make-constructor-code-generators and compute-constructor-code.
;;;
;;; The function defined by define-constructor-code-type will receive the
;;; class object, and the 4 original arguments to defconstructor. It can
;;; return a constructor code generator, or return nil if this type of code
;;; is determined to not be appropriate after looking at the defconstructor
;;; arguments.
;;;
;;; When compute-constructor-code is called, it first performs basic checks
;;; to make sure that the basic assumptions common to all the code types are
;;; valid. (For details see method definition). If any of the tests fail,
;;; the fallback constructor code type is used. If none of the tests fail,
;;; the constructor code generators are called in order. They receive 5
;;; arguments:
;;;
;;;   CLASS	the class the constructor is making instances of
;;;   WRAPPER      that class's wrapper
;;;   DEFAULTS     the result of calling class-default-initargs on class
;;;   INITIALIZE   the applicable methods on initialize-instance
;;;   SHARED       the applicable methosd on shared-initialize
;;;
;;; The first code generator to return code is used. The code generators are
;;; called in reverse order of definition, so define-constructor-code-type
;;; forms which define better code should appear after ones that define less
;;; good code. The fallback code type appears first. Note that redefining a
;;; code type does not change its position in the list. To do that,  define
;;; a new type at the end with the behavior.

(defvar *constructor-code-types* ())

(defmacro define-constructor-code-type (type arglist &body body)
  (let ((fn-name (intern (format nil
				 "CONSTRUCTOR-CODE-GENERATOR ~A ~A"
				 (package-name (symbol-package type))
				 (symbol-name type))
			 *pcl-package*)))
    `(progn
       (defun ,fn-name ,arglist .,body)
       (load-define-constructor-code-type ',type ',fn-name))))

(defun load-define-constructor-code-type (type generator)
  (let ((old-entry (assq type *constructor-code-types*)))
    (if old-entry
	(setf (cadr old-entry) generator)
	(push (list type generator) *constructor-code-types*))
    type))

(defmethod make-constructor-code-generators
	   ((class slot-class)
	    name lambda-list supplied-initarg-names supplied-initargs)
  (cons 'list
	(gathering1 (collecting)
	  (dolist (entry *constructor-code-types*)
	    (let ((generator
		    (funcall (cadr entry) class name lambda-list
					  supplied-initarg-names
					  supplied-initargs)))
	      (when generator
		(gather1 `',(car entry))
		(gather1 generator)))))))

(defmethod compute-constructor-code ((class slot-class)
				     (constructor constructor))
  (let* ((proto (class-prototype class))
	 (wrapper (class-wrapper class))
	 (defaults (class-default-initargs class))
	 (make
	   (compute-applicable-methods (gdefinition 'make-instance) (list class)))
	 (supplied-initarg-names
	   (constructor-supplied-initarg-names constructor))
	 (default
	   (compute-applicable-methods (gdefinition 'default-initargs)
				       (list class supplied-initarg-names))) ;?
	 (allocate
	   (compute-applicable-methods (gdefinition 'allocate-instance)
				       (list class)))
	 (initialize
	   (compute-applicable-methods (gdefinition 'initialize-instance)
				       (list proto)))
	 (shared
	   (compute-applicable-methods (gdefinition 'shared-initialize)
				       (list proto t)))
	 (code-generators
	   (constructor-code-generators constructor)))
    (flet ((call-code-generator (generator)
	     (when (null generator)
	       (unless (setq generator (getf code-generators 'fallback))
		 (error "No FALLBACK generator?")))
	     (funcall generator class wrapper defaults initialize shared)))
      (if (or (cdr make)
	      (cdr default)
	      (cdr allocate)
	      (not (check-initargs-1 class
				     supplied-initarg-names
				     (append initialize shared)
				     nil nil)))
	  ;; These are basic shared assumptions, if one of the
	  ;; has been violated, we have to resort to the fallback
	  ;; case. Any of these assumptions could be moved out
	  ;; of here and into the individual code types if there
	  ;; was a need to do so.
	  (values (call-code-generator nil) 'fallback)
	  ;; Otherwise try all the generators until one produces
	  ;; code for us.
	  (doplist (type generator) code-generators
	    (let ((code (call-code-generator generator)))
	      (when code (return (values code type)))))))))

;;; The facilities are useful for debugging, and to measure the performance
;;; boost from constructors.
;;;
;;; FIXME: so they should probably be #+SB-SHOW instead of unconditional

(defun map-constructors (fn)
  (let ((nclasses 0)
	(nconstructors 0))
    (labels ((recurse (class)
	       (incf nclasses)
	       (dolist (constructor (class-constructors class))
		 (incf nconstructors)
		 (funcall fn constructor))
	       (dolist (subclass (class-direct-subclasses class))
		 (recurse subclass))))
      (recurse (find-class 't))
      (values nclasses nconstructors))))

(defun reset-constructors ()
  (multiple-value-bind (nclass ncons)
      (map-constructors #'install-lazy-constructor-installer )
    (format t "~&~D classes, ~D constructors." nclass ncons)))

(defun disable-constructors ()
  (multiple-value-bind (nclass ncons)
      (map-constructors
	#'(lambda (c)
	    (let ((gen (getf (constructor-code-generators c) 'fallback)))
	      (if (null gen)
		  (error "No fallback constructor for ~S." c)
		  (set-constructor-code c
					(funcall gen
						 (constructor-class c)
						 () () () ())
					'fallback)))))
    (format t "~&~D classes, ~D constructors." nclass ncons)))

(defun enable-constructors ()
  (reset-constructors))

;;; helper functions and utilities that are shared by all of the code types
;;; and by the main compute-constructor-code method as well

(defvar *standard-initialize-instance-method*
	(get-method #'initialize-instance
		    ()
		    (list *the-class-slot-object*)))

(defvar *standard-shared-initialize-method*
	(get-method #'shared-initialize
		    ()
		    (list *the-class-slot-object* *the-class-t*)))

(defun non-pcl-initialize-instance-methods-p (methods)
  (notevery #'(lambda (m) (eq m *standard-initialize-instance-method*))
	    methods))

(defun non-pcl-shared-initialize-methods-p (methods)
  (notevery #'(lambda (m) (eq m *standard-shared-initialize-method*))
	    methods))

(defun non-pcl-or-after-initialize-instance-methods-p (methods)
  (notevery #'(lambda (m) (or (eq m *standard-initialize-instance-method*)
			      (equal '(:after) (method-qualifiers m))))
	    methods))

(defun non-pcl-or-after-shared-initialize-methods-p (methods)
  (notevery #'(lambda (m) (or (eq m *standard-shared-initialize-method*)
			      (equal '(:after) (method-qualifiers m))))
	    methods))

;;; This returns two values. The first is a vector which can be used as the
;;; initial value of the slots vector for the instance. The second is a symbol
;;; describing the initforms this class has.
;;;
;;;  If the first value is:
;;;
;;;    :UNSUPPLIED    no slot has an initform
;;;    :CONSTANTS     all slots have either a constant initform
;;;		      or no initform at all
;;;    T	      there is at least one non-constant initform
(defun compute-constant-vector (class)
  ;;(declare (values constants flag))
  (let* ((wrapper (class-wrapper class))
	 (layout (wrapper-instance-slots-layout wrapper))
	 (flag :unsupplied)
	 (constants ()))
    (dolist (slotd (class-slots class))
      (let ((name (slot-definition-name slotd))
	    (initform (slot-definition-initform slotd))
	    (initfn (slot-definition-initfunction slotd)))
	(cond ((null (memq name layout)))
	      ((null initfn)
	       (push (cons name *slot-unbound*) constants))
	      ((constantp initform)
	       (push (cons name (eval initform)) constants)
	       (when (eq flag ':unsupplied) (setq flag ':constants)))
	      (t
	       (push (cons name *slot-unbound*) constants)
	       (setq flag 't)))))
    (let* ((constants-alist (sort constants #'(lambda (x y)
						(memq (car y)
						      (memq (car x) layout)))))
	   (constants-list (mapcar #'cdr constants-alist)))
    (values constants-list flag))))

;;; This takes a class and a list of initarg-names, and returns an alist
;;; indicating the positions of the slots those initargs may fill. The
;;; order of the initarg-names argument is important of course, since we
;;; have to respect the rules about the leftmost initarg that fills a slot
;;; having precedence. This function allows initarg names to appear twice
;;; in the list, it only considers the first appearance.
(defun compute-initarg-positions (class initarg-names)
  (let* ((layout (wrapper-instance-slots-layout (class-wrapper class)))
	 (positions
	   (gathering1 (collecting)
	     (iterate ((slot-name (list-elements layout))
		       (position (interval :from 0)))
	       (gather1 (cons slot-name position)))))
	 (slot-initargs
	   (mapcar #'(lambda (slotd)
		       (list (slot-definition-initargs slotd)
			     (or (cdr (assq (slot-definition-name slotd)
					    positions))
				 ':class)))
		   (class-slots class))))
    ;; Go through each of the initargs, and figure out what position
    ;; it fills by replacing the entries in slot-initargs it fills.
    (dolist (initarg initarg-names)
      (dolist (slot-entry slot-initargs)
	(let ((slot-initargs (car slot-entry)))
	  (when (and (listp slot-initargs)
		     (not (null slot-initargs))
		     (memq initarg slot-initargs))
	    (setf (car slot-entry) initarg)))))
    (gathering1 (collecting)
      (dolist (initarg initarg-names)
	(let ((positions (gathering1 (collecting)
			   (dolist (slot-entry slot-initargs)
			     (when (eq (car slot-entry) initarg)
			       (gather1 (cadr slot-entry)))))))
	  (when positions
	    (gather1 (cons initarg positions))))))))

;;; The FALLBACK case allows anything. This always works, and always appears
;;; as the last of the generators for a constructor. It does a full call to
;;; make-instance.
(define-constructor-code-type fallback
	(class name arglist supplied-initarg-names supplied-initargs)
  (declare (ignore name supplied-initarg-names))
  `(function
     (lambda (&rest ignore)
       (declare (ignore ignore))
       (function
	 (sb-kernel:instance-lambda ,arglist
	   (make-instance
	     ',(class-name class)
	     ,@(gathering1 (collecting)
		 (iterate ((tail (*list-tails supplied-initargs :by #'cddr)))
		   (gather1 `',(car tail))
		   (gather1 (cadr tail))))))))))

;;; The GENERAL case allows:
;;;   constant, unsupplied or non-constant initforms
;;;   constant or non-constant default initargs
;;;   supplied initargs
;;;   slot-filling initargs
;;;   :after methods on shared-initialize and initialize-instance
(define-constructor-code-type general
	(class name arglist supplied-initarg-names supplied-initargs)
  (declare (ignore name))
  (let ((raw-allocator (raw-instance-allocator class))
	(slots-fetcher (slots-fetcher class)))
    `(function
       (lambda (class .wrapper. defaults init shared)
	 (multiple-value-bind (.constants.
			       .constant-initargs.
			       .initfns-initargs-and-positions.
			       .supplied-initarg-positions.
			       .shared-initfns.
			       .initfns.)
	     (general-generator-internal class
					 defaults
					 init
					 shared
					 ',supplied-initarg-names
					 ',supplied-initargs)
	   .supplied-initarg-positions.
	   (when (and .constants.
		      (null (non-pcl-or-after-initialize-instance-methods-p
			      init))
		      (null (non-pcl-or-after-shared-initialize-methods-p
			      shared)))
	     (function
	       (sb-kernel:instance-lambda ,arglist
		 (declare #.*optimize-speed*)
		 (let* ((.instance. (,raw-allocator .wrapper. .constants.))
			(.slots. (,slots-fetcher .instance.))
			(.positions. .supplied-initarg-positions.)
			(.initargs. .constant-initargs.))
		   .positions.

		   (dolist (entry .initfns-initargs-and-positions.)
		     (let ((val (funcall (car entry)))
			   (initarg (cadr entry)))
		       (when initarg
			 (push val .initargs.)
			 (push initarg .initargs.))
		       (dolist (pos (cddr entry))
			 (setf (%instance-ref .slots. pos) val))))

		   ,@(gathering1 (collecting)
		       (doplist (initarg value) supplied-initargs
			 (unless (constantp value)
			   (gather1 `(let ((.value. ,value))
				       (push .value. .initargs.)
				       (push ',initarg .initargs.)
				       (dolist (.p. (pop .positions.))
					 (setf (%instance-ref .slots. .p.)
					       .value.)))))))

		   (dolist (fn .shared-initfns.)
		     (apply fn .instance. t .initargs.))
		   (dolist (fn .initfns.)
		     (apply fn .instance. .initargs.))

		   .instance.)))))))))

(defun general-generator-internal
       (class defaults init shared supplied-initarg-names supplied-initargs)
  (flet ((bail-out () (return-from general-generator-internal nil)))
    (let* ((constants (compute-constant-vector class))
	   (layout (wrapper-instance-slots-layout (class-wrapper class)))
	   (initarg-positions
	     (compute-initarg-positions class
					(append supplied-initarg-names
						(mapcar #'car defaults))))
	   (initfns-initargs-and-positions ())
	   (supplied-initarg-positions ())
	   (constant-initargs ())
	   (used-positions ()))

      ;; Go through each of the supplied initargs for three reasons.
      ;;
      ;;   - If it fills a class slot, bail out.
      ;;   - If its a constant form, fill the constant vector.
      ;;   - Otherwise remember the positions no two initargs
      ;;     will try to fill the same position, since compute
      ;;     initarg positions already took care of that, but
      ;;     we do need to know what initforms will and won't
      ;;     be needed.
      (doplist (initarg val) supplied-initargs
	(let ((positions (cdr (assq initarg initarg-positions))))
	  (cond ((memq :class positions) (bail-out))
		((constantp val)
		 (setq val (eval val))
		 (push val constant-initargs)
		 (push initarg constant-initargs)
		 (dolist (pos positions) (setf (svref constants pos) val)))
		(t
		 (push positions supplied-initarg-positions)))
	  (setq used-positions (append positions used-positions))))

      ;; Go through each of the default initargs, for three reasons.
      ;;
      ;;   - If it fills a class slot, bail out.
      ;;   - If it is a constant, and it does fill a slot, put that
      ;;     into the constant vector.
      ;;   - If it isn't a constant, record its initfn and position.
      (dolist (default defaults)
	(let* ((name (car default))
	       (initfn (cadr default))
	       (form (caddr default))
	       (value ())
	       (positions (cdr (assq name initarg-positions))))
	  (unless (memq name supplied-initarg-names)
	    (cond ((memq :class positions) (bail-out))
		  ((constantp form)
		   (setq value (eval form))
		   (push value constant-initargs)
		   (push name constant-initargs)
		   (dolist (pos positions)
		     (setf (svref constants pos) value)))
		  (t
		   (push (list* initfn name positions)
			 initfns-initargs-and-positions)))
	    (setq used-positions (append positions used-positions)))))

      ;; Go through each of the slot initforms:
      ;;
      ;;    - If its position has already been filled, do nothing.
      ;;      The initfn won't need to be called, and the slot won't
      ;;      need to be touched.
      ;;    - If it is a class slot, and has an initform, bail out.
      ;;    - If its a constant or unsupplied, ignore it, it is
      ;;      already in the constant vector.
      ;;    - Otherwise, record its initfn and position
      (dolist (slotd (class-slots class))
	(let* ((alloc (slot-definition-allocation slotd))
	       (name (slot-definition-name slotd))
	       (form (slot-definition-initform slotd))
	       (initfn (slot-definition-initfunction slotd))
	       (position (position name layout)))
	  (cond ((neq alloc :instance)
		 (unless (null initfn)
		   (bail-out)))
		((member position used-positions))
		((or (constantp form)
		     (null initfn)))
		(t
		 (push (list initfn nil position)
		       initfns-initargs-and-positions)))))

      (values constants
	      constant-initargs
	      (nreverse initfns-initargs-and-positions)
	      (nreverse supplied-initarg-positions)
	      (mapcar #'method-function
		      (remove *standard-shared-initialize-method* shared))
	      (mapcar #'method-function
		      (remove *standard-initialize-instance-method* init))))))

;;; The NO-METHODS case allows:
;;;   constant, unsupplied or non-constant initforms
;;;   constant or non-constant default initargs
;;;   supplied initargs that are arguments to constructor, or constants
;;;   slot-filling initargs
(define-constructor-code-type no-methods
	(class name arglist supplied-initarg-names supplied-initargs)
  (declare (ignore name))
  (let ((raw-allocator (raw-instance-allocator class))
	(slots-fetcher (slots-fetcher class)))
    `(function
       (lambda (class .wrapper. defaults init shared)
	 (multiple-value-bind (.constants.
			       .initfns-and-positions.
			       .supplied-initarg-positions.)
	     (no-methods-generator-internal class
					    defaults
					    ',supplied-initarg-names
					    ',supplied-initargs)
	   .initfns-and-positions.
	   .supplied-initarg-positions.
	   (when (and .constants.
		      (null (non-pcl-initialize-instance-methods-p init))
		      (null (non-pcl-shared-initialize-methods-p shared)))
	     #'(sb-kernel:instance-lambda ,arglist
		 (declare #.*optimize-speed*)
		 (let* ((.instance. (,raw-allocator .wrapper. .constants.))
			(.slots. (,slots-fetcher .instance.))
			(.positions. .supplied-initarg-positions.))
		   .positions.

		   (dolist (entry .initfns-and-positions.)
		     (let ((val (funcall (car entry))))
		       (dolist (pos (cdr entry))
			 (setf (%instance-ref .slots. pos) val))))

		   ,@(gathering1 (collecting)
		       (doplist (initarg value) supplied-initargs
			 (unless (constantp value)
			   (gather1
			     `(let ((.value. ,value))
				(dolist (.p. (pop .positions.))
				  (setf (%instance-ref .slots. .p.) .value.)))))))

		   .instance.))))))))

(defun no-methods-generator-internal
       (class defaults supplied-initarg-names supplied-initargs)
  (flet ((bail-out () (return-from no-methods-generator-internal nil)))
    (let* ((constants	(compute-constant-vector class))
	   (layout (wrapper-instance-slots-layout (class-wrapper class)))
	   (initarg-positions
	     (compute-initarg-positions class
					(append supplied-initarg-names
						(mapcar #'car defaults))))
	   (initfns-and-positions ())
	   (supplied-initarg-positions ())
	   (used-positions ()))

      ;; Go through each of the supplied initargs for three reasons.
      ;;
      ;;   - If it fills a class slot, bail out.
      ;;   - If its a constant form, fill the constant vector.
      ;;   - Otherwise remember the positions, no two initargs
      ;;     will try to fill the same position, since compute
      ;;     initarg positions already took care of that, but
      ;;     we do need to know what initforms will and won't
      ;;     be needed.
      (doplist (initarg val) supplied-initargs
	(let ((positions (cdr (assq initarg initarg-positions))))
	  (cond ((memq :class positions) (bail-out))
		((constantp val)
		 (setq val (eval val))
		 (dolist (pos positions)
		   (setf (svref constants pos) val)))
		(t
		 (push positions supplied-initarg-positions)))
	  (setq used-positions (append positions used-positions))))

      ;; Go through each of the default initargs, for three reasons.
      ;;
      ;;   - If it fills a class slot, bail out.
      ;;   - If it is a constant, and it does fill a slot, put that
      ;;     into the constant vector.
      ;;   - If it isn't a constant, record its initfn and position.
      (dolist (default defaults)
	(let* ((name (car default))
	       (initfn (cadr default))
	       (form (caddr default))
	       (value ())
	       (positions (cdr (assq name initarg-positions))))
	  (unless (memq name supplied-initarg-names)
	    (cond ((memq :class positions) (bail-out))
		  ((constantp form)
		   (setq value (eval form))
		   (dolist (pos positions)
		     (setf (svref constants pos) value)))
		  (t
		   (push (cons initfn positions)
			 initfns-and-positions)))
	    (setq used-positions (append positions used-positions)))))

      ;; Go through each of the slot initforms:
      ;;
      ;;    - If its position has already been filled, do nothing.
      ;;      The initfn won't need to be called, and the slot won't
      ;;      need to be touched.
      ;;    - If it is a class slot, and has an initform, bail out.
      ;;    - If its a constant or unsupplied, do nothing, we know
      ;;      that it is already in the constant vector.
      ;;    - Otherwise, record its initfn and position
      (dolist (slotd (class-slots class))
	(let* ((alloc (slot-definition-allocation slotd))
	       (name (slot-definition-name slotd))
	       (form (slot-definition-initform slotd))
	       (initfn (slot-definition-initfunction slotd))
	       (position (position name layout)))
	  (cond ((neq alloc :instance)
		 (unless (null initfn)
		   (bail-out)))
		((member position used-positions))
		((or (constantp form)
		     (null initfn)))
		(t
		 (push (list initfn position) initfns-and-positions)))))

      (values constants
	      (nreverse initfns-and-positions)
	      (nreverse supplied-initarg-positions)))))

;;; The SIMPLE-SLOTS case allows:
;;;   constant or unsupplied initforms
;;;   constant default initargs
;;;   supplied initargs
;;;   slot filling initargs
(define-constructor-code-type simple-slots
	(class name arglist supplied-initarg-names supplied-initargs)
  (declare (ignore name))
  (let ((raw-allocator (raw-instance-allocator class))
	(slots-fetcher (slots-fetcher class)))
    `(function
       (lambda (class .wrapper. defaults init shared)
	 (when (and (null (non-pcl-initialize-instance-methods-p init))
		    (null (non-pcl-shared-initialize-methods-p shared)))
	   (multiple-value-bind (.constants. .supplied-initarg-positions.)
	       (simple-slots-generator-internal class
						defaults
						',supplied-initarg-names
						',supplied-initargs)
	     (when .constants.
	       (function
		 (sb-kernel:instance-lambda ,arglist
		   (declare #.*optimize-speed*)
		   (let* ((.instance. (,raw-allocator .wrapper. .constants.))
			  (.slots. (,slots-fetcher .instance.))
			  (.positions. .supplied-initarg-positions.))
		     .positions.

		     ,@(gathering1 (collecting)
			 (doplist (initarg value) supplied-initargs
			   (unless (constantp value)
			     (gather1
			       `(let ((.value. ,value))
				  (dolist (.p. (pop .positions.))
				    (setf (%instance-ref .slots. .p.)
					  .value.)))))))

		     .instance.))))))))))

(defun simple-slots-generator-internal
       (class defaults supplied-initarg-names supplied-initargs)
  (flet ((bail-out () (return-from simple-slots-generator-internal nil)))
    (let* ((constants (compute-constant-vector class))
	   (layout (wrapper-instance-slots-layout (class-wrapper class)))
	   (initarg-positions
	     (compute-initarg-positions class
					(append supplied-initarg-names
						(mapcar #'car defaults))))
	   (supplied-initarg-positions ())
	   (used-positions ()))

      ;; Go through each of the supplied initargs for three reasons.
      ;;
      ;;   - If it fills a class slot, bail out.
      ;;   - If its a constant form, fill the constant vector.
      ;;   - Otherwise remember the positions, no two initargs
      ;;     will try to fill the same position, since compute
      ;;     initarg positions already took care of that, but
      ;;     we do need to know what initforms will and won't
      ;;     be needed.
      (doplist (initarg val) supplied-initargs
	(let ((positions (cdr (assq initarg initarg-positions))))
	  (cond ((memq :class positions) (bail-out))
		((constantp val)
		 (setq val (eval val))
		 (dolist (pos positions)
		   (setf (svref constants pos) val)))
		(t
		 (push positions supplied-initarg-positions)))
	  (setq used-positions (append used-positions positions))))

      ;; Go through each of the default initargs for three reasons.
      ;;
      ;;   - If it isn't a constant form, bail out.
      ;;   - If it fills a class slot, bail out.
      ;;   - If it is a constant, and it does fill a slot, put that
      ;;     into the constant vector.
      (dolist (default defaults)
	(let* ((name (car default))
	       (form (caddr default))
	       (value ())
	       (positions (cdr (assq name initarg-positions))))
	  (unless (memq name supplied-initarg-names)
	    (cond ((memq :class positions) (bail-out))
		  ((not (constantp form))
		   (bail-out))
		  (t
		   (setq value (eval form))
		   (dolist (pos positions)
		     (setf (svref constants pos) value)))))))

      ;; Go through each of the slot initforms:
      ;;
      ;;    - If its position has already been filled, do nothing.
      ;;      The initfn won't need to be called, and the slot won't
      ;;      need to be touched, we are OK.
      ;;    - If it has a non-constant initform, bail-out. This
      ;;      case doesn't handle those.
      ;;    - If it has a constant or unsupplied initform we don't
      ;;      really need to do anything, the value is in the
      ;;      constants vector.
      (dolist (slotd (class-slots class))
	(let* ((alloc (slot-definition-allocation slotd))
	       (name (slot-definition-name slotd))
	       (form (slot-definition-initform slotd))
	       (initfn (slot-definition-initfunction slotd))
	       (position (position name layout)))
	  (cond ((neq alloc :instance)
		 (unless (null initfn)
		   (bail-out)))
		((member position used-positions))
		((or (constantp form)
		     (null initfn)))
		(t
		 (bail-out)))))

      (values constants (nreverse supplied-initarg-positions)))))


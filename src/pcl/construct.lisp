;;;; This file defines MAKE-INSTANCE optimization mechanisms.
;;;;
;;;; KLUDGE: I removed the old DEFCONSTRUCTOR, MAKE-CONSTRUCTOR, and
;;;; LOAD-CONSTRUCTOR families of definitions in sbcl-0.pre7.99, since
;;;; it was clear from a few minutes with egrep that they were dead
;;;; code, but I suspect more dead code remains in this file. (Maybe
;;;; it's all dead?) -- WHN 2001-12-26

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
						;is optional. The old
					        ;DEFCONSTRUCTOR macro made
						;named constructors, but
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

(defmethod describe-object ((constructor constructor) stream)
  (format stream
	  "~S is a constructor for the class ~S.~%~
	    The current code type is ~S.~%~
	    Other possible code types are ~S."
	  constructor (constructor-class constructor)
	  (constructor-code-type constructor)
          (let ((collect nil))
	    (doplist (key val) (constructor-code-generators constructor)
              (push key collect))
            (nreverse collect))))

;;;; Here is the actual smarts for making the code generators and then
;;;; trying each generator to get constructor code. This extensible
;;;; mechanism allows new kinds of constructor code types to be added.
;;;; A programmer defining a specialization of the constructor class
;;;; can use this mechanism to define new code types.
;;;;
;;;; original PCL comment from before dead DEFCONSTRUCTOR was deleted:
;;;;   The function defined by define-constructor-code-type will receive
;;;;   the class object, and the 4 original arguments to DEFCONSTRUCTOR.
;;;;   It can return a constructor code generator, or return NIL if this
;;;;   type of code is determined to not be appropriate after looking at
;;;;   the DEFCONSTRUCTOR arguments.
;;;;
;;;; original PCL comment from before dead COMPUTE-CONSTRUCTOR-CODE
;;;; was deleted:
;;;;    When compute-constructor-code is called, it first performs
;;;;    basic checks to make sure that the basic assumptions common to
;;;;    all the code types are valid. (For details see method
;;;;    definition). If any of the tests fail, the fallback
;;;;    constructor code type is used. If none of the tests fail, the
;;;;    constructor code generators are called in order. They receive
;;;;    5 arguments:
;;;;
;;;;   CLASS	the class the constructor is making instances of
;;;;   WRAPPER      that class's wrapper
;;;;   DEFAULTS     the result of calling class-default-initargs on class
;;;;   INITIALIZE   the applicable methods on initialize-instance
;;;;   SHARED       the applicable methosd on shared-initialize
;;;;
;;;; The first code generator to return code is used. The code
;;;; generators are called in reverse order of definition, so
;;;; DEFINE-CONSTRUCTOR-CODE-TYPE forms which define better code
;;;; should appear after ones that define less good code. The fallback
;;;; code type appears first. Note that redefining a code type does
;;;; not change its position in the list. To do that, define a new
;;;; type at the end with the behavior.

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

;;;; helper functions and utilities that are shared by all of the code
;;;; types

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
	       (push (cons name +slot-unbound+) constants))
	      ((constantp initform)
	       (push (cons name (eval initform)) constants)
	       (when (eq flag ':unsupplied) (setq flag ':constants)))
	      (t
	       (push (cons name +slot-unbound+) constants)
	       (setq flag t)))))
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
           (loop for slot-name in layout
                 for position from 0
                 collect (cons slot-name position)))
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
    (let (collect)
      (dolist (initarg initarg-names)
	(let ((positions (let (collect)
			   (dolist (slot-entry slot-initargs)
			     (when (eq (car slot-entry) initarg)
			       (push (cadr slot-entry) collect)))
                           (nreverse collect))))
	  (when positions
	    (push (cons initarg positions) collect))))
      (nreverse collect))))

;;; The FALLBACK case allows anything. This always works, and always
;;; appears as the last of the generators for a constructor. It does a
;;; full call to make-instance.
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
	     ,@(let (collect)
                 (loop for tail on supplied-initargs by #'cddr
                       do (push `',(car tail) collect)
                          (push (cadr tail) collect))
                 (nreverse collect))))))))

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
                         (setf (clos-slots-ref .slots. pos) val))))

		   ,@(let (collect)
                       (doplist (initarg value) supplied-initargs
			 (unless (constantp value)
			   (push `(let ((.value. ,value))
                                   (push .value. .initargs.)
                                   (push ',initarg .initargs.)
                                   (dolist (.p. (pop .positions.))
                                     (setf (clos-slots-ref .slots. .p.)
                                           .value.)))
                                 collect)))
                       (nreverse collect))

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
			 (setf (clos-slots-ref .slots. pos) val))))

		   ,@(let (collect)
		       (doplist (initarg value) supplied-initargs
			 (unless (constantp value)
			   (push
			     `(let ((.value. ,value))
				(dolist (.p. (pop .positions.))
				  (setf (clos-slots-ref .slots. .p.)
					.value.)))
                             collect)))
                       (nreverse collect))

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

		     ,@(let (collect)
			 (doplist (initarg value) supplied-initargs
			   (unless (constantp value)
			     (push
			       `(let ((.value. ,value))
				  (dolist (.p. (pop .positions.))
				    (setf (clos-slots-ref .slots. .p.)
					  .value.)))
                               collect)))
                         (nreverse collect))

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

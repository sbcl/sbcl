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

;;;; ANSI CL condition for unbound slots

(define-condition unbound-slot (cell-error)
  ((instance :reader unbound-slot-instance :initarg :instance)
   (slot :reader unbound-slot-slot :initarg :slot))
  (:report (lambda(condition stream)
	     (format stream "The slot ~S is unbound in the object ~S."
		     (unbound-slot-slot condition)
		     (unbound-slot-instance condition)))))

(defmethod wrapper-fetcher ((class standard-class))
  'std-instance-wrapper)

(defmethod slots-fetcher ((class standard-class))
  'std-instance-slots)

(defmethod raw-instance-allocator ((class standard-class))
  'allocate-standard-instance)

;;; These four functions work on std-instances and fsc-instances. These are
;;; instances for which it is possible to change the wrapper and the slots.
;;;
;;; For these kinds of instances, most specified methods from the instance
;;; structure protocol are promoted to the implementation-specific class
;;; std-class. Many of these methods call these four functions.

(defun set-wrapper (inst new)
  (cond ((std-instance-p inst)
	 (setf (std-instance-wrapper inst) new))
	((fsc-instance-p inst)
	 (setf (fsc-instance-wrapper inst) new))
	(t
	 (error "unrecognized instance type"))))

(defun swap-wrappers-and-slots (i1 i2)
  (sb-sys:without-interrupts
   (cond ((std-instance-p i1)
	  (let ((w1 (std-instance-wrapper i1))
		(s1 (std-instance-slots i1)))
	    (setf (std-instance-wrapper i1) (std-instance-wrapper i2))
	    (setf (std-instance-slots i1) (std-instance-slots i2))
	    (setf (std-instance-wrapper i2) w1)
	    (setf (std-instance-slots i2) s1)))
	 ((fsc-instance-p i1)
	  (let ((w1 (fsc-instance-wrapper i1))
		(s1 (fsc-instance-slots i1)))
	    (setf (fsc-instance-wrapper i1) (fsc-instance-wrapper i2))
	    (setf (fsc-instance-slots i1) (fsc-instance-slots i2))
	    (setf (fsc-instance-wrapper i2) w1)
	    (setf (fsc-instance-slots i2) s1)))
	 (t
	  (error "unrecognized instance type")))))

(defun get-class-slot-value-1 (object wrapper slot-name)
  (let ((entry (assoc slot-name (wrapper-class-slots wrapper))))
    (if (null entry)
	(slot-missing (wrapper-class wrapper) object slot-name 'slot-value)
	(if (eq (cdr entry) +slot-unbound+)
	    (slot-unbound (wrapper-class wrapper) object slot-name)
	    (cdr entry)))))

(defun set-class-slot-value-1 (new-value object wrapper slot-name)
  (let ((entry (assoc slot-name (wrapper-class-slots wrapper))))
    (if (null entry)
	(slot-missing (wrapper-class wrapper)
		      object
		      slot-name
		      'setf
		      new-value)
	(setf (cdr entry) new-value))))

(defmethod class-slot-value ((class std-class) slot-name)
  (let ((wrapper (class-wrapper class))
	(prototype (class-prototype class)))
    (get-class-slot-value-1 prototype wrapper slot-name)))

(defmethod (setf class-slot-value) (nv (class std-class) slot-name)
  (let ((wrapper (class-wrapper class))
	(prototype (class-prototype class)))
    (set-class-slot-value-1 nv prototype wrapper slot-name)))

(defun find-slot-definition (class slot-name)
  (dolist (slot (class-slots class) nil)
    (when (eql slot-name (slot-definition-name slot))
      (return slot))))

(defun slot-value (object slot-name)
  (let* ((class (class-of object))
	 (slot-definition (find-slot-definition class slot-name)))
    (if (null slot-definition)
	(slot-missing class object slot-name 'slot-value)
	(slot-value-using-class class object slot-definition))))

(setf (gdefinition 'slot-value-normal) #'slot-value)

(define-compiler-macro slot-value (object-form slot-name-form)
  (if (and (constantp slot-name-form)
	   (let ((slot-name (eval slot-name-form)))
	     (and (symbolp slot-name) (symbol-package slot-name))))
      `(accessor-slot-value ,object-form ,slot-name-form)
      `(slot-value-normal ,object-form ,slot-name-form)))

(defun set-slot-value (object slot-name new-value)
  (let* ((class (class-of object))
	 (slot-definition (find-slot-definition class slot-name)))
    (if (null slot-definition)
	(slot-missing class object slot-name 'setf new-value)
	(setf (slot-value-using-class class object slot-definition)
	      new-value))))

(setf (gdefinition 'set-slot-value-normal) #'set-slot-value)

(define-compiler-macro set-slot-value (object-form
				       slot-name-form
				       new-value-form)
  (if (and (constantp slot-name-form)
	   (let ((slot-name (eval slot-name-form)))
	     (and (symbolp slot-name) (symbol-package slot-name))))
      `(accessor-set-slot-value ,object-form ,slot-name-form ,new-value-form)
      `(set-slot-value-normal ,object-form ,slot-name-form ,new-value-form)))

(defconstant +optimize-slot-boundp+ nil)

(defun slot-boundp (object slot-name)
  (let* ((class (class-of object))
	 (slot-definition (find-slot-definition class slot-name)))
    (if (null slot-definition)
	(slot-missing class object slot-name 'slot-boundp)
	(slot-boundp-using-class class object slot-definition))))

(setf (gdefinition 'slot-boundp-normal) #'slot-boundp)

(define-compiler-macro slot-boundp (object-form slot-name-form)
  (if (and (constantp slot-name-form)
	   (let ((slot-name (eval slot-name-form)))
	     (and (symbolp slot-name) (symbol-package slot-name))))
      `(accessor-slot-boundp ,object-form ,slot-name-form)
      `(slot-boundp-normal ,object-form ,slot-name-form)))

(defun slot-makunbound (object slot-name)
  (let* ((class (class-of object))
	 (slot-definition (find-slot-definition class slot-name)))
    (if (null slot-definition)
	(slot-missing class object slot-name 'slot-makunbound)
	(slot-makunbound-using-class class object slot-definition))))

(defun slot-exists-p (object slot-name)
  (let ((class (class-of object)))
    (not (null (find-slot-definition class slot-name)))))

;;; This isn't documented, but is used within PCL in a number of print
;;; object methods. (See NAMED-OBJECT-PRINT-FUNCTION.)
(defun slot-value-or-default (object slot-name &optional (default "unbound"))
  (if (slot-boundp object slot-name)
      (slot-value object slot-name)
      default))

(defun standard-instance-access (instance location)
  (instance-ref (std-instance-slots instance) location))

(defun funcallable-standard-instance-access (instance location)
  (instance-ref (fsc-instance-slots instance) location))

(defmethod slot-value-using-class ((class std-class)
				   (object std-object)
				   (slotd standard-effective-slot-definition))
  (let* ((location (slot-definition-location slotd))
	 (value (typecase location
		  (fixnum
		   (cond ((std-instance-p object)
			  ;; FIXME: EQ T (WRAPPER-STATE ..) is better done
			  ;; through INVALID-WRAPPER-P (here and below).
			  (unless (eq t (wrapper-state (std-instance-wrapper
							object)))
			    (check-wrapper-validity object))
			  (instance-ref (std-instance-slots object) location))
			 ((fsc-instance-p object)
			  (unless (eq t (wrapper-state (fsc-instance-wrapper
							object)))
			    (check-wrapper-validity object))
			  (instance-ref (fsc-instance-slots object) location))
			 (t (error "unrecognized instance type"))))
		  (cons
		   (cdr location))
		  (t
		   (error "~@<The slot ~S has neither :INSTANCE nor :CLASS ~
                           allocation, so it can't be read by the default ~
                           ~S method.~@:>"
			  slotd 'slot-value-using-class)))))
    (if (eq value +slot-unbound+)
	(slot-unbound class object (slot-definition-name slotd))
	value)))

(defmethod (setf slot-value-using-class)
	   (new-value (class std-class)
		      (object std-object)
		      (slotd standard-effective-slot-definition))
  (let ((location (slot-definition-location slotd)))
    (typecase location
      (fixnum
       (cond ((std-instance-p object)
	      (unless (eq t (wrapper-state (std-instance-wrapper object)))
		(check-wrapper-validity object))
               (setf (instance-ref (std-instance-slots object) location)
                       new-value))
	     ((fsc-instance-p object)
	      (unless (eq t (wrapper-state (fsc-instance-wrapper object)))
		(check-wrapper-validity object))
               (setf (instance-ref (fsc-instance-slots object) location)
                       new-value))
	     (t (error "unrecognized instance type"))))
      (cons
       (setf (cdr location) new-value))
      (t
       (error "~@<The slot ~S has neither :INSTANCE nor :CLASS allocation, ~
		   so it can't be written by the default ~S method.~:@>"
	      slotd '(setf slot-value-using-class))))))

(defmethod slot-boundp-using-class
	   ((class std-class)
	    (object std-object)
	    (slotd standard-effective-slot-definition))
  (let* ((location (slot-definition-location slotd))
	 (value (typecase location
		  (fixnum
		   (cond ((std-instance-p object)
			  (unless (eq t (wrapper-state (std-instance-wrapper
							object)))
			    (check-wrapper-validity object))
			  (instance-ref (std-instance-slots object) location))
			 ((fsc-instance-p object)
			  (unless (eq t (wrapper-state (fsc-instance-wrapper
							object)))
			    (check-wrapper-validity object))
			  (instance-ref (fsc-instance-slots object) location))
			 (t (error "unrecognized instance type"))))
		  (cons
		   (cdr location))
		  (t
		   (error "~@<The slot ~S has neither :INSTANCE nor :CLASS ~
                           allocation, so it can't be read by the default ~S ~
                           method.~@:>"
			  slotd 'slot-boundp-using-class)))))
    (not (eq value +slot-unbound+))))

(defmethod slot-makunbound-using-class
	   ((class std-class)
	    (object std-object)
	    (slotd standard-effective-slot-definition))
  (let ((location (slot-definition-location slotd)))
    (typecase location
      (fixnum
       (cond ((std-instance-p object)
	      (unless (eq t (wrapper-state (std-instance-wrapper object)))
		(check-wrapper-validity object))
               (setf (instance-ref (std-instance-slots object) location)
                       +slot-unbound+))
	     ((fsc-instance-p object)
	      (unless (eq t (wrapper-state (fsc-instance-wrapper object)))
		(check-wrapper-validity object))
               (setf (instance-ref (fsc-instance-slots object) location)
                       +slot-unbound+))
	     (t (error "unrecognized instance type"))))
      (cons
       (setf (cdr location) +slot-unbound+))
      (t
       (error "~@<The slot ~S has neither :INSTANCE nor :CLASS allocation, ~
	       so it can't be written by the default ~S method.~@:>"
	      slotd 'slot-makunbound-using-class))))
  nil)

(defmethod slot-value-using-class
    ((class structure-class)
     (object structure-object)
     (slotd structure-effective-slot-definition))
  (let* ((function (slot-definition-internal-reader-function slotd))
	 (value (funcall function object)))
    (declare (type function function))
    (if (eq value +slot-unbound+)
	(slot-unbound class object (slot-definition-name slotd))
	value)))

(defmethod (setf slot-value-using-class)
    (new-value (class structure-class)
	       (object structure-object)
	       (slotd structure-effective-slot-definition))
  (let ((function (slot-definition-internal-writer-function slotd)))
    (declare (type function function))
    (funcall function new-value object)))

(defmethod slot-boundp-using-class
	   ((class structure-class)
	    (object structure-object)
	    (slotd structure-effective-slot-definition))
  t)

(defmethod slot-makunbound-using-class
	   ((class structure-class)
	    (object structure-object)
	    (slotd structure-effective-slot-definition))
  (error "Structure slots can't be unbound."))

(defmethod slot-missing
	   ((class t) instance slot-name operation &optional new-value)
  (error "~@<When attempting to ~A, the slot ~S is missing from the ~
          object ~S.~@:>"
	 (ecase operation
	   (slot-value "read the slot's value (slot-value)")
	   (setf (format nil
			 "set the slot's value to ~S (SETF of SLOT-VALUE)"
			 new-value))
	   (slot-boundp "test to see whether slot is bound (SLOT-BOUNDP)")
	   (slot-makunbound "make the slot unbound (SLOT-MAKUNBOUND)"))
	 slot-name
	 instance))

(defmethod slot-unbound ((class t) instance slot-name)
  (error 'unbound-slot :slot slot-name :instance instance))

(defun slot-unbound-internal (instance position)
  (slot-unbound (class-of instance) instance
		(etypecase position
		  (fixnum
		   (nth position
			(wrapper-instance-slots-layout (wrapper-of instance))))
		  (cons
		   (car position)))))

(defmethod allocate-instance ((class standard-class) &rest initargs)
  (declare (ignore initargs))
  (unless (class-finalized-p class) (finalize-inheritance class))
  (allocate-standard-instance (class-wrapper class)))

(defmethod allocate-instance ((class structure-class) &rest initargs)
  (declare (ignore initargs))
  (let ((constructor (class-defstruct-constructor class)))
    (if constructor
	(funcall constructor)
	(error "can't allocate an instance of class ~S" (class-name class)))))

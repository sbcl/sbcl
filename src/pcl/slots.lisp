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
  ((instance :reader unbound-slot-instance :initarg :instance))
  (:report (lambda (condition stream)
             (format stream "The slot ~S is unbound in the object ~S."
                     (cell-error-name condition)
                     (unbound-slot-instance condition)))))

(defmethod wrapper-fetcher ((class standard-class))
  'std-instance-wrapper)

(defmethod slots-fetcher ((class standard-class))
  'std-instance-slots)

(defmethod raw-instance-allocator ((class standard-class))
  'allocate-standard-instance)

;;; These three functions work on std-instances and fsc-instances. These are
;;; instances for which it is possible to change the wrapper and the slots.
;;;
;;; For these kinds of instances, most specified methods from the instance
;;; structure protocol are promoted to the implementation-specific class
;;; std-class. Many of these methods call these four functions.

(defun %swap-wrappers-and-slots (i1 i2)
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
         (error "unrecognized instance type"))))

;;;; STANDARD-INSTANCE-ACCESS

(declaim (inline standard-instance-access (setf standard-instance-access)
                 funcallable-standard-instance-access
                 (setf funcallable-standard-instance-access)))

(defun standard-instance-access (instance location)
  (clos-slots-ref (std-instance-slots instance) location))

(defun (setf standard-instance-access) (new-value instance location)
  (setf (clos-slots-ref (std-instance-slots instance) location) new-value))

(defun funcallable-standard-instance-access (instance location)
  (clos-slots-ref (fsc-instance-slots instance) location))

(defun (setf funcallable-standard-instance-access) (new-value instance location)
  (setf (clos-slots-ref (fsc-instance-slots instance) location) new-value))

;;;; SLOT-VALUE, (SETF SLOT-VALUE), SLOT-BOUNDP, SLOT-MAKUNBOUND

(declaim (ftype (sfunction (t symbol) t) slot-value))
(defun slot-value (object slot-name)
  (let* ((wrapper (valid-wrapper-of object))
         (cell (find-slot-cell wrapper slot-name))
         (location (car cell))
         (value
          (cond ((fixnump location)
                 (if (std-instance-p object)
                     (standard-instance-access object location)
                     (funcallable-standard-instance-access object location)))
                ((consp location)
                 (cdr location))
                ((not cell)
                 (return-from slot-value
                   (values (slot-missing (wrapper-class* wrapper) object slot-name
                                         'slot-value))))
                ((not location)
                 (return-from slot-value
                   (slot-value-using-class (wrapper-class* wrapper) object (cddr cell))))
                (t
                 (bug "Bogus slot cell in SLOT-VALUE: ~S" cell)))))
    (if (eq +slot-unbound+ value)
        (slot-unbound (wrapper-class* wrapper) object slot-name)
        value)))

;;; This is used during the PCL build, but gets replaced by a deftransform
;;; in fixup.lisp.
(define-compiler-macro slot-value (&whole form object slot-name
                                   &environment env)
  (if (and (constantp slot-name env)
           (interned-symbol-p (constant-form-value slot-name env)))
      `(accessor-slot-value ,object ,slot-name)
      form))

(defun set-slot-value (object slot-name new-value)
  (let* ((wrapper (valid-wrapper-of object))
         (cell (find-slot-cell wrapper slot-name))
         (location (car cell))
         (type-check-function (cadr cell)))
    (when type-check-function
      (funcall (the function type-check-function) new-value))
    (cond ((fixnump location)
           (if (std-instance-p object)
               (setf (standard-instance-access object location) new-value)
               (setf (funcallable-standard-instance-access object location)
                     new-value)))
          ((consp location)
           (setf (cdr location) new-value))
          ((not cell)
           (slot-missing (wrapper-class* wrapper) object slot-name 'setf new-value))
          ((not location)
           (setf (slot-value-using-class (wrapper-class* wrapper) object (cddr cell))
                 new-value))
          (t
           (bug "Bogus slot-cell in SET-SLOT-VALUE: ~S" cell))))
  new-value)

;;; A version of SET-SLOT-VALUE for use in safe code, where we want to
;;; check types when writing to slots:
;;;   * Doesn't have an optimizing compiler-macro
;;;   * Isn't special-cased in WALK-METHOD-LAMBDA
(defun safe-set-slot-value (object slot-name new-value)
  (set-slot-value object slot-name new-value))

;;; This is used during the PCL build, but gets replaced by a deftransform
;;; in fixup.lisp.
(define-compiler-macro set-slot-value (&whole form object slot-name new-value
                                      &environment env)
  (if (and (constantp slot-name env)
           (interned-symbol-p (constant-form-value slot-name env))
           ;; We can't use the ACCESSOR-SET-SLOT-VALUE path in safe
           ;; code, since it'll use the global automatically generated
           ;; accessor, which won't do typechecking. (SLOT-OBJECT
           ;; won't have been compiled with SAFETY 3, so SAFE-P will
           ;; be NIL in MAKE-STD-WRITER-METHOD-FUNCTION).
           (not (safe-code-p env)))
      `(accessor-set-slot-value ,object ,slot-name ,new-value)
      form))

(defun slot-boundp (object slot-name)
  (let* ((wrapper (valid-wrapper-of object))
         (cell (find-slot-cell wrapper slot-name))
         (location (car cell))
         (value
          (cond ((fixnump location)
                 (if (std-instance-p object)
                     (standard-instance-access object location)
                     (funcallable-standard-instance-access object location)))
                ((consp location)
                 (cdr location))
                ((not cell)
                 (return-from slot-boundp
                   (and (slot-missing (wrapper-class* wrapper) object slot-name
                                      'slot-boundp)
                        t)))
                ((not location)
                 (return-from slot-boundp
                   (slot-boundp-using-class (wrapper-class* wrapper) object (cddr cell))))
                (t
                 (bug "Bogus slot cell in SLOT-VALUE: ~S" cell)))))
    (not (eq +slot-unbound+ value))))

(define-compiler-macro slot-boundp (&whole form object slot-name
                                    &environment env)
  (if (and (constantp slot-name env)
           (interned-symbol-p (constant-form-value slot-name env)))
      `(accessor-slot-boundp ,object ,slot-name)
      form))

(defun slot-makunbound (object slot-name)
  (let* ((wrapper (valid-wrapper-of object))
         (cell (find-slot-cell wrapper slot-name))
         (location (car cell)))
    (cond ((fixnump location)
           (if (std-instance-p object)
               (setf (standard-instance-access object location) +slot-unbound+)
               (setf (funcallable-standard-instance-access object location)
                     +slot-unbound+)))
          ((consp location)
           (setf (cdr location) +slot-unbound+))
          ((not cell)
           (slot-missing (wrapper-class* wrapper) object slot-name 'slot-makunbound))
          ((not location)
           (slot-makunbound-using-class (wrapper-class* wrapper) object (cddr cell)))
          (t
           (bug "Bogus slot-cell in SLOT-MAKUNBOUND: ~S" cell))))
  object)

(defun slot-exists-p (object slot-name)
  (let ((class (class-of object)))
    (not (null (find-slot-definition class slot-name)))))

(defvar *unbound-slot-value-marker* (make-unprintable-object "unbound slot"))

;;; This isn't documented, but is used within PCL in a number of print
;;; object methods. (See NAMED-OBJECT-PRINT-FUNCTION.)
(defun slot-value-or-default (object slot-name &optional
                              (default *unbound-slot-value-marker*))
  (if (slot-boundp object slot-name)
      (slot-value object slot-name)
      default))

(defmethod slot-value-using-class ((class std-class)
                                   (object standard-object)
                                   (slotd standard-effective-slot-definition))
  ;; FIXME: Do we need this? SLOT-VALUE checks for obsolete
  ;; instances. Are users allowed to call this directly?
  (check-obsolete-instance object)
  (let* ((location (slot-definition-location slotd))
         (value
          (typecase location
            (fixnum
             (cond ((std-instance-p object)
                    (clos-slots-ref (std-instance-slots object)
                                    location))
                   ((fsc-instance-p object)
                    (clos-slots-ref (fsc-instance-slots object)
                                    location))
                   (t (bug "unrecognized instance type in ~S"
                           'slot-value-using-class))))
            (cons
             (cdr location))
            (t
             (instance-structure-protocol-error slotd
                                                'slot-value-using-class)))))
    (if (eq value +slot-unbound+)
        (values (slot-unbound class object (slot-definition-name slotd)))
        value)))

(defmethod (setf slot-value-using-class)
           (new-value (class std-class)
                      (object standard-object)
                      (slotd standard-effective-slot-definition))
  ;; FIXME: Do we need this? SET-SLOT-VALUE checks for obsolete
  ;; instances. Are users allowed to call this directly?
  (check-obsolete-instance object)
  (let ((location (slot-definition-location slotd))
        (type-check-function
         (when (safe-p class)
           (slot-definition-type-check-function slotd))))
    (flet ((check (new-value)
             (when type-check-function
               (funcall (the function type-check-function) new-value))
             new-value))
      (typecase location
        (fixnum
         (cond ((std-instance-p object)
                (setf (clos-slots-ref (std-instance-slots object) location)
                      (check new-value)))
               ((fsc-instance-p object)
                (setf (clos-slots-ref (fsc-instance-slots object) location)
                      (check new-value)))
                (t (bug "unrecognized instance type in ~S"
                        '(setf slot-value-using-class)))))
        (cons
         (setf (cdr location) (check new-value)))
        (t
         (instance-structure-protocol-error
          slotd '(setf slot-value-using-class)))))))

(defmethod slot-boundp-using-class
           ((class std-class)
            (object standard-object)
            (slotd standard-effective-slot-definition))
  ;; FIXME: Do we need this? SLOT-BOUNDP checks for obsolete
  ;; instances. Are users allowed to call this directly?
  (check-obsolete-instance object)
  (let* ((location (slot-definition-location slotd))
         (value
          (typecase location
            (fixnum
             (cond ((std-instance-p object)
                          (clos-slots-ref (std-instance-slots object)
                                          location))
                   ((fsc-instance-p object)
                    (clos-slots-ref (fsc-instance-slots object)
                                    location))
                   (t (bug "unrecognized instance type in ~S"
                           'slot-boundp-using-class))))
            (cons
             (cdr location))
            (t
             (instance-structure-protocol-error slotd
                                                'slot-boundp-using-class)))))
    (not (eq value +slot-unbound+))))

(defmethod slot-makunbound-using-class
           ((class std-class)
            (object standard-object)
            (slotd standard-effective-slot-definition))
  (check-obsolete-instance object)
  (let ((location (slot-definition-location slotd)))
    (typecase location
      (fixnum
       (cond ((std-instance-p object)
              (setf (clos-slots-ref (std-instance-slots object) location)
                    +slot-unbound+))
             ((fsc-instance-p object)
              (setf (clos-slots-ref (fsc-instance-slots object) location)
                    +slot-unbound+))
             (t (bug "unrecognized instance type in ~S"
                     'slot-makunbound-using-class))))
      (cons
       (setf (cdr location) +slot-unbound+))
      (t
       (instance-structure-protocol-error slotd
                                          'slot-makunbound-using-class))))
  object)

(defmethod slot-value-using-class
    ((class condition-class)
     (object condition)
     (slotd condition-effective-slot-definition))
  (let ((fun (slot-definition-reader-function slotd)))
    (declare (type function fun))
    (funcall fun object)))

(defmethod (setf slot-value-using-class)
    (new-value
     (class condition-class)
     (object condition)
     (slotd condition-effective-slot-definition))
  (let ((fun (slot-definition-writer-function slotd)))
    (declare (type function fun))
    (funcall fun new-value object)))

(defmethod slot-boundp-using-class
    ((class condition-class)
     (object condition)
     (slotd condition-effective-slot-definition))
  (let ((fun (slot-definition-boundp-function slotd)))
    (declare (type function fun))
    (funcall fun object)))

(defmethod slot-makunbound-using-class ((class condition-class) object slot)
  (error "attempt to unbind slot ~S in condition object ~S."
         slot object))

(defmethod slot-value-using-class
    ((class structure-class)
     (object structure-object)
     (slotd structure-effective-slot-definition))
  (let* ((function (slot-definition-internal-reader-function slotd))
         (value (funcall function object)))
    (declare (type function function))
    ;; FIXME: Is this really necessary? Structure slots should surely
    ;; never be unbound!
    (if (eq value +slot-unbound+)
        (values (slot-unbound class object (slot-definition-name slotd)))
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
  (restart-case
      (error 'unbound-slot :name slot-name :instance instance)
    (use-value (v)
      :report "Return a value as the slot-value."
      :interactive read-evaluated-form
      v)
    (store-value (v)
      :report "Store and return a value as the slot-value."
      :interactive read-evaluated-form
      (setf (slot-value instance slot-name) v))))

(defun slot-unbound-internal (instance position)
  (values
   (slot-unbound
    (class-of instance)
    instance
    (etypecase position
      (fixnum
       (nth position (wrapper-instance-slots-layout (wrapper-of instance))))
      (cons
       (car position))))))

;;; FIXME: AMOP says that allocate-instance imples finalize-inheritance
;;; if the class is not yet finalized, but we don't seem to be taking
;;; care of this for non-standard-classes.x
(defmethod allocate-instance ((class standard-class) &rest initargs)
  (declare (ignore initargs))
  (unless (class-finalized-p class)
    (finalize-inheritance class))
  (allocate-standard-instance (class-wrapper class)))

(defmethod allocate-instance ((class structure-class) &rest initargs)
  (declare (ignore initargs))
  (let ((constructor (class-defstruct-constructor class)))
    (if constructor
        (funcall constructor)
        (error "Don't know how to allocate ~S" class))))

;;; FIXME: It would be nicer to have allocate-instance return
;;; uninitialized objects for conditions as well.
(defmethod allocate-instance ((class condition-class) &rest initargs)
  (declare (ignore initargs))
  (make-condition (class-name class)))

(defmethod allocate-instance ((class built-in-class) &rest initargs)
  (declare (ignore initargs))
  (error "Cannot allocate an instance of ~S." class)) ; So sayeth AMOP

;;; AMOP says that CLASS-SLOTS signals an error for unfinalized classes.
(defmethod class-slots :before ((class slot-class))
  (unless (class-finalized-p class)
    (error 'simple-reference-error
           :format-control "~S called on ~S, which is not yet finalized."
           :format-arguments (list 'class-slots class)
           :references (list '(:amop :generic-function class-slots)))))

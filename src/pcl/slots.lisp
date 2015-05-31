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
             (handler-case
                 (format stream "~@<The slot ~/sb-ext:print-symbol-with-prefix/ ~
                         is unbound in the object ~A.~@:>"
                         (cell-error-name condition)
                         (unbound-slot-instance condition))
               (serious-condition ()
                 ;; In case of an error try again avoiding custom PRINT-OBJECT's.
                 (format stream "~&Error during printing.~%~@<The slot ~
                         ~/sb-ext:print-symbol-with-prefix/ ~
                         is unbound in an instance of ~
                         ~/sb-ext:print-symbol-with-prefix/.~@:>"
                         (cell-error-name condition)
                         (type-of (unbound-slot-instance condition))))))))

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

(declaim (inline standard-instance-access
                 (setf standard-instance-access)
                 (cas stadard-instance-access)
                 funcallable-standard-instance-access
                 (setf funcallable-standard-instance-access)
                 (cas funcallable-standard-instance-access)))

(defun standard-instance-access (instance location)
  (clos-slots-ref (std-instance-slots instance) location))

(defun (setf standard-instance-access) (new-value instance location)
  (setf (clos-slots-ref (std-instance-slots instance) location) new-value))

(defun (cas standard-instance-access) (old-value new-value instance location)
  ;; FIXME: Maybe get rid of CLOS-SLOTS-REF entirely?
  (cas (svref (std-instance-slots instance) location) old-value new-value))

(defun funcallable-standard-instance-access (instance location)
  (clos-slots-ref (fsc-instance-slots instance) location))

(defun (setf funcallable-standard-instance-access) (new-value instance location)
  (setf (clos-slots-ref (fsc-instance-slots instance) location) new-value))

(defun (cas funcallable-standard-instance-access) (old-value new-value instance location)
  ;; FIXME: Maybe get rid of CLOS-SLOTS-REF entirely?
  (cas (svref (fsc-instance-slots instance) location) old-value new-value))

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
                ((not location)
                 (return-from slot-value
                   (if cell
                       (funcall (slot-info-reader (cdr cell)) object)
                       (values (slot-missing (wrapper-class* wrapper) object
                                             slot-name 'slot-value)))))
                ;; this next test means CONSP, but the transform that weakens
                ;; CONSP to LISTP isn't working here for some reason.
                ((listp location)
                 (cdr location))
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
         (cell (or (find-slot-cell wrapper slot-name)
                   (return-from set-slot-value
                     (progn (slot-missing (wrapper-class* wrapper)
                                          object slot-name 'setf new-value)
                            new-value))))
         (location (car cell))
         (info (cdr cell))
         (typecheck (slot-info-typecheck info)))
    (when typecheck
      (funcall typecheck new-value))
    (cond ((fixnump location)
           (if (std-instance-p object)
               (setf (standard-instance-access object location) new-value)
               (setf (funcallable-standard-instance-access object location)
                     new-value)))
          ((not location)
           (funcall (slot-info-writer info) new-value object))
          ((listp location) ; forcibly transform CONSP to LISTP
           (setf (cdr location) new-value))
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

(defun (cas slot-value) (old-value new-value object slot-name)
  (let* ((wrapper (valid-wrapper-of object))
         (cell (or (find-slot-cell wrapper slot-name)
                   (return-from slot-value
                     (values (slot-missing (wrapper-class* wrapper) object slot-name
                                           'cas (list old-value new-value))))))
         (location (car cell))
         (info (cdr cell))
         (typecheck (slot-info-typecheck info)))
    (when typecheck
      (funcall typecheck new-value))
    (let ((old (cond ((fixnump location)
                      (if (std-instance-p object)
                          (cas (standard-instance-access object location) old-value new-value)
                          (cas (funcallable-standard-instance-access object location)
                               old-value new-value)))
                     ((not location)
                      ;; FIXME: (CAS SLOT-VALUE-USING-CLASS)...
                      (error "Cannot compare-and-swap slot ~S on: ~S" slot-name object))
                     ((listp location) ; forcibly transform CONSP to LISTP
                      (cas (cdr location) old-value new-value))
                     (t
                      (bug "Bogus slot-cell in (CAS SLOT-VALUE): ~S" cell)))))
      (if (and (eq +slot-unbound+ old)
               (neq old old-value))
          (slot-unbound (wrapper-class* wrapper) object slot-name)
          old))))

(defun slot-boundp (object slot-name)
  (let* ((wrapper (valid-wrapper-of object))
         (cell (find-slot-cell wrapper slot-name))
         (location (car cell))
         (value
          (cond ((fixnump location)
                 (if (std-instance-p object)
                     (standard-instance-access object location)
                     (funcallable-standard-instance-access object location)))
                ((not location)
                 (return-from slot-boundp
                   (if cell
                       (funcall (slot-info-boundp (cdr cell)) object)
                       (and (slot-missing (wrapper-class* wrapper) object
                                          slot-name 'slot-boundp)
                            t))))
                ((listp location) ; forcibly transform CONSP to LISTP
                 (cdr location))
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
          ((not location)
           (if cell
               (let ((class (wrapper-class* wrapper)))
                 (slot-makunbound-using-class class object
                                              (find-slot-definition class slot-name)))
               (slot-missing (wrapper-class* wrapper) object slot-name
                             'slot-makunbound)))
          ((listp location) ; forcibly transform CONSP to LISTP
           (setf (cdr location) +slot-unbound+))
          (t
           (bug "Bogus slot-cell in SLOT-MAKUNBOUND: ~S" cell))))
  object)

;; Note that CLHS "encourages" implementors to base this on
;; SLOT-EXISTS-P-USING-CLASS, whereas 88-002R made no such claim,
;; however Appendix D of AMOP sketches out such an implementation.
(defun slot-exists-p (object slot-name)
  (not (null (find-slot-cell (valid-wrapper-of object) slot-name))))

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
  (let* ((info (slot-definition-info slotd))
         (location (slot-definition-location slotd))
         (typecheck (slot-info-typecheck info))
         (new-value (if typecheck
                        (funcall (the function typecheck) new-value)
                        new-value)))
    (typecase location
      (fixnum
       (cond ((std-instance-p object)
              (setf (clos-slots-ref (std-instance-slots object) location)
                    new-value))
             ((fsc-instance-p object)
              (setf (clos-slots-ref (fsc-instance-slots object) location)
                    new-value))
             (t (bug "unrecognized instance type in ~S"
                     '(setf slot-value-using-class)))))
      (cons
       (setf (cdr location) new-value))
      (t
       (instance-structure-protocol-error
        slotd '(setf slot-value-using-class))))))

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
  (let ((fun (slot-info-reader (slot-definition-info slotd))))
    (funcall fun object)))

(defmethod (setf slot-value-using-class)
    (new-value
     (class condition-class)
     (object condition)
     (slotd condition-effective-slot-definition))
  (let ((fun (slot-info-writer (slot-definition-info slotd))))
    (funcall fun new-value object)))

(defmethod slot-boundp-using-class
    ((class condition-class)
     (object condition)
     (slotd condition-effective-slot-definition))
  (let ((fun (slot-info-boundp (slot-definition-info slotd))))
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
       ;; In the vast majority of cases location corresponds to the position
       ;; in list. The only exceptions are when there are non-local slots
       ;; before the one we want.
       (let* ((slots (layout-slot-list (layout-of instance)))
              (guess (nth position slots)))
         (if (eql position (slot-definition-location guess))
             (slot-definition-name guess)
             (slot-definition-name
              (car (member position (class-slots instance) :key #'slot-definition-location))))))
      (cons
       (car position))))))

;;; FIXME: AMOP says that allocate-instance imples finalize-inheritance
;;; if the class is not yet finalized, but we don't seem to be taking
;;; care of this for non-standard-classes.
(defmethod allocate-instance ((class standard-class) &rest initargs)
  (declare (ignore initargs))
  (allocate-standard-instance
   (class-wrapper (ensure-class-finalized class))))

(defmethod allocate-instance ((class structure-class) &rest initargs)
  (declare (ignore initargs))
  (let ((constructor (class-defstruct-constructor class)))
    (if constructor
        (funcall constructor)
        (error "Don't know how to allocate ~S" class))))

(defmethod allocate-instance ((class condition-class) &rest initargs)
  (declare (ignore initargs))
  (allocate-condition (class-name class)))

(macrolet ((def (name class)
             `(defmethod ,name ((class ,class) &rest initargs)
                (declare (ignore initargs))
                (error "Cannot allocate an instance of ~S." class))))
  (def allocate-instance system-class))

;;; AMOP says that CLASS-SLOTS signals an error for unfinalized classes.
(defmethod class-slots :before ((class slot-class))
  (unless (class-finalized-p class)
    (error 'simple-reference-error
           :format-control "~S called on ~S, which is not yet finalized."
           :format-arguments (list 'class-slots class)
           :references (list '(:amop :generic-function class-slots)))))

;;;; This file defines the initialization and related protocols.

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

(defmethod make-instance ((class symbol) &rest initargs)
  (apply #'make-instance (find-class class) initargs))

(defmethod make-instance ((class class) &rest initargs)
  (let ((instance-or-nil (maybe-call-ctor class initargs)))
    (when instance-or-nil
      (return-from make-instance instance-or-nil)))
  (unless (class-finalized-p class) (finalize-inheritance class))
  (let ((class-default-initargs (class-default-initargs class)))
    (when class-default-initargs
      (setf initargs (default-initargs initargs class-default-initargs)))
    (when initargs
      (when (eq **boot-state** 'complete)
        (check-mi-initargs class initargs)))
    (let ((instance (apply #'allocate-instance class initargs)))
      (apply #'initialize-instance instance initargs)
      instance)))

(defun default-initargs (supplied-initargs class-default-initargs)
  (loop for (key nil fun) in class-default-initargs
        when (eq (getf supplied-initargs key '.not-there.) '.not-there.)
          append (list key (funcall fun)) into default-initargs
        finally
          (return (append supplied-initargs default-initargs))))

(defmethod initialize-instance ((instance slot-object) &rest initargs)
  (apply #'shared-initialize instance t initargs))

(defmethod reinitialize-instance ((instance slot-object) &rest initargs)
  ;; the ctor machinery allows us to track when memoization of
  ;; validity of initargs should be cleared.
  (check-ri-initargs instance initargs)
  (apply #'shared-initialize instance nil initargs)
  instance)

(defmethod update-instance-for-different-class
    ((previous standard-object) (current standard-object) &rest initargs)
  ;; First we must compute the newly added slots. The spec defines
  ;; newly added slots as "those local slots for which no slot of
  ;; the same name exists in the previous class."
  (let ((added-slots '())
        (current-slotds (class-slots (class-of current)))
        (previous-slot-names (mapcar #'slot-definition-name
                                     (class-slots (class-of previous)))))
    (dolist (slotd current-slotds)
      (if (and (not (memq (slot-definition-name slotd) previous-slot-names))
               (eq (slot-definition-allocation slotd) :instance))
          (push (slot-definition-name slotd) added-slots)))
    (check-initargs-1
     (class-of current) initargs
     (list (list* 'update-instance-for-different-class previous current initargs)
           (list* 'shared-initialize current added-slots initargs)))
    (apply #'shared-initialize current added-slots initargs)))

(defmethod update-instance-for-redefined-class
    ((instance standard-object) added-slots discarded-slots property-list
     &rest initargs)
  (check-initargs-1
   (class-of instance) initargs
   (list (list* 'update-instance-for-redefined-class
                instance added-slots discarded-slots property-list initargs)
         (list* 'shared-initialize instance added-slots initargs)))
  (apply #'shared-initialize instance added-slots initargs))

(defmethod shared-initialize ((instance slot-object) slot-names &rest initargs)
  (flet ((initialize-slot-from-initarg (class instance slotd)
           (let ((slot-initargs (slot-definition-initargs slotd)))
             (doplist (initarg value) initargs
               (when (memq initarg slot-initargs)
                 (setf (slot-value-using-class class instance slotd)
                       value)
                 (return t)))))
         (initialize-slot-from-initfunction (class instance slotd)
           ;; CLHS: If a before method stores something in a slot,
           ;; that slot won't be initialized from its :INITFORM, if any.
           (let ((initfun (slot-definition-initfunction slotd)))
             (if (typep instance 'structure-object)
                 ;; We don't have a consistent unbound marker for structure
                 ;; object slots, and structure object redefinition is not
                 ;; really supported anyways -- so unconditionally
                 ;; initializing the slot should be fine.
                 (when initfun
                   (setf (slot-value-using-class class instance slotd)
                         (funcall initfun)))
                 (unless (or (not initfun)
                             (slot-boundp-using-class class instance slotd))
                   (setf (slot-value-using-class class instance slotd)
                         (funcall initfun)))))))
    (let* ((class (class-of instance))
           (initfn-slotds
            (loop for slotd in (class-slots class)
                  unless (initialize-slot-from-initarg class instance slotd)
                  collect slotd)))
      (dolist (slotd initfn-slotds)
        (when (or (eq t slot-names)
                  (memq (slot-definition-name slotd) slot-names))
          (initialize-slot-from-initfunction class instance slotd))))
    instance))

;;; If initargs are valid return nil, otherwise signal an error.
(defun check-initargs-1 (class initargs call-list
                         &optional (plist-p t) (error-p t))
  (multiple-value-bind (legal allow-other-keys)
      (check-initargs-values class call-list)
    (unless allow-other-keys
      (if plist-p
          (check-initargs-2-plist initargs class legal error-p)
          (check-initargs-2-list initargs class legal error-p)))))

(defun check-initargs-values (class call-list)
  (let ((methods (mapcan (lambda (call)
                           (if (consp call)
                               (copy-list (compute-applicable-methods
                                           (gdefinition (car call))
                                           (cdr call)))
                               (list call)))
                         call-list))
        (legal (apply #'append (mapcar #'slot-definition-initargs
                                       (class-slots class)))))
    ;; Add to the set of slot-filling initargs the set of
    ;; initargs that are accepted by the methods. If at
    ;; any point we come across &allow-other-keys, we can
    ;; just quit.
    (dolist (method methods)
      (multiple-value-bind (nreq nopt keysp restp allow-other-keys keys)
          (analyze-lambda-list (if (consp method)
                                   (early-method-lambda-list method)
                                   (method-lambda-list method)))
        (declare (ignore nreq nopt keysp restp))
        (when allow-other-keys
          (return-from check-initargs-values (values nil t)))
        (setq legal (append keys legal))))
    (values legal nil)))

(define-condition initarg-error (reference-condition program-error)
  ((class :reader initarg-error-class :initarg :class)
   (initargs :reader initarg-error-initargs :initarg :initargs))
  (:default-initargs :references (list '(:ansi-cl :section (7 1 2))))
  (:report (lambda (condition stream)
             (format stream "~@<Invalid initialization argument~P: ~2I~_~
                             ~<~{~S~^, ~} ~@:>~I~_in call for class ~S.~:>"
                     (length (initarg-error-initargs condition))
                     (list (initarg-error-initargs condition))
                     (initarg-error-class condition)))))

(defun check-initargs-2-plist (initargs class legal &optional (error-p t))
  (let ((invalid-keys ()))
    (unless (getf initargs :allow-other-keys)
      ;; Now check the supplied-initarg-names and the default initargs
      ;; against the total set that we know are legal.
      (doplist (key val) initargs
        (unless (or (memq key legal)
                    ;; :ALLOW-OTHER-KEYS NIL gets here
                    (eq key :allow-other-keys))
          (push key invalid-keys)))
      (when (and invalid-keys error-p)
        (error 'initarg-error :class class :initargs invalid-keys)))
    invalid-keys))

(defun check-initargs-2-list (initkeys class legal &optional (error-p t))
  (let ((invalid-keys ()))
    (unless (memq :allow-other-keys initkeys)
      ;; Now check the supplied-initarg-names and the default initargs
      ;; against the total set that we know are legal.
      (dolist (key initkeys)
        (unless (memq key legal)
          (push key invalid-keys)))
      (when (and invalid-keys error-p)
        (error 'initarg-error :class class :initargs invalid-keys)))
    invalid-keys))


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
  (unless (class-finalized-p class) (finalize-inheritance class))
  (setq initargs (default-initargs class initargs))
  #||
  (check-initargs-1
   class initargs
   (list (list* 'allocate-instance class initargs)
	 (list* 'initialize-instance (class-prototype class) initargs)
	 (list* 'shared-initialize (class-prototype class) t initargs)))
  ||#
  (let* ((info (initialize-info class initargs))
	 (valid-p (initialize-info-valid-p info)))
    (when (and (consp valid-p) (eq (car valid-p) :invalid))
      (error "Invalid initialization argument ~S for class ~S"
	     (cdr valid-p) (class-name class))))
  (let ((instance (apply #'allocate-instance class initargs)))
    (apply #'initialize-instance instance initargs)
    instance))

(defvar *default-initargs-flag* (list nil))

(defmethod default-initargs ((class slot-class) supplied-initargs)
  (call-initialize-function
   (initialize-info-default-initargs-function
    (initialize-info class supplied-initargs))
   nil supplied-initargs)
  #||
  ;; This implementation of default initargs is critically dependent
  ;; on all-default-initargs not having any duplicate initargs in it.
  (let ((all-default (class-default-initargs class))
	(miss *default-initargs-flag*))
    (flet ((getf* (plist key)
	     (do ()
		 ((null plist) miss)
	       (if (eq (car plist) key)
		   (return (cadr plist))
		   (setq plist (cddr plist))))))
      (labels ((default-1 (tail)
		 (if (null tail)
		     nil
		     (if (eq (getf* supplied-initargs (caar tail)) miss)
			 (list* (caar tail)
				(funcall (cadar tail))
				(default-1 (cdr tail)))
			 (default-1 (cdr tail))))))
	(append supplied-initargs (default-1 all-default)))))
  ||#)

(defmethod initialize-instance ((instance slot-object) &rest initargs)
  (apply #'shared-initialize instance t initargs))

(defmethod reinitialize-instance ((instance slot-object) &rest initargs)
  #||
  (check-initargs-1
   (class-of instance) initargs
   (list (list* 'reinitialize-instance instance initargs)
	 (list* 'shared-initialize instance nil initargs)))
  ||#
  (let* ((class (class-of instance))
	 (info (initialize-info class initargs))
	 (valid-p (initialize-info-ri-valid-p info)))
    (when (and (consp valid-p) (eq (car valid-p) :invalid))
      (error "Invalid initialization argument ~S for class ~S"
	     (cdr valid-p) (class-name class))))
  (apply #'shared-initialize instance nil initargs)
  instance)

(defmethod update-instance-for-different-class ((previous std-object)
						(current std-object)
						&rest initargs)
  ;; First we must compute the newly added slots. The spec defines
  ;; newly added slots as "those local slots for which no slot of
  ;; the same name exists in the previous class."
  (let ((added-slots '())
	(current-slotds (class-slots (class-of current)))
	(previous-slot-names (mapcar #'slot-definition-name
				     (class-slots (class-of previous)))))
    (dolist (slotd current-slotds)
      (if (and (not (memq (slot-definition-name slotd) previous-slot-names))
	       (eq (slot-definition-allocation slotd) ':instance))
	  (push (slot-definition-name slotd) added-slots)))
    (check-initargs-1
     (class-of current) initargs
     (list (list* 'update-instance-for-different-class previous current initargs)
	   (list* 'shared-initialize current added-slots initargs)))
    (apply #'shared-initialize current added-slots initargs)))

(defmethod update-instance-for-redefined-class ((instance std-object)
						added-slots
						discarded-slots
						property-list
						&rest initargs)
  (check-initargs-1
   (class-of instance) initargs
   (list (list* 'update-instance-for-redefined-class
		instance added-slots discarded-slots property-list initargs)
	 (list* 'shared-initialize instance added-slots initargs)))
  (apply #'shared-initialize instance added-slots initargs))

(defmethod shared-initialize
    ((instance slot-object) slot-names &rest initargs)
  (when (eq slot-names t)
    (return-from shared-initialize
      (call-initialize-function
       (initialize-info-shared-initialize-t-fun
	(initialize-info (class-of instance) initargs))
       instance initargs)))
  (when (eq slot-names nil)
    (return-from shared-initialize
      (call-initialize-function
       (initialize-info-shared-initialize-nil-fun
	(initialize-info (class-of instance) initargs))
       instance initargs)))
  ;; Initialize the instance's slots in a two step process:
  ;;   (1) A slot for which one of the initargs in initargs can set
  ;;       the slot, should be set by that initarg. If more than
  ;;       one initarg in initargs can set the slot, the leftmost
  ;;       one should set it.
  ;;   (2) Any slot not set by step 1, may be set from its initform
  ;;       by step 2. Only those slots specified by the slot-names
  ;;       argument are set. If slot-names is:
  ;;       T
  ;;	      then any slot not set in step 1 is set from its
  ;;	      initform.
  ;;       <list of slot names>
  ;;	      then any slot in the list, and not set in step 1
  ;;	      is set from its initform.
  ;;       ()
  ;;	      then no slots are set from initforms.
  (let* ((class (class-of instance))
	 (slotds (class-slots class))
	 (std-p (pcl-instance-p instance)))
    (dolist (slotd slotds)
      (let ((slot-name (slot-definition-name slotd))
	    (slot-initargs (slot-definition-initargs slotd)))
	(unless (progn
		  ;; Try to initialize the slot from one of the initargs.
		  ;; If we succeed return T, otherwise return nil.
		  (doplist (initarg val) initargs
			   (when (memq initarg slot-initargs)
			     (setf (slot-value-using-class class
							   instance
							   slotd)
				   val)
			     (return t))))
	  ;; Try to initialize the slot from its initform.
	  (if (and slot-names
		   (or (eq slot-names t)
		       (memq slot-name slot-names))
		   (or (and (not std-p) (eq slot-names t))
		       (not (slot-boundp-using-class class instance slotd))))
	      (let ((initfunction (slot-definition-initfunction slotd)))
		(when initfunction
		  (setf (slot-value-using-class class instance slotd)
			(funcall initfunction))))))))
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

(defun check-initargs-2-plist (initargs class legal &optional (error-p t))
  (unless (getf initargs :allow-other-keys)
    ;; Now check the supplied-initarg-names and the default initargs
    ;; against the total set that we know are legal.
    (doplist (key val) initargs
      (unless (memq key legal)
	(if error-p
	    (error "Invalid initialization argument ~S for class ~S"
		   key
		   (class-name class))
	    (return-from check-initargs-2-plist nil)))))
  t)

(defun check-initargs-2-list (initkeys class legal &optional (error-p t))
  (unless (memq :allow-other-keys initkeys)
    ;; Now check the supplied-initarg-names and the default initargs
    ;; against the total set that we know are legal.
    (dolist (key initkeys)
      (unless (memq key legal)
	(if error-p
	    (error "Invalid initialization argument ~S for class ~S"
		   key
		   (class-name class))
	    (return-from check-initargs-2-list nil)))))
  t)


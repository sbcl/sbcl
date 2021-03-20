;;;; bootstrapping the meta-braid
;;;;
;;;; The code in this file takes the early definitions that have been
;;;; saved up and actually builds those class objects. This work is
;;;; largely driven off of those class definitions, but the fact that
;;;; STANDARD-CLASS is the class of all metaclasses in the braid is
;;;; built into this code pretty deeply.

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

(defun allocate-standard-instance (wrapper)
  (let* ((instance (%make-instance (1+ sb-vm:instance-data-start)))
         (slots (make-array (layout-length wrapper) :initial-element +slot-unbound+)))
    (setf (%instance-layout instance) wrapper)
    (setf (std-instance-slots instance) slots)
    instance))

(define-condition unset-funcallable-instance-function
    (reference-condition simple-error)
  ()
  (:default-initargs
   :references '((:amop :generic-function allocate-instance)
                 (:amop :function set-funcallable-instance-function))))

(defmacro error-no-implementation-function (fin)
  `(error 'unset-funcallable-instance-function
          :format-control "~@<The function of funcallable instance ~
                           ~S has not been set.~@:>"
          :format-arguments (list ,fin)))

(defun allocate-standard-funcallable-instance (wrapper name)
  (declare (layout wrapper))
  (let* ((hash (if name
                   (mix (sxhash name) (sxhash :generic-function)) ; arb. constant
                   (sb-impl::quasi-random-address-based-hash
                    (load-time-value (make-array 1 :element-type '(and fixnum unsigned-byte)))
                    most-positive-fixnum)))
         (slots (make-array (layout-length wrapper) :initial-element +slot-unbound+))
         (fin (cond #+(and immobile-code)
                    ((/= (layout-bitmap wrapper) +layout-all-tagged+)
                     (let ((f (truly-the funcallable-instance
                                         (sb-vm::make-immobile-funinstance wrapper slots))))
                       ;; set the upper 4 bytes of wordindex 5
                       (sb-sys:with-pinned-objects (f)
                         (setf (sb-impl::fsc-instance-trailer-hash f) (ldb (byte 32 0) hash)))
                       f))
                    (t
                     (let ((f (truly-the funcallable-instance
                                         (%make-standard-funcallable-instance slots hash))))
                       (setf (%fun-layout f) wrapper)
                       f)))))
    (setf (%funcallable-instance-fun fin)
          (lambda (&rest args)
            (declare (ignore args))
            (error-no-implementation-function fin)))
    fin))

(defun classify-slotds (slotds)
  (let (instance-slots class-slots custom-slots bootp)
    (dolist (slotd slotds)
      (let ((alloc (cond ((consp slotd) ; bootstrap
                          (setf bootp t)
                          :instance)
                         (t
                          (slot-definition-allocation slotd)))))
        (case alloc
          (:instance
           (push slotd instance-slots))
          (:class
           (push slotd class-slots))
          (t
           (push slotd custom-slots)))))
    (values (if bootp
                (nreverse instance-slots)
                (when slotds
                  (sort instance-slots #'< :key #'slot-definition-location)))
            class-slots
            custom-slots)))

;;;; BOOTSTRAP-META-BRAID
;;;;
;;;; This function builds the base metabraid from the early class definitions.

(defmacro !initial-classes-and-wrappers (&rest classes)
  `(progn
     ,@(mapcar (lambda (class)
                 (let ((wr (format-symbol *pcl-package* "~A-WRAPPER" class)))
                   `(setf ,wr ,(if (eq class 'standard-generic-function)
                                   '*sgf-wrapper*
                                   `(!boot-make-wrapper
                                     (!early-class-size ',class)
                                     ',class))
                          ,class (allocate-standard-instance
                                  ,(if (eq class 'standard-generic-function)
                                       'funcallable-standard-class-wrapper
                                       'standard-class-wrapper))
                          (wrapper-class ,wr) ,class
                          (find-class ',class) ,class)))
               classes)))

(defmacro wrapper-info (x) `(sb-kernel::layout-%info ,x))
(declaim (inline wrapper-slot-list))
(defun wrapper-slot-list (wrapper)
  (let ((info (wrapper-info wrapper)))
    (if (listp info) info)))
(defun (setf wrapper-slot-list) (newval wrapper)
  ;; The current value must be a list, otherwise we'd clobber
  ;; a defstruct-description.
  (aver (listp (wrapper-info wrapper)))
  (setf (wrapper-info wrapper) newval))

(defun !bootstrap-meta-braid ()
  (let* ((*create-classes-from-internal-structure-definitions-p* nil)
         standard-class-wrapper standard-class
         funcallable-standard-class-wrapper funcallable-standard-class
         slot-class-wrapper slot-class
         system-class-wrapper system-class
         built-in-class-wrapper built-in-class
         structure-class-wrapper structure-class
         condition-class-wrapper condition-class
         standard-direct-slot-definition-wrapper
         standard-direct-slot-definition
         standard-effective-slot-definition-wrapper
         standard-effective-slot-definition
         class-eq-specializer-wrapper class-eq-specializer
         standard-generic-function-wrapper standard-generic-function)
    (!initial-classes-and-wrappers
     standard-class funcallable-standard-class
     slot-class system-class built-in-class structure-class condition-class
     standard-direct-slot-definition standard-effective-slot-definition
     class-eq-specializer standard-generic-function)
    ;; First, make a class metaobject for each of the early classes. For
    ;; each metaobject we also set its wrapper. Except for the class T,
    ;; the wrapper is always that of STANDARD-CLASS.
    (dolist (definition *!early-class-definitions*)
      (let* ((name (ecd-class-name definition))
             (meta (ecd-metaclass definition))
             (wrapper (ecase meta
                        (slot-class slot-class-wrapper)
                        (standard-class standard-class-wrapper)
                        (funcallable-standard-class
                         funcallable-standard-class-wrapper)
                        (built-in-class built-in-class-wrapper)
                        (system-class system-class-wrapper)
                        (structure-class structure-class-wrapper)
                        (condition-class condition-class-wrapper)))
             (class (or (find-class name nil)
                        (allocate-standard-instance wrapper))))
        (setf (find-class name) class)))
    (dolist (definition *!early-class-definitions*)
      (let ((name (ecd-class-name definition))
            (meta (ecd-metaclass definition))
            (source (ecd-source-location definition))
            (direct-supers (ecd-superclass-names definition))
            (direct-slots  (ecd-canonical-slots definition))
            (other-initargs (ecd-other-initargs definition)))
        (let ((direct-default-initargs
               (getf other-initargs :direct-default-initargs)))
          (multiple-value-bind (slots cpl default-initargs direct-subclasses)
              (!early-collect-inheritance name)
            (let* ((class (find-class name))
                   (bitmap (if (memq name '(standard-generic-function))
                               sb-kernel::standard-gf-primitive-obj-layout-bitmap
                               +layout-all-tagged+))
                   (wrapper (cond ((eq class slot-class)
                                   slot-class-wrapper)
                                  ((eq class standard-class)
                                   standard-class-wrapper)
                                  ((eq class funcallable-standard-class)
                                   funcallable-standard-class-wrapper)
                                  ((eq class standard-direct-slot-definition)
                                   standard-direct-slot-definition-wrapper)
                                  ((eq class
                                       standard-effective-slot-definition)
                                   standard-effective-slot-definition-wrapper)
                                  ((eq class system-class) system-class-wrapper)
                                  ((eq class built-in-class)
                                   built-in-class-wrapper)
                                  ((eq class structure-class)
                                   structure-class-wrapper)
                                  ((eq class condition-class)
                                   condition-class-wrapper)
                                  ((eq class class-eq-specializer)
                                   class-eq-specializer-wrapper)
                                  ((eq class standard-generic-function)
                                   standard-generic-function-wrapper)
                                  (t
                                   (!boot-make-wrapper (length slots) name bitmap))))
                   (proto nil))
              (let ((symbol (make-class-symbol name)))
                (when (eq (info :variable :kind symbol) :global)
                  (set symbol class)))
              (dolist (slot slots)
                (unless (eq (getf slot :allocation :instance) :instance)
                  (error "Slot allocation ~S is not supported in bootstrap."
                         (getf slot :allocation))))

              (when (layout-for-pcl-obj-p wrapper)
                (setf (wrapper-slot-list wrapper) slots))

              (setq proto (if (eq meta 'funcallable-standard-class)
                              (allocate-standard-funcallable-instance wrapper name)
                              (allocate-standard-instance wrapper)))

              (setq direct-slots
                    (!bootstrap-make-slot-definitions
                     name class direct-slots
                     standard-direct-slot-definition-wrapper nil))
              (setq slots
                    (!bootstrap-make-slot-definitions
                     name class slots
                     standard-effective-slot-definition-wrapper t))

              (setf (layout-slot-table wrapper) (make-slot-table class slots t))
              (when (layout-for-pcl-obj-p wrapper)
                (setf (wrapper-slot-list wrapper) slots))

              (ecase meta
                ((standard-class funcallable-standard-class)
                 (!bootstrap-initialize-class
                  meta
                  class name class-eq-specializer-wrapper source
                  direct-supers direct-subclasses cpl wrapper proto
                  direct-slots slots direct-default-initargs default-initargs))
                (built-in-class         ; *the-class-t*
                 (!bootstrap-initialize-class
                  meta
                  class name class-eq-specializer-wrapper source
                  direct-supers direct-subclasses cpl wrapper proto))
                (system-class
                 (!bootstrap-initialize-class
                  meta
                  class name class-eq-specializer-wrapper source
                  direct-supers direct-subclasses cpl wrapper proto))
                (slot-class             ; *the-class-slot-object*
                 (!bootstrap-initialize-class
                  meta
                  class name class-eq-specializer-wrapper source
                  direct-supers direct-subclasses cpl wrapper proto))
                (structure-class        ; *the-class-structure-object*
                 (!bootstrap-initialize-class
                  meta
                  class name class-eq-specializer-wrapper source
                  direct-supers direct-subclasses cpl wrapper))
                (condition-class
                 (!bootstrap-initialize-class
                  meta
                  class name class-eq-specializer-wrapper source
                  direct-supers direct-subclasses cpl wrapper))))))))

    (setq **standard-method-classes**
          (mapcar (lambda (name)
                    (symbol-value (make-class-symbol name)))
                  +standard-method-class-names+))

    (flet ((make-method-combination (class-name)
             (let* ((class (find-class class-name))
                    (wrapper (!bootstrap-get-slot
                              'standard-class class 'wrapper))
                    (instance (allocate-standard-instance wrapper)))
               (flet ((set-slot (name value)
                        (!bootstrap-set-slot class-name instance name value)))
                 (values instance #'set-slot)))))
      ;; Create the STANDARD method combination object.
      (multiple-value-bind (method-combination set-slot)
          (make-method-combination 'standard-method-combination)
        (funcall set-slot 'source nil)
        (funcall set-slot 'type-name 'standard)
        (funcall set-slot 'options '())
        (funcall set-slot '%generic-functions (make-gf-hash-table))
        (funcall set-slot '%documentation "The standard method combination.")
        (setq *standard-method-combination* method-combination))
      ;; Create an OR method combination object.
      (multiple-value-bind (method-combination set-slot)
          (make-method-combination 'short-method-combination)
        (funcall set-slot 'source 'nil)
        (funcall set-slot 'type-name 'or)
        (funcall set-slot 'operator 'or)
        (funcall set-slot 'identity-with-one-argument t)
        (funcall set-slot '%generic-functions (make-gf-hash-table))
        (funcall set-slot '%documentation nil)
        (funcall set-slot 'options '(:most-specific-first))
        (setq *or-method-combination* method-combination)))))

;;; I have no idea why we care so much about being able to create an instance
;;; of STRUCTURE-OBJECT, when (almost) no other structure class in the system
;;; begins life such that MAKE-INSTANCE works on it.
;;; And ALLOCATE-INSTANCE seems to work fine anyway. e.g. you can call
;;; (ALLOCATE-INSTANCE (FIND-CLASS 'HASH-TABLE)).
;;; Anyway, see below in !BOOTSTRAP-INITIALIZE-CLASS where we refer to
;;; the name of this seemingly useless constructor function.
(defun |STRUCTURE-OBJECT class constructor| ()
  (sb-kernel:%make-structure-instance
   #.(sb-kernel:find-defstruct-description 'structure-object)
   nil))

;;; Initialize a class metaobject.
(defun !bootstrap-initialize-class
       (metaclass-name class name
        class-eq-wrapper source direct-supers direct-subclasses cpl wrapper
        &optional
        (proto nil proto-p)
        direct-slots slots direct-default-initargs default-initargs)
  (flet ((classes (names) (mapcar #'find-class names))
         (set-slot (slot-name value)
           (!bootstrap-set-slot metaclass-name class slot-name value)))
    (set-slot 'name name)
    (set-slot 'finalized-p t)
    (set-slot 'source source)
    (set-slot 'safe-p nil)
    (set-slot '%type (if (eq class (find-class t))
                         t
                         ;; FIXME: Could this just be CLASS instead
                         ;; of `(CLASS ,CLASS)? If not, why not?
                         ;; (See also similar expression in
                         ;; SHARED-INITIALIZE :BEFORE (CLASS).)
                         `(class ,class)))
    (set-slot 'class-eq-specializer
              (let ((spec (allocate-standard-instance class-eq-wrapper)))
                (!bootstrap-set-slot 'class-eq-specializer spec '%type
                                     `(class-eq ,class))
                (!bootstrap-set-slot 'class-eq-specializer spec 'object
                                     class)
                spec))
    (set-slot '%class-precedence-list (classes cpl))
    (set-slot 'cpl-available-p t)
    (set-slot 'can-precede-list (classes (cdr cpl)))
    (set-slot 'incompatible-superclass-list nil)
    (set-slot 'direct-superclasses (classes direct-supers))
    (set-slot 'direct-subclasses (classes direct-subclasses))
    (set-slot 'direct-methods (cons nil nil))
    (set-slot 'wrapper wrapper)
    (set-slot '%documentation nil)
    (set-slot 'plist
              `(,@(and direct-default-initargs
                       `(direct-default-initargs ,direct-default-initargs))
                ,@(and default-initargs
                       `(default-initargs ,default-initargs))))
    (when (memq metaclass-name '(standard-class funcallable-standard-class
                                 structure-class condition-class
                                 slot-class))
      (set-slot 'direct-slots direct-slots)
      (set-slot 'slots slots)
      (setf (layout-slot-table wrapper)
            (make-slot-table class slots
                             (member metaclass-name
                                     '(standard-class funcallable-standard-class))))
      (when (layout-for-pcl-obj-p wrapper)
        (setf (wrapper-slot-list wrapper) slots)))

    ;; For all direct superclasses SUPER of CLASS, make sure CLASS is
    ;; a direct subclass of SUPER.  Note that METACLASS-NAME doesn't
    ;; matter here for the slot DIRECT-SUBCLASSES, since every class
    ;; inherits the slot from class CLASS.
    (dolist (super direct-supers)
      (let* ((super (find-class super))
             (subclasses (!bootstrap-get-slot metaclass-name super
                                              'direct-subclasses)))
        (cond ((unbound-marker-p subclasses)
               (!bootstrap-set-slot metaclass-name super 'direct-subclasses
                                    (list class)))
              ((not (memq class subclasses))
               (!bootstrap-set-slot metaclass-name super 'direct-subclasses
                                    (cons class subclasses))))))

    (case metaclass-name
      (structure-class
       (let ((constructor-sym '|STRUCTURE-OBJECT class constructor|))
         (set-slot 'defstruct-form
                   `(defstruct (structure-object (:constructor
                                                  ,constructor-sym)
                                                 (:copier nil))))
         (set-slot 'defstruct-constructor constructor-sym)
         (set-slot 'from-defclass-p t)
         (set-slot 'plist nil)
         (set-slot 'prototype (funcall constructor-sym))))
      (condition-class
       (set-slot 'prototype (make-condition name)))
      (t
       (set-slot 'prototype
                 (if proto-p proto (allocate-standard-instance wrapper)))))
    class))

(defun !bootstrap-make-slot-definitions (name class slots wrapper effective-p)
  (let ((index -1))
    (mapcar (lambda (slot)
              (incf index)
              (!bootstrap-make-slot-definition
               name class slot wrapper effective-p index))
            slots)))

(defun !bootstrap-make-slot-definition
    (name class slot wrapper effective-p index)
  (let* ((slotd-class-name (if effective-p
                               'standard-effective-slot-definition
                               'standard-direct-slot-definition))
         (slotd (allocate-standard-instance wrapper))
         (slot-name (getf slot :name)))
    (flet ((get-val (name) (getf slot name))
           (set-val (name val)
                    (!bootstrap-set-slot slotd-class-name slotd name val)))
      (set-val 'name         slot-name)
      (set-val 'initform     (get-val :initform))
      (set-val 'initfunction (get-val :initfunction))
      (set-val 'initargs     (get-val :initargs))
      (unless effective-p
        (set-val 'readers      (get-val :readers))
        (set-val 'writers      (get-val :writers)))
      (set-val 'allocation   :instance)
      (set-val '%type        (or (get-val :type) t))
      (set-val '%documentation (or (get-val :documentation) ""))
      (set-val '%class   class)
      (when effective-p
        (set-val 'location index)
        (set-val 'accessor-flags 7)
        (set-val
         'info
         (make-slot-info
          :reader
          (make-optimized-std-reader-method-function nil nil slot-name index)
          :writer
          (make-optimized-std-writer-method-function nil nil slot-name index)
          :boundp
          (make-optimized-std-boundp-method-function nil nil slot-name index))))
      (when (and (eq name 'standard-class)
                 (eq slot-name 'slots) effective-p)
        (setq *the-eslotd-standard-class-slots* slotd))
      (when (and (eq name 'funcallable-standard-class)
                 (eq slot-name 'slots) effective-p)
        (setq *the-eslotd-funcallable-standard-class-slots* slotd))
      slotd)))

(defun !bootstrap-accessor-definitions (early-p)
  (let ((*early-p* early-p))
    (dolist (definition *!early-class-definitions*)
      (let ((name (ecd-class-name definition))
            (meta (ecd-metaclass definition)))
        (unless (or (eq meta 'built-in-class) (eq meta 'system-class))
          (let ((direct-slots  (ecd-canonical-slots definition)))
            (dolist (slotd direct-slots)
              (let ((slot-name (getf slotd :name))
                    (readers (getf slotd :readers))
                    (writers (getf slotd :writers)))
                (!bootstrap-accessor-definitions1
                 name
                 slot-name
                 readers
                 writers
                 nil
                 (ecd-source-location definition))))))))))

(defun !bootstrap-accessor-definition (class-name accessor-name slot-name type source-location)
  (multiple-value-bind (accessor-class make-method-function arglist specls doc)
      (ecase type
        (reader (values 'standard-reader-method
                        #'make-std-reader-method-function
                        (list class-name)
                        (list class-name)
                        "automatically generated reader method"))
        (writer (values 'standard-writer-method
                        #'make-std-writer-method-function
                        (list 'new-value class-name)
                        (list t class-name)
                        "automatically generated writer method"))
        (boundp (values 'standard-boundp-method
                        #'make-std-boundp-method-function
                        (list class-name)
                        (list class-name)
                        "automatically generated boundp method")))
    (let ((gf (ensure-generic-function accessor-name :lambda-list arglist)))
      (if (find specls (early-gf-methods gf)
                :key #'early-method-specializers
                :test 'equal)
          (unless (assoc accessor-name *!generic-function-fixups*
                         :test #'equal)
            (update-dfun gf))
          (add-method gf
                      (make-a-method accessor-class
                                     ()
                                     arglist specls
                                     (funcall make-method-function
                                              class-name slot-name)
                                     doc
                                     :slot-name slot-name
                                     :object-class class-name
                                     :method-class-function (constantly (find-class accessor-class))
                                     'source source-location))))))

(defun !bootstrap-accessor-definitions1 (class-name
                                         slot-name
                                         readers
                                         writers
                                         boundps
                                         source-location)
  (flet ((do-reader-definition (reader)
           (!bootstrap-accessor-definition class-name
                                           reader
                                           slot-name
                                           'reader
                                           source-location))
         (do-writer-definition (writer)
           (!bootstrap-accessor-definition class-name
                                           writer
                                           slot-name
                                           'writer
                                           source-location))
         (do-boundp-definition (boundp)
           (!bootstrap-accessor-definition class-name
                                           boundp
                                           slot-name
                                           'boundp
                                           source-location)))
    (dolist (reader readers) (do-reader-definition reader))
    (dolist (writer writers) (do-writer-definition writer))
    (dolist (boundp boundps) (do-boundp-definition boundp))))

(defun !bootstrap-built-in-classes ()

  ;; First make sure that all the supers listed in
  ;; *BUILT-IN-CLASS-LATTICE* are themselves defined by
  ;; *BUILT-IN-CLASS-LATTICE*. This is just to check for typos and
  ;; other sorts of brainos.  (The exceptions, T and SEQUENCE, are
  ;; those classes which are SYSTEM-CLASSes which nevertheless have
  ;; BUILT-IN-CLASS subclasses.)
  (dolist (e *built-in-classes*)
    (dolist (super (cadr e))
      (unless (or (eq super t)
                  (eq super 'sequence)
                  (assq super *built-in-classes*))
        (error "in *BUILT-IN-CLASSES*: ~S has ~S as a super,~%~
                but ~S is not itself a class in *BUILT-IN-CLASSES*."
               (car e) super super))))

  ;; In the first pass, we create a skeletal object to be bound to the
  ;; class name.
  (let* ((built-in-class (find-class 'built-in-class))
         (built-in-class-wrapper (class-wrapper built-in-class)))
    (dolist (e *built-in-classes*)
      (let ((class (allocate-standard-instance built-in-class-wrapper)))
        (setf (find-class (car e)) class))))

  ;; In the second pass, we initialize the class objects.
  (let ((class-eq-wrapper (class-wrapper (find-class 'class-eq-specializer))))
    (dolist (e *built-in-classes*)
      (destructuring-bind (name supers subs cpl prototype) e
        (let* ((class (find-class name))
               (lclass (find-classoid name))
               (wrapper (classoid-layout lclass)))
          (setf (classoid-pcl-class lclass) class)

          (!bootstrap-initialize-class 'built-in-class class
                                       name class-eq-wrapper nil
                                       supers subs
                                       (cons name cpl)
                                       wrapper prototype))))))

(defun class-of (x)
  (declare (explicit-check))
  (wrapper-class (layout-of x)))

(defun eval-form (form)
  (lambda () (eval form)))

(defun ensure-non-standard-class (name classoid &optional existing-class)
  (flet
      ((ensure (metaclass slots)
         (let ((supers (mapcar #'classoid-name (classoid-direct-superclasses classoid))))
           (ensure-class-using-class existing-class name
                                     :metaclass metaclass :name name
                                     :direct-superclasses supers
                                     :direct-slots slots)))
       (slot-initargs-from-structure-slotd (slotd)
         (let ((accessor (structure-slotd-accessor-symbol slotd)))
           `(:name ,(structure-slotd-name slotd)
             :defstruct-accessor-symbol ,accessor
             :internal-reader-function ,(structure-slotd-reader-function slotd)
             :internal-writer-function ,(structure-slotd-writer-function name slotd)
             :type ,(or (structure-slotd-type slotd) t)
             :initform ,(structure-slotd-init-form slotd)
             :initfunction ,(eval-form (structure-slotd-init-form slotd)))))
       (slot-initargs-from-condition-slot (slot)
         `(:name ,(condition-slot-name slot)
           :initargs ,(condition-slot-initargs slot)
           :readers ,(condition-slot-readers slot)
           :writers ,(condition-slot-writers slot)
           ,@(when (condition-slot-initform-p slot)
               (let ((initform (condition-slot-initform slot))
                     (initfun (condition-slot-initfunction slot)))
                 `(:initform ',initform :initfunction ,initfun)))
           :allocation ,(condition-slot-allocation slot)
           :documentation ,(condition-slot-documentation slot))))
    (cond ((structure-type-p name)
           (ensure 'structure-class
                   (mapcar #'slot-initargs-from-structure-slotd
                           (structure-type-slot-description-list name))))
          ((condition-type-p name)
           (ensure 'condition-class
                   (mapcar #'slot-initargs-from-condition-slot
                           (condition-classoid-slots classoid))))
          (t
           (error "~@<~S is not the name of a class.~@:>" name)))))

(defun ensure-deffoo-class (classoid)
  (let ((class (classoid-pcl-class classoid)))
    (cond (class
           (ensure-non-standard-class (class-name class) classoid class))
          ((eq 'complete **boot-state**)
           (ensure-non-standard-class (classoid-name classoid) classoid)))))

(pushnew 'ensure-deffoo-class sb-kernel::*defstruct-hooks*)
(pushnew 'ensure-deffoo-class sb-kernel::*define-condition-hooks*)

(defun !make-class-predicate (class name source-location)
  (let* ((gf (ensure-generic-function name :lambda-list '(object)
                                      'source source-location))
         (mlist (if (eq **boot-state** 'complete)
                    (early-gf-methods gf)
                    (generic-function-methods gf))))
    (unless mlist
      (unless (eq class *the-class-t*)
        (let* ((default-method-function #'constantly-nil)
               (default-method-initargs (list :function default-method-function
                                              'plist '(:constant-value nil)))
               (default-method (make-a-method
                                'standard-method
                                ()
                                (list 'object)
                                (list *the-class-t*)
                                default-method-initargs
                                "class predicate default method")))
          (add-method gf default-method)))
      (let* ((class-method-function #'constantly-t)
             (class-method-initargs (list :function class-method-function
                                          'plist '(:constant-value t)))
             (class-method (make-a-method 'standard-method
                                          ()
                                          (list 'object)
                                          (list class)
                                          class-method-initargs
                                          "class predicate class method")))
        (add-method gf class-method)))
    gf))

(define-load-time-global *simple-stream-root-classoid* :unknown)

(defun assign-layout-bitmap (layout)
  ;; We decide only at class finalization time whether it is funcallable.
  ;; Picking the right bitmap could probably be done sooner given the metaclass,
  ;; but this approach avoids changing how PCL uses MAKE-LAYOUT.
  ;; The big comment above MAKE-IMMOBILE-FUNINSTANCE in src/code/x86-64-vm
  ;; explains why we differentiate between SGF and everything else.
  (let ((inherits (layout-inherits layout)))
    (when (find #.(find-layout 'stream) inherits)
      (let ((flags 0))
        (dovector (layout inherits)
          (when (eq (layout-classoid layout) *simple-stream-root-classoid*)
            (setq flags (logior flags +simple-stream-layout-flag+)))
          (case layout
            (#.(find-layout 'file-stream)
             (setq flags (logior flags +file-stream-layout-flag+)))
            (#.(find-layout 'string-stream)
             (setq flags +string-stream-layout-flag+))))
        (setf (layout-flags layout)
              (logior flags +stream-layout-flag+ (layout-flags layout)))))
    (when (find #.(find-layout 'function) inherits)
      (setf (%raw-instance-ref/signed-word layout (sb-kernel::type-dd-length layout))
             #+immobile-code ; there are two possible bitmaps
             (if (or (find *sgf-wrapper* inherits) (eq layout *sgf-wrapper*))
                 sb-kernel::standard-gf-primitive-obj-layout-bitmap
                 +layout-all-tagged+)
             ;; there is only one possible bitmap otherwise
             #-immobile-code sb-kernel::standard-gf-primitive-obj-layout-bitmap))))

;;; Set the inherits from CPL, and register the layout. This actually
;;; installs the class in the Lisp type system.
(defun %update-lisp-class-layout (class layout)
  ;; Protected by *world-lock* in callers.
  (let ((classoid (layout-classoid layout)))
    (unless (eq (classoid-layout classoid) layout)
      (set-layout-inherits layout
                           (order-layout-inherits
                            (map 'simple-vector #'class-wrapper
                                 (reverse (rest (class-precedence-list class)))))
                           nil 0)
      (assign-layout-bitmap layout)
      (register-layout layout :invalidate t)

      ;; FIXME: I don't think this should be necessary, but without it
      ;; we are unable to compile (TYPEP foo '<class-name>) in the
      ;; same file as the class is defined.  If we had environments,
      ;; then I think the classsoid whould only be associated with the
      ;; name in that environment...  Alternatively, fix the compiler
      ;; so that TYPEP foo '<class-name> is slow but compileable.
      (let ((name (class-name class)))
        (when (and name (symbolp name) (eq name (classoid-name classoid)))
          (setf (find-classoid name) classoid))))))

(!bootstrap-meta-braid)
(!bootstrap-accessor-definitions t)
(!bootstrap-accessor-definitions nil)
(!bootstrap-built-in-classes)

(loop for (name . x)
      in (let (classoid-cells)
           (do-all-symbols (s classoid-cells)
             (let ((cell (sb-int:info :type :classoid-cell s)))
               (when cell
                 (push (cons s cell) classoid-cells)))))
      do
  (when (classoid-cell-pcl-class x)
    (let* ((class (find-class-from-cell name x))
           (layout (class-wrapper class))
           (lclass (layout-classoid layout))
           (lclass-pcl-class (classoid-pcl-class lclass))
           (olclass (find-classoid name nil)))
      (if lclass-pcl-class
          (aver (eq class lclass-pcl-class))
          (setf (classoid-pcl-class lclass) class))

      (%update-lisp-class-layout class layout)

      (cond (olclass
             (aver (eq lclass olclass)))
            (t
             (setf (find-classoid name) lclass)))

      )))

(setq **boot-state** 'braid)

(define-condition effective-method-condition (reference-condition)
  ((generic-function :initarg :generic-function
                     :reader effective-method-condition-generic-function)
   (method :initarg :method :initform nil
           :reader effective-method-condition-method)
   (args :initarg :args
         :reader effective-method-condition-args))
  (:default-initargs
   :generic-function (missing-arg)
   :args (missing-arg)))

(define-condition effective-method-error (error
                                          effective-method-condition)
  ((problem :initarg :problem :reader effective-method-error-problem))
  (:default-initargs :problem (missing-arg))
  (:report
   (lambda (condition stream)
     (format stream "~@<~A for the generic function ~2I~_~S ~I~_when ~
                     called ~@[from method ~2I~_~S~I~_~]with arguments ~
                     ~2I~_~S.~:>"
             (effective-method-error-problem condition)
             (effective-method-condition-generic-function condition)
             (effective-method-condition-method condition)
             (effective-method-condition-args condition)))))

(define-condition no-applicable-method-error (effective-method-error)
  ()
  (:default-initargs
   :problem "There is no applicable method"
   :references '((:ansi-cl :section (7 6 6)))))
(defmethod no-applicable-method (generic-function &rest args)
  (error 'no-applicable-method-error
         :generic-function generic-function
         :args args))

(define-condition no-next-method-error (effective-method-error)
  ()
  (:default-initargs
   :problem "There is no next method"
   :references '((:ansi-cl :section (7 6 6 2)))))
(defmethod no-next-method ((generic-function standard-generic-function)
                           (method standard-method) &rest args)
  (error 'no-next-method-error
         :generic-function generic-function
         :method method
         :args args))

;;; An extension to the ANSI standard: in the presence of e.g. a
;;; :BEFORE method, it would seem that going through
;;; NO-APPLICABLE-METHOD is prohibited, as in fact there is an
;;; applicable method.  -- CSR, 2002-11-15
(define-condition no-primary-method-error (effective-method-error)
  ()
  (:default-initargs
   :problem "There is no primary method"
   :references '((:ansi-cl :section (7 6 6 2)))))
(defmethod no-primary-method (generic-function &rest args)
  (error 'no-primary-method-error
         :generic-function generic-function
         :args args))

;; FIXME shouldn't this specialize on STANDARD-METHOD-COMBINATION?
(defmethod invalid-qualifiers ((gf generic-function)
                               combin
                               method)
  (let* ((qualifiers (method-qualifiers method))
         (qualifier (first qualifiers))
         (type-name (method-combination-type-name combin))
         (why (cond
                ((cdr qualifiers)
                 "has too many qualifiers")
                (t
                 (aver (not (standard-method-combination-qualifier-p
                             qualifier)))
                 "has an invalid qualifier"))))
    (invalid-method-error
     method
     "~@<The method ~S on ~S ~A.~
      ~@:_~@:_~
      ~@(~A~) method combination requires all methods to have one of ~
      the single qualifiers ~{~S~#[~; and ~:;, ~]~} or to have no ~
      qualifier at all.~@:>"
     method gf why type-name +standard-method-combination-qualifiers+)))

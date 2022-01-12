;;;; This file was split out from braid.lisp so that we could do some
;;;; of the work of building the meta-braid at cold initialization
;;;; time.

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

(declaim (list **standard-method-classes**))
(defglobal **standard-method-classes** nil)

(defconstant-eqx +standard-method-class-names+
  '(standard-method standard-reader-method
    standard-writer-method standard-boundp-method
    global-reader-method global-writer-method
    global-boundp-method)
  #'equal)

(define-load-time-global *sgf-wrapper*
  (!boot-make-wrapper (!early-class-size 'standard-generic-function)
                      'standard-generic-function
                      sb-kernel::standard-gf-primitive-obj-layout-bitmap))


(defun allocate-standard-instance (wrapper)
  (let* ((instance (%make-instance (1+ sb-vm:instance-data-start)))
         (slots (make-array (wrapper-length wrapper) :initial-element +slot-unbound+)))
    (setf (%instance-wrapper instance) wrapper)
    (%instance-set instance sb-vm:instance-data-start slots)
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
  (declare (wrapper wrapper))
  (let* ((hash (if name
                   (mix (sxhash name) (sxhash :generic-function)) ; arb. constant
                   (sb-impl::quasi-random-address-based-hash
                    (load-time-value (make-array 1 :element-type '(and fixnum unsigned-byte)))
                    most-positive-fixnum)))
         (slots (make-array (wrapper-length wrapper) :initial-element +slot-unbound+))
         (fin (cond #+(and immobile-code)
                    ((not (sb-kernel::bitmap-all-taggedp (wrapper-friend wrapper)))
                     (let ((f (truly-the funcallable-instance
                                         (sb-vm::make-immobile-funinstance wrapper slots))))
                       ;; set the upper 4 bytes of wordindex 5
                       (sb-sys:with-pinned-objects (f)
                         (setf (sb-impl::fsc-instance-trailer-hash f) (ldb (byte 32 0) hash)))
                       f))
                    (t
                     (let ((f (truly-the funcallable-instance
                                         (%make-standard-funcallable-instance slots hash))))
                       (setf (%fun-wrapper f) wrapper)
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

(declaim (inline wrapper-slot-list))
(defun wrapper-slot-list (wrapper)
  (let ((info (sb-kernel::wrapper-%info wrapper)))
    (if (listp info) info)))
(defun (setf wrapper-slot-list) (newval wrapper)
  ;; The current value must be a list, otherwise we'd clobber
  ;; a defstruct-description.
  (aver (listp (sb-kernel::wrapper-%info wrapper)))
  (setf (sb-kernel::wrapper-%info wrapper) newval))

(macrolet
    ((with-initial-classes-and-wrappers ((&rest classes) &body body)
       `(let* ((*create-classes-from-internal-structure-definitions-p* nil)
               ,@(mapcar (lambda (class)
                           `(,(symbolicate class "-WRAPPER")
                              ,(if (eq class 'standard-generic-function)
                                   '*sgf-wrapper*
                                   `(!boot-make-wrapper (!early-class-size ',class)
                                                        ',class))))
                         classes)
               ,@(mapcar (lambda (class)
                           `(,class (allocate-standard-instance
                                     ,(if (eq class 'standard-generic-function)
                                          'funcallable-standard-class-wrapper
                                          'standard-class-wrapper))))
                         classes))
          ,@(mapcan (lambda (class &aux (wr (symbolicate class "-WRAPPER")))
                      `((setf (wrapper-class ,wr) ,class
                              (find-class ',class) ,class)))
                    classes)
          ,@body)))
(defun !bootstrap-meta-braid ()
  (with-initial-classes-and-wrappers
    (standard-class funcallable-standard-class
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
                   ;; With #+compact-instance-header there are two possible bitmaps
                   ;; for funcallable instances - for self-contained trampoline
                   ;; instructions and external trampoline instructions.
                   ;; With #-compact-instance-header all funcallable layouts
                   ;; need the same bitmap, which is that of standard-GF.
                   ;; These requirements are checked in verify_range() of gencgc.
                   (bitmap (if (memq name '(standard-generic-function
                                            #-compact-instance-header funcallable-standard-object
                                            #-compact-instance-header generic-function))
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

              (setf (wrapper-slot-table wrapper) (make-slot-table class slots t))
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
        (setq *or-method-combination* method-combination))))))

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
         (built-in-class-wrapper (!bootstrap-get-slot 'standard-class built-in-class 'wrapper)))
    (dolist (e *built-in-classes*)
      (let ((class (allocate-standard-instance built-in-class-wrapper)))
        (setf (find-class (car e)) class))))

  ;; In the second pass, we initialize the class objects.
  (let ((class-eq-wrapper (!bootstrap-get-slot 'standard-class (find-class 'class-eq-specializer) 'wrapper)))
    (dolist (e *built-in-classes*)
      (destructuring-bind (name supers subs cpl prototype) e
        (let* ((class (find-class name))
               (lclass (find-classoid name))
               (wrapper (classoid-wrapper lclass)))
          (setf (classoid-pcl-class lclass) class)

          (!bootstrap-initialize-class 'built-in-class class
                                       name class-eq-wrapper nil
                                       supers subs
                                       (cons name cpl)
                                       wrapper prototype))))))

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
      (setf (wrapper-slot-table wrapper)
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

;;;; from slots-boot.lisp

(defun make-optimized-std-reader-method-function
    (fsc-p slotd slot-name location)
  (set-fun-name
   (etypecase location
     (fixnum
      (if fsc-p
          (lambda (instance)
            (check-obsolete-instance instance)
            (let ((value (clos-slots-ref (fsc-instance-slots instance)
                                         location)))
              (if (unbound-marker-p value)
                  (values
                   (slot-unbound (class-of instance) instance slot-name))
                  value)))
          (lambda (instance)
            (check-obsolete-instance instance)
            (let ((value (clos-slots-ref (std-instance-slots instance)
                                         location)))
              (if (unbound-marker-p value)
                  (values
                   (slot-unbound (class-of instance) instance slot-name))
                  value)))))
     (cons
      (lambda (instance)
        (check-obsolete-instance instance)
        (let ((value (cdr location)))
          (if (unbound-marker-p value)
              (values (slot-unbound (class-of instance) instance slot-name))
              value))))
     (null
      (lambda (instance)
        (declare (ignore instance))
        (instance-structure-protocol-error slotd 'slot-value-using-class))))
   `(reader ,slot-name)))

(defun make-optimized-std-writer-method-function (fsc-p slotd slot-name location)
  ;; The (WHEN SLOTD ...) gunk is for building early slot definitions.
  (let* ((class (when slotd (slot-definition-class slotd)))
         (safe-p (when slotd (safe-p class)))
         (orig-wrapper (when safe-p (class-wrapper class)))
         (info (when safe-p (slot-definition-info slotd)))
         (writer-fun (etypecase location
                       ;; In SAFE-P case the typechecking already validated the instance.
                       (fixnum
                        (if fsc-p
                            (if safe-p
                                (lambda (nv instance)
                                  (setf (clos-slots-ref (fsc-instance-slots instance)
                                                        location)
                                        nv))
                                (lambda (nv instance)
                                  (check-obsolete-instance instance)
                                  (setf (clos-slots-ref (fsc-instance-slots instance)
                                                        location)
                                        nv)))
                            (if safe-p
                                (lambda (nv instance)
                                  (setf (clos-slots-ref (std-instance-slots instance)
                                                        location)
                                        nv))
                                (lambda (nv instance)
                                  (check-obsolete-instance instance)
                                  (setf (clos-slots-ref (std-instance-slots instance)
                                                        location)
                                        nv)))))
                       (cons
                        (if safe-p
                            (lambda (nv instance)
                              (declare (ignore instance))
                              (setf (cdr location) nv))
                            (lambda (nv instance)
                              (check-obsolete-instance instance)
                              (setf (cdr location) nv))))
                       (null
                        (lambda (nv instance)
                          (declare (ignore nv instance))
                          (instance-structure-protocol-error
                           slotd
                           '(setf slot-value-using-class))))))
         (checking-fun (when safe-p
                         (lambda (new-value instance)
                           ;; If we have a TYPE-CHECK-FUNCTION, call it.
                           (let* (;; Note that the class of INSTANCE here is not
                                  ;; neccessarily the SLOT-DEFINITION-CLASS of
                                  ;; the SLOTD passed to M-O-S-W-M-F, since it's
                                  ;; e.g. possible for a subclass to define a
                                  ;; slot of the same name but with no
                                  ;; accessors. So we may need to fetch the
                                  ;; right SLOT-INFO from the wrapper instead of
                                  ;; just closing over it.
                                  (wrapper (valid-wrapper-of instance))
                                  (typecheck
                                   (slot-info-typecheck
                                    (if (eq wrapper orig-wrapper)
                                        info
                                        (cdr (find-slot-cell wrapper slot-name))))))
                             (when typecheck
                               (setf new-value (funcall typecheck new-value))))
                           ;; Then call the real writer.
                           (funcall writer-fun new-value instance)))))
    (set-fun-name (if safe-p
                      checking-fun
                      writer-fun)
                  `(writer ,slot-name))))

(defun make-optimized-std-boundp-method-function
    (fsc-p slotd slot-name location)
  (set-fun-name
   (etypecase location
     (fixnum (if fsc-p
                 (lambda (instance)
                   (check-obsolete-instance instance)
                   (not (unbound-marker-p (clos-slots-ref (fsc-instance-slots instance)
                                                          location))))
                 (lambda (instance)
                   (check-obsolete-instance instance)
                   (not (unbound-marker-p (clos-slots-ref (std-instance-slots instance)
                                                          location))))))
     (cons (lambda (instance)
             (check-obsolete-instance instance)
             (not (unbound-marker-p (cdr location)))))
     (null
      (lambda (instance)
        (declare (ignore instance))
        (instance-structure-protocol-error slotd 'slot-boundp-using-class))))
   `(boundp ,slot-name)))

;;;; FINDING SLOT DEFINITIONS
;;;
;;; Historical PCL found slot definitions by iterating over
;;; CLASS-SLOTS, which is O(N) for number of slots, and moreover
;;; requires a GF call (for SLOT-DEFINITION-NAME) for each slot in
;;; list up to the desired one.
;;;
;;; Current SBCL hashes the effective slot definitions, and some
;;; information pulled out from them into a simple-vector, with bucket
;;; chains made out of plists keyed by the slot names. This fixes
;;; gives O(1) performance, and avoid the GF calls.
;;;
;;; MAKE-SLOT-TABLE constructs the hashed vector out of a list of
;;; effective slot definitions and the class they pertain to, and
;;; FIND-SLOT-DEFINITION knows how to look up slots in that vector.
;;;
;;; The only bit of cleverness in the implementation is to make the
;;; vectors fairly tight, but always longer then 0 elements:
;;;
;;; -- We don't want to waste huge amounts of space no these vectors,
;;;    which are mostly required by things like SLOT-VALUE with a
;;;    variable slot name, so a constant extension over the minimum
;;;    size seems like a good choise.
;;;
;;; -- As long as the vector always has a length > 0
;;;    FIND-SLOT-DEFINITION doesn't need to handle the rare case of an
;;;    empty vector separately: it just returns a NIL.
;;;
;;; In addition to the slot-definition we also store the slot-location
;;; and type-check function for instances of standard metaclasses, so
;;; that SLOT-VALUE &co using variable slot names can get at them
;;; without additional GF calls.
;;;
;;; Notes:
;;;   It would also be nice to have STANDARD-INSTANCE-STRUCTURE-P
;;;   generic instead of checking versus STANDARD-CLASS and
;;;   FUNCALLABLE-STANDARD-CLASS.
;;;
;;;   Uh, the comments above talking about how FIND-SLOT-DEFINITION
;;;   does something with slot vectors has no basis in reality.
;;;   Probably the comments need fixing, rather than the code.

(defun find-slot-definition (class slot-name &optional errorp)
  (unless (class-finalized-p class)
    (or (try-finalize-inheritance class)
        (if errorp
            (error "Cannot look up slot-definition for ~S in ~S (too early to finalize.)"
                   slot-name class)
            (return-from find-slot-definition (values nil nil)))))
  (dolist (slotd (class-slots class)
           (if errorp
               (error "No slot called ~S in ~S." slot-name class)
               (values nil t)))
    (when (eq slot-name (slot-definition-name slotd))
      (return (values slotd t)))))

(defun find-slot-cell (wrapper slot-name)
  (declare (symbol slot-name))
  (declare (optimize (sb-c:insert-array-bounds-checks 0)))
  (let* ((vector (wrapper-slot-table wrapper))
         (modulus (truly-the index (svref vector 0)))
         ;; Can elide the 'else' branch of (OR symbol-hash ensure-symbol-hash)
         ;; because every symbol in the slot-table already got a nonzero hash.
         (index (rem (symbol-hash slot-name) modulus))
         (probe (svref vector (1+ index))))
    (declare (simple-vector vector) (index index))
    (cond ((fixnump probe)
           (do* ((count (svref vector (1- (truly-the index probe))))
                 (end (truly-the index (+ probe count)))
                 (j probe (1+ j)))
                ((>= j end))
             (declare (index count j))
             (when (eq (svref vector j) slot-name)
               (return (svref vector (truly-the index (+ j count)))))))
          ((eq (car (truly-the list probe)) slot-name)
           (cdr probe)))))

(defun make-slot-table (class slots &optional bootstrap)
  (unless slots
    ;; *** If changing this empty table value to something else,
    ;;     be sure to make a similar change to MAKE-COLD-LAYOUT in
    ;;     compiler/generic/genesis as well as in DEFSTRUCT LAYOUT.
    ;;     A DEFCONSTANT for this would only transfer the problem
    ;;     to cold-init in a different sort of way. :-(
    (return-from make-slot-table #(1 nil)))
  (let* ((n (+ (logior (length slots) 1) 2)) ; an odd divisor is preferred
         (vector (make-array n :initial-element nil)))
    (flet ((add-to-vector (name slot)
             (declare (symbol name)
                      (optimize (sb-c:insert-array-bounds-checks 0)))
             (let ((index (rem (ensure-symbol-hash name) n)))
               (setf (svref vector index)
                     (acons name
                            (cons (when (or bootstrap
                                            (and (standard-class-p class)
                                                 (slot-accessor-std-p slot 'all)))
                                    (if bootstrap
                                        (early-slot-definition-location slot)
                                        (slot-definition-location slot)))
                                  (the slot-info
                                    (if bootstrap
                                        (early-slot-definition-info slot)
                                        (slot-definition-info slot))))
                            (svref vector index))))))
      (if (eq 'complete **boot-state**)
          (dolist (slot slots)
            (add-to-vector (slot-definition-name slot) slot))
          (dolist (slot slots)
            (add-to-vector (early-slot-definition-name slot) slot))))
    ;; The VECTOR as computed above implements a hash table with chaining.
    ;; Rather than store chains using cons cells, chains can be stored in the
    ;; vector itself at the end, with the table entry pointing to another
    ;; index in the vector. The chain length is stored first, then all keys,
    ;; then all values. The resulting structure takes less memory than
    ;; linked lists, and can be scanned faster. As an exception, for lists
    ;; of length 1, the table cell holds a (key . value) pair directly.
    (let* ((final-n
            (+ 1 n
               ;; number of additional cells needed to represent linked lists
               ;; as length-prefixed subsequences in the final vector.
               (loop for cell across vector
                     for count = (length cell)
                     sum (if (<= count 1) 0 (1+ (* count 2))))))
           (final-vector (make-array final-n))
           (data-index (1+ n))) ; after the hashtable portion of the vector
      (setf (aref final-vector 0) n) ; the modulus
      (dotimes (i n final-vector)
        (let ((alist (aref vector i)))
          (if (not (cdr alist)) ; store it in the final vector as-is
              (setf (aref final-vector (1+ i)) (car alist))
              (let ((count (length alist)))
                ;; Probed cell holds the index of the first symbol.
                ;; The symbol count precedes the first symbol cell.
                (setf (aref final-vector (1+ i)) (1+ data-index)
                      (aref final-vector data-index) count)
                (dolist (cell alist)
                  (setf (aref final-vector (incf data-index)) (car cell)))
                (dolist (cell alist)
                  (setf (aref final-vector (incf data-index)) (cdr cell)))
                (incf data-index))))))))


;;;; initialize the initial class hierarchy

(!bootstrap-meta-braid)
(!bootstrap-built-in-classes)

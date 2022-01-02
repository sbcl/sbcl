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

;;; (These are left over from the days when PCL was an add-on package
;;; for a pre-CLOS Common Lisp. They shouldn't happen in a normal
;;; build, of course, but they might happen if someone is experimenting
;;; and debugging, and it's probably worth complaining if they do,
;;; so we've left 'em in.)
(when (eq **boot-state** 'complete)
  (error "Trying to load (or compile) PCL in an environment in which it~%~
          has already been loaded. This doesn't work, you will have to~%~
          get a fresh lisp (reboot) and then load PCL."))
(when **boot-state**
  (cerror "Try loading (or compiling) PCL anyways."
          "Trying to load (or compile) PCL in an environment in which it~%~
           has already been partially loaded. This may not work, you may~%~
           need to get a fresh lisp (reboot) and then load PCL."))

(declaim (inline gdefinition))
(defun gdefinition (spec)
  ;; This is null layer right now, but once FDEFINITION stops bypasssing
  ;; fwrappers/encapsulations we can do that here.
  (fdefinition spec))

(defun (setf gdefinition) (new-value spec)
  ;; This is almost a null layer right now, but once (SETF
  ;; FDEFINITION) stops bypasssing fwrappers/encapsulations we can do
  ;; that here.
  (sb-c::note-name-defined spec :function) ; FIXME: do we need this? Why?
  (setf (fdefinition spec) new-value))

;;;; type specifier hackery

;;; internal to this file
(defun coerce-to-class (class &optional make-forward-referenced-class-p)
  (if (symbolp class)
      (or (find-class class (not make-forward-referenced-class-p))
          (ensure-class class))
      class))

;;; interface
(defun type-from-specializer (specl)
  (cond ((eq specl t)
         t)
        ((consp specl)
         (unless (member (car specl) '(class prototype class-eq eql))
           (error "~S is not a legal specializer type." specl))
         specl)
        ((progn
           (when (symbolp specl)
             ;;maybe (or (find-class specl nil) (ensure-class specl)) instead?
             (setq specl (find-class specl)))
           (or (not (eq **boot-state** 'complete))
               (specializerp specl)))
         (specializer-type specl))
        (t
         (error "~S is neither a type nor a specializer." specl))))

(defun type-class (type)
  (setq type (type-from-specializer type))
  (if (atom type)
      (if (eq type t)
          *the-class-t*
          (error "bad argument to TYPE-CLASS"))
      (case (car type)
        (eql (class-of (cadr type)))
        (prototype (class-of (cadr type))) ;?
        (class-eq (cadr type))
        (class (cadr type)))))

(defun class-eq-type (class)
  (specializer-type (class-eq-specializer class)))

;;; internal to this file..
;;;
;;; These functions are a pale imitation of their namesake. They accept
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
                 (ensure-list type))
               `(,type))))
        ((or (not (eq **boot-state** 'complete))
             (specializerp type))
         (specializer-type type))
        (t
         (error "~S is not a type." type))))

;;; internal to this file...
(defun convert-to-system-type (type)
  (case (car type)
    ((not and or) `(,(car type) ,@(mapcar #'convert-to-system-type
                                          (cdr type))))
    ((class class-eq) ; class-eq is impossible to do right
     (wrapper-classoid (class-wrapper (cadr type))))
    (eql type)
    (t (if (null (cdr type))
           (car type)
           type))))

;;; Writing the missing NOT and AND clauses will improve the quality
;;; of code generated by GENERATE-DISCRIMINATION-NET, but calling
;;; SUBTYPEP in place of just returning (VALUES NIL NIL) can be very
;;; slow. *SUBTYPEP is used by PCL itself, and must be fast.
;;;
;;; FIXME: SB-KERNEL has fast-and-not-quite-precise type code for use
;;; in the compiler. Could we share some of it here?
(defvar *in-*subtypep* nil)

(defun *subtypep (type1 type2)
  (if (equal type1 type2)
      (values t t)
      (if (eq **boot-state** 'early)
          (values (eq type1 type2) t)
          (let ((*in-*subtypep* t))
            (setq type1 (*normalize-type type1))
            (setq type2 (*normalize-type type2))
            (case (car type2)
              (not
               (values nil nil)) ; XXX We should improve this.
              (and
               (values nil nil)) ; XXX We should improve this.
              ((eql wrapper-eq class-eq class)
               (multiple-value-bind (app-p maybe-app-p)
                   (specializer-applicable-using-type-p type2 type1)
                 (values app-p (or app-p (not maybe-app-p)))))
              (t
               (subtypep (convert-to-system-type type1)
                         (convert-to-system-type type2))))))))

(defun make-class-symbol (class-name)
  (format-symbol #.(find-package "SB-PCL")
                 "*THE-CLASS-~A*" (symbol-name class-name)))

(defvar *standard-method-combination*)
(defvar *or-method-combination*)

(defun plist-value (object name)
  (getf (object-plist object) name))

(defun (setf plist-value) (new-value object name)
  (if new-value
      (setf (getf (object-plist object) name) new-value)
      (progn
        (remf (object-plist object) name)
        nil)))

;;;; built-in classes

;;; Grovel over SB-KERNEL::*BUILTIN-CLASSOIDS* in order to set
;;; SB-PCL:*BUILT-IN-CLASSES*.
(/show "about to set up SB-PCL::*BUILT-IN-CLASSES*")
(define-load-time-global *built-in-classes*
    (macrolet
        ((frob ()
           (labels ((direct-supers (class)
                      (/noshow "entering DIRECT-SUPERS" (classoid-name class))
                      (if (typep class 'built-in-classoid)
                          (built-in-classoid-direct-superclasses class)
                          (let ((inherits (wrapper-inherits (classoid-wrapper class))))
                            (/noshow inherits)
                            (list (svref inherits (1- (length inherits)))))))
                    (direct-subs (class)
                      (/noshow "entering DIRECT-SUBS" (classoid-name class))
                      (collect ((res))
                        (let ((subs (classoid-subclasses class)))
                          (/noshow subs)
                          (when subs
                            (sb-kernel::do-subclassoids ((sub wrapper) class)
                              (declare (ignore wrapper))
                              (/noshow sub)
                              (when (member class (direct-supers sub) :test #'eq)
                                (res sub)))))
                        (res))))
             `(list ,@(mapcar (lambda (kernel-bic-entry)
                                (/noshow "setting up" kernel-bic-entry)
                                (let* ((name (car kernel-bic-entry))
                                       (class (find-classoid name)))
                                  (/noshow name class)
                                  `(list ',name
                                         ',(mapcar #'classoid-name (direct-supers class))
                                         ',(mapcar #'classoid-name (direct-subs class))
                                         ',(map 'list #'wrapper-classoid-name
                                                (reverse (wrapper-inherits (classoid-wrapper class))))
                                         ,(getf (cdr kernel-bic-entry) :prototype-form))))
                            (remove-if (lambda (kernel-bic-entry)
                                         (member (first kernel-bic-entry)
                                                 ;; remove special classes (T and our
                                                 ;; SYSTEM-CLASSes) from the
                                                 ;; BUILT-IN-CLASS list
                                                 '(t function stream sequence
                                                   file-stream string-stream
                                                   slot-object)))
                                       sb-kernel::*builtin-classoids*))))))
      (frob)))
(/noshow "done setting up SB-PCL::*BUILT-IN-CLASSES*")

;;;; the classes that define the kernel of the metabraid

(defclass t () ()
  ;; AMOP specifies that the class named T should be an instance of
  ;; BUILT-IN-CLASS, but this conflicts with other specifications in
  ;; AMOP and CLHS.
  (:metaclass system-class))

(defclass function (t) ()
  (:metaclass system-class))

(defclass stream (t) ()
  (:metaclass system-class))

(defclass file-stream (stream) ()
  (:metaclass system-class))

(defclass string-stream (stream) ()
  (:metaclass system-class))

(defclass sequence (t) ()
  (:metaclass system-class))

(defclass slot-object (t) ()
  (:metaclass slot-class))

(defclass condition (slot-object) ()
  (:metaclass condition-class))

(defclass structure-object (slot-object) ()
  (:metaclass structure-class))

(defclass standard-object (slot-object) ())

(defclass funcallable-standard-object (function standard-object)
  ()
  (:metaclass funcallable-standard-class))

(defclass metaobject (standard-object) ())

(defclass generic-function (dependent-update-mixin
                            definition-source-mixin
                            metaobject
                            funcallable-standard-object)
  ((%documentation :initform nil :initarg :documentation)
   ;; We need to make a distinction between the methods initially set
   ;; up by :METHOD options to DEFGENERIC and the ones set up later by
   ;; DEFMETHOD, because ANSI specifies that executing DEFGENERIC on
   ;; an already-DEFGENERICed function clears the methods set by the
   ;; previous DEFGENERIC, but not methods set by DEFMETHOD. (Making
   ;; this distinction seems a little kludgy, but it has the positive
   ;; effect of making it so that loading a file a.lisp containing
   ;; DEFGENERIC, then loading a second file b.lisp containing
   ;; DEFMETHOD, then modifying and reloading a.lisp and/or b.lisp
   ;; tends to leave the generic function in a state consistent with
   ;; the most-recently-loaded state of a.lisp and b.lisp.)
   (initial-methods :initform () :accessor generic-function-initial-methods)
   (encapsulations :initform () :accessor generic-function-encapsulations))
  (:metaclass funcallable-standard-class))

(defclass standard-generic-function (generic-function)
  ((name
    :initform nil
    :initarg :name
    :reader generic-function-name)
   (methods
    :initform ()
    :accessor generic-function-methods
    :type list)
   (method-class
    :initarg :method-class
    :accessor generic-function-method-class)
   (%method-combination
    :initarg :method-combination
    :accessor generic-function-method-combination)
   (declarations
    ;; KLUDGE: AMOP specifies :DECLARATIONS, while ANSI specifies
    ;; :DECLARE.  Allow either (but FIXME: maybe a note or a warning
    ;; might be appropriate).
    :initarg :declarations
    :initarg :declare
    :initform ()
    :accessor generic-function-declarations)
   (arg-info
    :initform (make-arg-info)
    :reader gf-arg-info)
   (dfun-state
    :initform ()
    :accessor gf-dfun-state)
   ;; Used to make DFUN-STATE & FIN-FUNCTION updates atomic.
   (%lock
    :initform (sb-thread:make-mutex :name "GF lock")
    :reader gf-lock))
  (:metaclass funcallable-standard-class)
  (:default-initargs :method-class *the-class-standard-method*
                     :method-combination *standard-method-combination*))

(defclass method (metaobject) ())

(defclass standard-method (plist-mixin definition-source-mixin method)
  ((%generic-function :initform nil :accessor method-generic-function)
   (qualifiers :initform () :initarg :qualifiers :reader method-qualifiers)
   (specializers :initform () :initarg :specializers
                 :reader method-specializers)
   (lambda-list :initform () :initarg :lambda-list :reader method-lambda-list)
   (%function :initform nil :initarg :function :reader method-function)
   (%documentation :initform nil :initarg :documentation)
   (%cache :initform nil :accessor method-em-cache)
   ;; True IFF method is known to have no CALL-NEXT-METHOD in it, or
   ;; just a plain (CALL-NEXT-METHOD).
   (simple-next-method-call
    :initform nil
    :initarg simple-next-method-call
    :reader simple-next-method-call-p)))

(defclass accessor-method (standard-method)
  ((slot-name :initform nil :initarg :slot-name
              :reader accessor-method-slot-name)))

(defclass standard-accessor-method (accessor-method)
  ((%slot-definition :initform nil :initarg :slot-definition
                     :reader accessor-method-slot-definition)))

(defclass standard-reader-method (standard-accessor-method) ())
(defclass standard-writer-method (standard-accessor-method) ())
;;; an extension, apparently.
(defclass standard-boundp-method (standard-accessor-method) ())

;;; for (SLOT-VALUE X 'FOO) / ACCESSOR-SLOT-VALUE optimization, which
;;; can't be STANDARD-READER-METHOD because there is no associated
;;; slot definition.
(defclass global-reader-method (accessor-method) ())
(defclass global-writer-method (accessor-method) ())
(defclass global-boundp-method (accessor-method) ())

(defclass method-combination (metaobject)
  ((%documentation :initform nil :initarg :documentation)))

(defun make-gf-hash-table ()
  (make-hash-table :test 'eq
                   :hash-function #'fsc-instance-hash ; stable hash
                   :weakness :key
                   :synchronized t))

(defclass standard-method-combination (definition-source-mixin
                                       method-combination)
  ((type-name :reader method-combination-type-name :initarg :type-name)
   (options :reader method-combination-options :initarg :options)
   (%generic-functions :initform (make-gf-hash-table)
                       :reader method-combination-%generic-functions)))

(defclass long-method-combination (standard-method-combination)
  ((function :initarg :function :reader long-method-combination-function)
   (args-lambda-list :initarg :args-lambda-list
                     :reader long-method-combination-args-lambda-list)))

(defclass short-method-combination (standard-method-combination)
  ((operator :reader short-combination-operator :initarg :operator)
   (identity-with-one-argument :reader short-combination-identity-with-one-argument
    :initarg :identity-with-one-argument)))

(defclass slot-definition (metaobject definition-source-mixin)
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
   (initargs
    :initform nil
    :initarg :initargs
    :accessor slot-definition-initargs)
   (%type :initform t :initarg :type :accessor slot-definition-type)
   (%documentation
    :initform nil :initarg :documentation
    ;; KLUDGE: we need a reader for bootstrapping purposes, in
    ;; COMPUTE-EFFECTIVE-SLOT-DEFINITION-INITARGS.
    :reader %slot-definition-documentation)
   (%class :initform nil :initarg :class :accessor slot-definition-class)))

(defclass standard-slot-definition (slot-definition)
  ((allocation
    :initform :instance
    :initarg :allocation
    :accessor slot-definition-allocation)
   (allocation-class
    :initform nil
    :initarg :allocation-class
    :accessor slot-definition-allocation-class)))

(defclass condition-slot-definition (slot-definition)
  ((allocation
    :initform :instance
    :initarg :allocation
    :accessor slot-definition-allocation)
   (allocation-class
    :initform nil
    :initarg :allocation-class
    :accessor slot-definition-allocation-class)))

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
  ((readers
    :initform nil
    :initarg :readers
    :accessor slot-definition-readers)
   (writers
    :initform nil
    :initarg :writers
    :accessor slot-definition-writers)))

(defclass effective-slot-definition (slot-definition)
  ((accessor-flags
    :initform 0)
   (info
    :accessor slot-definition-info)))

;;; We use a structure here, because fast slot-accesses to this information
;;; are critical to making SLOT-VALUE-USING-CLASS &co fast: places that need
;;; these functions can access the SLOT-INFO directly, avoiding the overhead
;;; of accessing a standard-instance.
(defstruct (slot-info
            (:copier nil)
            (:constructor make-slot-info
                (&key slotd typecheck allocation location
                 (reader (uninitialized-accessor-function :reader slotd))
                 (writer (uninitialized-accessor-function :writer slotd))
                 (boundp (uninitialized-accessor-function :boundp slotd)))))
  (typecheck nil :type (or null function))
  (allocation nil)
  (location nil)
  (reader (missing-arg) :type function)
  (writer (missing-arg) :type function)
  (boundp (missing-arg) :type function))
(declaim (freeze-type slot-info))

(defclass standard-direct-slot-definition (standard-slot-definition
                                           direct-slot-definition)
  ())

(defclass standard-effective-slot-definition (standard-slot-definition
                                              effective-slot-definition)
  ((location ; nil, a fixnum, a cons: (slot-name . value)
    :initform nil
    :accessor slot-definition-location)))

(defclass condition-direct-slot-definition (condition-slot-definition
                                            direct-slot-definition)
  ())

(defclass condition-effective-slot-definition (condition-slot-definition
                                               effective-slot-definition)
  ())

(defclass structure-direct-slot-definition (structure-slot-definition
                                            direct-slot-definition)
  ())

(defclass structure-effective-slot-definition (structure-slot-definition
                                               effective-slot-definition)
  ())

(defclass specializer (metaobject)
  ;; KLUDGE: in sbcl-0.9.10.2 this was renamed from TYPE, which was an
  ;; external symbol of the CL package and hence potentially collides
  ;; with user code.  Renaming this to %TYPE, however, is the coward's
  ;; way out, because the objects that PCL puts in this slot aren't
  ;; (quite) types: they are closer to kinds of specializer.  However,
  ;; the wholesale renaming and disentangling of specializers didn't
  ;; appeal.  (See also message <sqd5hrclb2.fsf@cam.ac.uk> and
  ;; responses in comp.lang.lisp).  -- CSR, 2006-02-27
  ((%type :initform nil :reader specializer-type)))

;;; STANDARD in this name doesn't mean "blessed by a standard" but
;;; "comes as standard with PCL"; that is, it includes CLASS-EQ
;;; and vestiges of PROTOTYPE specializers
(defclass standard-specializer (specializer) ())

;;; Note that this class cannot define the OBJECT slot and
;;; SPECIALIZER-OBJECT reader because of bootstrapping limitations.
(defclass specializer-with-object (specializer) ())

(defclass exact-class-specializer (specializer) ())

(defclass class-eq-specializer (standard-specializer
                                exact-class-specializer
                                specializer-with-object)
  ((object :initarg :class
           :reader specializer-class
           :reader specializer-object)))

(defclass class-prototype-specializer (standard-specializer
                                       specializer-with-object)
  ((object :initarg :class
           :reader specializer-class
           :reader specializer-object)))

(defclass eql-specializer (standard-specializer
                           exact-class-specializer
                           specializer-with-object)
  ((object :initarg :object :reader specializer-object
           :reader eql-specializer-object)
   ;; created on demand (if and when needed), the CTYPE is the representation
   ;; of this metaobject as an internalized type object understood by the
   ;; kernel's type machinery. The CLOS object is really just a MEMBER-TYPE,
   ;; but the type system doesn't know that.
   (sb-kernel:ctype)
   ;; Because EQL specializers are interned, any two putative instances
   ;; of EQL-specializer referring to the same object are in fact EQ to
   ;; each other. Therefore a list of direct methods in the specializer can
   ;; reliably track all methods that are specialized on the identical object.
   ;; FIXME: explain why this is a cons of two NILs.
   (direct-methods :initform (cons nil nil))))

;; Why is this weak-value, not weak-key: suppose the value is unreachable (dead)
;; but the key is reachable - this should allow dropping the entry, because
;; you're indifferent to getting a fresh EQL specializer if you re-intern the
;; same key. There's no way to know that you got a new VALUE, since the
;; pre-condition of this case was that the old value was unreachable.
;; KEY weakness is actually equivalent to KEY-OR-VALUE weakness, which would
;; be less likely to drop the entry. The equivalence stems from the fact that
;; holding the value (the specializer) also holds the key.
;; Whereas, with :VALUE weakness, you can drop the specializer as soon as
;; nothing needs it, even if OBJECT persists. You might think that calling
;; gethash on a live key should get the identical specializer, but since
;; nothing referenced the old specializer, consing a new one is fine.
(defglobal *eql-specializer-table*
  (sb-impl::make-system-hash-table :test 'eql :weakness :value :synchronized nil))

(defun intern-eql-specializer (object)
  ;; Avoid style-warning about compiler-macro being unavailable.
  (declare (notinline make-instance))
  (with-system-mutex ((hash-table-lock *eql-specializer-table*))
    (ensure-gethash object *eql-specializer-table*
                    (make-instance 'eql-specializer :object object))))

(defclass class (dependent-update-mixin
                 definition-source-mixin
                 standard-specializer)
  ((name
    :initform nil
    :initarg :name
    :reader class-name)
   (class-eq-specializer
    :initform nil
    :reader class-eq-specializer)
   (direct-superclasses
    :initform ()
    :reader class-direct-superclasses)
   ;; Note: The (CLASS-)DIRECT-SUBCLASSES for STRUCTURE-CLASSes and
   ;; CONDITION-CLASSes are lazily computed whenever the subclass info
   ;; becomes available, i.e. when the PCL class is created.
   (direct-subclasses
    :initform ()
    :reader class-direct-subclasses)
   (direct-methods
    :initform (cons nil nil))
   (%documentation
    :initform nil
    :initarg :documentation)
   ;; True if the class definition was compiled with a (SAFETY 3)
   ;; optimization policy.
   (safe-p
    :initform nil
    :initarg safe-p
    :accessor safe-p)
   (finalized-p
    :initform nil
    :reader class-finalized-p)))

;;; The class PCL-CLASS is an implementation-specific common
;;; superclass of all specified subclasses of the class CLASS.
(defclass pcl-class (class)
  ((%class-precedence-list
    :reader class-precedence-list)
   ;; KLUDGE: see note in CPL-OR-NIL
   (cpl-available-p
    :reader cpl-available-p
    :initform nil)
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
    :reader class-direct-slots)
   (slots
    :initform ()
    :reader class-slots)))

;;; The class STD-CLASS is an implementation-specific common
;;; superclass of the classes STANDARD-CLASS and
;;; FUNCALLABLE-STANDARD-CLASS.
(defclass std-class (slot-class)
  ())

(defclass standard-class (std-class)
  ()
  (:default-initargs
   :direct-superclasses (list *the-class-standard-object*)))

(defclass funcallable-standard-class (std-class)
  ()
  (:default-initargs
   :direct-superclasses (list *the-class-funcallable-standard-object*)))

(defclass forward-referenced-class (pcl-class) ())

(defclass system-class (pcl-class) ())

(defclass built-in-class (system-class) ())

(defclass condition-class (slot-class) ())

(defclass structure-class (slot-class)
  ((defstruct-form :initform () :accessor class-defstruct-form)
   (defstruct-constructor :initform nil :accessor class-defstruct-constructor)
   (from-defclass-p :initform nil :initarg :from-defclass-p)))

(defclass definition-source-mixin (standard-object)
  ((source
    :initform nil
    :reader definition-source
    :initarg source)))

(defclass plist-mixin (standard-object)
  ((plist :initform () :accessor object-plist :initarg plist)))

(defclass dependent-update-mixin (plist-mixin) ())

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

;;;; DEFCLASS macro and close personal friends

;;; state for the current DEFCLASS expansion
(defvar *initfunctions-for-this-defclass*)
(defvar *readers-for-this-defclass*)
(defvar *writers-for-this-defclass*)
(defvar *slot-names-for-this-defclass*)

;;; Like the DEFMETHOD macro, the expansion of the DEFCLASS macro is
;;; fixed. DEFCLASS always expands into a call to LOAD-DEFCLASS. Until
;;; the meta-braid is set up, LOAD-DEFCLASS has a special definition
;;; which simply collects all class definitions up, when the metabraid
;;; is initialized it is done from those class definitions.
;;;
;;; After the metabraid has been setup, and the protocol for defining
;;; classes has been defined, the real definition of LOAD-DEFCLASS is
;;; installed by the file std-class.lisp
(defmacro defclass (&environment env name direct-superclasses direct-slots &rest options)
  (let (*initfunctions-for-this-defclass*
        *readers-for-this-defclass* ;Truly a crock, but we got
        *writers-for-this-defclass* ;to have it to live nicely.
        *slot-names-for-this-defclass*)
    ;; FIXME: It would be nice to collect all errors from the
    ;; expansion of a defclass and signal them in a single go.
    (multiple-value-bind (metaclass canonical-options)
        (canonize-defclass-options name options)
    (let ((canonical-slots (canonize-defclass-slots name direct-slots env))
          ;; DEFSTRUCT-P should be true if the class is defined
          ;; with a metaclass STRUCTURE-CLASS, so that a DEFSTRUCT
          ;; is compiled for the class.
          (defstruct-p (and (eq **boot-state** 'complete)
                            (let ((mclass (find-class metaclass nil)))
                              (and mclass
                                   (*subtypep
                                    mclass
                                    *the-class-structure-class*))))))
      (let* ((defclass-form
              `(let ,(mapcar #'cdr *initfunctions-for-this-defclass*)
                 (load-defclass ',name
                                ',metaclass
                                ',direct-superclasses
                                (list ,@canonical-slots)
                                (list ,@(apply #'append
                                               (when defstruct-p
                                                 '(:from-defclass-p t))
                                                canonical-options))
                                ',*readers-for-this-defclass*
                                ',*writers-for-this-defclass*
                                ',*slot-names-for-this-defclass*
                                (sb-c:source-location)
                                ,@(and (safe-code-p env)
                                       '(t))))))
        (if defstruct-p
            (progn
              ;; FIXME: (YUK!) Why do we do this? Because in order
              ;; to make the defstruct form, we need to know what
              ;; the accessors for the slots are, so we need already
              ;; to have hooked into the CLOS machinery.
              ;;
              ;; There may be a better way to do this: it would
              ;; involve knowing enough about PCL to ask "what will
              ;; my slot names and accessors be"; failing this, we
              ;; currently just evaluate the whole kaboodle, and
              ;; then use CLASS-DIRECT-SLOTS. -- CSR, 2002-06-07
              (eval defclass-form)
              (let* ((include (or (and direct-superclasses
                                       (find-class (car direct-superclasses) nil))
                                  (and (not (eq name 'structure-object))
                                       *the-class-structure-object*)))
                     (defstruct-form (make-structure-class-defstruct-form
                                      name (class-direct-slots (find-class name))
                                      include)))
                `(progn
                   (eval-when (:compile-toplevel :load-toplevel :execute)
                     ,defstruct-form) ; really compile the defstruct-form
                   (eval-when (:compile-toplevel :load-toplevel :execute)
                     ,defclass-form))))
            `(progn
               ;; By telling the type system at compile time about
               ;; the existence of a class named NAME, we can avoid
               ;; various bogus warnings about "type isn't defined yet"
               ;; for code elsewhere in the same file which uses
               ;; the name of the type.
               ;;
               ;; We only need to do this at compile time, because
               ;; at load and execute time we write the actual
               ;; full-blown class, so the "a class of this name is
               ;; coming" note we write here would be irrelevant.
               (eval-when (:compile-toplevel)
                 (%compiler-defclass ',name
                                     ',*readers-for-this-defclass*
                                     ',*writers-for-this-defclass*
                                     ',*slot-names-for-this-defclass*))
               ,defclass-form)))))))

(defun canonize-defclass-options (class-name options)
  (maplist (lambda (sublist)
             (let ((option-name (first (pop sublist))))
               (when (member option-name sublist :key #'first :test #'eq)
                 (error 'simple-program-error
                        :format-control "Multiple ~S options in DEFCLASS ~S."
                        :format-arguments (list option-name class-name)))))
           options)
  (let (metaclass
        default-initargs
        documentation
        canonized-options)
      (dolist (option options)
        (unless (listp option)
          (error "~S is not a legal defclass option." option))
        (case (first option)
          (:metaclass
           (let ((maybe-metaclass (second option)))
             (unless (and maybe-metaclass (legal-class-name-p maybe-metaclass))
               (error 'simple-program-error
                      :format-control "~@<The value of the :metaclass option (~S) ~
                         is not a legal class name.~:@>"
                      :format-arguments (list maybe-metaclass)))
             (setf metaclass maybe-metaclass)))
          (:default-initargs
           (let (initargs arg-names)
             (doplist (key val) (cdr option)
               (when (member key arg-names :test #'eq)
                 (error 'simple-program-error
                        :format-control "~@<Duplicate initialization argument ~
                                           name ~S in :DEFAULT-INITARGS of ~
                                           DEFCLASS ~S.~:>"
                        :format-arguments (list key class-name)))
               (push key arg-names)
               (push ``(,',key ,',val ,,(make-initfunction val)) initargs))
             (setf default-initargs t)
             (push `(:direct-default-initargs (list ,@(nreverse initargs)))
                   canonized-options)))
          (:documentation
           (unless (stringp (second option))
             (error "~S is not a legal :documentation value" (second option)))
           (setf documentation t)
           (push `(:documentation ,(second option)) canonized-options))
          (otherwise
           (push `(',(car option) ',(cdr option)) canonized-options))))
      (unless default-initargs
        (push '(:direct-default-initargs nil) canonized-options))
      (values (or metaclass 'standard-class) (nreverse canonized-options))))

(defun canonize-defclass-slots (class-name slots env)
  (let (canonized-specs)
    (dolist (spec slots)
      (when (atom spec)
        (setf spec (list spec)))
      (when (and (cdr spec) (null (cddr spec)))
        (error 'simple-program-error
               :format-control "~@<in DEFCLASS ~S, the slot specification ~S ~
                                is invalid; the probable intended meaning may ~
                                be achieved by specifiying ~S instead.~:>"
               :format-arguments (list class-name spec
                                       `(,(car spec) :initform ,(cadr spec)))))
      (let* ((name (car spec))
             (plist (cdr spec))
             (readers ())
             (writers ())
             (initargs ())
             (others ())
             (unsupplied (list nil))
             (type t)
             (initform unsupplied))
        (check-slot-name-for-defclass name class-name env)
        (push name *slot-names-for-this-defclass*)
        (flet ((note-reader (x)
                 (unless (symbolp x)
                   (error 'simple-program-error
                          :format-control "Slot reader name ~S for slot ~S in ~
                                           DEFCLASS ~S is not a symbol."
                          :format-arguments (list x name class-name)))
                 (push x readers)
                 (push x *readers-for-this-defclass*))
               (note-writer (x)
                 (push x writers)
                 (push x *writers-for-this-defclass*)))
          (doplist (key val) plist
            (case key
              (:accessor (note-reader val) (note-writer `(setf ,val)))
              (:reader   (note-reader val))
              (:writer   (note-writer val))
              (:initarg
               (unless (symbolp val)
                 (error 'simple-program-error
                        :format-control "Slot initarg name ~S for slot ~S in ~
                                         DEFCLASS ~S is not a symbol."
                        :format-arguments (list val name class-name)))
               (push val initargs))
              (otherwise
               (when (member key '(:initform :allocation :type :documentation))
                 (when (eq key :initform)
                   (setf initform val))
                 (when (eq key :type)
                   (setf type val))
                 (when (get-properties others (list key))
                   (error 'simple-program-error
                          :format-control "Duplicate slot option ~S for slot ~
                                           ~S in DEFCLASS ~S."
                          :format-arguments (list key name class-name))))
               ;; For non-standard options multiple entries go in a list
               (push val (getf others key))))))
        ;; Unwrap singleton lists (AMOP 5.4.2)
        (do ((head others (cddr head)))
            ((null head))
          (unless (cdr (second head))
            (setf (second head) (car (second head)))))
        (let ((canon `(:name ',name :readers ',readers :writers ',writers
                             :initargs ',initargs ',others)))
          (push (if (eq initform unsupplied)
                    `(list* ,@canon)
                    `(list* :initfunction ,(make-initfunction initform)
                            ,@canon))
                canonized-specs))))
    (nreverse canonized-specs)))


(defun check-slot-name-for-defclass (name class-name env)
  (flet ((slot-name-illegal (reason)
           (error 'simple-program-error
                  :format-control
                  (format nil "~~@<In DEFCLASS ~~S, the slot name ~~S ~
                               is ~A.~~@:>" reason)
                  :format-arguments (list class-name name))))
    (cond ((not (symbolp name))
           (slot-name-illegal "not a symbol"))
          ((keywordp name)
           (slot-name-illegal "a keyword"))
          ((constantp name env)
           (slot-name-illegal "a constant"))
          ((member name *slot-names-for-this-defclass* :test #'eq)
           (error 'simple-program-error
                  :format-control "Multiple slots named ~S in DEFCLASS ~S."
                  :format-arguments (list name class-name))))))

(defun make-initfunction (initform)
  (cond ((or (eq initform t)
             (equal initform ''t))
         '(function constantly-t))
        ((or (eq initform nil)
             (equal initform ''nil))
         '(function constantly-nil))
        ((or (eql initform 0)
             (equal initform ''0))
         '(function constantly-0))
        (t
         (let ((entry (assoc initform *initfunctions-for-this-defclass*
                             :test #'equal)))
           (unless entry
             (setq entry (list initform
                               (gensym)
                               `(function (lambda ()
                                  (declare (optimize
                                            (sb-c:store-coverage-data 0)))
                                  ,initform))))
             (push entry *initfunctions-for-this-defclass*))
           (cadr entry)))))

(defun %compiler-defclass (name readers writers slots)
  ;; ANSI says (Macro DEFCLASS, section 7.7) that DEFCLASS, if it
  ;; "appears as a top level form, the compiler must make the class
  ;; name be recognized as a valid type name in subsequent
  ;; declarations (as for deftype) and be recognized as a valid class
  ;; name for defmethod parameter specializers and for use as the
  ;; :metaclass option of a subsequent defclass."
  (preinform-compiler-about-class-type name)
  (preinform-compiler-about-accessors readers writers slots))

(defun preinform-compiler-about-class-type (name)
  ;; Unless the type system already has an actual type attached to
  ;; NAME (in which case (1) writing a placeholder value over that
  ;; actual type as a compile-time side-effect would probably be a bad
  ;; idea and (2) anyway we don't need to modify it in order to make
  ;; NAME be recognized as a valid type name)
  (with-single-package-locked-error (:symbol name "proclaiming ~S as a class"))
  (unless (info :type :kind name)
    ;; Tell the compiler to expect a class with the given NAME, by
    ;; writing a kind of minimal placeholder type information. This
    ;; placeholder will be overwritten later when the class is defined.
    (setf (info :type :kind name) :forthcoming-defclass-type))
  (values))

(defun preinform-compiler-about-accessors (readers writers slots)
  (flet ((inform (names type &key key)
           (mapc (lambda (name)
                   (let ((name (if key (funcall key name) name)))
                     (when (eq (info :function :where-from name) :assumed)
                       (sb-c:proclaim-ftype name type :defined))))
                 names)))
    (let ((rtype (specifier-type '(function (t) t)))
          (wtype (specifier-type '(function (t t) t))))
      (inform readers rtype)
      (inform writers wtype)
      (inform slots rtype :key #'slot-reader-name)
      (inform slots rtype :key #'slot-boundp-name)
      (inform slots wtype :key #'slot-writer-name))))

;;; This is the early definition of LOAD-DEFCLASS. It just collects up
;;; all the class definitions in a list. Later, in braid1.lisp, these
;;; are actually defined.

;;; Each entry in *EARLY-CLASS-DEFINITIONS* is an EARLY-CLASS-DEFINITION.
(defparameter *early-class-definitions* ())

(defun early-class-definition (class-name)
  (or (find class-name *early-class-definitions* :key #'ecd-class-name)
      (error "~S is not a class in *early-class-definitions*." class-name)))

(defun make-early-class-definition
       (name source-location metaclass
        superclass-names canonical-slots other-initargs)
  (list 'early-class-definition
        name source-location metaclass
        superclass-names canonical-slots other-initargs))

(defun ecd-class-name        (ecd) (nth 1 ecd))
(defun ecd-source-location   (ecd) (nth 2 ecd))
(defun ecd-metaclass         (ecd) (nth 3 ecd))
(defun ecd-superclass-names  (ecd) (nth 4 ecd))
(defun ecd-canonical-slots   (ecd) (nth 5 ecd))
(defun ecd-other-initargs    (ecd) (nth 6 ecd))

(defvar *early-class-slots* nil)

(defun canonical-slot-name (canonical-slot)
  (getf canonical-slot :name))

(defun early-class-slots (class-name)
  (cdr (or (assoc class-name *early-class-slots*)
           (let ((a (cons class-name
                          (mapcar #'canonical-slot-name
                                  (early-collect-inheritance class-name)))))
             (push a *early-class-slots*)
             a))))

(defun early-class-size (class-name)
  (length (early-class-slots class-name)))

(defun early-collect-inheritance (class-name)
  ;;(declare (values slots cpl default-initargs direct-subclasses))
  (let ((cpl (early-collect-cpl class-name)))
    (values (early-collect-slots cpl)
            cpl
            (early-collect-default-initargs cpl)
            (let (collect)
              (dolist (definition *early-class-definitions*)
                (when (memq class-name (ecd-superclass-names definition))
                  (push (ecd-class-name definition) collect)))
              (nreverse collect)))))

(defun early-collect-slots (cpl)
  (let* ((definitions (mapcar #'early-class-definition cpl))
         (super-slots (mapcar #'ecd-canonical-slots definitions))
         (slots (apply #'append (reverse super-slots))))
    (dolist (s1 slots)
      (let ((name1 (canonical-slot-name s1)))
        (dolist (s2 (cdr (memq s1 slots)))
          (when (eq name1 (canonical-slot-name s2))
            (error "More than one early class defines a slot with the~%~
                    name ~S. This can't work because the bootstrap~%~
                    object system doesn't know how to compute effective~%~
                    slots."
                   name1)))))
    slots))

(defun early-collect-cpl (class-name)
  (labels ((walk (c)
             (let* ((definition (early-class-definition c))
                    (supers (ecd-superclass-names definition)))
               (cons c
                     (apply #'append (mapcar #'early-collect-cpl supers))))))
    (remove-duplicates (walk class-name) :from-end nil :test #'eq)))

(defun early-collect-default-initargs (cpl)
  (let ((default-initargs ()))
    (dolist (class-name cpl)
      (let* ((definition (early-class-definition class-name))
             (others (ecd-other-initargs definition)))
        (loop (when (null others) (return nil))
              (let ((initarg (pop others)))
                (unless (eq initarg :direct-default-initargs)
                 (error "~@<The defclass option ~S is not supported by ~
                        the bootstrap object system.~:@>"
                        initarg)))
              (setq default-initargs
                    (nconc default-initargs (reverse (pop others)))))))
    (reverse default-initargs)))

(defun !bootstrap-slot-index (class-name slot-name)
  (or (position slot-name (early-class-slots class-name))
      (error "~S not found" slot-name)))

;;; !BOOTSTRAP-GET-SLOT and !BOOTSTRAP-SET-SLOT are used to access and
;;; change the values of slots during bootstrapping. During
;;; bootstrapping, there are only two kinds of objects whose slots we
;;; need to access, CLASSes and SLOT-DEFINITIONs. The first argument
;;; to these functions tells whether the object is a CLASS or a
;;; SLOT-DEFINITION.
;;;
;;; Note that the way this works it stores the slot in the same place
;;; in memory that the full object system will expect to find it
;;; later. This is critical to the bootstrapping process, the whole
;;; changeover to the full object system is predicated on this.
;;;
;;; One important point is that the layout of standard classes and
;;; standard slots must be computed the same way in this file as it is
;;; by the full object system later.
(defmacro !bootstrap-get-slot (type object slot-name)
  `(clos-slots-ref (get-slots ,object)
                   (!bootstrap-slot-index ,type ,slot-name)))
(defun !bootstrap-set-slot (type object slot-name new-value)
  (setf (!bootstrap-get-slot type object slot-name) new-value))

(defun early-class-name (class)
  (!bootstrap-get-slot 'class class 'name))

(defun early-class-precedence-list (class)
  (!bootstrap-get-slot 'pcl-class class '%class-precedence-list))

(defun early-class-name-of (instance)
  (early-class-name (class-of instance)))

(defun early-class-slotds (class)
  (!bootstrap-get-slot 'slot-class class 'slots))

(defun early-slot-definition-name (slotd)
  (!bootstrap-get-slot 'standard-effective-slot-definition slotd 'name))

(defun early-slot-definition-location (slotd)
  (!bootstrap-get-slot 'standard-effective-slot-definition slotd 'location))

(defun early-slot-definition-info (slotd)
  (!bootstrap-get-slot 'standard-effective-slot-definition slotd 'info))

(defun early-accessor-method-slot-name (method)
  (!bootstrap-get-slot 'standard-accessor-method method 'slot-name))

(unless (fboundp 'class-name-of)
  (setf (symbol-function 'class-name-of)
        (symbol-function 'early-class-name-of)))
(unintern 'early-class-name-of)

(defun early-class-direct-subclasses (class)
  (!bootstrap-get-slot 'class class 'direct-subclasses))

(declaim (notinline load-defclass))
(defun load-defclass (name metaclass supers canonical-slots canonical-options
                      readers writers slot-names source-location &optional safe-p)
  ;; SAFE-P is used by REAL-LOAD-DEFCLASS, but can be ignored here, since
  ;; during the bootstrap we won't have (SAFETY 3).
  (declare (ignore safe-p))
  (%compiler-defclass name readers writers slot-names)
  (setq supers  (copy-tree supers)
        canonical-slots   (copy-tree canonical-slots)
        canonical-options (copy-tree canonical-options))
  (let ((ecd
         (make-early-class-definition name
                                      source-location
                                      metaclass
                                      supers
                                      canonical-slots
                                      canonical-options))
        (existing
         (find name *early-class-definitions* :key #'ecd-class-name)))
    (setq *early-class-definitions*
          (cons ecd (remove existing *early-class-definitions*)))
    ecd))

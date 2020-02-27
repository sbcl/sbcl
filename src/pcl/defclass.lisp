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

(declaim (global  *the-class-t*
                  *the-class-slot-object*
                  *the-class-structure-object*
                  *the-class-standard-object*
                  *the-class-function*
                  *the-class-funcallable-standard-object*
                  *the-class-system-class*
                  *the-class-slot-class*
                  *the-class-condition-class*
                  *the-class-structure-class*
                  *the-class-standard-class*
                  *the-class-funcallable-standard-class*
                  *the-class-forward-referenced-class*
                  *the-class-method*
                  *the-class-standard-method*
                  *the-class-standard-reader-method*
                  *the-class-standard-writer-method*
                  *the-class-standard-boundp-method*
                  *the-class-global-reader-method*
                  *the-class-global-writer-method*
                  *the-class-global-boundp-method*
                  *the-class-standard-generic-function*
                  *the-class-standard-direct-slot-definition*
                  *the-class-standard-effective-slot-definition*

                  *the-eslotd-standard-class-slots*
                  *the-eslotd-funcallable-standard-class-slots*))

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
  (check-class-name name nil)
  (let (*initfunctions-for-this-defclass*
        *readers-for-this-defclass* ;Truly a crock, but we got
        *writers-for-this-defclass* ;to have it to live nicely.
        *slot-names-for-this-defclass*)
    ;; FIXME: It would be nice to collect all errors from the
    ;; expansion of a defclass and signal them in a single go.
    (multiple-value-bind (metaclass canonical-options)
        (canonize-defclass-options name options)
      ;; Check deprecation status of direct superclasses and
      ;; metaclass.
      (mapc #'sb-int:check-deprecated-type direct-superclasses)
      (sb-int:check-deprecated-type metaclass)
      (let ((canonical-slots (canonize-defclass-slots name metaclass direct-slots env))
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
                   (sb-kernel::%compiler-defclass
                    ',name
                    ',*readers-for-this-defclass*
                    ',*writers-for-this-defclass*
                    ',*slot-names-for-this-defclass*))
                 ,defclass-form)))))))

(defun canonize-defclass-options (class-name options)
  (maplist (lambda (sublist)
             (let ((option-name (first (pop sublist))))
               (when (member option-name sublist :key #'first :test #'eq)
                 (%program-error "Multiple ~S options in DEFCLASS ~S."
                                 option-name class-name))))
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
               (%program-error "~@<The value of the :metaclass ~
                                option (~S) is not a legal class ~
                                name.~:@>"
                               maybe-metaclass))
             (setf metaclass maybe-metaclass)))
          (:default-initargs
           (let (initargs arg-names)
             (doplist (key val) (cdr option)
               (when (member key arg-names :test #'eq)
                 (%program-error "~@<Duplicate initialization argument ~
                                  name ~S in :DEFAULT-INITARGS of ~
                                  DEFCLASS ~S.~:>"
                                 key class-name))
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

(defun canonize-defclass-slot (class-name metaclass spec env)
  (let ((location (sb-c::make-definition-source-location))
        (spec (sb-int:ensure-list spec)))
    (when (and (cdr spec) (null (cddr spec)))
      (%program-error "~@<in DEFCLASS ~S, the slot specification ~S ~
                       is invalid; the probable intended meaning may ~
                       be achieved by specifiying ~S instead.~:>"
                      class-name spec `(,(car spec) :initform ,(cadr spec))))
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
                 (%program-error "Slot reader name ~S for slot ~S in ~
                                  DEFCLASS ~S is not a symbol."
                                 x name class-name))
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
               (%program-error "Slot initarg name ~S for slot ~S in ~
                                DEFCLASS ~S is not a symbol."
                               val name class-name))
             (push val initargs))
            ((:initform :type :allocation :documentation)
             (case key
               (:initform
                (setf initform val))
               (:type
                (when (eq metaclass 'standard-class)
                  (sb-kernel::check-slot-type-specifier
                   val name (cons 'defclass class-name))
                  (setf type val))))
             (when (get-properties others (list key))
               (%program-error "Duplicate slot option ~S for slot ~
                                ~S in DEFCLASS ~S."
                               key name class-name))
             (push val (getf others key)))
            (otherwise
             ;; For non-standard options multiple entries go in a list
             (push val (getf others key))))))
      ;; Unwrap singleton lists (AMOP 5.4.2)
      (do ((head others (cddr head)))
          ((null head))
        (unless (cdr (second head))
          (setf (second head) (car (second head)))))
      (let ((canon `(:name ',name :readers ',readers :writers ',writers
                     :initargs ',initargs  'source ,location ',others)))
        (if (eq initform unsupplied)
            `(list* ,@canon)
            `(list* :initfunction ,(make-initfunction initform type spec)
                    ,@canon))))))

(defun canonize-defclass-slots (class-name metaclass slots env)
  (map 'list (lambda (spec)
               (with-current-source-form (spec)
                 (canonize-defclass-slot class-name metaclass spec env)))
       slots))

(defun check-slot-name-for-defclass (name class-name env)
  (flet ((slot-name-illegal (reason)
           (%program-error "~@<In DEFCLASS ~S, the slot name ~S is ~
                            ~A.~@:>"
                           class-name name reason)))
    (cond ((not (symbolp name))
           (slot-name-illegal "not a symbol"))
          ((keywordp name)
           (slot-name-illegal "a keyword"))
          ((constantp name env)
           (slot-name-illegal "a constant"))
          ((member name *slot-names-for-this-defclass* :test #'eq)
           (%program-error "Multiple slots named ~S in DEFCLASS ~S."
                           name class-name)))))

(defun make-initfunction (initform &optional (type t)
                                             source-form)
  (cond ((and (or (eq initform t)
                  (equal initform ''t))
              (neq type t))
         '(function constantly-t))
        ((and (or (eq initform nil)
                  (equal initform ''nil))
              (neq type t))
         '(function constantly-nil))
        ((and (or (eql initform 0)
                  (equal initform ''0))
              (neq type t))
         '(function constantly-0))
        (t
         (let* ((initform (if (eq type t)
                              initform
                              `(sb-c::with-source-form ,source-form
                                (the* (,type :source-form ,initform
                                       ;; Don't want to insert a cast,
                                       ;; as a subclass may change the type,
                                       ;; just report this at compile-time.
                                             :use-annotations t)
                                      ,initform))))
                (entry (assoc initform *initfunctions-for-this-defclass*
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


;;; This is the early definition of LOAD-DEFCLASS. It just collects up
;;; all the class definitions in a list. Later, in braid1.lisp, these
;;; are actually defined.

;;; Each entry in *EARLY-CLASS-DEFINITIONS* is an EARLY-CLASS-DEFINITION.
(defparameter *!early-class-definitions* ())

(defun !early-class-definition (class-name)
  (or (find class-name *!early-class-definitions* :key #'ecd-class-name)
      (error "~S is not a class in *early-class-definitions*." class-name)))

(defun !make-early-class-definition
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

(defvar *!early-class-slots* nil)

(defun canonical-slot-name (canonical-slot)
  (getf canonical-slot :name))

(defun !early-class-slots (class-name)
  (cdr (or (assoc class-name *!early-class-slots*)
           (let ((a (cons class-name
                          (mapcar #'canonical-slot-name
                                  (!early-collect-inheritance class-name)))))
             (push a *!early-class-slots*)
             a))))

(defun !early-class-size (class-name)
  (length (!early-class-slots class-name)))

(defun !early-collect-inheritance (class-name)
  ;;(declare (values slots cpl default-initargs direct-subclasses))
  (let ((cpl (!early-collect-cpl class-name)))
    (values (!early-collect-slots cpl)
            cpl
            (!early-collect-default-initargs cpl)
            (let (collect)
              (dolist (definition *!early-class-definitions*)
                (when (memq class-name (ecd-superclass-names definition))
                  (push (ecd-class-name definition) collect)))
              (nreverse collect)))))

(defun !early-collect-slots (cpl)
  (let* ((definitions (mapcar #'!early-class-definition cpl))
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

(defun !early-collect-cpl (class-name)
  (labels ((walk (c)
             (let* ((definition (!early-class-definition c))
                    (supers (ecd-superclass-names definition)))
               (cons c
                     (apply #'append (mapcar #'!early-collect-cpl supers))))))
    (remove-duplicates (walk class-name) :from-end nil :test #'eq)))

(defun !early-collect-default-initargs (cpl)
  (let ((default-initargs ()))
    (dolist (class-name cpl)
      (let* ((definition (!early-class-definition class-name))
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
  (or (position slot-name (!early-class-slots class-name))
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

(defun early-class-precedence-list (class)
  (!bootstrap-get-slot 'pcl-class class '%class-precedence-list))

(defun early-class-slotds (class)
  (!bootstrap-get-slot 'slot-class class 'slots))

(defun early-slot-definition-name (slotd)
  (!bootstrap-get-slot 'standard-effective-slot-definition slotd 'name))

(defun early-slot-definition-location (slotd)
  (!bootstrap-get-slot 'standard-effective-slot-definition slotd 'location))

(defun early-slot-definition-info (slotd)
  (!bootstrap-get-slot 'standard-effective-slot-definition slotd 'info))

(defun early-class-direct-subclasses (class)
  (!bootstrap-get-slot 'class class 'direct-subclasses))

(declaim (notinline load-defclass))
(defun load-defclass (name metaclass supers canonical-slots canonical-options
                      readers writers slot-names source-location &optional safe-p)
  ;; SAFE-P is used by REAL-LOAD-DEFCLASS, but can be ignored here, since
  ;; during the bootstrap we won't have (SAFETY 3).
  (declare (ignore safe-p))
  (sb-kernel::%%compiler-defclass name readers writers slot-names)
  (let ((ecd (!make-early-class-definition name
                                          source-location
                                          metaclass
                                          (copy-tree supers)
                                          (copy-tree canonical-slots)
                                          (copy-tree canonical-options)))
        (existing
         (find name *!early-class-definitions* :key #'ecd-class-name)))
    (setq *!early-class-definitions*
          (cons ecd (remove existing *!early-class-definitions*)))
    ecd))

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

(in-package "SB-KERNEL")

(defun call-with-defining-class (kind name thunk)
  (declare (ignorable kind name))
  (with-single-package-locked-error
      (:symbol name "defining ~S as a ~(~A~)" kind)
    (funcall thunk)))

(defun preinform-compiler-about-class-type (name forthcoming-info)
  ;; Unless the type system already has an actual type attached to
  ;; NAME (in which case (1) writing a placeholder value over that
  ;; actual type as a compile-time side-effect would probably be a bad
  ;; idea and (2) anyway we don't need to modify it in order to make
  ;; NAME be recognized as a valid type name)
  (when (and forthcoming-info (not (info :type :kind name)))
    ;; Tell the compiler to expect a class with the given NAME, by
    ;; writing a kind of minimal placeholder type information. This
    ;; placeholder will be overwritten later when the class is
    ;; defined.
    (setf (info :type :kind name) :forthcoming-defclass-type)))

(symbol-macrolet
    ((reader-function-type (specifier-type '(function (t) t)))
     (writer-function-type (specifier-type '(function (t t) t))))
  (flet ((proclaim-ftype-for-name (kind name type)
           (ecase kind
             (condition
              (proclaim `(ftype ,(type-specifier type) ,name)))
             (class
              (when (eq (info :function :where-from name) :assumed)
                (sb-c:proclaim-ftype name type nil :defined))))))

    (defun preinform-compiler-about-accessors (kind readers writers)
      (flet ((inform (names type)
               (mapc (lambda (name) (proclaim-ftype-for-name kind name type))
                     names)))
        (inform readers reader-function-type)
        (inform writers writer-function-type)))

    (defun preinform-compiler-about-slot-functions (kind slots)
      (flet ((inform (slots key type)
               (mapc (lambda (slot)
                       (let ((name (funcall key slot)))
                         (proclaim-ftype-for-name kind name type)))
                     slots)))
        (inform slots #'sb-pcl::slot-reader-name reader-function-type)
        (inform slots #'sb-pcl::slot-boundp-name reader-function-type)
        (inform slots #'sb-pcl::slot-makunbound-name reader-function-type)
        (inform slots #'sb-pcl::slot-writer-name writer-function-type)))))

(defun %%compiler-defclass (name readers writers slots)
  ;; ANSI says (Macro DEFCLASS, section 7.7) that DEFCLASS, if it
  ;; "appears as a top level form, the compiler must make the class
  ;; name be recognized as a valid type name in subsequent
  ;; declarations (as for deftype) and be recognized as a valid class
  ;; name for defmethod parameter specializers and for use as the
  ;; :metaclass option of a subsequent defclass."
  (preinform-compiler-about-class-type name t)
  (preinform-compiler-about-accessors 'class readers writers)
  (preinform-compiler-about-slot-functions 'class slots))

(defun %compiler-defclass (name readers writers slots)
  (call-with-defining-class
   'class name
   (lambda ()
     (%%compiler-defclass name readers writers slots))))

(in-package "SB-PCL")

;;;; DEFCLASS macro and close personal friends

;;; state for the current DEFCLASS expansion
(defvar *initfunctions-for-this-defclass*)
(defvar *readers-for-this-defclass*)
(defvar *writers-for-this-defclass*)
(defvar *slot-names-for-this-defclass*)

;; forward declarations so the host doesn't warn these to be undefined functions.
(declaim (ftype (function (t t) (values t t &optional)) *subtypep))
(declaim (ftype function class-direct-slots))
(declaim (ftype (function (t t t) (values cons t t t &optional)) make-structure-class-defstruct-form))

;;; Like the DEFMETHOD macro, the expansion of the DEFCLASS macro is
;;; fixed. DEFCLASS always expands into a call to LOAD-DEFCLASS. Until
;;; the meta-braid is set up, LOAD-DEFCLASS has a special definition
;;; which simply collects all class definitions up, when the metabraid
;;; is initialized it is done from those class definitions.
;;;
;;; After the metabraid has been setup, and the protocol for defining
;;; classes has been defined, the real definition of LOAD-DEFCLASS is
;;; installed by the file std-class.lisp
(sb-xc:defmacro defclass (&environment env name direct-superclasses direct-slots &rest options)
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
                  (when (sb-kernel::check-slot-type-specifier
                         val name (cons 'defclass class-name))
                    (setf type val)))))
             (when (get-properties others (list key))
               (%program-error "Duplicate slot option ~S for slot ~
                                ~S in DEFCLASS ~S."
                               key name class-name))
             (push val (getf others key)))
            ;; While any option can be passed to slot definitions, MOP
            ;; says :INITARGs are passed as a list to :INITARGS for
            ;; slot-definitions, so this will be just shadowed.
            (:initargs
             (warn "~s can't be used as a slot option" key))
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
              (eq type t))
         '(function constantly-t))
        ((and (or (eq initform nil)
                  (equal initform ''nil))
              (eq type t))
         '(function constantly-nil))
        ((and (or (eql initform 0)
                  (equal initform ''0))
              (eq type t))
         '(function constantly-0))
        (t
         (let* ((initform `(sb-c::with-source-form ,source-form
                             (the* (,type :source-form ,initform
                                    ;; Don't want to insert a cast,
                                    ;; as a subclass may change the type,
                                    ;; just report this at compile-time.
                                          :use-annotations t)
                                   ,initform)))
                (entry (assoc initform *initfunctions-for-this-defclass*
                              :test #'equal)))
           (unless entry
             (setq entry (list initform
                               (gensym)
                               `(function
                                 (lambda ()
                                  (declare (optimize (sb-c:store-coverage-data 0)
                                                     (sb-c:verify-arg-count 0)))
                                  ,initform))))
             (push entry *initfunctions-for-this-defclass*))
           (cadr entry)))))

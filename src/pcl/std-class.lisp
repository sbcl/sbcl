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

(defmethod slot-accessor-function ((slotd effective-slot-definition) type)
  (ecase type
    (reader (slot-definition-reader-function slotd))
    (writer (slot-definition-writer-function slotd))
    (boundp (slot-definition-boundp-function slotd))))

(defmethod (setf slot-accessor-function) (function
                                          (slotd effective-slot-definition)
                                          type)
  (ecase type
    (reader (setf (slot-definition-reader-function slotd) function))
    (writer (setf (slot-definition-writer-function slotd) function))
    (boundp (setf (slot-definition-boundp-function slotd) function))))

(defconstant +slotd-reader-function-std-p+ 1)
(defconstant +slotd-writer-function-std-p+ 2)
(defconstant +slotd-boundp-function-std-p+ 4)
(defconstant +slotd-all-function-std-p+ 7)

(defmethod slot-accessor-std-p ((slotd effective-slot-definition) type)
  (let ((flags (slot-value slotd 'accessor-flags)))
    (declare (type fixnum flags))
    (if (eq type 'all)
        (eql +slotd-all-function-std-p+ flags)
        (let ((mask (ecase type
                      (reader +slotd-reader-function-std-p+)
                      (writer +slotd-writer-function-std-p+)
                      (boundp +slotd-boundp-function-std-p+))))
          (declare (type fixnum mask))
          (not (zerop (the fixnum (logand mask flags))))))))

(defmethod (setf slot-accessor-std-p) (value
                                       (slotd effective-slot-definition)
                                       type)
  (let ((mask (ecase type
                (reader +slotd-reader-function-std-p+)
                (writer +slotd-writer-function-std-p+)
                (boundp +slotd-boundp-function-std-p+)))
        (flags (slot-value slotd 'accessor-flags)))
    (declare (type fixnum mask flags))
    (setf (slot-value slotd 'accessor-flags)
          (if value
              (the fixnum (logior mask flags))
              (the fixnum (logand (the fixnum (lognot mask)) flags)))))
  value)

(defmethod initialize-internal-slot-functions
    ((slotd effective-slot-definition))
  (let* ((name (slot-value slotd 'name))
         (class (slot-value slotd '%class)))
    (dolist (type '(reader writer boundp))
      (let* ((gf-name (ecase type
                              (reader 'slot-value-using-class)
                              (writer '(setf slot-value-using-class))
                              (boundp 'slot-boundp-using-class)))
             (gf (gdefinition gf-name)))
        ;; KLUDGE: this logic is cut'n'pasted from
        ;; GET-ACCESSOR-METHOD-FUNCTION, which (for STD-CLASSes) is
        ;; only called later, because it does things that can't be
        ;; computed this early in class finalization; however, we need
        ;; this bit as early as possible.  -- CSR, 2009-11-05
        (setf (slot-accessor-std-p slotd type)
              (let* ((std-method (standard-svuc-method type))
                     (str-method (structure-svuc-method type))
                     (types1 `((eql ,class) (class-eq ,class) (eql ,slotd)))
                     (types (if (eq type 'writer) `(t ,@types1) types1))
                     (methods (compute-applicable-methods-using-types gf types)))
                (null (cdr methods))))
        (setf (slot-accessor-function slotd type)
              (lambda (&rest args)
                ;; FIXME: a tiny amount of wasted SLOT-ACCESSOR-STD-P
                ;; work here (see KLUDGE comment above).
                (let ((fun (compute-slot-accessor-info slotd type gf)))
                  (apply fun args))))))))

(defmethod finalize-internal-slot-functions ((slotd effective-slot-definition))
  (let* ((name (slot-value slotd 'name)))
    (dolist (type '(reader writer boundp))
      (let* ((gf-name (ecase type
                              (reader 'slot-value-using-class)
                              (writer '(setf slot-value-using-class))
                              (boundp 'slot-boundp-using-class)))
             (gf (gdefinition gf-name)))
        (compute-slot-accessor-info slotd type gf)))))

;;; CMUCL (Gerd PCL 2003-04-25) comment:
;;;
;;; Compute an effective method for SLOT-VALUE-USING-CLASS, (SETF
;;; SLOT-VALUE-USING-CLASS) or SLOT-BOUNDP-USING-CLASS for reading/
;;; writing/testing effective slot SLOTD.
;;;
;;; TYPE is one of the symbols READER, WRITER or BOUNDP, depending on
;;; GF.  Store the effective method in the effective slot definition
;;; object itself; these GFs have special dispatch functions calling
;;; effective methods directly retrieved from effective slot
;;; definition objects, as an optimization.
;;;
;;; FIXME: Change the function name to COMPUTE-SVUC-SLOTD-FUNCTION,
;;; or some such.
(defmethod compute-slot-accessor-info ((slotd effective-slot-definition)
                                       type gf)
  (let* ((name (slot-value slotd 'name))
         (class (slot-value slotd '%class)))
    (multiple-value-bind (function std-p)
        (if (eq **boot-state** 'complete)
            (get-accessor-method-function gf type class slotd)
            (get-optimized-std-accessor-method-function class slotd type))
      (setf (slot-accessor-std-p slotd type) std-p)
      (setf (slot-accessor-function slotd type) function))))

(defmethod slot-definition-allocation ((slotd structure-slot-definition))
  :instance)

;;;; various class accessors that are a little more complicated than can be
;;;; done with automatically generated reader methods

(defmethod class-prototype :before (class)
  (unless (class-finalized-p class)
    (error "~@<~S is not finalized.~:@>" class)))

;;; KLUDGE: For some reason factoring the common body into a function
;;; breaks PCL bootstrapping, so just generate it with a macrolet for
;;; all.
(macrolet ((def (class)
             `(defmethod class-prototype ((class ,class))
                (with-slots (prototype) class
                  (or prototype
                      (setf prototype (allocate-instance class)))))))
  (def std-class)
  (def condition-class)
  (def structure-class))

(defmethod class-direct-default-initargs ((class slot-class))
  (plist-value class 'direct-default-initargs))

(defmethod class-default-initargs ((class slot-class))
  (plist-value class 'default-initargs))

(defmethod class-slot-cells ((class std-class))
  (plist-value class 'class-slot-cells))
(defmethod (setf class-slot-cells) (new-value (class std-class))
  (setf (plist-value class 'class-slot-cells) new-value))

;;;; class accessors that are even a little bit more complicated than those
;;;; above. These have a protocol for updating them, we must implement that
;;;; protocol.

;;; Maintaining the direct subclasses backpointers. The update methods are
;;; here, the values are read by an automatically generated reader method.
(defmethod add-direct-subclass ((class class) (subclass class))
  (with-slots (direct-subclasses) class
    (pushnew subclass direct-subclasses :test #'eq)
    subclass))
(defmethod remove-direct-subclass ((class class) (subclass class))
  (with-slots (direct-subclasses) class
    (setq direct-subclasses (remove subclass direct-subclasses))
    subclass))

;;; Maintaining the direct-methods and direct-generic-functions backpointers.
;;;
;;; There are four generic functions involved, each has one method for the
;;; class case and another method for the damned EQL specializers. All of
;;; these are specified methods and appear in their specified place in the
;;; class graph.
;;;
;;;   ADD-DIRECT-METHOD
;;;   REMOVE-DIRECT-METHOD
;;;   SPECIALIZER-DIRECT-METHODS
;;;   SPECIALIZER-DIRECT-GENERIC-FUNCTIONS
;;;
;;; In each case, we maintain one value which is a cons. The car is the list
;;; methods. The cdr is a list of the generic functions. The cdr is always
;;; computed lazily.

;;; This needs to be used recursively, in case a non-trivial user
;;; defined ADD/REMOVE-DIRECT-METHOD method ends up calling another
;;; function using the same lock.
(defvar *specializer-lock* (sb-thread::make-spinlock :name "Specializer lock"))

(defmethod add-direct-method :around ((specializer specializer) method)
  ;; All the actions done under this lock are done in an order
  ;; that is safe to unwind at any point.
  (sb-thread::with-recursive-system-spinlock (*specializer-lock*)
    (call-next-method)))

(defmethod remove-direct-method :around ((specializer specializer) method)
  ;; All the actions done under this lock are done in an order
  ;; that is safe to unwind at any point.
  (sb-thread::with-recursive-system-spinlock (*specializer-lock*)
    (call-next-method)))

(defmethod add-direct-method ((specializer class) (method method))
  (let ((cell (slot-value specializer 'direct-methods)))
    ;; We need to first smash the CDR, because a parallel read may
    ;; be in progress, and because if an interrupt catches us we
    ;; need to have a consistent state.
    (setf (cdr cell) ()
          (car cell) (adjoin method (car cell) :test #'eq)))
  method)

(defmethod remove-direct-method ((specializer class) (method method))
  (let ((cell (slot-value specializer 'direct-methods)))
    ;; We need to first smash the CDR, because a parallel read may
    ;; be in progress, and because if an interrupt catches us we
    ;; need to have a consistent state.
    (setf (cdr cell) ()
          (car cell) (remove method (car cell))))
  method)

(defmethod specializer-direct-methods ((specializer class))
  (with-slots (direct-methods) specializer
    (car direct-methods)))

(defmethod specializer-direct-generic-functions ((specializer class))
  (let ((cell (slot-value specializer 'direct-methods)))
    ;; If an ADD/REMOVE-METHOD is in progress, no matter: either
    ;; we behave as if we got just first or just after -- it's just
    ;; for update that we need to lock.
    (or (cdr cell)
        (sb-thread::with-spinlock (*specializer-lock*)
          (setf (cdr cell)
                (let (collect)
                  (dolist (m (car cell))
                    ;; the old PCL code used COLLECTING-ONCE which used
                    ;; #'EQ to check for newness
                    (pushnew (method-generic-function m) collect :test #'eq))
                  (nreverse collect)))))))

;;; This hash table is used to store the direct methods and direct generic
;;; functions of EQL specializers. Each value in the table is the cons.
;;;
;;; These tables are shared between threads, so they need to be synchronized.
(defvar *eql-specializer-methods* (make-hash-table :test 'eql :synchronized t))
(defvar *class-eq-specializer-methods* (make-hash-table :test 'eq :synchronized t))

(defmethod specializer-method-table ((specializer eql-specializer))
  *eql-specializer-methods*)

(defmethod specializer-method-table ((specializer class-eq-specializer))
  *class-eq-specializer-methods*)

(defmethod add-direct-method ((specializer specializer-with-object)
                              (method method))
  (let* ((object (specializer-object specializer))
         (table (specializer-method-table specializer))
         (entry (gethash object table)))
    (unless entry
      (setf entry
            (setf (gethash object table) (cons nil nil))))
    ;; We need to first smash the CDR, because a parallel read may
    ;; be in progress, and because if an interrupt catches us we
    ;; need to have a consistent state.
    (setf (cdr entry) ()
          (car entry) (adjoin method (car entry) :test #'eq))
    method))

(defmethod remove-direct-method ((specializer specializer-with-object)
                                 (method method))
  (let* ((object (specializer-object specializer))
         (entry (gethash object (specializer-method-table specializer))))
    (when entry
      ;; We need to first smash the CDR, because a parallel read may
      ;; be in progress, and because if an interrupt catches us we
      ;; need to have a consistent state.
      (setf (cdr entry) ()
            (car entry) (remove method (car entry))))
    method))

(defmethod specializer-direct-methods ((specializer specializer-with-object))
  (car (gethash (specializer-object specializer)
                (specializer-method-table specializer))))

(defmethod specializer-direct-generic-functions ((specializer
                                                  specializer-with-object))
  (let* ((object (specializer-object specializer))
         (entry (gethash object (specializer-method-table specializer))))
    (when entry
      (or (cdr entry)
          (sb-thread::with-spinlock (*specializer-lock*)
            (setf (cdr entry)
                  (let (collect)
                    (dolist (m (car entry))
                      (pushnew (method-generic-function m) collect :test #'eq))
                    (nreverse collect))))))))

(defun map-specializers (function)
  (map-all-classes (lambda (class)
                     (funcall function (class-eq-specializer class))
                     (funcall function class)))
  (maphash (lambda (object methods)
             (declare (ignore methods))
             (intern-eql-specializer object))
           *eql-specializer-methods*)
  (maphash (lambda (object specl)
             (declare (ignore object))
             (funcall function specl))
           *eql-specializer-table*)
  nil)

(defun map-all-generic-functions (function)
  (let ((all-generic-functions (make-hash-table :test 'eq)))
    (map-specializers (lambda (specl)
                        (dolist (gf (specializer-direct-generic-functions
                                     specl))
                          (unless (gethash gf all-generic-functions)
                            (setf (gethash gf all-generic-functions) t)
                            (funcall function gf))))))
  nil)

(defmethod shared-initialize :after ((specl class-eq-specializer)
                                     slot-names
                                     &key)
  (declare (ignore slot-names))
  (setf (slot-value specl '%type) `(class-eq ,(specializer-class specl))))

(defmethod shared-initialize :after ((specl eql-specializer) slot-names &key)
  (declare (ignore slot-names))
  (setf (slot-value specl '%type)
        `(eql ,(specializer-object specl)))
  (setf (info :type :translator specl)
        (constantly (make-member-type :members (list (specializer-object specl))))))

(defun real-load-defclass (name metaclass-name supers slots other
                           readers writers slot-names source-location safe-p)
  (with-single-package-locked-error (:symbol name "defining ~S as a class")
    (%compiler-defclass name readers writers slot-names)
    (let ((res (apply #'ensure-class name :metaclass metaclass-name
                      :direct-superclasses supers
                      :direct-slots slots
                      :definition-source source-location
                      'safe-p safe-p
                      other)))
      res)))

(setf (gdefinition 'load-defclass) #'real-load-defclass)

(defun ensure-class (name &rest args)
  (with-world-lock ()
    (apply #'ensure-class-using-class
           (let ((class (find-class name nil)))
             (when (and class (eq name (class-name class)))
               ;; NAME is the proper name of CLASS, so redefine it
               class))
           name
           args)))

(defmethod ensure-class-using-class ((class null) name &rest args &key)
  (with-world-lock ()
    (multiple-value-bind (meta initargs)
        (frob-ensure-class-args args)
      (setf class (apply #'make-instance meta :name name initargs))
      (without-package-locks
        (setf (find-class name) class))))
  ;; After boot (SETF FIND-CLASS) does this.
  (unless (eq **boot-state** 'complete)
    (%set-class-type-translation class name))
  class)

(defmethod ensure-class-using-class ((class pcl-class) name &rest args &key)
  (with-world-lock ()
    (multiple-value-bind (meta initargs)
        (frob-ensure-class-args args)
      (unless (eq (class-of class) meta)
        (apply #'change-class class meta initargs))
      (apply #'reinitialize-instance class initargs)
      (without-package-locks
        (setf (find-class name) class))))
  ;; After boot (SETF FIND-CLASS) does this.
  (unless (eq **boot-state** 'complete)
    (%set-class-type-translation class name))
  class)

(defun frob-ensure-class-args (args)
  (let (metaclass metaclassp reversed-plist)
    (flet ((frob-superclass (s)
             (cond
               ((classp s) s)
               ((legal-class-name-p s)
                (or (find-class s nil)
                    (ensure-class s :metaclass 'forward-referenced-class)))
               (t (error "Not a class or a legal class name: ~S." s)))))
      (doplist (key val) args
        (cond ((eq key :metaclass)
               (unless metaclassp
                 (setf metaclass val metaclassp key)))
              (t
               (when (eq key :direct-superclasses)
                 (setf val (mapcar #'frob-superclass val)))
               (setf reversed-plist (list* val key reversed-plist)))))
      (values (cond (metaclassp
                     (if (classp metaclass)
                         metaclass
                         (find-class metaclass)))
                    (t *the-class-standard-class*))
              (nreverse reversed-plist)))))

(defun call-initfun (fun slotd safe)
  (declare (function fun))
  (let ((value (funcall fun)))
    (when safe
      (let ((typecheck (slot-definition-type-check-function slotd)))
        (when typecheck
          (funcall (the function typecheck) value))))
    value))

(defmethod shared-initialize :after
    ((class std-class) slot-names &key
     (direct-superclasses nil direct-superclasses-p)
     (direct-slots nil direct-slots-p)
     (direct-default-initargs nil direct-default-initargs-p)
     definition-source)
  (cond (direct-superclasses-p
         (setq direct-superclasses
               (or direct-superclasses
                   (list (if (funcallable-standard-class-p class)
                             *the-class-funcallable-standard-object*
                             *the-class-standard-object*))))
         (dolist (superclass direct-superclasses)
           (unless (validate-superclass class superclass)
             (error "~@<The class ~S was specified as a ~
                     super-class of the class ~S, ~
                     but the meta-classes ~S and ~S are incompatible.  ~
                     Define a method for ~S to avoid this error.~@:>"
                    superclass class (class-of superclass) (class-of class)
                    'validate-superclass)))
         (setf (slot-value class 'direct-superclasses) direct-superclasses))
        (t
         (setq direct-superclasses (slot-value class 'direct-superclasses))))
  (setq direct-slots
        (if direct-slots-p
            (setf (slot-value class 'direct-slots)
                  (mapcar (lambda (pl) (make-direct-slotd class pl))
                          direct-slots))
            (slot-value class 'direct-slots)))
  (if direct-default-initargs-p
      (setf (plist-value class 'direct-default-initargs)
            direct-default-initargs)
      (setq direct-default-initargs
            (plist-value class 'direct-default-initargs)))
  (setf (plist-value class 'class-slot-cells)
        (let ((old-class-slot-cells (plist-value class 'class-slot-cells))
              (safe (safe-p class))
              (collect '()))
          (dolist (dslotd direct-slots)
            (when (eq :class (slot-definition-allocation dslotd))
              ;; see CLHS 4.3.6
              (let* ((name (slot-definition-name dslotd))
                     (old (assoc name old-class-slot-cells)))
                (if (or (not old)
                        (eq t slot-names)
                        (member name slot-names :test #'eq))
                    (let* ((initfunction (slot-definition-initfunction dslotd))
                           (value
                            (if initfunction
                                (call-initfun initfunction dslotd safe)
                                +slot-unbound+)))
                      (push (cons name value) collect))
                    (push old collect)))))
          (nreverse collect)))
  (add-direct-subclasses class direct-superclasses)
  (if (class-finalized-p class)
      ;; required by AMOP, "Reinitialization of Class Metaobjects"
      (finalize-inheritance class)
      (update-class class nil))
  (add-slot-accessors class direct-slots definition-source)
  (make-preliminary-layout class))

(defmethod shared-initialize :after ((class forward-referenced-class)
                                     slot-names &key &allow-other-keys)
  (declare (ignore slot-names))
  (make-preliminary-layout class))

(defvar *allow-forward-referenced-classes-in-cpl-p* nil)

;;; Give CLASS a preliminary layout if it doesn't have one already, to
;;; make it known to the type system.
(defun make-preliminary-layout (class)
  (flet ((compute-preliminary-cpl (root)
           (let ((*allow-forward-referenced-classes-in-cpl-p* t))
             (compute-class-precedence-list root))))
    (with-world-lock ()
      (without-package-locks
        (unless (class-finalized-p class)
          (let ((name (class-name class)))
            ;; KLUDGE: This is fairly horrible.  We need to make a
            ;; full-fledged CLASSOID here, not just tell the compiler that
            ;; some class is forthcoming, because there are legitimate
            ;; questions one can ask of the type system, implemented in
            ;; terms of CLASSOIDs, involving forward-referenced classes. So.
            (let ((layout (make-wrapper 0 class)))
              (setf (slot-value class 'wrapper) layout)
              (let ((cpl (compute-preliminary-cpl class)))
                (setf (layout-inherits layout)
                      (order-layout-inherits
                       (map 'simple-vector #'class-wrapper
                            (reverse (rest cpl))))))
              (register-layout layout :invalidate t)
              (%set-class-type-translation class (layout-classoid layout)))))
        (mapc #'make-preliminary-layout (class-direct-subclasses class))))))


(defmethod shared-initialize :before ((class class) slot-names &key name)
  (declare (ignore slot-names name))
  ;; FIXME: Could this just be CLASS instead of `(CLASS ,CLASS)? If not,
  ;; why not? (See also similar expression in !BOOTSTRAP-INITIALIZE-CLASS.)
  (setf (slot-value class '%type) `(class ,class))
  (setf (slot-value class 'class-eq-specializer)
        (make-instance 'class-eq-specializer :class class)))

(defmethod reinitialize-instance :before ((class slot-class) &key direct-superclasses)
  (dolist (old-super (set-difference (class-direct-superclasses class) direct-superclasses))
    (remove-direct-subclass old-super class))
  (remove-slot-accessors    class (class-direct-slots class)))

(defmethod reinitialize-instance :after ((class slot-class)
                                         &rest initargs
                                         &key)
  (map-dependents class
                  (lambda (dependent)
                    (apply #'update-dependent class dependent initargs))))

(defmethod reinitialize-instance :after ((class condition-class) &key)
  (let* ((name (class-name class))
         (classoid (find-classoid name))
         (slots (condition-classoid-slots classoid)))
    ;; to balance the REMOVE-SLOT-ACCESSORS call in
    ;; REINITIALIZE-INSTANCE :BEFORE (SLOT-CLASS).
    (dolist (slot slots)
      (let ((slot-name (condition-slot-name slot)))
        (dolist (reader (condition-slot-readers slot))
          ;; FIXME: see comment in SHARED-INITIALIZE :AFTER
          ;; (CONDITION-CLASS T), below.  -- CSR, 2005-11-18
          (sb-kernel::install-condition-slot-reader reader name slot-name))
        (dolist (writer (condition-slot-writers slot))
          (sb-kernel::install-condition-slot-writer writer name slot-name))))))

(defmethod shared-initialize :after ((class condition-class) slot-names
                                     &key direct-slots direct-superclasses)
  (declare (ignore slot-names))
  (let ((classoid (find-classoid (slot-value class 'name))))
    (with-slots (wrapper %class-precedence-list cpl-available-p
                         prototype (direct-supers direct-superclasses))
        class
      (setf (slot-value class 'direct-slots)
            (mapcar (lambda (pl) (make-direct-slotd class pl))
                    direct-slots))
      (setf (slot-value class 'finalized-p) t)
      (setf (classoid-pcl-class classoid) class)
      (setq direct-supers direct-superclasses)
      (setq wrapper (classoid-layout classoid))
      (setq %class-precedence-list (compute-class-precedence-list class))
      (setq cpl-available-p t)
      (add-direct-subclasses class direct-superclasses)
      (let ((slots (compute-slots class)))
        (setf (slot-value class 'slots) slots)
        (setf (layout-slot-table wrapper) (make-slot-table class slots)))))
  ;; Comment from Gerd's PCL, 2003-05-15:
  ;;
  ;; We don't ADD-SLOT-ACCESSORS here because we don't want to
  ;; override condition accessors with generic functions.  We do this
  ;; differently.
  ;;
  ;; ??? What does the above comment mean and why is it a good idea?
  ;; CMUCL (which still as of 2005-11-18 uses this code and has this
  ;; comment) loses slot information in its condition classes:
  ;; DIRECT-SLOTS is always NIL.  We have the right information, so we
  ;; remove slot accessors but never put them back.  I've added a
  ;; REINITIALIZE-INSTANCE :AFTER (CONDITION-CLASS) method, but what
  ;; was meant to happen?  -- CSR, 2005-11-18
  )

(defmethod direct-slot-definition-class ((class condition-class)
                                         &rest initargs)
  (declare (ignore initargs))
  (find-class 'condition-direct-slot-definition))

(defmethod effective-slot-definition-class ((class condition-class)
                                            &rest initargs)
  (declare (ignore initargs))
  (find-class 'condition-effective-slot-definition))

(defmethod finalize-inheritance ((class condition-class))
  (aver (slot-value class 'finalized-p))
  nil)

(defmethod compute-effective-slot-definition
    ((class condition-class) slot-name dslotds)
  (let ((slotd (call-next-method)))
    (setf (slot-definition-reader-function slotd)
          (lambda (x)
            (handler-case (condition-reader-function x slot-name)
              ;; FIXME: FIND-SLOT-DEFAULT throws an error if the slot
              ;; is unbound; maybe it should be a CELL-ERROR of some
              ;; sort?
              (error () (values (slot-unbound class x slot-name))))))
    (setf (slot-definition-writer-function slotd)
          (lambda (v x)
            (condition-writer-function x v slot-name)))
    (setf (slot-definition-boundp-function slotd)
          (lambda (x)
            (multiple-value-bind (v c)
                (ignore-errors (condition-reader-function x slot-name))
              (declare (ignore v))
              (null c))))
    slotd))

(defmethod compute-slots ((class condition-class))
  (mapcan (lambda (superclass)
            (mapcar (lambda (dslotd)
                      (compute-effective-slot-definition
                       class (slot-definition-name dslotd) (list dslotd)))
                    (class-direct-slots superclass)))
          (reverse (slot-value class '%class-precedence-list))))

(defmethod compute-slots :around ((class condition-class))
  (let ((eslotds (call-next-method)))
    (mapc #'finalize-internal-slot-functions eslotds)
    eslotds))

(defmethod shared-initialize :after
    ((slotd structure-slot-definition) slot-names &key
     (allocation :instance) allocation-class)
  (declare (ignore slot-names allocation-class))
  (unless (eq allocation :instance)
    (error "Structure slots must have :INSTANCE allocation.")))

(defun make-structure-class-defstruct-form (name direct-slots include)
  (let* ((conc-name (format-symbol *package* "~S structure class " name))
         (constructor (format-symbol *package* "~Aconstructor" conc-name))
         (included-name (class-name include))
         (included-slots
          (when include
            (mapcar #'dsd-name (dd-slots (find-defstruct-description included-name)))))
         (old-slots nil)
         (new-slots nil)
         (reader-names nil)
         (writer-names nil))
    (dolist (slotd (reverse direct-slots))
      (let* ((slot-name (slot-definition-name slotd))
             (initform (slot-definition-initform slotd))
             (type (slot-definition-type slotd))
             (desc `(,slot-name ,initform :type ,type)))
        (push `(slot-accessor ,name ,slot-name reader)
              reader-names)
        (push `(slot-accessor ,name ,slot-name writer)
              writer-names)
        (if (member slot-name included-slots :test #'eq)
            (push desc old-slots)
            (push desc new-slots))))
    (let* ((defstruct `(defstruct (,name
                                    ,@(when include
                                            `((:include ,included-name
                                                        ,@old-slots)))
                                    (:constructor ,constructor ())
                                    (:predicate nil)
                                    (:conc-name ,conc-name)
                                    (:copier nil))
                         ,@new-slots))
           (readers-init
            (mapcar (lambda (slotd reader-name)
                      (let ((accessor
                             (slot-definition-defstruct-accessor-symbol
                              slotd)))
                        `(defun ,reader-name (obj)
                           (declare (type ,name obj))
                           (,accessor obj))))
                    direct-slots reader-names))
           (writers-init
            (mapcar (lambda (slotd writer-name)
                      (let ((accessor
                             (slot-definition-defstruct-accessor-symbol
                              slotd)))
                        `(defun ,writer-name (nv obj)
                           (declare (type ,name obj))
                           (setf (,accessor obj) nv))))
                    direct-slots writer-names))
           (defstruct-form
            `(progn
               ,defstruct
               ,@readers-init ,@writers-init
               (cons nil nil))))
      (values defstruct-form constructor reader-names writer-names))))

(defun make-defstruct-allocation-function (name)
  ;; FIXME: Why don't we go class->layout->info == dd
  (let ((dd (find-defstruct-description name)))
    (ecase (dd-type dd)
      (structure
       (%make-structure-instance-allocator dd nil))
      (funcallable-structure
       (%make-funcallable-structure-instance-allocator dd nil)))))

(defmethod shared-initialize :after
    ((class structure-class) slot-names &key
     (direct-superclasses nil direct-superclasses-p)
     (direct-slots nil direct-slots-p)
     direct-default-initargs
     definition-source)
  (declare (ignore slot-names direct-default-initargs))
  (if direct-superclasses-p
      (setf (slot-value class 'direct-superclasses)
            (or direct-superclasses
                (setq direct-superclasses
                      (and (not (eq (slot-value class 'name) 'structure-object))
                           (list *the-class-structure-object*)))))
      (setq direct-superclasses (slot-value class 'direct-superclasses)))
  (let* ((name (slot-value class 'name))
         (from-defclass-p (slot-value class 'from-defclass-p))
         (defstruct-p (or from-defclass-p (not (structure-type-p name)))))
    (if direct-slots-p
        (setf (slot-value class 'direct-slots)
              (setq direct-slots
                    (mapcar (lambda (pl)
                              (when defstruct-p
                                (let* ((slot-name (getf pl :name))
                                       (accessor
                                        (format-symbol *package*
                                                       "~S structure class ~A"
                                                       name slot-name)))
                                  (setq pl (list* :defstruct-accessor-symbol
                                                  accessor pl))))
                              (make-direct-slotd class pl))
                            direct-slots)))
        (setq direct-slots (slot-value class 'direct-slots)))
    (if defstruct-p
        (let ((include (car (slot-value class 'direct-superclasses))))
          (multiple-value-bind (defstruct-form constructor reader-names writer-names)
              (make-structure-class-defstruct-form name direct-slots include)
            (unless (structure-type-p name) (eval defstruct-form))
            (mapc (lambda (dslotd reader-name writer-name)
                    (let* ((reader (gdefinition reader-name))
                           (writer (when (fboundp writer-name)
                                     (gdefinition writer-name))))
                      (setf (slot-value dslotd 'internal-reader-function)
                            reader)
                      (setf (slot-value dslotd 'internal-writer-function)
                            writer)))
                  direct-slots reader-names writer-names)
            (setf (slot-value class 'defstruct-form) defstruct-form)
            (setf (slot-value class 'defstruct-constructor) constructor)))
        (setf (slot-value class 'defstruct-constructor)
              ;; KLUDGE: not class; in fixup.lisp, can't access slots
              ;; outside methods yet.
              (make-defstruct-allocation-function name)))
    (add-direct-subclasses class direct-superclasses)
    (setf (slot-value class '%class-precedence-list)
          (compute-class-precedence-list class))
    (setf (slot-value class 'cpl-available-p) t)
    (let ((slots (compute-slots class)))
      (setf (slot-value class 'slots) slots)
      (let* ((lclass (find-classoid (slot-value class 'name)))
             (layout (classoid-layout lclass)))
        (setf (classoid-pcl-class lclass) class)
        (setf (slot-value class 'wrapper) layout)
        (setf (layout-slot-table layout) (make-slot-table class slots))))
    (setf (slot-value class 'finalized-p) t)
    (add-slot-accessors class direct-slots definition-source)))

(defmethod direct-slot-definition-class ((class structure-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'structure-direct-slot-definition))

(defmethod finalize-inheritance ((class structure-class))
  nil) ; always finalized

(defun add-slot-accessors (class dslotds &optional source-location)
  (fix-slot-accessors class dslotds 'add source-location))

(defun remove-slot-accessors (class dslotds)
  (fix-slot-accessors class dslotds 'remove))

(defun fix-slot-accessors (class dslotds add/remove &optional source-location)
  (flet ((fix (gfspec name r/w doc)
           (let ((gf (cond ((eq add/remove 'add)
                            (or (find-generic-function gfspec nil)
                                (ensure-generic-function
                                 gfspec :lambda-list (case r/w
                                                       (r '(object))
                                                       (w '(new-value object))))))
                           (t
                            (find-generic-function gfspec nil)))))
             (when gf
               (case r/w
                 (r (if (eq add/remove 'add)
                        (add-reader-method class gf name doc source-location)
                        (remove-reader-method class gf)))
                 (w (if (eq add/remove 'add)
                        (add-writer-method class gf name doc source-location)
                        (remove-writer-method class gf))))))))
    (dolist (dslotd dslotds)
      (let ((slot-name (slot-definition-name dslotd))
            (slot-doc (%slot-definition-documentation dslotd)))
        (dolist (r (slot-definition-readers dslotd))
          (fix r slot-name 'r slot-doc))
        (dolist (w (slot-definition-writers dslotd))
          (fix w slot-name 'w slot-doc))))))

(defun add-direct-subclasses (class supers)
  (dolist (super supers)
    (unless (memq class (class-direct-subclasses class))
      (add-direct-subclass super class))))

(defmethod finalize-inheritance ((class std-class))
  (update-class class t))

(defmethod finalize-inheritance ((class forward-referenced-class))
  ;; FIXME: should we not be thinking a bit about what kinds of error
  ;; we're throwing?  Maybe we need a clos-error type to mix in?  Or
  ;; possibly a forward-referenced-class-error, though that's
  ;; difficult given e.g. class precedence list calculations...
  (error
   "~@<FINALIZE-INHERITANCE was called on a forward referenced class:~
       ~2I~_~S~:>"
   class))


(defun class-has-a-forward-referenced-superclass-p (class)
  (or (forward-referenced-class-p class)
      (some #'class-has-a-forward-referenced-superclass-p
            (class-direct-superclasses class))))

;;; This is called by :after shared-initialize whenever a class is initialized
;;; or reinitialized. The class may or may not be finalized.
(defun update-class (class finalizep)
  (without-package-locks
    (with-world-lock ()
      (when (or finalizep (class-finalized-p class))
        (%update-cpl class (compute-class-precedence-list class))
        ;; This invocation of UPDATE-SLOTS, in practice, finalizes the
        ;; class.
        (%update-slots class (compute-slots class))
        (update-gfs-of-class class)
        (setf (plist-value class 'default-initargs) (compute-default-initargs class))
        (update-ctors 'finalize-inheritance :class class))
      (dolist (sub (class-direct-subclasses class))
        (update-class sub nil)))))

(define-condition cpl-protocol-violation (reference-condition error)
  ((class :initarg :class :reader cpl-protocol-violation-class)
   (cpl :initarg :cpl :reader cpl-protocol-violation-cpl))
  (:default-initargs :references (list '(:sbcl :node "Metaobject Protocol")))
  (:report
   (lambda (c s)
     (format s "~@<Protocol violation: the ~S class ~S ~
                ~:[has~;does not have~] the class ~S in its ~
                class precedence list: ~S.~@:>"
             (class-name (class-of (cpl-protocol-violation-class c)))
             (cpl-protocol-violation-class c)
             (eq (class-of (cpl-protocol-violation-class c))
                 *the-class-funcallable-standard-class*)
             (find-class 'function)
             (cpl-protocol-violation-cpl c)))))

(defun %update-cpl (class cpl)
  (when (eq (class-of class) *the-class-standard-class*)
    (when (find (find-class 'function) cpl)
      (error 'cpl-protocol-violation :class class :cpl cpl)))
  (when (eq (class-of class) *the-class-funcallable-standard-class*)
    (unless (find (find-class 'function) cpl)
      (error 'cpl-protocol-violation :class class :cpl cpl)))
  (if (class-finalized-p class)
      (unless (and (equal (class-precedence-list class) cpl)
                   (dolist (c cpl t)
                     (when (position :class (class-direct-slots c)
                                     :key #'slot-definition-allocation)
                       (return nil))))
        ;; comment from the old CMU CL sources:
        ;;   Need to have the cpl setup before %update-lisp-class-layout
        ;;   is called on CMU CL.
        (setf (slot-value class '%class-precedence-list) cpl)
        (setf (slot-value class 'cpl-available-p) t)
        (%force-cache-flushes class))
      (progn
        (setf (slot-value class '%class-precedence-list) cpl)
        (setf (slot-value class 'cpl-available-p) t)))
  (update-class-can-precede-p cpl))

(defun update-class-can-precede-p (cpl)
  (when cpl
    (let ((first (car cpl)))
      (dolist (c (cdr cpl))
        (pushnew c (slot-value first 'can-precede-list) :test #'eq)))
    (update-class-can-precede-p (cdr cpl))))

(defun class-can-precede-p (class1 class2)
  (member class2 (class-can-precede-list class1) :test #'eq))

(defun %update-slots (class eslotds)
  (let ((instance-slots ())
        (class-slots    ()))
    (dolist (eslotd eslotds)
      (let ((alloc (slot-definition-allocation eslotd)))
        (case alloc
          (:instance (push eslotd instance-slots))
          (:class (push eslotd class-slots)))))

    ;; If there is a change in the shape of the instances then the
    ;; old class is now obsolete.
    (let* ((nlayout (mapcar #'slot-definition-name
                            (sort instance-slots #'<
                                  :key #'slot-definition-location)))
           (nslots (length nlayout))
           (nwrapper-class-slots (compute-class-slots class-slots))
           (owrapper (when (class-finalized-p class)
                       (class-wrapper class)))
           (olayout (when owrapper
                      (wrapper-instance-slots-layout owrapper)))
           (owrapper-class-slots (and owrapper (wrapper-class-slots owrapper)))
           (nwrapper
            (cond ((null owrapper)
                   (make-wrapper nslots class))
                  ((and (equal nlayout olayout)
                        (not
                         (loop for o in owrapper-class-slots
                               for n in nwrapper-class-slots
                               do (unless (eq (car o) (car n)) (return t)))))
                   owrapper)
                  (t
                   ;; This will initialize the new wrapper to have the
                   ;; same state as the old wrapper. We will then have
                   ;; to change that. This may seem like wasted work
                   ;; (and it is), but the spec requires that we call
                   ;; MAKE-INSTANCES-OBSOLETE.
                   (make-instances-obsolete class)
                   (class-wrapper class)))))

      (%update-lisp-class-layout class nwrapper)
      (setf (slot-value class 'slots) eslotds
            (wrapper-slot-table nwrapper) (make-slot-table class eslotds)
            (wrapper-instance-slots-layout nwrapper) nlayout
            (wrapper-class-slots nwrapper) nwrapper-class-slots
            (wrapper-length nwrapper) nslots
            (slot-value class 'wrapper) nwrapper)
      (do* ((slots (slot-value class 'slots) (cdr slots))
            (dupes nil))
           ((null slots)
            (when dupes
              (style-warn
               "~@<slot names with the same SYMBOL-NAME but ~
                  different SYMBOL-PACKAGE (possible package problem) ~
                  for class ~S:~4I~@:_~<~@{~/sb-impl::print-symbol-with-prefix/~^~:@_~}~:>~@:>"
               class dupes)))
        (let* ((slot (car slots))
               (oslots (remove (slot-definition-name slot) (cdr slots)
                               :test #'string/=
                               :key #'slot-definition-name)))
          (when oslots
            (pushnew (cons (slot-definition-name slot)
                           (mapcar #'slot-definition-name oslots))
                     dupes
                     :test #'string= :key #'car))))
      (setf (slot-value class 'finalized-p) t)
      (unless (eq owrapper nwrapper)
        (maybe-update-standard-slot-locations class)))))

(defun compute-class-slots (eslotds)
  (let (collect)
    (dolist (eslotd eslotds (nreverse collect))
      (let ((cell (assoc (slot-definition-name eslotd)
                         (class-slot-cells
                          (slot-definition-allocation-class eslotd)))))
        (aver cell)
        (push cell collect)))))

(defun update-gf-dfun (class gf)
  (let ((*new-class* class)
        (arg-info (gf-arg-info gf)))
    (cond
      ((special-case-for-compute-discriminating-function-p gf))
      ((gf-precompute-dfun-and-emf-p arg-info)
       (multiple-value-bind (dfun cache info) (make-final-dfun-internal gf)
         (update-dfun gf dfun cache info))))))

(defun update-gfs-of-class (class)
  (when (and (class-finalized-p class)
             (let ((cpl (class-precedence-list class)))
               (or (member *the-class-slot-class* cpl :test #'eq)
                   (member *the-class-standard-effective-slot-definition*
                           cpl :test #'eq))))
    (let ((gf-table (make-hash-table :test 'eq)))
      (labels ((collect-gfs (class)
                 (dolist (gf (specializer-direct-generic-functions class))
                   (setf (gethash gf gf-table) t))
                 (mapc #'collect-gfs (class-direct-superclasses class))))
        (collect-gfs class)
        (maphash (lambda (gf ignore)
                   (declare (ignore ignore))
                   (update-gf-dfun class gf))
                 gf-table)))))

(defmethod compute-default-initargs ((class slot-class))
  (let ((initargs (loop for c in (class-precedence-list class)
                        append (class-direct-default-initargs c))))
    (delete-duplicates initargs :test #'eq :key #'car :from-end t)))

;;;; protocols for constructing direct and effective slot definitions

(defmethod direct-slot-definition-class ((class std-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'standard-direct-slot-definition))

(defun make-direct-slotd (class initargs)
  (apply #'make-instance
         (apply #'direct-slot-definition-class class initargs)
         :class class
         initargs))

;;; I (CSR) am not sure, but I believe that the particular order of
;;; slots is quite important: it is ideal to attempt to have a
;;; constant slot location for the same notional slots as much as
;;; possible, so that clever discriminating functions (ONE-INDEX et
;;; al.) have a chance of working.  The below at least walks through
;;; the slots predictably, but maybe it would be good to compute some
;;; kind of optimal slot layout by looking at locations of slots in
;;; superclasses?
(defun std-compute-slots (class)
  ;; As specified, we must call COMPUTE-EFFECTIVE-SLOT-DEFINITION once
  ;; for each different slot name we find in our superclasses. Each
  ;; call receives the class and a list of the dslotds with that name.
  ;; The list is in most-specific-first order.
  (let ((name-dslotds-alist ()))
    (dolist (c (reverse (class-precedence-list class)))
      (dolist (slot (class-direct-slots c))
        (let* ((name (slot-definition-name slot))
               (entry (assq name name-dslotds-alist)))
          (if entry
              (push slot (cdr entry))
              (push (list name slot) name-dslotds-alist)))))
    (mapcar (lambda (direct)
              (compute-effective-slot-definition class
                                                 (car direct)
                                                 (cdr direct)))
            (nreverse name-dslotds-alist))))

(defmethod compute-slots ((class standard-class))
  (std-compute-slots class))
(defmethod compute-slots ((class funcallable-standard-class))
  (std-compute-slots class))

(defun std-compute-slots-around (class eslotds)
  (let ((location -1)
        (safe (safe-p class)))
    (dolist (eslotd eslotds eslotds)
      (setf (slot-definition-location eslotd)
            (case (slot-definition-allocation eslotd)
              (:instance
               (incf location))
              (:class
               (let* ((name (slot-definition-name eslotd))
                      (from-class
                       (or
                        (slot-definition-allocation-class eslotd)
                        ;; we get here if the user adds an extra slot
                        ;; himself...
                        (setf (slot-definition-allocation-class eslotd)
                              class)))
                      ;; which raises the question of what we should
                      ;; do if we find that said user has added a slot
                      ;; with the same name as another slot...
                      (cell (or (assq name (class-slot-cells from-class))
                                (let ((c (cons name +slot-unbound+)))
                                  (push c (class-slot-cells from-class))
                                  c))))
                 (aver (consp cell))
                 (if (eq +slot-unbound+ (cdr cell))
                     ;; We may have inherited an initfunction FIXME: Is this
                     ;; really right? Is the initialization in
                     ;; SHARED-INITIALIZE (STD-CLASS) not enough?
                     (let ((initfun (slot-definition-initfunction eslotd)))
                       (if initfun
                           (rplacd cell (call-initfun initfun eslotd safe))
                           cell))
                     cell)))))
      (unless (slot-definition-class eslotd)
        (setf (slot-definition-class eslotd) class))
      (initialize-internal-slot-functions eslotd))))

(defmethod compute-slots :around ((class standard-class))
  (let ((eslotds (call-next-method)))
    (std-compute-slots-around class eslotds)))
(defmethod compute-slots :around ((class funcallable-standard-class))
  (let ((eslotds (call-next-method)))
    (std-compute-slots-around class eslotds)))

(defmethod compute-slots ((class structure-class))
  (mapcan (lambda (superclass)
            (mapcar (lambda (dslotd)
                      (compute-effective-slot-definition
                       class
                       (slot-definition-name dslotd)
                       (list dslotd)))
                    (class-direct-slots superclass)))
          (reverse (slot-value class '%class-precedence-list))))

(defmethod compute-slots :around ((class structure-class))
  (let ((eslotds (call-next-method)))
    (mapc #'finalize-internal-slot-functions eslotds)
    eslotds))

(defmethod compute-effective-slot-definition ((class slot-class) name dslotds)
  (declare (ignore name))
  (let* ((initargs (compute-effective-slot-definition-initargs class dslotds))
         (class (apply #'effective-slot-definition-class class initargs)))
    (apply #'make-instance class initargs)))

(defmethod effective-slot-definition-class ((class std-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'standard-effective-slot-definition))

(defmethod effective-slot-definition-class ((class structure-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'structure-effective-slot-definition))

(defmethod compute-effective-slot-definition-initargs
    ((class slot-class) direct-slotds)
  (let* ((name nil)
         (initfunction nil)
         (initform nil)
         (initargs nil)
         (allocation nil)
         (allocation-class nil)
         (type t)
         (type-check-function nil)
         (documentation nil)
         (documentationp nil)
         (namep  nil)
         (initp  nil)
         (allocp nil))

    (dolist (slotd direct-slotds)
      (when slotd
        (unless namep
          (setq name (slot-definition-name slotd)
                namep t))
        (unless initp
          (awhen (slot-definition-initfunction slotd)
            (setq initform (slot-definition-initform slotd)
                  initfunction it
                  initp t)))
        (unless documentationp
          (awhen (%slot-definition-documentation slotd)
            (setq documentation it
                  documentationp t)))
        (unless allocp
          (setq allocation (slot-definition-allocation slotd)
                allocation-class (slot-definition-class slotd)
                allocp t))
        (setq initargs (append (slot-definition-initargs slotd) initargs))
        (let ((fun (slot-definition-type-check-function slotd)))
          (when fun
            (setf type-check-function
                  (if type-check-function
                      (let ((old-function type-check-function))
                        (declare (function old-function fun))
                        (lambda (value)
                          (funcall old-function value)
                          (funcall fun value)))
                      fun))))
        (let ((slotd-type (slot-definition-type slotd)))
          (setq type (cond
                       ((eq type t) slotd-type)
                       ;; This pairwise type intersection is perhaps a
                       ;; little inefficient and inelegant, but it's
                       ;; unlikely to lie on the critical path.  Shout
                       ;; if I'm wrong.  -- CSR, 2005-11-24
                       (t (type-specifier
                           (specifier-type `(and ,type ,slotd-type)))))))))
    (list :name name
          :initform initform
          :initfunction initfunction
          :initargs initargs
          :allocation allocation
          :allocation-class allocation-class
          :type type
          'type-check-function type-check-function
          :class class
          :documentation documentation)))

(defmethod compute-effective-slot-definition-initargs :around
    ((class structure-class) direct-slotds)
  (let ((slotd (car direct-slotds)))
    (list* :defstruct-accessor-symbol
           (slot-definition-defstruct-accessor-symbol slotd)
           :internal-reader-function
           (slot-definition-internal-reader-function slotd)
           :internal-writer-function
           (slot-definition-internal-writer-function slotd)
           (call-next-method))))

;;; NOTE: For bootstrapping considerations, these can't use MAKE-INSTANCE
;;;       to make the method object. They have to use make-a-method which
;;;       is a specially bootstrapped mechanism for making standard methods.
(defmethod reader-method-class ((class slot-class) direct-slot &rest initargs)
  (declare (ignore direct-slot initargs))
  (find-class 'standard-reader-method))

(defmethod add-reader-method ((class slot-class) generic-function slot-name slot-documentation source-location)
  (add-method generic-function
              (make-a-method 'standard-reader-method
                             ()
                             (list (or (class-name class) 'object))
                             (list class)
                             (make-reader-method-function class slot-name)
                             (or slot-documentation "automatically generated reader method")
                             :slot-name slot-name
                             :object-class class
                             :method-class-function #'reader-method-class
                             :definition-source source-location)))

(defmethod writer-method-class ((class slot-class) direct-slot &rest initargs)
  (declare (ignore direct-slot initargs))
  (find-class 'standard-writer-method))

(defmethod add-writer-method ((class slot-class) generic-function slot-name slot-documentation source-location)
  (add-method generic-function
              (make-a-method 'standard-writer-method
                             ()
                             (list 'new-value (or (class-name class) 'object))
                             (list *the-class-t* class)
                             (make-writer-method-function class slot-name)
                             (or slot-documentation "automatically generated writer method")
                             :slot-name slot-name
                             :object-class class
                             :method-class-function #'writer-method-class
                             :definition-source source-location)))

(defmethod add-boundp-method ((class slot-class) generic-function slot-name slot-documentation source-location)
  (add-method generic-function
              (make-a-method (constantly (find-class 'standard-boundp-method))
                             class
                             ()
                             (list (or (class-name class) 'object))
                             (list class)
                             (make-boundp-method-function class slot-name)
                             (or slot-documentation "automatically generated boundp method")
                             :slot-name slot-name
                             :definition-source source-location)))

(defmethod remove-reader-method ((class slot-class) generic-function)
  (let ((method (get-method generic-function () (list class) nil)))
    (when method (remove-method generic-function method))))

(defmethod remove-writer-method ((class slot-class) generic-function)
  (let ((method
          (get-method generic-function () (list *the-class-t* class) nil)))
    (when method (remove-method generic-function method))))

(defmethod remove-boundp-method ((class slot-class) generic-function)
  (let ((method (get-method generic-function () (list class) nil)))
    (when method (remove-method generic-function method))))

;;; MAKE-READER-METHOD-FUNCTION and MAKE-WRITER-METHOD-FUNCTION
;;; function are NOT part of the standard protocol. They are however
;;; useful; PCL makes use of them internally and documents them for
;;; PCL users.  (FIXME: but SBCL certainly doesn't)
;;;
;;; *** This needs work to make type testing by the writer functions which
;;; *** do type testing faster. The idea would be to have one constructor
;;; *** for each possible type test.
;;;
;;; *** There is a subtle bug here which is going to have to be fixed.
;;; *** Namely, the simplistic use of the template has to be fixed. We
;;; *** have to give the OPTIMIZE-SLOT-VALUE method the user might have
;;; *** defined for this metaclass a chance to run.

(defmethod make-reader-method-function ((class slot-class) slot-name)
  (make-std-reader-method-function class slot-name))

(defmethod make-writer-method-function ((class slot-class) slot-name)
  (make-std-writer-method-function class slot-name))

(defmethod make-boundp-method-function ((class slot-class) slot-name)
  (make-std-boundp-method-function class slot-name))

(defmethod compatible-meta-class-change-p (class proto-new-class)
  (eq (class-of class) (class-of proto-new-class)))

(defmethod validate-superclass ((class class) (superclass class))
  (or (eq superclass *the-class-t*)
      (eq (class-of class) (class-of superclass))
      (and (eq (class-of superclass) *the-class-standard-class*)
           (eq (class-of class) *the-class-funcallable-standard-class*))
      (and (eq (class-of superclass) *the-class-funcallable-standard-class*)
           (eq (class-of class) *the-class-standard-class*))))

;;; What this does depends on which of the four possible values of
;;; LAYOUT-INVALID the PCL wrapper has; the simplest case is when it
;;; is (:FLUSH <wrapper>) or (:OBSOLETE <wrapper>), when there is
;;; nothing to do, as the new wrapper has already been created.  If
;;; LAYOUT-INVALID returns NIL, then we invalidate it (setting it to
;;; (:FLUSH <wrapper>); UPDATE-SLOTS later gets to choose whether or
;;; not to "upgrade" this to (:OBSOLETE <wrapper>).
;;;
;;; This leaves the case where LAYOUT-INVALID returns T, which happens
;;; when REGISTER-LAYOUT has invalidated a superclass of CLASS (which
;;; invalidated all the subclasses in SB-KERNEL land).  Again, here we
;;; must flush the caches and allow UPDATE-SLOTS to decide whether to
;;; obsolete the wrapper.
;;;
;;; FIXME: either here or in INVALID-WRAPPER-P looks like a good place
;;; for (AVER (NOT (EQ (LAYOUT-INVALID OWRAPPER)
;;;                    :UNINITIALIZED)))
;;;
;;; Thanks to Gerd Moellmann for the explanation.  -- CSR, 2002-10-29
(defun %force-cache-flushes (class)
  (let* ((owrapper (class-wrapper class)))
    ;; We only need to do something if the wrapper is still valid. If
    ;; the wrapper isn't valid, state will be FLUSH or OBSOLETE, and
    ;; both of those will already be doing what we want. In
    ;; particular, we must be sure we never change an OBSOLETE into a
    ;; FLUSH since OBSOLETE means do what FLUSH does and then some.
    (when (or (not (invalid-wrapper-p owrapper))
              ;; KLUDGE: despite the observations above, this remains
              ;; a violation of locality or what might be considered
              ;; good style.  There has to be a better way!  -- CSR,
              ;; 2002-10-29
              (eq (layout-invalid owrapper) t))
      (let ((nwrapper (make-wrapper (layout-length owrapper)
                                    class)))
        (setf (wrapper-instance-slots-layout nwrapper)
              (wrapper-instance-slots-layout owrapper))
        (setf (wrapper-class-slots nwrapper)
              (wrapper-class-slots owrapper))
        (setf (wrapper-slot-table nwrapper)
              (wrapper-slot-table owrapper))
        (%update-lisp-class-layout class nwrapper)
        (setf (slot-value class 'wrapper) nwrapper)
        ;; Use :OBSOLETE instead of :FLUSH if any superclass has
        ;; been obsoleted.
        (if (find-if (lambda (x)
                       (and (consp x) (eq :obsolete (car x))))
                     (layout-inherits owrapper)
                     :key #'layout-invalid)
            (%invalidate-wrapper owrapper :obsolete nwrapper)
            (%invalidate-wrapper owrapper :flush nwrapper))))))

;;; MAKE-INSTANCES-OBSOLETE can be called by user code. It will cause
;;; the next access to the instance (as defined in 88-002R) to trap
;;; through the UPDATE-INSTANCE-FOR-REDEFINED-CLASS mechanism.
(defmethod make-instances-obsolete ((class std-class))
  (with-world-lock ()
    (let* ((owrapper (class-wrapper class))
           (nwrapper (make-wrapper (layout-length owrapper)
                                   class)))
      (unless (class-finalized-p class)
        (if (class-has-a-forward-referenced-superclass-p class)
            (return-from make-instances-obsolete class)
            (%update-cpl class (compute-class-precedence-list class))))
      (setf (wrapper-instance-slots-layout nwrapper)
            (wrapper-instance-slots-layout owrapper))
      (setf (wrapper-class-slots nwrapper)
            (wrapper-class-slots owrapper))
      (setf (wrapper-slot-table nwrapper)
            (wrapper-slot-table owrapper))
      (%update-lisp-class-layout class nwrapper)
      (setf (slot-value class 'wrapper) nwrapper)
      (%invalidate-wrapper owrapper :obsolete nwrapper)
      class)))

(defmethod make-instances-obsolete ((class symbol))
  (make-instances-obsolete (find-class class))
  ;; ANSI wants the class name when called with a symbol.
  class)

;;; OBSOLETE-INSTANCE-TRAP is the internal trap that is called when we
;;; see an obsolete instance. The times when it is called are:
;;;   - when the instance is involved in method lookup
;;;   - when attempting to access a slot of an instance
;;;
;;; It is not called by class-of, wrapper-of, or any of the low-level
;;; instance access macros.
;;;
;;; Of course these times when it is called are an internal
;;; implementation detail of PCL and are not part of the documented
;;; description of when the obsolete instance update happens. The
;;; documented description is as it appears in 88-002R.
;;;
;;; This has to return the new wrapper, so it counts on all the
;;; methods on obsolete-instance-trap-internal to return the new
;;; wrapper. It also does a little internal error checking to make
;;; sure that the traps are only happening when they should, and that
;;; the trap methods are computing appropriate new wrappers.

;;; OBSOLETE-INSTANCE-TRAP might be called on structure instances
;;; after a structure is redefined. In most cases,
;;; OBSOLETE-INSTANCE-TRAP will not be able to fix the old instance,
;;; so it must signal an error. The hard part of this is that the
;;; error system and debugger might cause OBSOLETE-INSTANCE-TRAP to be
;;; called again, so in that case, we have to return some reasonable
;;; wrapper, instead.

(defvar *in-obsolete-instance-trap* nil)
(defvar *the-wrapper-of-structure-object*
  (class-wrapper (find-class 'structure-object)))

(define-condition obsolete-structure (error)
  ((datum :reader obsolete-structure-datum :initarg :datum))
  (:report
   (lambda (condition stream)
     ;; Don't try to print the structure, since it probably won't work.
     (format stream
             "~@<obsolete structure error for a structure of type ~2I~_~S~:>"
             (type-of (obsolete-structure-datum condition))))))

(defun %obsolete-instance-trap (owrapper nwrapper instance)
  (if (not (layout-for-std-class-p owrapper))
      (if *in-obsolete-instance-trap*
          *the-wrapper-of-structure-object*
          (let ((*in-obsolete-instance-trap* t))
            (error 'obsolete-structure :datum instance)))
      (let* ((class (wrapper-class* nwrapper))
             (copy (allocate-instance class)) ;??? allocate-instance ???
             (olayout (wrapper-instance-slots-layout owrapper))
             (nlayout (wrapper-instance-slots-layout nwrapper))
             (oslots (get-slots instance))
             (nslots (get-slots copy))
             (oclass-slots (wrapper-class-slots owrapper))
             (added ())
             (discarded ())
             (plist ()))

        ;; local  --> local     transfer value
        ;; local  --> shared    discard value, discard slot
        ;; local  -->  --       discard slot
        ;; shared --> local     transfer value
        ;; shared --> shared    -- (cf SHARED-INITIALIZE :AFTER STD-CLASS)
        ;; shared -->  --       discard value
        ;;  --    --> local     add slot
        ;;  --    --> shared    --

        ;; Go through all the old local slots.
        (let ((opos 0))
          (dolist (name olayout)
            (let ((npos (posq name nlayout)))
              (if npos
                  (setf (clos-slots-ref nslots npos)
                        (clos-slots-ref oslots opos))
                  (progn
                    (push name discarded)
                    (unless (eq (clos-slots-ref oslots opos) +slot-unbound+)
                      (setf (getf plist name) (clos-slots-ref oslots opos))))))
            (incf opos)))

        ;; Go through all the old shared slots.
        (dolist (oclass-slot-and-val oclass-slots)
          (let ((name (car oclass-slot-and-val))
                (val (cdr oclass-slot-and-val)))
            (let ((npos (posq name nlayout)))
              (when npos
                (setf (clos-slots-ref nslots npos) val)))))

        ;; Go through all the new local slots to compute the added slots.
        (dolist (nlocal nlayout)
          (unless (or (memq nlocal olayout)
                      (assq nlocal oclass-slots))
            (push nlocal added)))

        (%swap-wrappers-and-slots instance copy)

        (update-instance-for-redefined-class instance
                                             added
                                             discarded
                                             plist)
        nwrapper)))

(defun %change-class (instance new-class initargs)
  (let* ((old-class (class-of instance))
         (copy (allocate-instance new-class))
         (new-wrapper (get-wrapper copy))
         (old-wrapper (class-wrapper old-class))
         (old-layout (wrapper-instance-slots-layout old-wrapper))
         (new-layout (wrapper-instance-slots-layout new-wrapper))
         (old-slots (get-slots instance))
         (new-slots (get-slots copy))
         (old-class-slots (wrapper-class-slots old-wrapper)))

    ;; "The values of local slots specified by both the class CTO and
    ;; CFROM are retained. If such a local slot was unbound, it
    ;; remains unbound."
    (let ((new-position 0))
      (dolist (new-slot new-layout)
        (let ((old-position (posq new-slot old-layout)))
          (when old-position
            (setf (clos-slots-ref new-slots new-position)
                  (clos-slots-ref old-slots old-position))))
        (incf new-position)))

    ;; "The values of slots specified as shared in the class CFROM and
    ;; as local in the class CTO are retained."
    (dolist (slot-and-val old-class-slots)
      (let ((position (posq (car slot-and-val) new-layout)))
        (when position
          (setf (clos-slots-ref new-slots position) (cdr slot-and-val)))))

    ;; Make the copy point to the old instance's storage, and make the
    ;; old instance point to the new storage.
    (%swap-wrappers-and-slots instance copy)

    (apply #'update-instance-for-different-class copy instance initargs)

    instance))

(defmethod change-class ((instance standard-object) (new-class standard-class)
                         &rest initargs)
  (with-world-lock ()
    (unless (class-finalized-p new-class)
      (finalize-inheritance new-class))
    (let ((cpl (class-precedence-list new-class)))
      (dolist (class cpl)
        (macrolet
            ((frob (class-name)
               `(when (eq class (find-class ',class-name))
                  (error 'metaobject-initialization-violation
                         :format-control "~@<Cannot ~S objects into ~S metaobjects.~@:>"
                         :format-arguments (list 'change-class ',class-name)
                         :references (list '(:amop :initialization ,class-name))))))
          (frob class)
          (frob generic-function)
          (frob method)
          (frob slot-definition))))
    (%change-class instance new-class initargs)))

(defmethod change-class ((instance forward-referenced-class)
                         (new-class standard-class) &rest initargs)
  (with-world-lock ()
    (let ((cpl (class-precedence-list new-class)))
      (dolist (class cpl
               (error 'metaobject-initialization-violation
                      :format-control
                      "~@<Cannot ~S ~S objects into non-~S objects.~@:>"
                      :format-arguments
                      (list 'change-class 'forward-referenced-class 'class)
                      :references
                      (list '(:amop :generic-function ensure-class-using-class)
                            '(:amop :initialization class))))
        (when (eq class (find-class 'class))
          (return nil))))
    (%change-class instance new-class initargs)))

(defmethod change-class ((instance funcallable-standard-object)
                         (new-class funcallable-standard-class)
                         &rest initargs)
  (with-world-lock ()
    (let ((cpl (class-precedence-list new-class)))
      (dolist (class cpl)
        (macrolet
            ((frob (class-name)
               `(when (eq class (find-class ',class-name))
                  (error 'metaobject-initialization-violation
                         :format-control "~@<Cannot ~S objects into ~S metaobjects.~@:>"
                         :format-arguments (list 'change-class ',class-name)
                         :references (list '(:amop :initialization ,class-name))))))
          (frob class)
          (frob generic-function)
          (frob method)
          (frob slot-definition))))
    (%change-class instance new-class initargs)))

(defmethod change-class ((instance standard-object)
                         (new-class funcallable-standard-class)
                         &rest initargs)
  (declare (ignore initargs))
  (error "You can't change the class of ~S to ~S~@
          because it isn't already an instance with metaclass ~S."
         instance new-class 'standard-class))

(defmethod change-class ((instance funcallable-standard-object)
                         (new-class standard-class)
                         &rest initargs)
  (declare (ignore initargs))
  (error "You can't change the class of ~S to ~S~@
          because it isn't already an instance with metaclass ~S."
         instance new-class 'funcallable-standard-class))

(defmethod change-class ((instance t) (new-class-name symbol) &rest initargs)
  (apply #'change-class instance (find-class new-class-name) initargs))

;;;; The metaclass BUILT-IN-CLASS
;;;;
;;;; This metaclass is something of a weird creature. By this point, all
;;;; instances of it which will exist have been created, and no instance
;;;; is ever created by calling MAKE-INSTANCE.
;;;;
;;;; But, there are other parts of the protocol we must follow and those
;;;; definitions appear here.

(macrolet ((def (name args control)
               `(defmethod ,name ,args
                 (declare (ignore initargs))
                 (error 'metaobject-initialization-violation
                  :format-control ,(format nil "~@<~A~@:>" control)
                  :format-arguments (list ',name)
                  :references (list '(:amop :initialization "Class"))))))
  (def initialize-instance ((class built-in-class) &rest initargs)
    "Cannot ~S an instance of BUILT-IN-CLASS.")
  (def reinitialize-instance ((class built-in-class) &rest initargs)
    "Cannot ~S an instance of BUILT-IN-CLASS."))

(macrolet ((def (name)
               `(defmethod ,name ((class built-in-class)) nil)))
  (def class-direct-slots)
  (def class-slots)
  (def class-direct-default-initargs)
  (def class-default-initargs))

(defmethod validate-superclass ((c class) (s built-in-class))
  (or (eq s *the-class-t*) (eq s *the-class-stream*)
      ;; FIXME: bad things happen if someone tries to mix in both
      ;; FILE-STREAM and STRING-STREAM (as they have the same
      ;; layout-depthoid).  Is there any way we can provide a useful
      ;; error message?  -- CSR, 2005-05-03
      (eq s *the-class-file-stream*) (eq s *the-class-string-stream*)
      ;; This probably shouldn't be mixed in with certain other
      ;; classes, too, but it seems to work both with STANDARD-OBJECT
      ;; and FUNCALLABLE-STANDARD-OBJECT
      (eq s *the-class-sequence*)))

;;; Some necessary methods for FORWARD-REFERENCED-CLASS
(defmethod class-direct-slots ((class forward-referenced-class)) ())
(defmethod class-direct-default-initargs ((class forward-referenced-class)) ())
(macrolet ((def (method)
             `(defmethod ,method ((class forward-referenced-class))
                (error "~@<~I~S was called on a forward referenced class:~2I~_~S~:>"
                       ',method class))))
  (def class-default-initargs)
  (def class-precedence-list)
  (def class-slots))

(defmethod validate-superclass ((c slot-class)
                                (f forward-referenced-class))
  t)

(defmethod add-dependent ((metaobject dependent-update-mixin) dependent)
  (pushnew dependent (plist-value metaobject 'dependents) :test #'eq))

(defmethod remove-dependent ((metaobject dependent-update-mixin) dependent)
  (setf (plist-value metaobject 'dependents)
        (delete dependent (plist-value metaobject 'dependents))))

(defmethod map-dependents ((metaobject dependent-update-mixin) function)
  (dolist (dependent (plist-value metaobject 'dependents))
    (funcall function dependent)))


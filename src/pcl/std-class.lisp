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
  (let ((info (the slot-info (slot-definition-info slotd))))
    (ecase type
      (reader (slot-info-reader info))
      (writer (slot-info-writer info))
      (boundp (slot-info-boundp info))
      (makunbound (slot-info-makunbound info)))))

(defmethod (setf slot-accessor-function) (function
                                          (slotd effective-slot-definition)
                                          type)
  (let ((info (the slot-info (slot-definition-info slotd))))
    (ecase type
      (reader (setf (slot-info-reader info) function))
      (writer (setf (slot-info-writer info) function))
      (boundp (setf (slot-info-boundp info) function))
      (makunbound (setf (slot-info-makunbound info) function)))))

(defconstant +slotd-reader-function-std-p+ 1)
(defconstant +slotd-writer-function-std-p+ 2)
(defconstant +slotd-boundp-function-std-p+ 4)
(defconstant +slotd-makunbound-function-std-p+ 8)
(defconstant +slotd-all-function-std-p+ 15)

(defmethod slot-accessor-std-p ((slotd effective-slot-definition) type)
  (let ((flags (slot-value slotd 'accessor-flags)))
    (declare (type fixnum flags))
    (if (eq type 'all)
        (eql +slotd-all-function-std-p+ flags)
        (logtest flags (ecase type
                        (reader +slotd-reader-function-std-p+)
                        (writer +slotd-writer-function-std-p+)
                        (boundp +slotd-boundp-function-std-p+)
                        (makunbound +slotd-makunbound-function-std-p+))))))

(defmethod (setf slot-accessor-std-p) (value
                                       (slotd effective-slot-definition)
                                       type)
  (let ((mask (ecase type
                (reader +slotd-reader-function-std-p+)
                (writer +slotd-writer-function-std-p+)
                (boundp +slotd-boundp-function-std-p+)
                (makunbound +slotd-makunbound-function-std-p+)))
        (flags (slot-value slotd 'accessor-flags)))
    (declare (type fixnum mask flags))
    (setf (slot-value slotd 'accessor-flags)
          (logior (logandc2 flags mask) (if value mask 0))))
  value)

(defmethod initialize-internal-slot-functions
    ((slotd effective-slot-definition))
  (let* ((name (slot-value slotd 'name)) ; flushable? (is it ever unbound?)
         (class (slot-value slotd '%class)))
    (declare (ignore name))
    (dolist (type '(reader writer boundp makunbound))
      (let* ((gf-name (ecase type
                              (reader 'slot-value-using-class)
                              (writer '(setf slot-value-using-class))
                              (boundp 'slot-boundp-using-class)
                              (makunbound 'slot-makunbound-using-class)))
             (gf (gdefinition gf-name)))
        ;; KLUDGE: this logic is cut'n'pasted from
        ;; GET-ACCESSOR-METHOD-FUNCTION, which (for STD-CLASSes) is
        ;; only called later, because it does things that can't be
        ;; computed this early in class finalization; however, we need
        ;; this bit as early as possible.  -- CSR, 2009-11-05
        (setf (slot-accessor-std-p slotd type)
              (let* ((types1 `((eql ,class) (class-eq ,class) (eql ,slotd)))
                     (types (if (eq type 'writer) `(t ,@types1) types1))
                     (methods (compute-applicable-methods-using-types gf types)))
                (null (cdr methods))))
        (setf (slot-accessor-function slotd type)
              (lambda (&rest args)
                (declare (dynamic-extent args))
                ;; FIXME: a tiny amount of wasted SLOT-ACCESSOR-STD-P
                ;; work here (see KLUDGE comment above).
                (let ((fun (compute-slot-accessor-info slotd type gf)))
                  (apply fun args))))))))

(defmethod finalize-internal-slot-functions ((slotd effective-slot-definition))
  (dolist (type '(reader writer boundp makunbound))
    (let* ((gf-name (ecase type
                      (reader 'slot-value-using-class)
                      (writer '(setf slot-value-using-class))
                      (boundp 'slot-boundp-using-class)
                      (makunbound 'slot-makunbound-using-class)))
           (gf (gdefinition gf-name)))
      (compute-slot-accessor-info slotd type gf))))

;;; CMUCL (Gerd PCL 2003-04-25) comment:
;;;
;;; Compute an effective method for SLOT-VALUE-USING-CLASS, (SETF
;;; SLOT-VALUE-USING-CLASS) or SLOT-BOUNDP-USING-CLASS for reading/
;;; writing/testing effective slot SLOTD.
;;;
;;; TYPE is one of the symbols READER, WRITER, BOUNDP or MAKUNBOUND,
;;; depending on GF.  Store the effective method in the effective slot
;;; definition object itself; these GFs have special dispatch
;;; functions calling effective methods directly retrieved from
;;; effective slot definition objects, as an optimization.
;;;
;;; FIXME: Change the function name to COMPUTE-SVUC-SLOTD-FUNCTION,
;;; or some such.
(defmethod compute-slot-accessor-info ((slotd effective-slot-definition)
                                       type gf)
  (let* ((name (slot-value slotd 'name)) ; flushable?
         (class (slot-value slotd '%class)))
    (declare (ignore name))
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

(defmethod class-prototype :before ((class pcl-class))
  (unless (class-finalized-p class)
    (error "~@<~S is not finalized.~:@>" class)))

;;; KLUDGE: For some reason factoring the common body into a function
;;; breaks PCL bootstrapping, so just generate it with a macrolet for
;;; all.
(macrolet ((def (class)
             `(defmethod class-prototype ((class ,class))
                (declare (notinline allocate-instance))
                (with-slots (prototype) class
                  (or prototype
                      (setf prototype (sb-vm:without-arena "class-prototype"
                                        (allocate-instance class))))))))
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
    (with-world-lock ()
      (pushnew subclass direct-subclasses :test #'eq)
      (let ((layout (class-wrapper subclass)))
        (when layout
          (let ((classoid (layout-classoid layout)))
            (dovector (super-layout (layout-inherits layout))
              (sb-kernel::add-subclassoid (layout-classoid super-layout)
                                          classoid layout))))))
    subclass))
(defmethod remove-direct-subclass ((class class) (subclass class))
  (with-slots (direct-subclasses) class
    (with-world-lock ()
      (setq direct-subclasses (remove subclass direct-subclasses))
      ;; Remove from classoid subclasses as well.
      (let ((classoid (class-classoid subclass)))
        (dovector (super-layout (layout-inherits (classoid-layout classoid)))
          (sb-kernel::remove-subclassoid classoid
                                         (layout-classoid super-layout)))))
    subclass))

;;; Maintaining the direct-methods and direct-generic-functions backpointers.
;;;
;;; There are four generic functions involved, each has one method. All of
;;; these are specified methods and appear in their specified place in the
;;; class graph.
;;;
;;;   ADD-DIRECT-METHOD
;;;   REMOVE-DIRECT-METHOD
;;;   SPECIALIZER-DIRECT-METHODS
;;;   SPECIALIZER-DIRECT-GENERIC-FUNCTIONS
;;;
;;; In each case, we maintain one value which is a cons. The car is the list
;;; of methods. The cdr is a list of the generic functions. The cdr is always
;;; computed lazily.

;;; This needs to be used recursively, in case a non-trivial user
;;; defined ADD/REMOVE-DIRECT-METHOD method ends up calling another
;;; function using the same lock.
(define-load-time-global *specializer-lock* (sb-thread:make-mutex :name "Specializer lock"))

(defmethod add-direct-method :around ((specializer specializer) (method method))
  ;; All the actions done under this lock are done in an order
  ;; that is safe to unwind at any point.
  (sb-thread::with-recursive-system-lock (*specializer-lock*)
    (call-next-method)))

(defmethod remove-direct-method :around ((specializer specializer) (method method))
  ;; All the actions done under this lock are done in an order
  ;; that is safe to unwind at any point.
  (sb-thread::with-recursive-system-lock (*specializer-lock*)
    (call-next-method)))

(defmethod add-direct-method ((specializer specializer) (method method))
  (let ((cell (specializer-method-holder specializer)))
    ;; We need to first smash the CDR, because a parallel read may
    ;; be in progress, and because if an interrupt catches us we
    ;; need to have a consistent state.
    (setf (cdr cell) ()
          (car cell) (adjoin method (car cell) :test #'eq)))
  method)

(defmethod remove-direct-method ((specializer specializer) (method method))
  (let ((cell (specializer-method-holder specializer)))
    ;; We need to first smash the CDR, because a parallel read may
    ;; be in progress, and because if an interrupt catches us we
    ;; need to have a consistent state.
    (setf (cdr cell) ()
          (car cell) (remove method (car cell))))
  method)

(defmethod specializer-direct-methods ((specializer specializer))
  (car (specializer-method-holder specializer nil)))

(defmethod specializer-direct-generic-functions ((specializer specializer))
  (let ((cell (specializer-method-holder specializer nil)))
    ;; If an ADD/REMOVE-METHOD is in progress, no matter: either
    ;; we behave as if we got just first or just after -- it's just
    ;; for update that we need to lock.
    (or (cdr cell)
        (when (car cell)
          (setf (cdr cell)
                (with-system-mutex (*specializer-lock*)
                  (let (collect)
                    (dolist (m (car cell) (nreverse collect))
                ;; the old PCL code used COLLECTING-ONCE which used
                ;; #'EQ to check for newness
                      (pushnew (method-generic-function m) collect
                               :test #'eq)))))))))

(defmethod specializer-method-holder ((self specializer) &optional create)
  ;; CREATE can be ignored, because instances of SPECIALIZER
  ;; other than CLASS-EQ-SPECIALIZER have their DIRECT-METHODS slot
  ;; preinitialized with (NIL . NIL).
  (declare (ignore create))
  (slot-value self 'direct-methods))

;; Same as for CLASS but the only ancestor it shares is SPECIALIZER.
;; If this were defined on SPECIALIZER-WITH-OBJECT then it would
;; apply as well to CLASS-EQ specializers which we don't want.
(defmethod specializer-method-holder ((self eql-specializer) &optional create)
  (declare (ignore create))
  (slot-value self 'direct-methods))

;;; This hash table is used to store the direct methods and direct generic
;;; functions of CLASS-EQ specializers.
;;; This is needed because CLASS-EQ specializers are not interned: two different
;;; specializers could refer to the identical CLASS. But semantically you don't
;;; want to collect the direct methods separately, you want them all together.
;;; So a logical place to do that would be in the CLASS. We could add another
;;; slot to CLASS holding the CLASS-EQ-DIRECT-METHODS.  I guess the idea though
;;; is to be able to define other kinds of specializers that hold an object
;;; where you don't have the ability to add a slot to that object.
;;; So that's fine, you look up the object in this table, and the approach
;;; generalizes by defining other tables similarly, but has the same problem
;;; of garbage retention as did EQL specializers.
;;; I suspect CLASS-EQ specializers aren't used enough to worry about.
;;;
;;; This table is shared between threads, so needs to be synchronized.
;;; (though insertions are only performed when holding the specializer-lock,
;;; so the preceding claim is probably overly paranoid.)
(define-load-time-global *class-eq-specializer-methods*
    (make-hash-table :test 'eq :synchronized t))

(defmethod specializer-method-table ((specializer class-eq-specializer))
  *class-eq-specializer-methods*)

(defmethod specializer-method-holder ((self specializer-with-object)
                                      &optional (create t))
  (let ((table (specializer-method-table self))
        (object (specializer-object self)))
    (if create
        (with-system-mutex ((hash-table-lock table))
          (ensure-gethash object table (cons nil nil)))
        ;; FIXME: in the CREATE case we're locking, because we might be inserting,
        ;; and must ensure mutual exclusion with other writers. Fine,
        ;; but SBCL's hash-tables aren't *reader* *safe* with any writer, so
        ;; how can we know that this branch is safe? Honestly I have no idea.
        ;; I believe that the lock should scope be widened,
        ;; surrounding either branch of the IF.
        (gethash object table))))

(defun map-specializers (function)
  (map-all-classes (lambda (class)
                     (funcall function (class-eq-specializer class))
                     (funcall function class)))
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
        `(eql ,(specializer-object specl))))

(defun real-load-defclass (name metaclass-name supers slots other
                           readers writers slot-names source-location &optional safe-p)
  (sb-kernel::call-with-defining-class
   'class name
   (lambda ()
     (sb-kernel::%%compiler-defclass name readers writers slot-names)
     (apply #'ensure-class name :metaclass metaclass-name
            :direct-superclasses supers
            :direct-slots slots
            'source source-location
            'safe-p safe-p
            other))))

(setf (gdefinition 'load-defclass) #'real-load-defclass)

(defun ensure-class (name &rest args)
  (with-world-lock ()
    (apply #'ensure-class-using-class
           (let ((class (find-class name nil)))
             (when (and class (eq name (class-name class)))
               ;; NAME is the proper name of CLASS, so redefine it
               class))
           name args)))

(defun parse-ensure-class-args (class name args)
  (let ((metaclass *the-class-standard-class*)
        (metaclassp nil)
        (reversed-plist '()))
    (labels ((find-class* (which class-or-name)
               (cond
                 ((classp class-or-name)
                  (cond
                    ((eq class-or-name class)
                     (error "~@<Class ~A specified as its own ~
                             ~(~A~)class.~@:>"
                            class-or-name which))
                    (t
                     class-or-name)))
                 ((and class-or-name (legal-class-name-p class-or-name))
                  (cond
                    ((eq class-or-name name)
                     (error "~@<Class named ~
                             ~/sb-ext:print-symbol-with-prefix/ ~
                             specified as its own ~(~A~)class.~@:>"
                            class-or-name which))
                    ((find-class class-or-name (eq which :meta)))
                    ((ensure-class
                      class-or-name :metaclass 'forward-referenced-class))))
                 (t
                  (error "~@<Not a class or a legal ~(~A~)class name: ~
                          ~S.~@:>"
                         which class-or-name))))
             (find-superclass (class-or-name)
               (find-class* :super class-or-name)))
      (doplist (key value) args
        (case key
          (:metaclass
           (unless metaclassp
             (setf metaclass (find-class* :meta value)
                   metaclassp key)))
          (:direct-superclasses
           (let ((superclasses (mapcar #'find-superclass value)))
             (setf reversed-plist (list* superclasses key reversed-plist))))
          (t
           (setf reversed-plist (list* value key reversed-plist)))))
      (values metaclass (nreverse reversed-plist)))))

(defun call-with-ensure-class-context (class name args thunk)
  (let ((class (with-world-lock ()
                 (multiple-value-bind (metaclass initargs)
                     (parse-ensure-class-args class name args)
                   (let ((class (funcall thunk class name metaclass initargs)))
                     (without-package-locks
                       (setf (find-class name) class)))))))
    class))

(defmethod ensure-class-using-class ((class null) name &rest args &key)
  (call-with-ensure-class-context
   class name args (lambda (class name metaclass initargs)
                     (declare (ignore class))
                     (apply #'make-instance metaclass :name name initargs))))

(defmethod ensure-class-using-class ((class pcl-class) name &rest args &key)
  (call-with-ensure-class-context
   class name args (lambda (class name metaclass initargs)
                     (aver (eq name (class-name class)))
                     (unless (eq (class-of class) metaclass)
                       (apply #'change-class class metaclass initargs))
                     (apply #'reinitialize-instance class initargs)
                     class)))

;;; This is used to call initfunctions of :allocation :class slots.
(defun call-initfun (fun slotd safe)
  (declare (function fun))
  (let ((value (funcall fun)))
    (when safe
      (let ((type (slot-definition-type slotd)))
        (unless (or (eq t type)
                    (typep value type))
          (error 'type-error :expected-type type :datum value))))
    value))

(defmethod shared-initialize :after
    ((class std-class) slot-names &key
     (direct-superclasses nil direct-superclasses-p)
     (direct-slots nil direct-slots-p)
     (direct-default-initargs nil direct-default-initargs-p))
  (cond (direct-superclasses-p
         (setq direct-superclasses
               (or direct-superclasses
                   (list (if (funcallable-standard-class-p class)
                             *the-class-funcallable-standard-object*
                             *the-class-standard-object*))))
         (dolist (superclass direct-superclasses)
           (unless (validate-superclass class superclass)
             (invalid-superclass class superclass)))
         (setf (slot-value class 'direct-superclasses) direct-superclasses))
        (t
         (setq direct-superclasses (slot-value class 'direct-superclasses))))
  (setq direct-slots
        (if direct-slots-p
            (setf (slot-value class 'direct-slots)
                  (mapcar (lambda (pl) (make-direct-slotd class pl))
                          direct-slots))
            (slot-value class 'direct-slots)))
  (when direct-default-initargs-p
    (setf (plist-value class 'direct-default-initargs) direct-default-initargs))
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
  (add-slot-accessors class direct-slots)
  (make-preliminary-layout class))

(define-condition invalid-superclass (reference-condition error)
  ((class :initarg :class :reader invalid-superclass-class)
   (superclass :initarg :superclass :reader invalid-superclass-superclass))
  (:report
   (lambda (c s)
     (let ((class (invalid-superclass-class c))
           (superclass (invalid-superclass-superclass c)))
       (format s
               "~@<The class ~S was specified as a superclass of the ~
                class ~S, but the metaclasses ~S and ~S are ~
                incompatible.~@[  Define a method for ~S to avoid this ~
                error.~]~@:>"
               superclass class (class-of superclass) (class-of class)
               (and (typep superclass 'standard-class)
                    'validate-superclass))))))

(defmethod invalid-superclass ((class class) (superclass class))
  (error 'invalid-superclass :class class :superclass superclass
         :references `((:amop :generic-function validate-superclass)
                       ,@(when (typep superclass 'built-in-class)
                           '((:ansi-cl :system-class built-in-class)
                             (:ansi-cl :section (4 3 7)))))))

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
          (let ((name (class-name class))) ; flushable?
            (declare (ignore name))
            ;; KLUDGE: This is fairly horrible.  We need to make a
            ;; full-fledged CLASSOID here, not just tell the compiler that
            ;; some class is forthcoming, because there are legitimate
            ;; questions one can ask of the type system, implemented in
            ;; terms of CLASSOIDs, involving forward-referenced classes. So.
            (let ((wrapper (make-wrapper 0 class)))
              (setf (slot-value class 'wrapper) wrapper)
              (let ((cpl (compute-preliminary-cpl class)))
                (set-layout-inherits wrapper
                                     (order-layout-inherits
                                      (map 'simple-vector #'class-wrapper
                                           (reverse (rest cpl))))
                                     nil 0))
              (set-bitmap-and-flags wrapper)
              (register-layout wrapper :invalidate t))))
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
  (remove-slot-accessors class (class-direct-slots class)))

(defmethod reinitialize-instance :after ((class slot-class)
                                         &rest initargs
                                         &key)
  (map-dependents class
                  (lambda (dependent)
                    (apply #'update-dependent class dependent initargs))))

(defmethod reinitialize-instance :after ((class condition-class) &key)
  (let* ((name (class-name class))
         (classoid (find-classoid name))
         (slots (condition-classoid-slots classoid))
         (source (sb-kernel::classoid-source-location classoid)))
    ;; to balance the REMOVE-SLOT-ACCESSORS call in
    ;; REINITIALIZE-INSTANCE :BEFORE (SLOT-CLASS).
    (flet ((add-source-location (method)
             (when source
               (setf (slot-value method 'source) source))))
     (dolist (slot slots)
       (let ((slot-name (condition-slot-name slot)))
         (dolist (reader (condition-slot-readers slot))
           ;; FIXME: see comment in SHARED-INITIALIZE :AFTER
           ;; (CONDITION-CLASS T), below.  -- CSR, 2005-11-18
           (add-source-location
            (sb-kernel::install-condition-slot-reader reader name slot-name)))
         (dolist (writer (condition-slot-writers slot))
           (add-source-location
            (sb-kernel::install-condition-slot-writer writer name slot-name))))))))

(defmethod shared-initialize :after ((class condition-class) slot-names
                                     &key direct-slots direct-superclasses)
  (declare (ignore slot-names))
  (let ((classoid (find-classoid (slot-value class 'name))))
    (with-slots (wrapper
                 %class-precedence-list cpl-available-p finalized-p
                 prototype (direct-supers direct-superclasses)
                 plist)
        class
      (setf (slot-value class 'direct-slots)
            (mapcar (lambda (pl) (make-direct-slotd class pl))
                    direct-slots)
            finalized-p t
            (classoid-pcl-class classoid) class
            direct-supers direct-superclasses
            wrapper (classoid-layout classoid)
            %class-precedence-list (compute-class-precedence-list class)
            cpl-available-p t
            (getf plist 'direct-default-initargs)
            (sb-kernel::condition-classoid-direct-default-initargs classoid))
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
  (let* ((slotd (call-next-method))
         (info (slot-definition-info slotd)))
    (setf (slot-info-reader info)
          (lambda (x)
            (handler-case (condition-slot-value x slot-name)
              ;; FIXME: FIND-SLOT-DEFAULT throws an error if the slot
              ;; is unbound; maybe it should be a CELL-ERROR of some
              ;; sort?
              (error () (values (slot-unbound class x slot-name))))))
    (setf (slot-info-writer info)
          (lambda (v x)
            (set-condition-slot-value x v slot-name)))
    (setf (slot-info-boundp info)
          (lambda (x)
            (condition-slot-boundp x slot-name)))
    (setf (slot-info-makunbound info)
          (lambda (x)
            (condition-slot-makunbound x slot-name)))
    slotd))

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
  (let* ((conc-name (pkg-format-symbol *package* "~S structure class " name))
         (constructor (pkg-format-symbol *package* "~Aconstructor" conc-name))
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

;;; Return a thunk to allocate an instance of CLASS named NAME.
;;; This is broken with regard to the expectation that all semantic
;;; processing is done by the compiler. e.g. after COMPILE-FILE on
;;; a file containing these two toplevel forms:
;;;  (eval-when (:compile-toplevel) (defmacro maker () '(list 1)))
;;;  (defstruct foo (a (maker)))
;;; then LOADing the resulting fasl in a fresh image will err:
;;;  (make-instance 'foo) => "The function MAKER is undefined."
;;;
;;; The way to fix that is to ensure that every defstruct has a zero-argument
;;; constructor made by the compiler and stashed in a random symbol.
(defun make-defstruct-allocation-function (name class)
  (declare (muffle-conditions code-deletion-note))
  ;; FIXME: Why don't we go class->layout->info == dd
  (let ((dd (find-defstruct-description name)))
    (ecase (dd-type dd)
      (structure
       ;; This used to call COMPILE directly, which is basically pointless,
       ;; because it certainly does not avoid compiling code at runtime,
       ;; which seems to have been the goal. We're also compiling:
       ;;  (LAMBDA () (SB-PCL::FAST-MAKE-INSTANCE #<STRUCTURE-CLASS THING>))
       ;; So maybe we can figure out how to bundle two lambdas together?
       (lambda ()
         (let* ((dd (layout-dd (class-wrapper class)))
                (f (%make-structure-instance-allocator dd nil nil)))
           (if (functionp f)
               (funcall (setf (slot-value class 'defstruct-constructor) f))
               (error "Can't allocate ~S" class)))))
      (funcallable-structure
       ;; FIXME: you can't dynamically define new funcallable structures
       ;; that are not GENERIC-FUNCTION subtypes, so why this branch?
       ;; We should pull this out, fixup PCL bootstrap, and not pretend
       ;; that this code is more general than it really is.
       (%make-funcallable-structure-instance-allocator dd nil)))))

(defmethod shared-initialize :after
    ((class structure-class) slot-names &key
     (direct-superclasses nil direct-superclasses-p)
     (direct-slots nil direct-slots-p)
     direct-default-initargs)
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
         ;; DEFSTRUCT-P means we should perform the effect of DEFSTRUCT,
         ;; and not that this structure came from a DEFSTRUCT.
         (defstruct-p (or from-defclass-p (not (structure-type-p name)))))
    (if direct-slots-p
        (setf (slot-value class 'direct-slots)
              (setq direct-slots
                    (mapcar (lambda (pl)
                              (when defstruct-p
                                (let* ((slot-name (getf pl :name))
                                       (accessor
                                        (pkg-format-symbol *package*
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
        ;; FIXME: If we always need a default constructor, then why not just make
        ;; a "hidden" one at actual-compile-time and store it?
        ;; And this is very broken with regard to the lexical environment.
        ;; And why isn't this just DD-DEFAULT-CONSTRUCTOR if there was one?
        (setf (slot-value class 'defstruct-constructor)
              ;; KLUDGE: not class; in fixup.lisp, can't access slots
              ;; outside methods yet.
              (make-defstruct-allocation-function name class)))
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
        (sb-kernel::install-struct-slot-mapper layout)
        (setf (layout-slot-table layout) (make-slot-table class slots))))
    (setf (slot-value class 'finalized-p) t)
    (add-slot-accessors class direct-slots)))

(defmethod direct-slot-definition-class ((class structure-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'structure-direct-slot-definition))

(defmethod finalize-inheritance ((class structure-class))
  nil) ; always finalized

(defun add-slot-accessors (class dslotds)
  (fix-slot-accessors class dslotds 'add))

(defun remove-slot-accessors (class dslotds)
  (fix-slot-accessors class dslotds 'remove))

(defun fix-slot-accessors (class dslotds add/remove)
  (flet ((fix (gfspec name r/w doc source-location)
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
            (slot-doc (%slot-definition-documentation dslotd))
            (location (definition-source dslotd)))
        (dolist (r (slot-definition-readers dslotd))
          (fix r slot-name 'r slot-doc location))
        (dolist (w (slot-definition-writers dslotd))
          (fix w slot-name 'w slot-doc location))))))

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
  (or (when (forward-referenced-class-p class)
        class)
      (some #'class-has-a-forward-referenced-superclass-p
            (class-direct-superclasses class))))

;;; This is called by :after shared-initialize whenever a class is initialized
;;; or reinitialized. The class may or may not be finalized.
(defun update-class (class finalizep)
  (labels ((rec (class finalizep &optional (seen '()))
             (when (find class seen :test #'eq)
               (error "~@<Specified class ~S as a superclass of ~
                       itself.~@:>"
                      class))
             (without-package-locks
               (with-world-lock ()
                 (when (or finalizep (class-finalized-p class))
                   (%update-cpl class (compute-class-precedence-list class))
                   ;; This invocation of UPDATE-SLOTS, in practice, finalizes the
                   ;; class
                   (%update-slots class (compute-slots class))
                   (update-gfs-of-class class)
                   (setf (plist-value class 'default-initargs) (compute-default-initargs class))
                   (update-ctors 'finalize-inheritance :class class))
                 (let ((seen (list* class seen)))
                   (dolist (sub (class-direct-subclasses class))
                     (rec sub nil seen)))))))
    (rec class finalizep)))

(define-condition cpl-protocol-violation (reference-condition error)
  ((class :initarg :class :reader cpl-protocol-violation-class)
   (cpl :initarg :cpl :reader cpl-protocol-violation-cpl))
  (:default-initargs :references '((:sbcl :node "Metaobject Protocol")))
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

(defun class-has-a-cpl-protocol-violation-p (class)
  (labels ((find-in-superclasses (class classes)
             (cond
               ((null classes) nil)
               ((eql class (car classes)) t)
               (t (find-in-superclasses class (append (class-direct-superclasses (car classes)) (cdr classes)))))))
    (let ((metaclass (class-of class)))
      (cond
        ((eql metaclass *the-class-standard-class*)
         (find-in-superclasses (find-class 'function) (list class)))
        ((eql metaclass *the-class-funcallable-standard-class*)
         (not (find-in-superclasses (find-class 'function) (list class))))))))

(defun %update-cpl (class cpl)
  (when (or (and
             (eq (class-of class) *the-class-standard-class*)
             (find *the-class-function* cpl))
            (and (eq (class-of class) *the-class-funcallable-standard-class*)
                 (not (and (find (find-class 'function) cpl)))))
    (error 'cpl-protocol-violation :class class :cpl cpl))
  (cond ((not (class-finalized-p class))
         (setf (slot-value class '%class-precedence-list) cpl
               (slot-value class 'cpl-available-p) t))
        ((not (and (equal (class-precedence-list class) cpl)
                   (dolist (c cpl t)
                     (when (position :class (class-direct-slots c)
                                     :key #'slot-definition-allocation)
                       (return nil)))))

         ;; comment from the old CMU CL sources:
         ;;   Need to have the cpl setup before %update-lisp-class-layout
         ;;   is called on CMU CL.
         (setf (slot-value class '%class-precedence-list) cpl
               (slot-value class 'cpl-available-p) t)
         (%force-cache-flushes class)))
  (update-class-can-precede-p cpl))

(defun update-class-can-precede-p (cpl)
  (when cpl
    (let ((first (car cpl)))
      (dolist (c (cdr cpl))
        ;; This is (PUSHNEW c (SLOT-VALUE FIRST 'can-precede-list) :TEST #'EQ)))
        ;; but avoids consing in an arena. Perhaps the ADJOIN transform could sense
        ;; whether to change %ADJOIN-EQ to SYS-TLAB-ADJOIN-EQ ?
        (with-slots (can-precede-list) first
          (setf can-precede-list (sys-tlab-adjoin-eq c can-precede-list)))))
    (update-class-can-precede-p (cdr cpl))))

(defun class-can-precede-p (class1 class2)
  (member class2 (class-can-precede-list class1) :test #'eq))

;;; This is called from %UPDATE-SLOTS to check if slot layouts are compatible.
;;;
;;; In addition to slot locations (implicit in the ordering of the slots), we
;;; must check classes: SLOT-INFO structures from old slotds may have been
;;; cached in permutation vectors, but new slotds have had new ones allocated
;;; to them. This is non-problematic for standard slotds, because we know the
;;; structure is compatible, but if a slot definition class changes, this can
;;; change the way SLOT-VALUE-USING-CLASS should dispatch.
;;;
;;; Also, if the slot has a non-standard allocation, we need to check that it
;;; doesn't change.
(defun slot-layouts-compatible-p
    (oslotds new-instance-slotds new-class-slotds new-custom-slotds)
  (multiple-value-bind (old-instance-slotds old-class-slotds old-custom-slotds)
      (classify-slotds oslotds)
    (and
     ;; Instance slots: name, type, and class.
     (dolist (o old-instance-slotds (not new-instance-slotds))
       (let ((n (pop new-instance-slotds)))
         (unless (and n
                      (eq (slot-definition-name o) (slot-definition-name n))
                      (eq (slot-definition-type o) (slot-definition-type n))
                      (eq (class-of o) (class-of n)))
           (return nil))))
     ;; Class slots: name and class. (FIXME: class slots not typechecked?)
     (dolist (o old-class-slotds (not new-class-slotds))
       (let ((n (pop new-class-slotds)))
         (unless (and n
                      (eq (slot-definition-name o) (slot-definition-name n))
                      (eq (class-of n) (class-of o)))
           (return nil))))
     ;; Custom slots: check name, type, allocation, and class. (FIXME: should we just punt?)
     (dolist (o old-custom-slotds (not new-custom-slotds))
       (let ((n (pop new-custom-slotds)))
         (unless (and n
                      (eq (slot-definition-name o) (slot-definition-name n))
                      (eq (slot-definition-type o) (slot-definition-type n))
                      (eq (slot-definition-allocation o) (slot-definition-allocation n))
                      (eq (class-of o) (class-of n)))
           (return nil)))))))

(defun style-warn-about-duplicate-slots (class)
  (flet ((symbol-exported-p (symbol)
           (let ((packages (list-all-packages))
                 (name (symbol-name symbol)))
             (dolist (package packages nil)
               (multiple-value-bind (s status) (find-symbol name package)
                 (when (and (eq s symbol) (eq status :external))
                   (return t)))))))
    (do* ((dslots (class-direct-slots class) (cdr dslots))
          (slots (slot-value class 'slots))
          (dupes nil))
         ((null dslots)
          (when dupes
            (style-warn
             "~@<slot names with the same SYMBOL-NAME but ~
                 different SYMBOL-PACKAGE (possible package problem) ~
                 for class ~S:~4I~@:_~<~@{~/sb-ext:print-symbol-with-prefix/~^~:@_~}~:>~@:>"
             class dupes)))
      (let* ((slot-name (slot-definition-name (car dslots)))
             (oslots (remove-if
                      (lambda (slot-name-2)
                        (or (eq slot-name slot-name-2)
                            (string/= slot-name slot-name-2)
                            (not (symbol-exported-p slot-name-2))))
                      slots
                      :key #'slot-definition-name)))
        (when oslots
          (pushnew (cons slot-name (mapcar #'slot-definition-name oslots)) dupes
                   :test #'string= :key #'car))))))

(defun %update-slots (class eslotds)
  (multiple-value-bind (instance-slots class-slots custom-slots)
      (classify-slotds eslotds)
    (let* ((nslots (length instance-slots))
           (owrapper (class-wrapper class))
           (nwrapper
             (cond ((and owrapper
                         (slot-layouts-compatible-p (wrapper-slot-list owrapper)
                                                    instance-slots class-slots custom-slots))
                    owrapper)
                   ((or (not owrapper)
                        (not (class-finalized-p class)))
                    (make-wrapper nslots class))
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
            (wrapper-slot-list nwrapper) eslotds
            (layout-slot-table nwrapper) (make-slot-table class eslotds)
            (layout-length nwrapper) nslots
            (slot-value class 'wrapper) nwrapper)
      (style-warn-about-duplicate-slots class)
      (setf (slot-value class 'finalized-p) t))))

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

;; It seems to me that this should be one method defined on SLOT-CLASS.
(defmethod compute-slots ((class standard-class))
  (std-compute-slots class))
(defmethod compute-slots ((class funcallable-standard-class))
  (std-compute-slots class))
(defmethod compute-slots ((class structure-class))
  (std-compute-slots class))
(defmethod compute-slots ((class condition-class))
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
                 (if (unbound-marker-p (cdr cell))
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
      (let ((info (slot-definition-info eslotd)))
        (setf (slot-info-allocation info) (slot-definition-allocation eslotd)
              (slot-info-location info) (slot-definition-location eslotd)))
      (initialize-internal-slot-functions eslotd))))

(defmethod compute-slots :around ((class standard-class))
  (let ((eslotds (call-next-method)))
    (std-compute-slots-around class eslotds)))
(defmethod compute-slots :around ((class funcallable-standard-class))
  (let ((eslotds (call-next-method)))
    (std-compute-slots-around class eslotds)))
(defmethod compute-slots :around ((class structure-class))
  (let ((eslotds (call-next-method)))
    (mapc #'finalize-internal-slot-functions eslotds)
    eslotds))

(defmethod compute-effective-slot-definition ((class slot-class) name dslotds)
  (let* ((initargs (compute-effective-slot-definition-initargs class dslotds))
         (class (apply #'effective-slot-definition-class class initargs))
         (slotd (apply #'make-instance class initargs)))
    slotd))

(defmethod effective-slot-definition-class ((class std-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'standard-effective-slot-definition))

(defmethod effective-slot-definition-class ((class structure-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'structure-effective-slot-definition))

(defmethod compute-effective-slot-definition-initargs
    ((class slot-class) direct-slotds)
  (let ((name nil)
        (initfunction nil)
        (initform nil)
        (initargs nil)
        (allocation nil)
        (allocation-class nil)
        (type t)
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
        (setq initargs (union (slot-definition-initargs slotd) initargs))
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
          :class class
          :documentation documentation)))

(defmethod compute-effective-slot-definition-initargs :around
    ((class structure-class) direct-slotds)
  (let* ((slotd (car direct-slotds))
         (accessor (slot-definition-defstruct-accessor-symbol slotd)))
    (list* :defstruct-accessor-symbol accessor
           :internal-reader-function
           (slot-definition-internal-reader-function slotd)
           :internal-writer-function
           (slot-definition-internal-writer-function slotd)
           :always-bound-p
           (slot-definition-always-bound-p slotd)
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
                             'source source-location)))

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
                             'source source-location)))

(defmethod remove-reader-method ((class slot-class) generic-function)
  (let ((method
          (and (= (length (arg-info-metatypes (gf-arg-info generic-function))) 1)
               (get-method generic-function () (list class) nil))))
    (when method (remove-method generic-function method))))

(defmethod remove-writer-method ((class slot-class) generic-function)
  (let ((method
         (and (= (length (arg-info-metatypes (gf-arg-info generic-function))) 2)
              (get-method generic-function () (list *the-class-t* class) nil))))
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

(defmethod compatible-meta-class-change-p (class proto-new-class)
  (eq (class-of class) (class-of proto-new-class)))

(defmethod validate-superclass ((class class) (superclass class))
  (or (eq (class-of class) (class-of superclass))
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
  (with-world-lock ()
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
          (setf (wrapper-slot-list nwrapper) (wrapper-slot-list owrapper))
          (setf (layout-slot-table nwrapper) (layout-slot-table owrapper))
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
  nil)

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
      (setf (wrapper-slot-list nwrapper) (wrapper-slot-list owrapper))
      (setf (layout-slot-table nwrapper) (layout-slot-table owrapper))
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

(defun %ensure-slot-value-type (context slot-name slot-type value
                                old-class new-class)
  (do () ((typep value slot-type))
    (restart-case
        (error 'simple-type-error
               :datum value
               :expected-type slot-type
               :format-control
                  (sb-format:tokens
                  "~@<Error during ~A. Current value in slot ~
                   ~/sb-ext:print-symbol-with-prefix/ of an instance ~
                   of ~S is ~S, which does not match the new slot type ~
                   ~S in class ~S.~:@>")
               :format-arguments
               (list context slot-name old-class value slot-type new-class))
      (use-value (new-value)
        :interactive read-evaluated-form
        :report (lambda (stream)
                  (format stream "~@<Specify a new value to by used ~
                                  for slot ~
                                  ~/sb-ext:print-symbol-with-prefix/ ~
                                  instead of ~S.~@:>"
                          slot-name value))
        (setf value new-value))))
  value)

(defun %set-slot-value-checking-type (context slots slot value
                                      safe old-class new-class)
  (let ((value (if (and safe (not (unbound-marker-p value)))
                   (let ((name (slot-definition-name slot))
                         (type (slot-definition-type slot)))
                     (%ensure-slot-value-type context name type value
                                              old-class new-class))
                   value)))
    (setf (clos-slots-ref slots (slot-definition-location slot)) value)))

(defvar *in-obsolete-instance-trap* nil)

(define-condition obsolete-structure (error)
  ((datum :reader obsolete-structure-datum :initarg :datum))
  (:report
   (lambda (condition stream)
     ;; Don't try to print the structure, since it probably won't work.
     (format stream
             "~@<obsolete structure error for a structure of type ~2I~_~S~:>"
             (type-of (obsolete-structure-datum condition))))))

(macrolet ((replace-wrapper-and-slots (thing layout slot-vector)
             `(if (functionp ,thing)
                  (setf (%fun-layout ,thing) ,layout
                        (fsc-instance-slots ,thing) ,slot-vector)
                  ;; TODO: use a double-wide CAS here if CPU supports it
                  (progn
                    (setf (%instance-layout ,thing) ,layout)
                    (%instance-set ,thing sb-vm:instance-data-start ,slot-vector)))))

(defun %obsolete-instance-trap (owrapper nwrapper instance)
  (cond
    ((layout-for-pcl-obj-p owrapper)
     (binding* ((class (wrapper-class nwrapper))
                (oslots (get-slots instance))
                (nwrapper (class-wrapper class))
                (nslots (make-array (layout-length nwrapper)
                                    :initial-element +slot-unbound+))
                (added ())
                (discarded ())
                (plist ())
                (safe (safe-p class))
                ((new-instance-slots nil new-custom-slots)
                 (classify-slotds (wrapper-slot-list nwrapper)))
                ((old-instance-slots old-class-slots old-custom-slots)
                 (classify-slotds (wrapper-slot-list owrapper)))
                (layout (mapcar (lambda (slotd)
                                  ;; Get the names only once.
                                  (cons (slot-definition-name slotd) slotd))
                                new-instance-slots)))
       ;; local  --> local     transfer value, check type
       ;; local  --> shared    discard value, discard slot
       ;; local  -->  --       discard slot
       ;; local  --> custom    XXX

       ;; shared --> local     transfer value, check type
       ;; shared --> shared    -- (cf SHARED-INITIALIZE :AFTER STD-CLASS)
       ;; shared -->  --       discard value
       ;; shared --> custom    XXX

       ;;  --    --> local     add slot
       ;;  --    --> shared    --
       ;;  --    --> custom    XXX
       (flet ((set-value (value cell)
                (%set-slot-value-checking-type
                 "updating obsolete instance"
                 nslots (cdr cell) value safe class class)
                ;; Prune from the list now that it's been dealt with.
                (setf layout (remove cell layout))))

         ;; Go through all the old local slots.
         (dolist (old old-instance-slots)
           (let* ((name (slot-definition-name old))
                  (cell (find-slot-cell owrapper name))
                  (location (car cell))
                  (value (cond
                           ((fixnump location)
                            (clos-slots-ref oslots location))
                           ((not location)
                            (clos-slots-ref oslots (slot-info-location (cdr cell))))
                           (t (bug "non-FIXNUM non-NULL location in cell: ~S" cell)))))
             (unless (unbound-marker-p value)
               (let ((new (assq name layout)))
                 (cond (new
                        (set-value value new))
                       (t
                        (push name discarded)
                        (setf (getf plist name) value)))))))

         ;; Go through all the old shared slots.
         (dolist (old old-class-slots)
           (binding* ((cell (slot-definition-location old))
                      (name (car cell))
                      (new (assq name layout) :exit-if-null))
             (set-value (cdr cell) new)))

         ;; Go through all custom slots to find added ones. CLHS
         ;; doesn't specify what to do about them, and neither does
         ;; AMOP. We do want them to get initialized, though, so we
         ;; list them in ADDED for the benefit of SHARED-INITIALIZE.
         (dolist (new new-custom-slots)
           (let* ((name (slot-definition-name new))
                  (old (find name old-custom-slots
                             :key #'slot-definition-name)))
             (unless old
               (push name added))))

         ;; Go through all the remaining new local slots to compute
         ;; the added slots.
         (dolist (cell layout)
           (push (car cell) added)))

       (replace-wrapper-and-slots instance nwrapper nslots)
       ;; The obsolete instance protocol does not specify what happens if
       ;; an error is signaled in U-I-F-R-C and there is a nonlocal exit
       ;; outside; it may result in a half-updated instance whose
       ;; structure is updated but whose added slots are not initialized.
       ;; (See CLHS 3.7.2.)
       ;; The approach taken here is to abort the update process, as defined
       ;; in CLHS 4.3.6, altogether, and restore the instance to its obsolete
       ;; state; this way the programmer can try to fix the U-I-F-R-C code
       ;; which signaled an error and try to access the instance again
       ;; in order to try and update it again.
       (sb-sys:nlx-protect (update-instance-for-redefined-class
                            instance added discarded plist)
         (replace-wrapper-and-slots instance owrapper oslots))

       nwrapper))
    (*in-obsolete-instance-trap* #.(find-layout 'structure-object))
    (t
     (let ((*in-obsolete-instance-trap* t))
       (error 'obsolete-structure :datum instance)))))

(defun %change-class (copy instance new-class initargs)
  (binding* ((new-wrapper (class-wrapper (ensure-class-finalized new-class)))
             (new-slots (make-array (layout-length new-wrapper)
                                    :initial-element +slot-unbound+))
             (old-wrapper (layout-of instance))
             (old-class (wrapper-class old-wrapper))
             (old-slots (get-slots instance))
             (safe (safe-p new-class))
             (new-wrapper-slots (wrapper-slot-list new-wrapper)))
    (replace-wrapper-and-slots copy new-wrapper new-slots)
    (flet ((initarg-for-slot-p (slot)
             (when initargs
               (dolist (slot-initarg (slot-definition-initargs slot))
                 (unless (unbound-marker-p
                          (getf initargs slot-initarg +slot-unbound+))
                   (return t)))))
           (set-value (value slotd)
             (%set-slot-value-checking-type
              'change-class new-slots slotd value safe
              old-class new-class)))

      ;; "The values of local slots specified by both the class CTO
      ;; and CFROM are retained. If such a local slot was unbound, it
      ;; remains unbound."
      (dolist (new new-wrapper-slots)
        (when (and (not (initarg-for-slot-p new))
                   (eq (slot-definition-allocation new) :instance))
          (binding* ((cell (find-slot-cell old-wrapper (slot-definition-name new))
                           :exit-if-null)
                     (location (car cell))
                     (value (cond
                              ((fixnump location)
                               (clos-slots-ref old-slots location))
                              ((not location)
                               (let ((info (cdr cell)))
                                 (case (slot-info-allocation info)
                                   (:instance
                                    (clos-slots-ref old-slots (slot-info-location info)))
                                   (:class (cdr (slot-info-location info))))))
                              (t
                               (cdr location)))))
            (set-value value new)))))

    ;; Make the copy point to the old instance's storage, and make the
    ;; old instance point to the new storage.
    ;; All uses of %CHANGE-CLASS are under the world lock, but that doesn't
    ;; preclude user code operating on the old slots + new layout or v.v.
    ;; Users need to synchronize their own access when changing class.
    (replace-wrapper-and-slots copy old-wrapper old-slots)
    (replace-wrapper-and-slots instance new-wrapper new-slots)

    ;; The CLHS does not specify what happens if an error is signaled in
    ;; U-I-F-D-C and there is a nonlocal exit outside; it may result in a
    ;; half-updated instance whose class is updated but whose added slots
    ;; are not initialized. (See CLHS 3.7.2.)
    ;; The approach taken here is to abort the change-class process, as
    ;; defined in CLHS 4.3.6, altogether, and restore the instance to its
    ;; previous state; this way the programmer can try to fix the U-I-F-D-C
    ;; code which signaled an error and try to CHANGE-CLASS the instance
    ;; again.
    (sb-sys:nlx-protect (apply #'update-instance-for-different-class
                               copy instance initargs)
      (replace-wrapper-and-slots instance old-wrapper old-slots))

    instance))
) ; end MACROLET

(defun check-new-class-not-metaobject (new-class)
  (dolist (class (class-precedence-list
                  (ensure-class-finalized new-class)))
    (macrolet
        ((check-metaobject (class-name)
           `(when (eq class (find-class ',class-name))
              (change-class-to-metaobject-violation
               ',class-name nil '((:amop :initialization ,class-name))))))
      (check-metaobject class)
      (check-metaobject generic-function)
      (check-metaobject method)
      (check-metaobject slot-definition))))

;;; "The first argument to update-instance-for-different-class, /previous/,
;;; is that copy; it holds the old slot values temporarily. This argument has
;;; dynamic extent within change-class; if it is referenced in any way once
;;; update-instance-for-different-class returns, the results are undefined."
;;; The full ALLOCATE-INSTANCE protocol can not possibly support dynamic-extent
;;; allocation (at least, for SBCL; maybe for others it can).
;;; Calling ALLOCATE-INSTANCE from CHANGE-CLASS doesn't seem to be mandatory.
;;; At least 4 other Lisp implementations I tested don't call it.
(macrolet ((with-temporary-instance ((var) &body body)
             `(dx-let ((,var (%make-instance (1+ sb-vm:instance-data-start))))
                ,@body)))
(defmethod change-class ((instance standard-object) (new-class standard-class)
                         &rest initargs)
  (with-world-lock ()
    (check-new-class-not-metaobject new-class)
    (with-temporary-instance (temp)
      (%change-class temp instance new-class initargs))))

(defmethod change-class ((instance forward-referenced-class)
                         (new-class standard-class) &rest initargs)
  (with-world-lock ()
    (dolist (class (class-precedence-list
                    (ensure-class-finalized new-class))
             (change-class-to-metaobject-violation
              '(not class) 'forward-referenced-class
              '((:amop :generic-function ensure-class-using-class)
                (:amop :initialization class))))
      (when (eq class (find-class 'class))
        (return nil)))
    (with-temporary-instance (temp)
      (%change-class temp instance new-class initargs)))))

(defmethod change-class ((instance t)
                         (new-class forward-referenced-class) &rest initargs)
  (declare (ignore initargs))
  (change-class-to-metaobject-violation
   'forward-referenced-class nil
   '((:amop :generic-function ensure-class-using-class)
     (:amop :initialization class))))

(macrolet ((with-temporary-funinstance ((var) &body body)
             `(dx-let ((,var (%make-funcallable-instance
                              (+ sb-vm:instance-data-start 2))))
                (dx-flet ((signal-error ()
                            (declare (optimize (sb-c::verify-arg-count 0)))
                            (error-no-implementation-function ,var)))
                  (setf (%funcallable-instance-fun ,var) #'signal-error)
                  ,@body))))
(defmethod change-class ((instance funcallable-standard-object)
                         (new-class funcallable-standard-class)
                         &rest initargs)
  (with-world-lock ()
    (check-new-class-not-metaobject new-class)
    (with-temporary-funinstance (temp)
      (%change-class temp instance new-class initargs)))))

(defmethod change-class ((instance standard-object)
                         (new-class funcallable-standard-class)
                         &rest initargs)
  (declare (ignore initargs))
  (error "You can't change the class of ~S to ~S~@
          because it isn't already an instance with metaclass ~S."
         instance new-class 'funcallable-standard-class))

(defmethod change-class ((instance funcallable-standard-object)
                         (new-class standard-class)
                         &rest initargs)
  (declare (ignore initargs))
  (error "You can't change the class of ~S to ~S~@
          because it isn't already an instance with metaclass ~S."
         instance new-class 'standard-class))

(defmethod change-class ((instance t) (new-class-name symbol) &rest initargs)
  (apply #'change-class instance (find-class new-class-name) initargs))

;;;; The metaclasses SYSTEM-CLASS and BUILT-IN-CLASS
;;;;
;;;; These metaclasses are something of a weird creature. By this
;;;; point, all instances which will exist have been created, and no
;;;; instance is ever created by calling MAKE-INSTANCE.  (The
;;;; distinction between the metaclasses is that we allow subclassing
;;;; of SYSTEM-CLASS, such as through STREAM and SEQUENCE protocols,
;;;; but not of BUILT-IN-CLASS.)
;;;;
;;;; AMOP mandates some behaviour of the implementation with respect
;;;; to BUILT-IN-CLASSes, and we implement that through methods on
;;;; SYSTEM-CLASS here.

(macrolet ((def (name args control)
             `(defmethod ,name ,args
                (declare (ignore initargs))
                (error 'metaobject-initialization-violation
                       :format-control ,(format nil "~~@<~A~~@:>" control)
                       :format-arguments (list (class-name class))
                       :references '((:amop :initialization "Class"))))))
  (def initialize-instance ((class system-class) &rest initargs)
    "Cannot initialize an instance of ~S.")
  (def reinitialize-instance ((class system-class) &rest initargs)
    "Cannot reinitialize an instance of ~S."))

(macrolet ((def (name) `(defmethod ,name ((class system-class)) nil)))
  (def class-direct-slots)
  (def class-slots)
  (def class-direct-default-initargs)
  (def class-default-initargs))

(defmethod validate-superclass ((c class) (s system-class))
  t)
(defmethod validate-superclass ((c class) (s built-in-class))
  nil)

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

(defmethod validate-superclass ((c slot-class) (f forward-referenced-class))
  t)

(defmethod add-dependent ((metaobject dependent-update-mixin) dependent)
  (pushnew dependent (plist-value metaobject 'dependents) :test #'eq))

(defmethod remove-dependent ((metaobject dependent-update-mixin) dependent)
  (setf (plist-value metaobject 'dependents)
        (delete dependent (plist-value metaobject 'dependents))))

(defmethod map-dependents ((metaobject dependent-update-mixin) function)
  (dolist (dependent (plist-value metaobject 'dependents))
    (funcall function dependent)))

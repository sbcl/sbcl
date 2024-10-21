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
  (declare (inline ensure-class-finalized))
  (let ((instance-or-nil (maybe-call-ctor class initargs)))
    (when instance-or-nil
      (return-from make-instance instance-or-nil)))
  (ensure-class-finalized class)
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

(defglobal **typecheck-cache** (make-hash-table :test #'equal :synchronized t))
(defvar *typecheck-stack* nil)

(defun generate-slotd-typecheck (slotd info)
  (let* ((type (slot-definition-type slotd))
         (class (slot-definition-class slotd))
         (cookie (cons class (slot-definition-name slotd))))
    (declare (dynamic-extent cookie))
    (when (and (neq t type) (safe-p class))
      (or
       ;; Have one already!
       (awhen (gethash type **typecheck-cache**)
         (setf (slot-info-typecheck info) it))
       ;; It is possible for compilation of a typecheck to trigger class
       ;; finalization, which in turn may trigger compilation of a
       ;; slot-typechecking function -- detects and break those cycles.
       ;;
       ;; We use the slow function here, but the outer call will replace it
       ;; with the fast one.
       (when (member cookie *typecheck-stack* :test #'equal)
         (setf (slot-info-typecheck info)
               (named-lambda slow-slot-typecheck (value)
                       (if (typep value type)
                           value
                           (error 'type-error
                                  :datum value
                                  :expected-type type)))))
       ;; The normal, good case: compile an efficient typecheck function.
       (let ((*typecheck-stack* (cons cookie *typecheck-stack*)))
         (handler-bind (((or style-warning compiler-note) #'muffle-warning))
           (let ((fun (pcl-compile
                       `(named-lambda (slot-typecheck ,type) (value)
                          (declare (optimize (sb-c:store-coverage-data 0)
                                             (sb-c::type-check 3)
                                             (sb-c:verify-arg-count 0)))
                          (the* (,type :restart t) value))
                       :safe)))
             (setf (gethash type **typecheck-cache**) fun
                   (slot-info-typecheck info) fun))))))))

(define-condition slotd-initialization-error (reference-condition error)
  ((initarg :initarg :initarg :reader slotd-initialization-error-initarg)
   (kind :initarg :kind :reader slotd-initialization-error-kind)
   (value :initarg :value :initform nil :reader slotd-initialization-error-value))
  (:default-initargs :references '((:amop :initialization slot-definition)))
  (:report (lambda (condition stream)
             (let ((initarg (slotd-initialization-error-initarg condition))
                   (kind (slotd-initialization-error-kind condition))
                   (value (slotd-initialization-error-value condition)))
               (format stream
                       "~@<Invalid ~S initialization: the initialization ~
                        argument ~S was ~
                        ~[missing~*~;not a symbol: ~S~;constant: ~S~].~@:>"
                       'slot-definition initarg
                       (getf '(:missing 0 :symbol 1 :constant 2) kind)
                       value)))))

(define-condition slotd-initialization-type-error (slotd-initialization-error type-error)
  ((value :initarg :datum))
  (:report (lambda (condition stream)
             (let ((initarg (slotd-initialization-error-initarg condition))
                   (datum (type-error-datum condition))
                   (expected-type (type-error-expected-type condition)))
               (format stream
                       "~@<Invalid ~S initialization: the initialization ~
                        argument ~S was ~S, which is not of type ~S.~@:>"
                       'slot-definition initarg
                       datum expected-type)))))

(defmethod initialize-instance :before ((slotd slot-definition)
                                        &key (name nil namep)
                                          (initform nil initformp)
                                          (initfunction nil initfunp)
                                          (type nil typep)
                                          (allocation nil allocationp)
                                          (initargs nil initargsp)
                                          (documentation nil docp))
  (declare (ignore initform initfunction type))
  (unless namep
    (error 'slotd-initialization-error :initarg :name :kind :missing))
  (unless (symbolp name)
    (error 'slotd-initialization-type-error :initarg :name :datum name :expected-type 'symbol))
  (when (and (constantp name)
             ;; KLUDGE: names of structure slots are weird, and their
             ;; weird behaviour gets grandfathered in this way.  (The
             ;; negative constraint is hard to express in normal
             ;; CLOS method terms).
             (not (typep slotd 'structure-slot-definition)))
    (error 'slotd-initialization-error :initarg :name :kind :constant :value name))
  (when (and initformp (not initfunp))
    (error 'slotd-initialization-error :initarg :initfunction :kind :missing))
  (when (and initfunp (not initformp))
    (error 'slotd-initialization-error :initarg :initform :kind :missing))
  (when (and typep (not t))
    ;; FIXME: do something.  Need SYNTACTICALLY-VALID-TYPE-SPECIFIER-P
    )
  (when (and allocationp (not (symbolp allocation)))
    (error 'slotd-initialization-type-error :initarg :allocation :datum allocation :expected-type 'symbol))
  (when initargsp
    (unless (typep initargs 'list)
      (error 'slotd-initialization-type-error :initarg :initarg :datum initargs :expected-type 'list))
    (do ((is initargs (cdr is)))
        ((atom is)
         (unless (null is)
           (error 'slotd-initialization-type-error :initarg :initarg :datum initargs :expected-type '(satisfies proper-list-p))))
      (unless (symbolp (car is))
        (error 'slotd-initialization-type-error :initarg :initarg :datum is :expected-type '(or null (cons symbol))))))
  (when docp
    (unless (typep documentation '(or null string))
      (error 'slotd-initialization-type-error :initarg :documentation :datum documentation :expected-type '(or null string)))))

(defmethod initialize-instance :before ((dslotd direct-slot-definition)
                                        &key
                                          (readers nil readersp)
                                          (writers nil writersp))
  (macrolet ((check (arg argp)
               `(when ,argp
                  (unless (typep ,arg 'list)
                    (error 'slotd-initialization-type-error
                           :initarg ,(keywordicate arg)
                           :datum ,arg :expected-type 'list))
                  (do ((as ,arg (cdr as)))
                      ((atom as)
                       (unless (null as)
                         (error 'slotd-initialization-type-error
                                :initarg ,(keywordicate arg)
                                :datum ,arg :expected-type '(satisfies proper-list-p))))
                    (unless (valid-function-name-p (car as))
                      (error 'slotd-initialization-type-error
                             :initarg ,(keywordicate arg)
                             :datum ,arg :expected-type '(or null (cons (satisfies valid-function-name-p)))))))))
    (check readers readersp)
    (check writers writersp)))

(defmethod initialize-instance :after ((slotd effective-slot-definition) &key)
  (let ((info (make-slot-info :slotd slotd)))
    (generate-slotd-typecheck slotd info)
    (setf (slot-definition-info slotd) info)))

;;; FIXME: Do we need (SETF SLOT-DEFINITION-TYPE) at all?
(defmethod (setf slot-definition-type) :after (new-type (slotd effective-slot-definition))
  (generate-slotd-typecheck slotd (slot-definition-info slotd)))

(defmethod update-instance-for-different-class
    ((previous standard-object) (current standard-object) &rest initargs)
  ;; First we must compute the newly added slots. The spec defines
  ;; newly added slots as "those local slots for which no slot of
  ;; the same name exists in the previous class."
  (let ((added-slots '())
        (current-slotds (class-slots (class-of current)))
        (previous-slotds (class-slots (class-of previous))))
    (dolist (slotd current-slotds)
      (when (and (eq (slot-definition-allocation slotd) :instance)
                 (not (member (slot-definition-name slotd) previous-slotds
                              :key #'slot-definition-name
                              :test #'eq)))
        (push (slot-definition-name slotd) added-slots)))
    (when initargs
      (let ((call-list (list (list* 'update-instance-for-different-class previous current initargs)
                             (list* 'shared-initialize current added-slots initargs))))
        (declare (dynamic-extent call-list))
        (check-initargs-1 (class-of current) initargs call-list)))
    (apply #'shared-initialize current added-slots initargs)))

(defmethod update-instance-for-redefined-class
    ((instance standard-object) added-slots discarded-slots property-list
     &rest initargs)
  (when initargs
    (check-initargs-1
     (class-of instance) initargs
     (list (list* 'update-instance-for-redefined-class
                  instance added-slots discarded-slots property-list initargs)
           (list* 'shared-initialize instance added-slots initargs))))
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
    (let ((class (class-of instance)))
      (loop for slotd in (class-slots class)
            unless (initialize-slot-from-initarg class instance slotd)
            do
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
      (multiple-value-bind (llks nreq nopt keys)
          (analyze-lambda-list (if (consp method)
                                   (early-method-lambda-list method)
                                   (method-lambda-list method)))
        (declare (ignore nreq nopt))
        (when (ll-kwds-allowp llks)
          (return-from check-initargs-values (values nil t)))
        (setq legal (append keys legal))))
    (values legal nil)))

(define-condition initarg-error (reference-condition program-error)
  ((class :reader initarg-error-class :initarg :class)
   (initargs :reader initarg-error-initargs :initarg :initargs))
  (:default-initargs :references '((:ansi-cl :section (7 1 2))))
  (:report (lambda (condition stream)
             (format stream "~@<Invalid initialization argument~P: ~2I~_~
                             ~<~{~S~^, ~} ~@:>~I~_in call for class ~S.~:>"
                     (length (initarg-error-initargs condition))
                     (list (initarg-error-initargs condition))
                     (initarg-error-class condition)))))

(defun initarg-error (class invalid-keys)
  (error 'initarg-error :class class :initargs invalid-keys))

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
        (initarg-error class invalid-keys)))
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
        (initarg-error class invalid-keys)))
    invalid-keys))

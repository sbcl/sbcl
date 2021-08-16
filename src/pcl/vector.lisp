;;;; permutation vectors

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

;;;; Up to 1.0.9.24 SBCL used to have a sketched out implementation
;;;; for optimizing GF calls inside method bodies using a PV approach,
;;;; inherited from the original PCL. This was never completed, and
;;;; was removed at that point to make the code easier to understand
;;;; -- but:
;;;;
;;;; FIXME: It would be possible to optimize GF calls inside method
;;;; bodies using permutation vectors: if all the arguments to the
;;;; GF are specializers parameters, we can assign a permutation index
;;;; to each such (GF . ARGS) tuple inside a method body, and use this
;;;; to cache effective method functions.

(declaim (inline make-pv-table))
(defstruct (pv-table (:predicate pv-tablep)
                     (:copier nil))
  (cache nil :type (or cache null))
  (pv-size 0 :type fixnum)
  (slot-name-lists nil :type list))
(declaim (freeze-type pv-table))

(defun make-pv-table-type-declaration (var)
  `(type pv-table ,var))

;;; Used for interning parts of SLOT-NAME-LISTS, as part of
;;; PV-TABLE interning -- just to save space.
(define-load-time-global *slot-name-lists* (make-hash-table :test 'equal))

;;; Used for interning PV-TABLES, keyed by the SLOT-NAME-LISTS
;;; used.
(define-load-time-global *pv-tables* (make-hash-table :test 'equal))

;;; ...and one lock to rule them. Lock because for certain (rare)
;;; cases this lock might be grabbed in the course of method dispatch
;;; -- and mostly this is already under the **world-lock**
(define-load-time-global *pv-lock*
  (sb-thread:make-mutex :name "pv table index lock"))

(defun intern-pv-table (&key slot-name-lists)
  (flet ((intern-slot-names (slot-names)
           (ensure-gethash slot-names *slot-name-lists* slot-names))
         (%intern-pv-table (snl)
           (ensure-gethash
            snl *pv-tables*
            (make-pv-table :slot-name-lists snl
                           :pv-size (* 2 (reduce #'+ snl :key #'length))))))
    (sb-thread:with-mutex (*pv-lock*)
      (%intern-pv-table (mapcar #'intern-slot-names slot-name-lists)))))

(defun use-standard-slot-access-p (class slot-name type)
  (or (not (eq **boot-state** 'complete))
      (and (standard-class-p class)
           (let ((slotd (find-slot-definition class slot-name)))
             (and slotd
                  (slot-accessor-std-p slotd type))))))

(defun slot-missing-info (class slot-name)
  (make-slot-info
   :reader (lambda (object)
             (values (slot-missing class object slot-name 'slot-value)))
   :boundp (lambda (object)
             (and (slot-missing class object slot-name 'slot-boundp) t))
   :writer (lambda (new-value object)
             (slot-missing class object slot-name 'setf new-value)
             new-value)))

(defun compute-pv (slot-name-lists wrappers)
  (let ((wrappers (ensure-list wrappers)))
    (collect ((pv))
      (dolist (slot-names slot-name-lists)
        (when slot-names
          (let* ((wrapper (pop wrappers))
                 (std-p (layout-for-pcl-obj-p wrapper))
                 (class (wrapper-class wrapper)))
            (dolist (slot-name slot-names)
              (destructuring-bind (location . info)
                  (or (find-slot-cell wrapper slot-name)
                      (cons nil (slot-missing-info class slot-name)))
                (unless info
                  (bug "No SLOT-INFO for ~S in ~S" slot-name class))
                (pv (when (and std-p (use-standard-slot-access-p class slot-name 'all))
                      location))
                (pv info))))))
      (coerce (pv) 'vector))))

(defun pv-table-lookup (pv-table pv-wrappers)
  (let* ((slot-name-lists (pv-table-slot-name-lists pv-table))
         (cache (or (pv-table-cache pv-table)
                    (setf (pv-table-cache pv-table)
                          (make-cache :key-count (- (length slot-name-lists)
                                                    (count nil slot-name-lists))
                                      :value t
                                      :size 2)))))
    (multiple-value-bind (hitp value) (probe-cache cache pv-wrappers)
      (if hitp
          value
          (let* ((pv (compute-pv slot-name-lists pv-wrappers))
                 (new-cache (fill-cache cache pv-wrappers pv)))
            ;; This is safe: if another thread races us here the loser just
            ;; misses the next time as well.
            (unless (eq new-cache cache)
              (setf (pv-table-cache pv-table) new-cache))
            pv)))))

(defun make-pv-type-declaration (var)
  `(type simple-vector ,var))

;;; Sometimes we want to finalize if we can, but it's OK if
;;; we can't.
(defun try-finalize-inheritance (class)
  (unless (typep class 'forward-referenced-class)
    (when (every (lambda (super)
                   (or (eq super class)
                       (class-finalized-p super)
                       (try-finalize-inheritance super)))
                 (class-direct-superclasses class))
      (finalize-inheritance class)
      t)))

(declaim (ftype (sfunction (class) class) ensure-class-finalized)
         (maybe-inline ensure-class-finalized))
(defun ensure-class-finalized (class)
  (unless (class-finalized-p class)
    (finalize-inheritance class))
  class)

(defun can-optimize-access (form required-parameters env)
  (destructuring-bind (op var-form slot-name-form &optional new-value) form
    (let ((type (ecase op
                  (slot-value 'reader)
                  (set-slot-value 'writer)
                  (slot-boundp 'boundp)))
          (var (extract-the var-form))
          (slot-name (constant-form-value slot-name-form env)))
      (when (and (symbolp var) (not (var-special-p var env)))
        (let* ((rebound? (caddr (var-declaration '%variable-rebinding var env)))
               (parameter-or-nil (car (memq (or rebound? var)
                                            required-parameters))))
          (when parameter-or-nil
            (let* ((class-name (caddr (var-declaration '%class
                                                       parameter-or-nil
                                                       env)))
                   (class (find-class class-name nil)))
              (cond ((or (not (eq **boot-state** 'complete))
                         (forward-referenced-class-p class))
                     (setf class nil))
                    ((and class (not (class-finalized-p class)))
                     (unless (try-finalize-inheritance class)
                       (when (boundp 'sb-c:*lexenv*)
                         (sb-c:compiler-notify
                          "~@<Cannot optimize slot access, inheritance of ~S is not ~
                           yet finalizable due to forward-referenced superclasses:~
                           ~%  ~S~:@>"
                          class form))
                       (setf class nil))))
              (when (and class-name (not (eq class-name t)))
                (when (not (and class
                                (memq *the-class-structure-object*
                                      (class-precedence-list class))))
                  (aver type)
                  (values (cons parameter-or-nil (or class class-name))
                          slot-name
                          new-value))))))))))

;;; Check whether the binding of the named variable is modified in the
;;; method body.
(defun parameter-modified-p (parameter-name env)
  (let ((modified-variables (%macroexpand '%parameter-binding-modified env)))
    (memq parameter-name modified-variables)))

(defun optimize-slot-value (form slots required-parameters env)
  (multiple-value-bind (sparameter slot-name)
      (can-optimize-access form required-parameters env)
    (if sparameter
        (let ((optimized-form
               (optimize-instance-access slots :read sparameter
                                         slot-name nil)))
          ;; We don't return the optimized form directly, since there's
          ;; still a chance that we'll find out later on that the
          ;; optimization should not have been done, for example due to
          ;; the walker encountering a SETQ on SPARAMETER later on in
          ;; the body [ see for example clos.impure.lisp test with :name
          ;; ((:setq :method-parameter) slot-value)) ]. Instead we defer
          ;; the decision until the compiler macroexpands
          ;; OPTIMIZED-SLOT-VALUE.
          ;;
          ;; Note that we must still call OPTIMIZE-INSTANCE-ACCESS at
          ;; this point (instead of when expanding
          ;; OPTIMIZED-SLOT-VALUE), since it mutates the structure of
          ;; SLOTS. If that mutation isn't done during the walking,
          ;; MAKE-METHOD-LAMBDA-INTERNAL won't wrap a correct PV-BINDING
          ;; form around the body, and compilation will fail.  -- JES,
          ;; 2006-09-18
          `(optimized-slot-value ,form ,(car sparameter) ,optimized-form))
        `(accessor-slot-value ,@(cdr form)))))

(defmacro optimized-slot-value (form parameter-name optimized-form
                                &environment env)
  ;; Either use OPTIMIZED-FORM or fall back to the safe
  ;; ACCESSOR-SLOT-VALUE.
  (if (parameter-modified-p parameter-name env)
      `(accessor-slot-value ,@(cdr form))
      optimized-form))

(defun optimize-set-slot-value (form slots required-parameters env)
  (multiple-value-bind (sparameter slot-name new-value)
      (can-optimize-access form required-parameters env)
    (if sparameter
        (let ((optimized-form
               (optimize-instance-access slots :write sparameter
                                         slot-name new-value (safe-code-p env))))
             ;; See OPTIMIZE-SLOT-VALUE
             `(optimized-set-slot-value ,form ,(car sparameter) ,optimized-form))
           `(accessor-set-slot-value ,@(cdr form)))))

(defmacro optimized-set-slot-value (form parameter-name optimized-form
                                    &environment env)
  (cond ((parameter-modified-p parameter-name env)
         ;; ACCESSOR-SET-SLOT-VALUE doesn't do type-checking,
         ;; so we need to use SAFE-SET-SLOT-VALUE.
         (if (safe-code-p env)
             `(safe-set-slot-value ,@(cdr form)))
             `(accessor-set-slot-value ,@(cdr form)))
        (t
         optimized-form)))

(defun optimize-slot-boundp (form slots required-parameters env)
  (multiple-value-bind (sparameter slot-name)
      (can-optimize-access form required-parameters env)
    (if sparameter
        (let ((optimized-form
               (optimize-instance-access slots :boundp sparameter
                                         slot-name nil)))
          ;; See OPTIMIZE-SLOT-VALUE
          `(optimized-slot-boundp ,form ,(car sparameter) ,optimized-form))
        `(accessor-slot-boundp ,@(cdr form)))))

(defmacro optimized-slot-boundp (form parameter-name optimized-form
                                 &environment env)
  (if (parameter-modified-p parameter-name env)
      `(accessor-slot-boundp ,@(cdr form))
      optimized-form))

;;; The SLOTS argument is an alist, the CAR of each entry is the name
;;; of a required parameter to the function. The alist is in order, so
;;; the position of an entry in the alist corresponds to the
;;; argument's position in the lambda list.
(defun optimize-instance-access (slots read/write sparameter slot-name
                                 new-value &optional safep)
  (let ((class (if (consp sparameter) (cdr sparameter) *the-class-t*))
        (parameter (if (consp sparameter) (car sparameter) sparameter)))
    (if (and (eq **boot-state** 'complete)
             (classp class)
             (memq *the-class-structure-object* (class-precedence-list class)))
        (let ((slotd (find-slot-definition class slot-name)))
          (ecase read/write
            (:read
             `(,(slot-definition-defstruct-accessor-symbol slotd) ,parameter))
            (:write
             `(setf (,(slot-definition-defstruct-accessor-symbol slotd)
                     ,parameter)
                    ,new-value))
            (:boundp
             t)))
        (let* ((parameter-entry (assq parameter slots))
               (slot-entry      (assq slot-name (cdr parameter-entry)))
               (position (posq parameter-entry slots))
               (pv-offset-form (list 'pv-offset ''.PV-OFFSET.)))
          (unless parameter-entry
            (bug "slot optimization bewilderment: O-I-A"))
          (unless slot-entry
            (setq slot-entry (list slot-name))
            (push slot-entry (cdr parameter-entry)))
          (push pv-offset-form (cdr slot-entry))
          (ecase read/write
            (:read
             `(instance-read ,pv-offset-form ,parameter ,position
                             ',slot-name ',class))
            (:write
             `(let ((.new-value. ,new-value))
                (instance-write ,pv-offset-form ,parameter ,position
                                ',slot-name ',class .new-value. ,safep)))
            (:boundp
             `(instance-boundp ,pv-offset-form ,parameter ,position
                               ',slot-name ',class)))))))

(define-walker-template pv-offset) ; These forms get munged by mutate slots.
(defmacro pv-offset (arg) arg)

;;; It is safe for these two functions to be wrong. They just try to
;;; guess what the most likely case will be.
(defun generate-fast-class-slot-access-p (class-form slot-name-form)
  (let ((class (and (constantp class-form) (constant-form-value class-form)))
        (slot-name (and (constantp slot-name-form)
                        (constant-form-value slot-name-form))))
    (and (eq **boot-state** 'complete)
         (standard-class-p class)
         (not (eq class *the-class-t*)) ; shouldn't happen, though.
         (let ((slotd (find-slot-definition class slot-name)))
           (and slotd (eq :class (slot-definition-allocation slotd)))))))

(defun constant-value-or-nil (form)
  (and (constantp form) (constant-form-value form)))

(defun slot-access-strategy (class slot-name type &optional conservative)
  ;; CONSERVATIVE means we should assume custom access pattern even if
  ;; there are no custom accessors defined if the metaclass is non-standard.
  ;;
  ;; This is needed because DEFCLASS generates accessor methods before possible
  ;; SLOT-VALUE-USING-CLASS methods are defined, which causes them to take
  ;; the slow path unless we make the conservative assumption here.
  (if (eq **boot-state** 'complete)
      (let (slotd)
        (cond ((or
                ;; Conditions, structures, and classes for which FIND-CLASS
                ;; doesn't return them yet.
                ;; FIXME: surely we can get faster accesses for structures?
                (not (standard-class-p class))
                ;; Should not happen... (FIXME: assert instead?)
                (eq class *the-class-t*)
                (not (class-finalized-p class))
                ;; Strangeness...
                (not (setf slotd (find-slot-definition class slot-name))))
               :accessor)
              ((and (slot-accessor-std-p slotd type)
                    (or (not conservative) (eq *the-class-standard-class* (class-of class))))
               ;; The best case.
               :standard)
              (t
               :custom)))
      :standard))

;;;; SLOT-VALUE

(defmacro instance-read (pv-offset parameter position slot-name class)
  (ecase (slot-access-strategy (constant-value-or-nil class)
                               (constant-value-or-nil slot-name)
                               'reader)
    (:standard
     `(instance-read-standard
       .pv. ,(slot-vector-symbol position)
       ,pv-offset (accessor-slot-value ,parameter ,slot-name)
       ,(if (generate-fast-class-slot-access-p class slot-name)
            :class :instance)))
    (:custom
     `(instance-read-custom .pv. ,pv-offset ,parameter))
    (:accessor
     `(accessor-slot-value ,parameter ,slot-name))))

(defmacro instance-read-standard (pv slots pv-offset default &optional kind)
  (unless (member kind '(nil :instance :class))
    (error "illegal kind argument to ~S: ~S" 'instance-read-standard kind))
  (let* ((index (gensym))
         (value index))
    `(locally (declare #.*optimize-speed*)
       (let ((,index (svref ,pv ,pv-offset))
             (,slots (truly-the simple-vector ,slots)))
         (setq ,value (typecase ,index
                        ;; FIXME: the line marked by KLUDGE below (and
                        ;; the analogous spot in
                        ;; INSTANCE-WRITE-STANDARD) is there purely to
                        ;; suppress a type mismatch warning that
                        ;; propagates through to user code.
                        ;; Presumably SLOTS at this point can never
                        ;; actually be NIL, but the compiler seems to
                        ;; think it could, so we put this here to shut
                        ;; it up.  (see also mail Rudi Schlatte
                        ;; sbcl-devel 2003-09-21) -- CSR, 2003-11-30
                        ,@(when (or (null kind) (eq kind :instance))
                                `((fixnum
                                   (clos-slots-ref ,slots ,index))))
                        ,@(when (or (null kind) (eq kind :class))
                                `((cons (cdr ,index))))
                        (t
                         +slot-unbound+)))
         (if (unbound-marker-p ,value)
             ,default
             ,value)))))

(defmacro instance-read-custom (pv pv-offset parameter)
  `(locally (declare #.*optimize-speed*)
     (funcall (slot-info-reader (svref ,pv (1+ ,pv-offset))) ,parameter)))

;;;; (SETF SLOT-VALUE)

(defmacro instance-write (pv-offset parameter position slot-name class new-value
                          &optional check-type-p)
  (ecase (slot-access-strategy (constant-value-or-nil class)
                               (constant-value-or-nil slot-name)
                               'writer)
    (:standard
     `(instance-write-standard
       .pv. ,(slot-vector-symbol position)
       ,pv-offset ,new-value
       ;; KLUDGE: .GOOD-NEW-VALUE. is type-checked by the time this form
       ;; is executed (if it is executed).
       (accessor-set-slot-value ,parameter ,slot-name .good-new-value.)
       ,(if (generate-fast-class-slot-access-p class slot-name)
            :class :instance)
       ,check-type-p))
    (:custom
     `(instance-write-custom .pv. ,pv-offset ,parameter ,new-value))
    (:accessor
     (if check-type-p
         ;; FIXME: We don't want this here. If it's _possible_ the fast path
         ;; is applicable, we want to use it as well.
         `(safe-set-slot-value ,parameter ,slot-name ,new-value)
         `(accessor-set-slot-value ,parameter ,slot-name ,new-value)))))

(defmacro instance-write-standard (pv slots pv-offset new-value default
                                   &optional kind safep)
  (unless (member kind '(nil :instance :class))
    (error "illegal kind argument to ~S: ~S" 'instance-write-standard kind))
  (let* ((index (gensym))
         (new-value-form
          (if safep
              `(let ((.typecheckfun. (slot-info-typecheck (svref ,pv (1+ ,pv-offset)))))
                 (declare (type (or function null) .typecheckfun.))
                 (if .typecheckfun.
                     (funcall .typecheckfun. ,new-value)
                     ,new-value))
              new-value)))
    `(locally (declare #.*optimize-speed*)
       (let ((.good-new-value. ,new-value-form)
             (,index (svref ,pv ,pv-offset)))
         (typecase ,index
           ,@(when (or (null kind) (eq kind :instance))
                   `((fixnum (and ,slots
                                  (setf (clos-slots-ref ,slots ,index)
                                        .good-new-value.)))))
           ,@(when (or (null kind) (eq kind :class))
                   `((cons (setf (cdr ,index) .good-new-value.))))
           (t ,default))))))

(defmacro instance-write-custom (pv pv-offset parameter new-value)
  `(locally (declare #.*optimize-speed*)
     (funcall (slot-info-writer (svref ,pv (1+ ,pv-offset))) ,new-value ,parameter)))

;;;; SLOT-BOUNDP

(defmacro instance-boundp (pv-offset parameter position slot-name class)
  (ecase (slot-access-strategy (constant-value-or-nil class)
                               (constant-value-or-nil slot-name)
                               'boundp)
    (:standard
     `(instance-boundp-standard
       .pv. ,(slot-vector-symbol position)
       ,pv-offset (accessor-slot-boundp ,parameter ,slot-name)
       ,(if (generate-fast-class-slot-access-p class slot-name)
            :class :instance)))
    (:custom
     `(instance-boundp-custom .pv. ,pv-offset ,parameter))
    (:accessor
     `(accessor-slot-boundp ,parameter ,slot-name))))

(defmacro instance-boundp-standard (pv slots pv-offset default
                                    &optional kind)
  (unless (member kind '(nil :instance :class))
    (error "illegal kind argument to ~S: ~S" 'instance-boundp-standard kind))
  (let* ((index (gensym)))
    `(locally (declare #.*optimize-speed*)
       (let ((,index (svref ,pv ,pv-offset)))
         (typecase ,index
           ,@(when (or (null kind) (eq kind :instance))
                   `((fixnum (not (and ,slots
                                       (unbound-marker-p
                                        (clos-slots-ref ,slots ,index)))))))
           ,@(when (or (null kind) (eq kind :class))
                   `((cons (not (unbound-marker-p (cdr ,index))))))
           (t ,default))))))

(defmacro instance-boundp-custom (pv pv-offset parameter)
  `(locally (declare #.*optimize-speed*)
     (funcall (slot-info-boundp (svref ,pv (1+ ,pv-offset))) ,parameter)))

;;; This magic function has quite a job to do indeed.
;;;
;;; The careful reader will recall that <slots> contains all of the
;;; optimized slot access forms produced by OPTIMIZE-INSTANCE-ACCESS.
;;; Each of these is a call to either INSTANCE-READ or INSTANCE-WRITE.
;;;
;;; At the time these calls were produced, the first argument was
;;; specified as the symbol .PV-OFFSET.; what we have to do now is
;;; convert those pv-offset arguments into the actual number that is
;;; the correct offset into the pv.
;;;
;;; But first, oh but first, we sort <slots> a bit so that for each
;;; argument we have the slots in an order defined by
;;; SYMBOL-OR-CONS-LESSP. This canonicalizes the PV-TABLEs a bit and
;;; will hopefully lead to having fewer PVs floating around. Even if
;;; the gain is only modest, it costs nothing.
(defun slot-name-lists-from-slots (slots)
  (mapcar (lambda (parameter-entry)
            (when (cdr parameter-entry)
              (mapcar #'car (cdr parameter-entry))))
          (mutate-slots slots)))

(defun mutate-slots (slots)
  (let ((sorted-slots (sort-slots slots))
        (pv-offset -1))
    (dolist (parameter-entry sorted-slots)
      (dolist (slot-entry (cdr parameter-entry))
        (incf pv-offset)
        (dolist (form (cdr slot-entry))
          (setf (cadr form) pv-offset))
        ;; Count one more for the slot we use for SLOT-INFO.
        (incf pv-offset)))
    sorted-slots))

(defun symbol-or-cons-lessp (a b)
  (etypecase a
    (symbol (etypecase b
              (symbol (< (symbol-hash a) (symbol-hash b)))
              (cons t)))
    (cons   (etypecase b
              (symbol nil)
              (cons (if (eq (car a) (car b))
                        (symbol-or-cons-lessp (cdr a) (cdr b))
                        (symbol-or-cons-lessp (car a) (car b))))))))

(defun sort-slots (slots)
  (mapcar (lambda (parameter-entry)
            (destructuring-bind (name . entries) parameter-entry
              (cons name (stable-sort entries #'symbol-or-cons-lessp
                                      :key #'car))))
          slots))


;;;; This needs to work in terms of metatypes and also needs to work
;;;; for automatically generated reader and writer functions.
;;;; Automatically generated reader and writer functions use this
;;;; stuff too.

(defmacro pv-binding ((required-parameters slot-name-lists pv-table-form)
                      &body body)
  (let (slot-vars pv-parameters)
    (loop for slots in slot-name-lists
          for required-parameter in required-parameters
          for i from 0
          do (when slots
               (push required-parameter pv-parameters)
               (push (slot-vector-symbol i) slot-vars)))
    `(pv-binding1 (,pv-table-form
                   ,(nreverse pv-parameters) ,(nreverse slot-vars))
       ,@body)))

(defmacro pv-binding1 ((pv-table-form pv-parameters slot-vars)
                       &body body)
  `(pv-env (,pv-table-form ,pv-parameters)
     (let (,@(mapcar (lambda (slot-var p) `(,slot-var (get-slots-or-nil ,p)))
                     slot-vars pv-parameters))
       (declare (ignorable ,@(mapcar #'identity slot-vars)))
       ,@body)))

;;; This will only be visible in PV-ENV when the default MAKE-METHOD-LAMBDA is
;;; overridden.
(define-symbol-macro pv-env-environment overridden)

(defmacro pv-env (&environment env
                  (pv-table-form pv-parameters)
                  &rest forms)
  ;; Decide which expansion to use based on the state of the PV-ENV-ENVIRONMENT
  ;; symbol-macrolet.
  (if (eq (macroexpand 'pv-env-environment env) 'default)
      `(locally (declare (simple-vector .pv.))
         ,@forms)
      `(let* ((.pv-table. ,pv-table-form)
              (.pv. (pv-table-lookup-pv-args .pv-table. ,@pv-parameters)))
        (declare ,(make-pv-type-declaration '.pv.))
        ,@forms)))

(defun split-declarations (body args req-args cnm-p parameters-setqd)
  (let ((inner-decls nil)
        (outer-decls nil)
        decl)
    (loop
      (when (null body)
        (return nil))
      (setq decl (car body))
      (unless (and (consp decl) (eq (car decl) 'declare))
        (return nil))
      (dolist (form (cdr decl))
        (when (consp form)
          (let* ((name (car form)))
            (cond ((eq '%class name)
                   (push `(declare ,form) inner-decls))
                  ((or (member name '(ignore ignorable special dynamic-extent type))
                       (info :type :kind name))
                   (let* ((inners nil)
                          (outers nil)
                          (tail (cdr form))
                          (head (if (eq 'type name)
                                    (list name (pop tail))
                                    (list name))))
                     (dolist (var tail)
                       (if (member var args :test #'eq)
                           ;; Quietly remove IGNORE declarations on
                           ;; args when a next-method is involved, to
                           ;; prevent compiler warnings about ignored
                           ;; args being read.
                           (unless (and (eq 'ignore name)
                                        (member var req-args :test #'eq)
                                        (or cnm-p (member var parameters-setqd)))
                             (push var outers))
                           (push var inners)))
                     (when outers
                       (push `(declare (,@head ,@outers)) outer-decls))
                     (when inners
                       (push `(declare (,@head ,@inners)) inner-decls))))
                  (t
                   ;; All other declarations are not variable declarations,
                   ;; so they become outer declarations.
                   (push `(declare ,form) outer-decls))))))
      (setq body (cdr body)))
    (values outer-decls inner-decls body)))

;;; Convert a lambda expression containing a SB-PCL::%METHOD-NAME
;;; declaration (which is a naming style internal to PCL) into an
;;; SB-INT:NAMED-LAMBDA expression (which is a naming style used
;;; throughout SBCL, understood by the main compiler); or if there's
;;; no SB-PCL::%METHOD-NAME declaration, then just return the original
;;; lambda expression.
(defun name-method-lambda (method-lambda)
  (let ((method-name *method-name*))
    (if method-name
        `(named-lambda (slow-method ,@method-name) ,@(rest method-lambda))
        method-lambda)))

(defun make-method-initargs-form-internal (method-lambda initargs env)
  (declare (ignore env))
  (let (method-lambda-args
        lmf ; becomes body of function
        lmf-params)
    (if (not (and (= 3 (length method-lambda))
                  (= 2 (length (setq method-lambda-args (cadr method-lambda))))
                  (consp (setq lmf (third method-lambda)))
                  (eq 'simple-lexical-method-functions (car lmf))
                  (eq (car method-lambda-args)
                      (cadr (setq lmf-params (cadr lmf))))
                  (eq (cadr method-lambda-args)
                      (caddr lmf-params))))
        `(list* :function ,(name-method-lambda method-lambda)
                ',initargs)
        (let* ((lambda-list (car lmf-params))
               (nreq 0)
               (restp nil)
               (args nil))
          (dolist (arg lambda-list)
            (when (member arg '(&optional &rest &key))
              (setq restp t)
              (return nil))
            (when (eq arg '&aux)
              (return nil))
            (incf nreq)
            (push arg args))
          (setq args (nreverse args))
          (setf (getf (getf initargs 'plist) :arg-info) (cons nreq restp))
          (make-method-initargs-form-internal1
           initargs (cddr lmf) args lmf-params restp)))))

(defun lambda-list-parameter-names (lambda-list)
  ;; Given a valid lambda list, extract the parameter names.
  (loop for x in lambda-list
        with res = nil
        do (unless (member x lambda-list-keywords :test #'eq)
             (if (consp x)
                 (let ((name (car x)))
                   (if (consp name)
                       ;; ... ((:BAR FOO) 1)
                       (push (second name) res)
                       ;; ... (FOO 1)
                       (push name res))
                   ;; ... (... 1 FOO-P)
                   (let ((name-p (cddr x)))
                     (when name-p
                       (push (car name-p) res))))
                 ;; ... FOO
                 (push x res)))
        finally (return res)))

(defun make-method-initargs-form-internal1
    (initargs body req-args lmf-params restp)
  (let* (;; The lambda-list of the method, minus specifiers
         (lambda-list (car lmf-params))
         ;; Names of the parameters that will be in the outermost lambda-list
         ;; (and whose bound declarations thus need to be in OUTER-DECLS).
         (outer-parameters req-args)
         ;; The lambda-list used by BIND-ARGS
         (bind-list lambda-list)
         (parameters-setqd (getf (cdr lmf-params) :parameters-setqd))
         (auxp (member '&aux bind-list))
         (call-next-method-p (getf (cdr lmf-params) :call-next-method-p)))
    ;; Try to use the normal function call machinery instead of BIND-ARGS
    ;; binding the arguments, unless:
    (unless (or ;; If all arguments are required, BIND-ARGS will be a no-op
                ;; in any case.
                (and (not restp) (not auxp))
                ;; CALL-NEXT-METHOD wants to use BIND-ARGS, and needs a
                ;; list of all non-required arguments.
                call-next-method-p)
      (setf ;; We don't want a binding for .REST-ARG.
            restp nil
            ;; Get all the parameters for declaration parsing
            outer-parameters (lambda-list-parameter-names lambda-list)
            ;; Ensure that BIND-ARGS won't do anything (since
            ;; BIND-LIST won't contain any non-required parameters,
            ;; and REQ-ARGS will be of an equal length). We still want
            ;; to pass BIND-LIST to FAST-LEXICAL-METHOD-FUNCTIONS so
            ;; that BIND-FAST-LEXICAL-METHOD-FUNCTIONS can take care
            ;; of rebinding SETQd required arguments around the method
            ;; body.
            bind-list req-args))
    (multiple-value-bind (outer-decls inner-decls body-sans-decls)
        (split-declarations
         body outer-parameters req-args call-next-method-p parameters-setqd)
      (let* ((rest-arg (when restp
                         '.rest-arg.))
             (fmf-lambda-list (if rest-arg
                                  (append req-args (list '&rest rest-arg))
                                  (if call-next-method-p
                                      req-args
                                      lambda-list))))
        `(list*
          :function
          (let* ((fmf (,(if *method-name* 'named-lambda 'lambda)
                        ,@(when *method-name*
                                ;; function name
                                (list `(fast-method ,@*method-name*)))
                        ;; The lambda-list of the FMF
                        (.pv. .next-method-call. ,@fmf-lambda-list)
                        ;; body of the function
                        (declare (ignorable .pv. .next-method-call.)
                                 (disable-package-locks pv-env-environment))
                        ,@outer-decls
                        (symbol-macrolet ((pv-env-environment default))
                          (fast-lexical-method-functions
                              (,bind-list .next-method-call. ,req-args ,rest-arg
                                ,@(cdddr lmf-params))
                            ,@inner-decls
                            ,@body-sans-decls))))
                 (mf (%make-method-function fmf)))
            (setf (%funcallable-instance-fun mf)
                  (method-function-from-fast-function fmf ',(getf initargs 'plist)))
            mf)
          ',initargs)))))

;;; Use arrays and hash tables and the fngen stuff to make this much
;;; better. It doesn't really matter, though, because a function
;;; returned by this will get called only when the user explicitly
;;; funcalls a result of method-function. BUT, this is needed to make
;;; early methods work.
(defun method-function-from-fast-function (fmf plist)
  (declare (type function fmf))
  (let* ((method-function nil)
         (snl (getf plist :slot-name-lists))
         (pv-table (when snl
                     (intern-pv-table :slot-name-lists snl))))
    (setq method-function
          (lambda (method-args next-methods)
            (let* ((pv (when pv-table
                         (get-pv method-args pv-table)))
                   (nm (car next-methods))
                   (nms (cdr next-methods))
                   (nmc (when nm
                          (make-method-call
                           :function (if (std-instance-p nm)
                                         (method-function nm)
                                         nm)
                           :call-method-args (list nms)))))
              (apply fmf pv nmc method-args))))
    ;; FIXME: this looks dangerous.
    (let* ((fname (%fun-name fmf)))
      (when (and fname (eq (car fname) 'fast-method))
        (set-fun-name method-function (cons 'slow-method (cdr fname)))))
    method-function))

;;; this is similar to the above, only not quite.  Only called when
;;; the MOP is heavily involved.  Not quite parallel to
;;; METHOD-FUNCTION-FROM-FAST-METHOD-FUNCTION, because we can close
;;; over the actual PV-CELL in this case.
(defun method-function-from-fast-method-call (fmc)
  (let* ((fmf (fast-method-call-function fmc))
         (pv (fast-method-call-pv fmc)))
    (lambda (method-args next-methods)
      (let* ((nm (car next-methods))
             (nms (cdr next-methods))
             (nmc (when nm
                    (make-method-call
                     :function (if (std-instance-p nm)
                                   (method-function nm)
                                   nm)
                     :call-method-args (list nms)))))
        (apply fmf pv nmc method-args)))))

(defun get-pv (method-args pv-table)
  (let ((pv-wrappers (pv-wrappers-from-all-args pv-table method-args)))
    (when pv-wrappers
      (pv-table-lookup pv-table pv-wrappers))))

(defun pv-table-lookup-pv-args (pv-table &rest pv-parameters)
  (pv-table-lookup pv-table (pv-wrappers-from-pv-args pv-parameters)))

(defun pv-wrappers-from-pv-args (&rest args)
  (mapcar #'valid-wrapper-of args))

(defun pv-wrappers-from-all-args (pv-table args)
  (loop for snl in (pv-table-slot-name-lists pv-table)
        and arg in args
        when snl
        collect (valid-wrapper-of arg)))

;;; Return the subset of WRAPPERS which is used in the cache
;;; of PV-TABLE.
(defun pv-wrappers-from-all-wrappers (pv-table wrappers)
  (loop for snl in (pv-table-slot-name-lists pv-table)
        and w in wrappers
        when snl
        collect w))

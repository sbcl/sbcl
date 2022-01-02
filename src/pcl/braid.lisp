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


(defun class-of (x)
  (declare (explicit-check))
  (wrapper-class (wrapper-of x)))

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

(defun set-bitmap-and-flags (wrapper &aux (inherits (wrapper-inherits wrapper))
                                          (flags (wrapper-flags wrapper))
                                          (layout (wrapper-friend wrapper)))
  (when (eq (wrapper-classoid wrapper) *simple-stream-root-classoid*)
    (setq flags (logior flags +simple-stream-layout-flag+)))
  ;; We decide only at class finalization time whether it is funcallable.
  ;; Picking the right bitmap could probably be done sooner given the metaclass,
  ;; but this approach avoids changing how PCL uses MAKE-LAYOUT.
  ;; The big comment above MAKE-IMMOBILE-FUNINSTANCE in src/code/x86-64-vm
  ;; explains why we differentiate between SGF and everything else.
  (dovector (ancestor inherits)
    (when (eq ancestor #.(find-layout 'function))
      (%raw-instance-set/signed-word layout (sb-kernel::type-dd-length sb-vm:layout)
            #+immobile-code ; there are two possible bitmaps
            (if (or (find *sgf-wrapper* inherits) (eq wrapper *sgf-wrapper*))
                sb-kernel::standard-gf-primitive-obj-layout-bitmap
                +layout-all-tagged+)
            ;; there is only one possible bitmap otherwise
            #-immobile-code sb-kernel::standard-gf-primitive-obj-layout-bitmap))
    (setq flags (logior (logand (logior +sequence-layout-flag+
                                        +stream-layout-flag+
                                        +simple-stream-layout-flag+
                                        +file-stream-layout-flag+
                                        +string-stream-layout-flag+)
                                (wrapper-flags ancestor))
                        flags)))
  (setf (layout-flags (wrapper-friend wrapper)) flags))

;;; Set the inherits from CPL, and register the layout. This actually
;;; installs the class in the Lisp type system.
(defun %update-lisp-class-layout (class wrapper)
  ;; Protected by **world-lock** in callers.
  (let ((classoid (wrapper-classoid wrapper)))
    (unless (eq (classoid-wrapper classoid) wrapper)
      (set-layout-inherits wrapper
                           (order-layout-inherits
                            (map 'simple-vector #'class-wrapper
                                 (reverse (rest (class-precedence-list class)))))
                           nil 0)
      (set-bitmap-and-flags wrapper)
      (register-layout wrapper :invalidate t)

      ;; FIXME: I don't think this should be necessary, but without it
      ;; we are unable to compile (TYPEP foo '<class-name>) in the
      ;; same file as the class is defined.  If we had environments,
      ;; then I think the classsoid whould only be associated with the
      ;; name in that environment...  Alternatively, fix the compiler
      ;; so that TYPEP foo '<class-name> is slow but compileable.
      (let ((name (class-name class)))
        (when (and name (symbolp name) (eq name (classoid-name classoid)))
          (setf (find-classoid name) classoid))))))

(!bootstrap-accessor-definitions t)
(!bootstrap-accessor-definitions nil)

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
           (lclass (wrapper-classoid layout))
           (lclass-pcl-class (classoid-pcl-class lclass))
           (olclass (find-classoid name nil)))
      (if lclass-pcl-class
          (aver (eq class lclass-pcl-class))
          (setf (classoid-pcl-class lclass) class))

      (%update-lisp-class-layout class layout)

      (cond (olclass
             (aver (eq lclass olclass)))
            (t
             (setf (find-classoid name) lclass))))))

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

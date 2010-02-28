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

(let ((reader-specializers '(slot-object))
      (writer-specializers '(t slot-object)))
  (defun ensure-accessor (type fun-name slot-name)
    (unless (fboundp fun-name)
      (multiple-value-bind (lambda-list specializers method-class initargs doc)
          (ecase type
            ;; FIXME: change SLOT-OBJECT here to T to get SLOT-MISSING
            ;; behaviour for non-slot-objects too?
            (reader
             (values '(object) reader-specializers 'global-reader-method
                     (make-std-reader-method-function 'slot-object slot-name)
                     "automatically-generated reader method"))
            (writer
             (values '(new-value object) writer-specializers
                     'global-writer-method
                     (make-std-writer-method-function 'slot-object slot-name)
                     "automatically-generated writer method"))
            (boundp
             (values '(object) reader-specializers 'global-boundp-method
                     (make-std-boundp-method-function 'slot-object slot-name)
                     "automatically-generated boundp method")))
        (let ((gf (ensure-generic-function fun-name :lambda-list lambda-list)))
          (add-method gf (make-a-method method-class
                                        () lambda-list specializers
                                        initargs doc :slot-name slot-name)))))
    t)
  ;; KLUDGE: this is maybe PCL bootstrap mechanism #6 or #7, invented
  ;; by CSR in June 2007.  Making the bootstrap sane is getting higher
  ;; on the "TODO: URGENT" list.
  (defun !fix-ensure-accessor-specializers ()
    (setf reader-specializers (mapcar #'find-class reader-specializers))
    (setf writer-specializers (mapcar #'find-class writer-specializers))))

(defmacro quiet-funcall (fun &rest args)
  ;; Don't give a style-warning about undefined function here.
  `(funcall (locally (declare (muffle-conditions style-warning))
              ,fun)
            ,@args))

(defmacro accessor-slot-value (object slot-name &environment env)
  (aver (constantp slot-name env))
  (let* ((slot-name (constant-form-value slot-name env))
         (reader-name (slot-reader-name slot-name)))
    `(let ((.ignore. (load-time-value
                      (ensure-accessor 'reader ',reader-name ',slot-name))))
       (declare (ignore .ignore.))
       (truly-the (values t &optional)
                  (quiet-funcall #',reader-name ,object)))))

(defmacro accessor-set-slot-value (object slot-name new-value &environment env)
  (aver (constantp slot-name env))
  (setq object (macroexpand object env))
  (let* ((slot-name (constant-form-value slot-name env))
         (bind-object (unless (or (constantp new-value env) (atom new-value))
                        (let* ((object-var (gensym))
                               (bind `((,object-var ,object))))
                          (setf object object-var)
                          bind)))
         (writer-name (slot-writer-name slot-name))
         (form
          `(let ((.ignore.
                  (load-time-value
                   (ensure-accessor 'writer ',writer-name ',slot-name)))
                 (.new-value. ,new-value))
            (declare (ignore .ignore.))
            (quiet-funcall #',writer-name .new-value. ,object)
            .new-value.)))
    (if bind-object
        `(let ,bind-object ,form)
        form)))

(defmacro accessor-slot-boundp (object slot-name &environment env)
  (aver (constantp slot-name env))
  (let* ((slot-name (constant-form-value slot-name env))
         (boundp-name (slot-boundp-name slot-name)))
    `(let ((.ignore. (load-time-value
                      (ensure-accessor 'boundp ',boundp-name ',slot-name))))
      (declare (ignore .ignore.))
      (funcall #',boundp-name ,object))))

(defun make-structure-slot-boundp-function (slotd)
  (declare (ignore slotd))
  (lambda (object)
    (declare (ignore object))
    t))

(define-condition instance-structure-protocol-error
    (reference-condition error)
  ((slotd :initarg :slotd :reader instance-structure-protocol-error-slotd)
   (fun :initarg :fun :reader instance-structure-protocol-error-fun))
  (:report
   (lambda (c s)
     (format s "~@<The slot ~S has neither ~S nor ~S ~
                allocation, so it can't be ~A by the default ~
                ~S method.~@:>"
             (instance-structure-protocol-error-slotd c)
             :instance :class
             (cond
               ((member (instance-structure-protocol-error-fun c)
                        '(slot-value-using-class slot-boundp-using-class))
                "read")
               (t "written"))
             (instance-structure-protocol-error-fun c)))))

(defun instance-structure-protocol-error (slotd fun)
  (error 'instance-structure-protocol-error
         :slotd slotd :fun fun
         :references (list `(:amop :generic-function ,fun)
                           '(:amop :section (5 5 3)))))

(defun get-optimized-std-accessor-method-function (class slotd name)
  (cond
    ((structure-class-p class)
     (ecase name
       (reader (slot-definition-internal-reader-function slotd))
       (writer (slot-definition-internal-writer-function slotd))
       (boundp (make-structure-slot-boundp-function slotd))))
    ((condition-class-p class)
     (ecase name
       (reader (slot-definition-reader-function slotd))
       (writer (slot-definition-writer-function slotd))
       (boundp (slot-definition-boundp-function slotd))))
    (t
     (let* ((fsc-p (cond ((standard-class-p class) nil)
                         ((funcallable-standard-class-p class) t)
                         ((std-class-p class)
                          ;; Shouldn't be using the optimized-std-accessors
                          ;; in this case.
                          #+nil (format t "* warning: ~S ~S~%   ~S~%"
                                        name slotd class)
                          nil)
                         (t (error "~S is not a STANDARD-CLASS." class))))
            (slot-name (slot-definition-name slotd))
            (location (slot-definition-location slotd))
            (function (ecase name
                        (reader #'make-optimized-std-reader-method-function)
                        (writer #'make-optimized-std-writer-method-function)
                        (boundp #'make-optimized-std-boundp-method-function)))
            ;; KLUDGE: we need this slightly hacky calling convention
            ;; for these functions for bootstrapping reasons: see
            ;; !BOOTSTRAP-MAKE-SLOT-DEFINITION in braid.lisp.  -- CSR,
            ;; 2004-07-12
            (value (funcall function fsc-p slotd slot-name location)))
       (declare (type function function))
       (values value (slot-definition-location slotd))))))

(defun make-optimized-std-reader-method-function
    (fsc-p slotd slot-name location)
  (declare #.*optimize-speed*)
  (set-fun-name
   (etypecase location
     (fixnum
      (if fsc-p
          (lambda (instance)
            (check-obsolete-instance instance)
            (let ((value (clos-slots-ref (fsc-instance-slots instance)
                                         location)))
              (if (eq value +slot-unbound+)
                  (values
                   (slot-unbound (class-of instance) instance slot-name))
                  value)))
          (lambda (instance)
            (check-obsolete-instance instance)
            (let ((value (clos-slots-ref (std-instance-slots instance)
                                         location)))
              (if (eq value +slot-unbound+)
                  (values
                   (slot-unbound (class-of instance) instance slot-name))
                  value)))))
     (cons
      (lambda (instance)
        (check-obsolete-instance instance)
        (let ((value (cdr location)))
          (if (eq value +slot-unbound+)
              (values (slot-unbound (class-of instance) instance slot-name))
              value))))
     (null
      (lambda (instance)
        (instance-structure-protocol-error slotd 'slot-value-using-class))))
   `(reader ,slot-name)))

(defun make-optimized-std-writer-method-function
    (fsc-p slotd slot-name location)
  (declare #.*optimize-speed*)
  (let* ((safe-p (and slotd
                      (slot-definition-class slotd)
                      (safe-p (slot-definition-class slotd))))
         (writer-fun (etypecase location
                       (fixnum
                        (if fsc-p
                            (lambda (nv instance)
                              (check-obsolete-instance instance)
                              (setf (clos-slots-ref (fsc-instance-slots instance)
                                                    location)
                                    nv))
                            (lambda (nv instance)
                              (check-obsolete-instance instance)
                              (setf (clos-slots-ref (std-instance-slots instance)
                                                    location)
                                    nv))))
                       (cons
                        (lambda (nv instance)
                          (check-obsolete-instance instance)
                          (setf (cdr location) nv)))
                       (null
                        (lambda (nv instance)
                          (declare (ignore nv instance))
                          (instance-structure-protocol-error
                           slotd
                           '(setf slot-value-using-class))))))
         (checking-fun (lambda (new-value instance)
                         ;; If we have a TYPE-CHECK-FUNCTION, call it.
                         (let* (;; Note that the class of INSTANCE here is not
                                ;; neccessarily the SLOT-DEFINITION-CLASS of
                                ;; the SLOTD passed to M-O-S-W-M-F, since it's
                                ;; e.g. possible for a subclass to define a
                                ;; slot of the same name but with no accessors.
                                ;; So we need to fetch the right type check function
                                ;; from the wrapper instead of just closing over it.
                                (wrapper (valid-wrapper-of instance))
                                (type-check-function
                                 (cadr (find-slot-cell wrapper slot-name))))
                           (declare (type (or function null) type-check-function))
                           (when type-check-function
                             (funcall type-check-function new-value)))
                         ;; Then call the real writer.
                         (funcall writer-fun new-value instance))))
    (set-fun-name (if safe-p
                      checking-fun
                      writer-fun)
                  `(writer ,slot-name))))

(defun make-optimized-std-boundp-method-function
    (fsc-p slotd slot-name location)
  (declare #.*optimize-speed*)
  (set-fun-name
   (etypecase location
     (fixnum (if fsc-p
                 (lambda (instance)
                   (check-obsolete-instance instance)
                   (not (eq (clos-slots-ref (fsc-instance-slots instance)
                                            location)
                            +slot-unbound+)))
                 (lambda (instance)
                   (check-obsolete-instance instance)
                   (not (eq (clos-slots-ref (std-instance-slots instance)
                                            location)
                            +slot-unbound+)))))
     (cons (lambda (instance)
             (check-obsolete-instance instance)
             (not (eq (cdr location) +slot-unbound+))))
     (null
      (lambda (instance)
        (instance-structure-protocol-error slotd 'slot-boundp-using-class))))
   `(boundp ,slot-name)))

(defun make-optimized-structure-slot-value-using-class-method-function
    (function)
  (declare (type function function))
  (lambda (class object slotd)
    (declare (ignore class slotd))
    (funcall function object)))

(defun make-optimized-structure-setf-slot-value-using-class-method-function
    (function)
  (declare (type function function))
  (lambda (nv class object slotd)
    (declare (ignore class slotd))
    (funcall function nv object)))

(defun make-optimized-structure-slot-boundp-using-class-method-function ()
  (lambda (class object slotd)
    (declare (ignore class object slotd))
    t))

(defun get-optimized-std-slot-value-using-class-method-function
    (class slotd name)
  (cond
    ((structure-class-p class)
     (ecase name
       (reader (make-optimized-structure-slot-value-using-class-method-function
                (slot-definition-internal-reader-function slotd)))
       (writer (make-optimized-structure-setf-slot-value-using-class-method-function
                (slot-definition-internal-writer-function slotd)))
       (boundp (make-optimized-structure-slot-boundp-using-class-method-function))))
    ((condition-class-p class)
     (ecase name
       (reader
        (let ((fun (slot-definition-reader-function slotd)))
          (declare (type function fun))
          (lambda (class object slotd)
            (declare (ignore class slotd))
            (funcall fun object))))
       (writer
        (let ((fun (slot-definition-writer-function slotd)))
          (declare (type function fun))
          (lambda (new-value class object slotd)
            (declare (ignore class slotd))
            (funcall fun new-value object))))
       (boundp
        (let ((fun (slot-definition-boundp-function slotd)))
          (declare (type function fun))
          (lambda (class object slotd)
            (declare (ignore class slotd))
            (funcall fun object))))))
    (t
     (let* ((fsc-p (cond ((standard-class-p class) nil)
                         ((funcallable-standard-class-p class) t)
                         (t (error "~S is not a standard-class" class))))
            (function
             (ecase name
               (reader
                #'make-optimized-std-slot-value-using-class-method-function)
               (writer
                #'make-optimized-std-setf-slot-value-using-class-method-function)
               (boundp
                #'make-optimized-std-slot-boundp-using-class-method-function))))
       (declare (type function function))
       (values (funcall function fsc-p slotd)
               (slot-definition-location slotd))))))

(defun make-optimized-std-slot-value-using-class-method-function (fsc-p slotd)
  (declare #.*optimize-speed*)
  (let ((location (slot-definition-location slotd))
        (slot-name (slot-definition-name slotd)))
    (etypecase location
      (fixnum (if fsc-p
                  (lambda (class instance slotd)
                    (declare (ignore slotd))
                    (check-obsolete-instance instance)
                    (let ((value (clos-slots-ref (fsc-instance-slots instance)
                                                 location)))
                      (if (eq value +slot-unbound+)
                          (values (slot-unbound class instance slot-name))
                          value)))
                  (lambda (class instance slotd)
                    (declare (ignore slotd))
                    (check-obsolete-instance instance)
                    (let ((value (clos-slots-ref (std-instance-slots instance)
                                                 location)))
                      (if (eq value +slot-unbound+)
                          (values (slot-unbound class instance slot-name))
                          value)))))
      (cons (lambda (class instance slotd)
              (declare (ignore slotd))
              (check-obsolete-instance instance)
              (let ((value (cdr location)))
                (if (eq value +slot-unbound+)
                    (values (slot-unbound class instance slot-name))
                    value))))
      (null
       (lambda (class instance slotd)
         (declare (ignore class instance))
         (instance-structure-protocol-error slotd 'slot-value-using-class))))))

(defun make-optimized-std-setf-slot-value-using-class-method-function
    (fsc-p slotd)
  (declare #.*optimize-speed*)
  (let ((location (slot-definition-location slotd))
        (type-check-function
         (when (and slotd
                    (slot-definition-class slotd)
                    (safe-p (slot-definition-class slotd)))
           (slot-definition-type-check-function slotd))))
    (macrolet ((make-mf-lambda (&body body)
                 `(lambda (nv class instance slotd)
                    (declare (ignore class slotd))
                    (check-obsolete-instance instance)
                    ,@body))
               (make-mf-lambdas (&body body)
                 ;; Having separate lambdas for the NULL / not-NULL cases of
                 ;; TYPE-CHECK-FUNCTION is done to avoid runtime overhead
                 ;; for CLOS typechecking when it's not in use.
                 `(if type-check-function
                      (make-mf-lambda
                       (funcall (the function type-check-function) nv)
                       ,@body)
                      (make-mf-lambda
                       ,@body))))
      (etypecase location
        (fixnum
         (if fsc-p
             (make-mf-lambdas
              (setf (clos-slots-ref (fsc-instance-slots instance) location)
                    nv))
             (make-mf-lambdas
              (setf (clos-slots-ref (std-instance-slots instance) location)
                    nv))))
        (cons
         (make-mf-lambdas (setf (cdr location) nv)))
        (null (lambda (nv class instance slotd)
                (declare (ignore nv class instance))
                (instance-structure-protocol-error
                 slotd '(setf slot-value-using-class))))))))

(defun make-optimized-std-slot-boundp-using-class-method-function
    (fsc-p slotd)
  (declare #.*optimize-speed*)
  (let ((location (slot-definition-location slotd)))
    (etypecase location
      (fixnum
       (if fsc-p
           (lambda (class instance slotd)
             (declare (ignore class slotd))
             (check-obsolete-instance instance)
             (not (eq (clos-slots-ref (fsc-instance-slots instance) location)
                      +slot-unbound+)))
           (lambda (class instance slotd)
             (declare (ignore class slotd))
             (check-obsolete-instance instance)
             (not (eq (clos-slots-ref (std-instance-slots instance) location)
                      +slot-unbound+)))))
      (cons (lambda (class instance slotd)
              (declare (ignore class slotd))
              (check-obsolete-instance instance)
              (not (eq (cdr location) +slot-unbound+))))
      (null
       (lambda (class instance slotd)
         (declare (ignore class instance))
         (instance-structure-protocol-error slotd
                                            'slot-boundp-using-class))))))

(defun get-accessor-from-svuc-method-function (class slotd sdfun name)
  (macrolet ((emf-funcall (emf &rest args)
               `(invoke-effective-method-function ,emf nil
                                                  :required-args ,args)))
    (set-fun-name
     (case name
       (reader (lambda (instance)
                 (emf-funcall sdfun class instance slotd)))
       (writer (lambda (nv instance)
                 (emf-funcall sdfun nv class instance slotd)))
       (boundp (lambda (instance)
                 (emf-funcall sdfun class instance slotd))))
     `(,name ,(class-name class) ,(slot-definition-name slotd)))))

(defun make-std-reader-method-function (class-or-name slot-name)
  (declare (ignore class-or-name))
  (let* ((initargs (copy-tree
                    (make-method-function
                     (lambda (instance)
                       (pv-binding1 ((bug "Please report this")
                                     (instance) (instance-slots))
                         (instance-read-internal
                          .pv. instance-slots 0
                          (slot-value instance slot-name))))))))
    (setf (getf (getf initargs 'plist) :slot-name-lists)
          (list (list nil slot-name)))
    initargs))

(defun make-std-writer-method-function (class-or-name slot-name)
  (let* ((class (when (eq **boot-state** 'complete)
                  (if (typep class-or-name 'class)
                      class-or-name
                      (find-class class-or-name nil))))
         (safe-p (and class
                      (safe-p class)))
         (check-fun (lambda (new-value instance)
                      (let* ((class (class-of instance))
                             (slotd (find-slot-definition class slot-name))
                             (type-check-function
                              (when slotd
                                (slot-definition-type-check-function slotd))))
                        (when type-check-function
                          (funcall type-check-function new-value)))))
         (initargs (copy-tree
                    (if safe-p
                        (make-method-function
                         (lambda (nv instance)
                           (funcall check-fun nv instance)
                           (pv-binding1 ((bug "Please report this")
                                         (instance) (instance-slots))
                             (instance-write-internal
                              .pv. instance-slots 0 nv
                              (setf (slot-value instance slot-name) nv)))))
                        (make-method-function
                         (lambda (nv instance)
                           (pv-binding1 ((bug "Please report this")
                                         (instance) (instance-slots))
                             (instance-write-internal
                              .pv. instance-slots 0 nv
                              (setf (slot-value instance slot-name) nv)))))))))
    (setf (getf (getf initargs 'plist) :slot-name-lists)
          (list nil (list nil slot-name)))
    initargs))

(defun make-std-boundp-method-function (class-or-name slot-name)
  (declare (ignore class-or-name))
  (let* ((initargs (copy-tree
                    (make-method-function
                     (lambda (instance)
                       (pv-binding1 ((bug "Please report this")
                                     (instance) (instance-slots))
                          (instance-boundp-internal
                           .pv. instance-slots 0
                           (slot-boundp instance slot-name))))))))
    (setf (getf (getf initargs 'plist) :slot-name-lists)
          (list (list nil slot-name)))
    initargs))

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
;;;   It would be probably better to store the vector in wrapper
;;;   instead: one less memory indirection, one less CLOS slot
;;;   access to get at it.
;;;
;;;   It would also be nice to have STANDARD-INSTANCE-STRUCTURE-P
;;;   generic instead of checking versus STANDARD-CLASS and
;;;   FUNCALLABLE-STANDARD-CLASS.

(defun find-slot-definition (class slot-name)
  (dolist (slotd (class-slots class))
    (when (eq slot-name (slot-definition-name slotd))
      (return slotd))))

(defun find-slot-cell (wrapper slot-name)
  (declare (symbol slot-name))
  (let* ((vector (layout-slot-table wrapper))
         (index (rem (sxhash slot-name) (length vector))))
    (declare (simple-vector vector) (index index)
             (optimize (sb-c::insert-array-bounds-checks 0)))
    (do ((plist (the list (svref vector index)) (cdr plist)))
        ((not (consp plist)))
      (let ((key (car plist)))
        (setf plist (cdr plist))
        (when (eq key slot-name)
          (return (car plist)))))))

(defun make-slot-table (class slots &optional bootstrap)
  (let* ((n (+ (length slots) 2))
         (vector (make-array n :initial-element nil))
         (save-slot-location-p
          (or bootstrap
              (when (eq 'complete **boot-state**)
                (let ((metaclass (class-of class)))
                  (or (eq metaclass *the-class-standard-class*)
                      (eq metaclass *the-class-funcallable-standard-class*))))))
         (save-type-check-function-p
          (unless bootstrap
            (and (eq 'complete **boot-state**) (safe-p class)))))
    (flet ((add-to-vector (name slot)
             (declare (symbol name)
                      (optimize (sb-c::insert-array-bounds-checks 0)))
             (let ((index (rem (sxhash name) n)))
               (setf (svref vector index)
                     (list* name (list* (when save-slot-location-p
                                          (if bootstrap
                                              (early-slot-definition-location slot)
                                              (slot-definition-location slot)))
                                        (when save-type-check-function-p
                                          (slot-definition-type-check-function slot))
                                        slot)
                            (svref vector index))))))
      (if (eq 'complete **boot-state**)
         (dolist (slot slots)
           (add-to-vector (slot-definition-name slot) slot))
         (dolist (slot slots)
           (add-to-vector (early-slot-definition-name slot) slot))))
    vector))

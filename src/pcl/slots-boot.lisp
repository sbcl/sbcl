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

(defvar *!temporary-ensure-accessor-functions* nil)
(defun ensure-accessor (fun-name)
  (when (member fun-name *!temporary-ensure-accessor-functions* :test 'equal)
    (error "ENSURE-ACCESSOR ~S called more than once!?" fun-name))
  (push fun-name *!temporary-ensure-accessor-functions*)
  #| We don't really need "fast" global slot accessors while building PCL.
  ;; With few exceptions, all methods use a permutation vector for slot access.
  ;; In a pinch, these would suffice, should it become utterly necessary:
  (destructuring-bind (slot-name method) (cddr fun-name)
    (setf (fdefinition fun-name)
          (ecase method
            (reader (lambda (object) (slot-value object slot-name)))
            (writer (lambda (newval object) (setf (slot-value object slot-name) newval)))
            (boundp (lambda (object) (slot-boundp object slot-name))))))|#
  (setf (fdefinition fun-name)
        (lambda (&rest args)
          (error "Nooooo! ~S accidentally invoked on ~S" fun-name args))))

(defun make-structure-slot-boundp-function (slotd)
  (declare (ignore slotd))
  (named-lambda always-bound (object)
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
         :references `((:amop :generic-function ,fun)
                       (:amop :section (5 5 3)))))

(defun get-optimized-std-accessor-method-function (class slotd name)
  (cond
    ((structure-class-p class)
     (ecase name
       (reader (slot-definition-internal-reader-function slotd))
       (writer (slot-definition-internal-writer-function slotd))
       (boundp (make-structure-slot-boundp-function slotd))))
    ((condition-class-p class)
     (let ((info (the slot-info (slot-definition-info slotd))))
       (ecase name
         (reader (slot-info-reader info))
         (writer (slot-info-writer info))
         (boundp (slot-info-boundp info)))))
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
     (let ((info (slot-definition-info slotd)))
       (ecase name
         (reader
          (let ((fun (slot-info-reader info)))
            (lambda (class object slotd)
              (declare (ignore class slotd))
              (funcall fun object))))
         (writer
          (let ((fun (slot-info-writer info)))
            (lambda (new-value class object slotd)
              (declare (ignore class slotd))
              (funcall fun new-value object))))
         (boundp
          (let ((fun (slot-info-boundp info)))
            (lambda (class object slotd)
              (declare (ignore class slotd))
              (funcall fun object)))))))
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
  (let ((location (slot-definition-location slotd))
        (slot-name (slot-definition-name slotd)))
    (etypecase location
      (fixnum (if fsc-p
                  (lambda (class instance slotd)
                    (declare (ignore slotd))
                    (check-obsolete-instance instance)
                    (let ((value (clos-slots-ref (fsc-instance-slots instance)
                                                 location)))
                      (if (unbound-marker-p value)
                          (values (slot-unbound class instance slot-name))
                          value)))
                  (lambda (class instance slotd)
                    (declare (ignore slotd))
                    (check-obsolete-instance instance)
                    (let ((value (clos-slots-ref (std-instance-slots instance)
                                                 location)))
                      (if (unbound-marker-p value)
                          (values (slot-unbound class instance slot-name))
                          value)))))
      (cons (lambda (class instance slotd)
              (declare (ignore slotd))
              (check-obsolete-instance instance)
              (let ((value (cdr location)))
                (if (unbound-marker-p value)
                    (values (slot-unbound class instance slot-name))
                    value))))
      (null
       (lambda (class instance slotd)
         (declare (ignore class instance))
         (instance-structure-protocol-error slotd 'slot-value-using-class))))))

(defun make-optimized-std-setf-slot-value-using-class-method-function
    (fsc-p slotd)
  (let* ((location (slot-definition-location slotd))
         (class (slot-definition-class slotd))
         (typecheck
          (when (safe-p class)
            (slot-info-typecheck (slot-definition-info slotd)))))
    (macrolet ((make-mf-lambda (&body body)
                 `(lambda (nv class instance slotd)
                    (declare (ignore class slotd))
                    (check-obsolete-instance instance)
                    ,@body))
               (make-mf-lambdas (&body body)
                 ;; Having separate lambdas for the NULL / not-NULL cases of
                 ;; TYPE-CHECK-FUNCTION is done to avoid runtime overhead
                 ;; for CLOS typechecking when it's not in use.
                 `(if typecheck
                      (make-mf-lambda
                       (setf nv (funcall (the function typecheck) nv))
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
  (let ((location (slot-definition-location slotd)))
    (etypecase location
      (fixnum
       (if fsc-p
           (lambda (class instance slotd)
             (declare (ignore class slotd))
             (check-obsolete-instance instance)
             (not (unbound-marker-p
                   (clos-slots-ref (fsc-instance-slots instance) location))))
           (lambda (class instance slotd)
             (declare (ignore class slotd))
             (check-obsolete-instance instance)
             (not (unbound-marker-p
                   (clos-slots-ref (std-instance-slots instance) location))))))
      (cons (lambda (class instance slotd)
              (declare (ignore class slotd))
              (check-obsolete-instance instance)
              (not (unbound-marker-p (cdr location)))))
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

(defun maybe-class (class-or-name)
  (when (eq **boot-state** 'complete)
    (if (typep class-or-name 'class)
        class-or-name
        (find-class class-or-name nil))))

(flet ((make-initargs (slot-name kind method-function)
         (let ((initargs (copy-tree method-function))
               (slot-names (list slot-name)))
           (setf (getf (getf initargs 'plist) :slot-name-lists)
                 (ecase kind
                   ((:reader :boundp) (list slot-names))
                   (:writer (list '() slot-names))))
           initargs)))

  (defun make-std-reader-method-function (class-or-name slot-name)
    (let ((class (maybe-class class-or-name)))
      (make-initargs
       slot-name :reader
       (ecase (slot-access-strategy class slot-name 'reader t)
         (:standard
          (make-method-function
           (lambda (instance)
             (pv-binding1 ((bug "Please report this")
                           (instance) (instance-slots))
               (instance-read-standard
                .pv. instance-slots 0
                (slot-value instance slot-name))))))
         ((:custom :accessor)
          (make-method-function
           (lambda (instance)
             (pv-binding1 ((bug "Please report this")
                           (instance) nil)
               (instance-read-custom .pv. 0 instance)))))))))

  (defun make-std-writer-method-function (class-or-name slot-name)
    (let ((class (maybe-class class-or-name)))
      (make-initargs
       slot-name :writer
       (ecase (slot-access-strategy class slot-name 'writer t)
         (:standard
          (macrolet ((writer-method-function (safe)
                       `(make-method-function
                         (lambda (nv instance)
                           (pv-binding1 ((bug "Please report this")
                                         (instance) (instance-slots))
                             (instance-write-standard
                              .pv. instance-slots 0 nv
                              (setf (slot-value instance slot-name)
                                    .good-new-value.)
                              ,@(when safe '(nil t))))))))
            (if (and class (safe-p class))
                (writer-method-function t)
                (writer-method-function nil))))
         ((:custom :accessor)
          (make-method-function
           (lambda (nv instance)
             (pv-binding1 ((bug "Please report this")
                           (instance) nil)
               (instance-write-custom .pv. 0 instance nv)))))))))

  (defun make-std-boundp-method-function (class-or-name slot-name)
    (let ((class (maybe-class class-or-name)))
      (make-initargs
       slot-name :boundp
       (ecase (slot-access-strategy class slot-name 'boundp t)
         (:standard
          (make-method-function
           (lambda (instance)
             (pv-binding1 ((bug "Please report this")
                           (instance) (instance-slots))
               (instance-boundp-standard
                .pv. instance-slots 0
                (slot-boundp instance slot-name))))))
         ((:custom :accessor)
          (make-method-function
           (lambda (instance)
             (pv-binding1 ((bug "Please report this")
                           (instance) nil)
               (instance-boundp-custom .pv. 0 instance)))))))))

  (defun make-fallback-reader-method-function (slot-name)
    (make-initargs
     slot-name :reader
     (make-method-function
      (lambda (instance)
        (slot-value instance slot-name)))))

  (defun make-fallback-writer-method-function (slot-name)
    (make-initargs
     slot-name :writer
     (make-method-function
      (lambda (nv instance)
        (setf (slot-value instance slot-name) nv)))))

  (defun make-fallback-boundp-method-function (slot-name)
    (make-initargs
     slot-name :boundp
     (make-method-function
      (lambda (instance)
        (slot-boundp instance slot-name))))))

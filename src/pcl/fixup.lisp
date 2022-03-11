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

(!fix-early-generic-functions)

(fmakunbound 'ensure-accessor)
(defun ensure-accessor (fun-name) ; Make FUN-NAME exist as a GF if it doesn't
  (destructuring-bind (slot-name method) (cddr fun-name)
    (let ((reader-specializers (load-time-value (list (find-class 'slot-object)) t))
          (writer-specializers (load-time-value (list (find-class 't)
                                                      (find-class 'slot-object)) t))
          (fallback-reader-specializers
            (load-time-value (list (find-class 't)) t))
          (fallback-writer-specializers
            (load-time-value (list (find-class 't) (find-class 't)) t)))
      (multiple-value-bind (lambda-list specializers method-class initargs doc
                            fallback-initargs
                            fallback-specializers)
          (ecase method
            (reader
             (values '(object) reader-specializers 'global-reader-method
                     (make-std-reader-method-function 'slot-object slot-name)
                     "automatically-generated reader method"
                     (make-fallback-reader-method-function slot-name)
                     fallback-reader-specializers))
            (writer
             (values '(new-value object) writer-specializers
                     'global-writer-method
                     (make-std-writer-method-function 'slot-object slot-name)
                     "automatically-generated writer method"
                     (make-fallback-writer-method-function slot-name)
                     fallback-writer-specializers))
            (boundp
             (values '(object) reader-specializers 'global-boundp-method
                     (make-std-boundp-method-function 'slot-object slot-name)
                     "automatically-generated boundp method"
                     (make-fallback-boundp-method-function slot-name)
                     fallback-reader-specializers)))
        (let ((gf (ensure-generic-function fun-name :lambda-list lambda-list)))
          (add-method gf (make-a-method method-class ()
                                        lambda-list fallback-specializers
                                        fallback-initargs doc :slot-name slot-name))
          (add-method gf (make-a-method method-class ()
                                        lambda-list specializers
                                        initargs doc :slot-name slot-name)))))))

(dolist (gf-name *!temporary-ensure-accessor-functions*)
  ; (format t "~&Genericizing ~S~%" gf-name)
  (fmakunbound gf-name)
  (ensure-accessor gf-name))

(compute-standard-slot-locations)
(dolist (s '(condition function structure-object))
  (sb-kernel::do-subclassoids ((k v) (find-classoid s))
    (declare (ignore v))
    (find-class (classoid-name k))))
(setq **boot-state** 'complete)

;;; CLASS-PROTOTYPE for FUNCTION should not use ALLOCATE-INSTANCE.
;;;
;;; FIXME: this causes an error
;;;  "A function with declared result type NIL returned:
;;;   (SLOT-ACCESSOR :GLOBAL PROTOTYPE WRITER)"
;;; if SB-EXT:*DERIVE-FUNCTION-TYPES* is T.
;;;
(let ((class (find-class 'function)))
  (setf (slot-value class 'prototype) #'identity))

(dolist (symbol '(add-method allocate-instance class-name compute-applicable-methods
                  ensure-generic-function make-instance method-qualifiers
                  remove-method add-dependent add-direct-method add-direct-subclass
                  class-default-initargs class-direct-default-initargs
                  class-direct-slots class-direct-subclasses
                  class-direct-superclasses class-finalized-p class-precedence-list
                  class-prototype class-slots
                  compute-applicable-methods-using-classes
                  compute-class-precedence-list compute-default-initargs
                  compute-discriminating-function compute-effective-method
                  compute-effective-slot-definition compute-slots
                  direct-slot-definition direct-slot-definition-class
                  effective-slot-definition effective-slot-definition-class
                  ensure-class ensure-class-using-class
                  ensure-generic-function-using-class eql-specializer
                  eql-specializer-object extract-lambda-list
                  extract-specializer-names finalize-inheritance
                  find-method-combination forward-referenced-class
                  funcallable-standard-class funcallable-standard-instance-access
                  funcallable-standard-object
                  generic-function-argument-precedence-order
                  generic-function-declarations generic-function-lambda-list
                  generic-function-method-class generic-function-method-combination
                  generic-function-methods generic-function-name
                  intern-eql-specializer make-method-lambda map-dependents
                  method-function method-generic-function method-lambda-list
                  method-specializers accessor-method-slot-definition
                  reader-method-class remove-dependent remove-direct-method
                  remove-direct-subclass set-funcallable-instance-function
                  slot-boundp-using-class slot-definition slot-definition-allocation
                  slot-definition-initargs slot-definition-initform
                  slot-definition-initfunction slot-definition-location
                  slot-definition-name slot-definition-readers
                  slot-definition-writers slot-definition-type
                  slot-makunbound-using-class slot-value-using-class specializer
                  specializer-direct-generic-functions specializer-direct-methods
                  standard-accessor-method standard-direct-slot-definition
                  standard-effective-slot-definition standard-instance-access
                  standard-reader-method standard-slot-definition
                  standard-writer-method update-dependent validate-superclass
                  writer-method-class))
  (sb-impl::deprecate-export *package* symbol :late "2.0.7"))

(in-package "SB-KERNEL")
(defun slot-object-p (x) (typep x '(or structure-object standard-object condition)))

(flet ((set-predicate (classoid-name pred)
         (let ((c (find-classoid classoid-name)))
           (%instance-set c (get-dsd-index built-in-classoid predicate)
                 pred))))
  (set-predicate 't #'constantly-t)
  (set-predicate 'random-class #'constantly-nil)
  (set-predicate 'sb-pcl::slot-object #'slot-object-p))

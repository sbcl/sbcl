;;;; This file contains the definition of the FUNCALLABLE-STANDARD-CLASS
;;;; metaclass. Much of the implementation of this metaclass is actually
;;;; defined on the class STD-CLASS. What appears in this file is a modest
;;;; number of simple methods related to the low-level differences in the
;;;; implementation of standard and funcallable-standard instances.
;;;;
;;;; As it happens, none of these differences are the ones reflected in
;;;; the MOP specification; STANDARD-CLASS and FUNCALLABLE-STANDARD-CLASS
;;;; share all their specified methods at STD-CLASS.

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

(defmethod wrapper-fetcher ((class funcallable-standard-class))
  'fsc-instance-wrapper)

(defmethod slots-fetcher ((class funcallable-standard-class))
  'fsc-instance-slots)

(defmethod raw-instance-allocator ((class funcallable-standard-class))
  'allocate-standard-funcallable-instance)

(defmethod allocate-instance
           ((class funcallable-standard-class) &rest initargs)
  (declare (ignore initargs))
  (unless (class-finalized-p class)
    (finalize-inheritance class))
  (allocate-standard-funcallable-instance (class-wrapper class)))

(defmethod make-reader-method-function ((class funcallable-standard-class)
                                        slot-name)
  (make-std-reader-method-function class slot-name))

(defmethod make-writer-method-function ((class funcallable-standard-class)
                                        slot-name)
  (make-std-writer-method-function class slot-name))

;;;; See the comment about reader-function--std and writer-function--sdt.
;;;;
;(define-function-template reader-function--fsc () '(slot-name)
;  `(function
;     (lambda (instance)
;       (slot-value-using-class (wrapper-class (get-wrapper instance))
;                              instance
;                              slot-name))))
;
;(define-function-template writer-function--fsc () '(slot-name)
;  `(function
;     (lambda (nv instance)
;       (setf
;        (slot-value-using-class (wrapper-class (get-wrapper instance))
;                                instance
;                                slot-name)
;        nv))))
;
;(eval-when (:load-toplevel)
;  (pre-make-templated-function-constructor reader-function--fsc)
;  (pre-make-templated-function-constructor writer-function--fsc))

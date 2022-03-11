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

(defmethod allocate-instance ((class funcallable-standard-class) &rest initargs)
  (declare (inline ensure-class-finalized))
  (allocate-standard-funcallable-instance
   (class-wrapper (ensure-class-finalized class))
   (getf initargs :name)))

(defmethod make-reader-method-function ((class funcallable-standard-class)
                                        slot-name)
  (make-std-reader-method-function class slot-name))

(defmethod make-writer-method-function ((class funcallable-standard-class)
                                        slot-name)
  (make-std-writer-method-function class slot-name))

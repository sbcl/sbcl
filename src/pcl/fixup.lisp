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
    ;; FIXME: change SLOT-OBJECT here to T to get SLOT-MISSING
    ;; behaviour for non-slot-objects too?
    (let ((reader-specializers (load-time-value (list (find-class 'slot-object)) t))
          (writer-specializers (load-time-value (list (find-class 't)
                                                      (find-class 'slot-object)) t)))
      (multiple-value-bind (lambda-list specializers method-class initargs doc)
          (ecase method
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
                                        initargs doc :slot-name slot-name)))))))

(dolist (gf-name *!temporary-ensure-accessor-functions*)
  ; (format t "~&Genericizing ~S~%" gf-name)
  (fmakunbound gf-name)
  (ensure-accessor gf-name))

(compute-standard-slot-locations)
(dolist (s '(condition function structure-object))
  (dohash ((k v) (classoid-subclasses (find-classoid s)))
    (declare (ignore v))
    (find-class (classoid-name k))))
(setq **boot-state** 'complete)

;;; CLASS-PROTOTYPE for FUNCTION should not use ALLOCATE-INSTANCE.
(let ((class (find-class 'function)))
  (setf (slot-value class 'prototype) #'identity))

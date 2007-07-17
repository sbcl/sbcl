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
(!fix-ensure-accessor-specializers)
(compute-standard-slot-locations)
(dolist (s '(condition structure-object))
  (dohash (k v (classoid-subclasses (find-classoid s)))
    (find-class (classoid-name k))))
(setq *boot-state* 'complete)

(defun print-std-instance (instance stream depth)
  (declare (ignore depth))
  (print-object instance stream))

;;; Access the slot-vector created by MAKE-SLOT-VECTOR.
(defun find-slot-definition (class slot-name)
  (declare (symbol slot-name) (inline getf))
  (let* ((vector (class-slot-vector class))
         (index (rem (sxhash slot-name) (length vector))))
    (declare (simple-vector vector) (index index))
    (do ((plist (svref vector index) (cdr plist)))
        ((not plist))
      (let ((key (car plist)))
        (setf plist (cdr plist))
        (when (eq key slot-name)
          (return (car plist)))))))

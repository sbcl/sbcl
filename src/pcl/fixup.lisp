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
(dolist (s '(condition function structure-object))
  (dohash ((k v) (classoid-subclasses (find-classoid s)))
    (declare (ignore v))
    (find-class (classoid-name k))))
(setq **boot-state** 'complete)

(defun print-std-instance (instance stream depth)
  (declare (ignore depth))
  (print-object instance stream))

(setf (compiler-macro-function 'slot-value) nil)
(setf (compiler-macro-function 'set-slot-value) nil)

(in-package "SB-C")

(deftransform slot-value ((object slot-name) (t (constant-arg symbol)) *
                          :node node)
  (let ((c-slot-name (lvar-value slot-name)))
    (if (sb-pcl::interned-symbol-p c-slot-name)
        (let* ((type (lvar-type object))
               (dd (when (structure-classoid-p type)
                     (find-defstruct-description
                      (sb-kernel::structure-classoid-name type))))
               (dsd (when dd
                      (find c-slot-name (dd-slots dd) :key #'dsd-name))))
          (cond (dsd
                 `(,(dsd-accessor-name dsd) object))
                (t
                 (delay-ir1-transform node :constraint)
                 `(sb-pcl::accessor-slot-value object ',c-slot-name))))
        (give-up-ir1-transform "slot name is not an interned symbol"))))

(deftransform sb-pcl::set-slot-value ((object slot-name new-value)
                                      (t (constant-arg symbol) t)
                                      * :node node)
  (let ((c-slot-name (lvar-value slot-name)))
    (if (sb-pcl::interned-symbol-p c-slot-name)
        (let* ((type (lvar-type object))
               (dd (when (structure-classoid-p type)
                     (find-defstruct-description
                      (sb-kernel::structure-classoid-name type))))
               (dsd (when dd
                      (find c-slot-name (dd-slots dd) :key #'dsd-name))))
          (cond (dsd
                 `(setf (,(dsd-accessor-name dsd) object) new-value))
                ((policy node (= safety 3))
                 ;; Safe code wants to check the type, and the global
                 ;; accessor won't do that. Also see the comment in the
                 ;; compiler-macro.
                 (give-up-ir1-transform "cannot use optimized accessor in safe code"))
                (t
                 (delay-ir1-transform node :constraint)
                 `(sb-pcl::accessor-set-slot-value object ',c-slot-name new-value))))
        (give-up-ir1-transform "slot name is not an interned symbol"))))

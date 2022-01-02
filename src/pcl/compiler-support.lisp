;;;; things which the main SBCL compiler needs to know about the
;;;; implementation of CLOS
;;;;
;;;; (Our CLOS is derived from PCL, which was implemented in terms of
;;;; portable high-level Common Lisp. But now that it no longer needs
;;;; to be portable, we can make some special hacks to support it
;;;; better.)

;;;; This software is part of the SBCL system. See the README file for more
;;;; information.

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

(in-package "SB-C")

;;;; very low-level representation of instances with meta-class
;;;; STANDARD-CLASS

(deftransform sb-pcl::pcl-instance-p ((object))
  ;; We declare SPECIFIER-TYPE notinline here because otherwise the
  ;; literal classoid reflection/dumping machinery will instantiate
  ;; the type at Genesis time, confusing PCL bootstrapping.
  (declare (notinline specifier-type))
  (let* ((otype (lvar-type object))
         (standard-object (specifier-type 'standard-object)))
    ;; Flush tests whose result is known at compile time.
    (cond ((csubtypep otype standard-object) t)
          ((not (types-equal-or-intersect otype standard-object)) nil)
          (t `(%pcl-instance-p object)))))

(declaim (ftype function sb-pcl::parse-specialized-lambda-list))
(define-source-context defmethod (name &rest stuff)
  (let ((arg-pos (position-if #'listp stuff)))
    (if arg-pos
        `(defmethod ,name ,@(subseq stuff 0 arg-pos)
           ,(handler-case
                (nth-value 2 (sb-pcl::parse-specialized-lambda-list
                              (elt stuff arg-pos)))
              (error () "<illegal syntax>")))
        `(defmethod ,name "<illegal syntax>"))))

(define-load-time-global sb-pcl::*internal-pcl-generalized-fun-name-symbols* nil)

(defmacro define-internal-pcl-function-name-syntax (name (var) &body body)
  `(progn
     (define-function-name-syntax ,name (,var) ,@body)
     (pushnew ',name sb-pcl::*internal-pcl-generalized-fun-name-symbols*)))

(pushnew 'sb-pcl::slot-accessor sb-pcl::*internal-pcl-generalized-fun-name-symbols*)

(define-internal-pcl-function-name-syntax sb-pcl::slot-accessor (list)
  (when (= (length list) 4)
    (destructuring-bind (class slot rwb) (cdr list)
      (when (and (member rwb '(sb-pcl::reader sb-pcl::writer sb-pcl::boundp))
                 (symbolp slot)
                 (symbolp class))
        (values t slot)))))

(define-internal-pcl-function-name-syntax sb-pcl::fast-method (list)
  (valid-function-name-p (cadr list)))

(define-internal-pcl-function-name-syntax sb-pcl::slow-method (list)
  (valid-function-name-p (cadr list)))

(flet ((struct-accessor-p (object slot-name)
         (let ((c-slot-name (lvar-value slot-name)))
           (unless (interned-symbol-p c-slot-name)
             (give-up-ir1-transform "slot name is not an interned symbol"))
           (let* ((type (lvar-type object))
                  (dd (when (structure-classoid-p type)
                        (find-defstruct-description
                         (sb-kernel::structure-classoid-name type)))))
              (when dd
                (find c-slot-name (dd-slots dd) :key #'dsd-name))))))

  (deftransform slot-boundp ((object slot-name) (t (constant-arg symbol)) *
                             :node node)
    (cond ((struct-accessor-p object slot-name) t) ; always boundp
          (t (delay-ir1-transform node :constraint)
             `(sb-pcl::accessor-slot-boundp object ',(lvar-value slot-name)))))

  (deftransform slot-value ((object slot-name) (t (constant-arg symbol)) *
                            :node node)
    (acond ((struct-accessor-p object slot-name)
            `(,(dsd-accessor-name it) object))
           (t
            (delay-ir1-transform node :constraint)
            `(sb-pcl::accessor-slot-value object ',(lvar-value slot-name)))))

  (deftransform sb-pcl::set-slot-value ((object slot-name new-value)
                                        (t (constant-arg symbol) t)
                                        * :node node)
    (acond ((struct-accessor-p object slot-name)
            `(setf (,(dsd-accessor-name it) object) new-value))
           ((policy node (= safety 3))
            ;; Safe code wants to check the type, and the global
            ;; accessor won't do that.
            (give-up-ir1-transform "cannot use optimized accessor in safe code"))
           (t
            (delay-ir1-transform node :constraint)
            `(sb-pcl::accessor-set-slot-value object ',(lvar-value slot-name)
                                              new-value)))))

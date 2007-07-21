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

(defknown sb-pcl::pcl-instance-p (t) boolean
  (movable foldable flushable explicit-check))

(deftransform sb-pcl::pcl-instance-p ((object))
  (let* ((otype (lvar-type object))
         (standard-object (specifier-type 'standard-object)))
    (cond
      ;; Flush tests whose result is known at compile time.
      ((csubtypep otype standard-object) t)
      ((not (types-equal-or-intersect otype standard-object)) nil)
      (t
       `(typep (layout-of object) 'sb-pcl::wrapper)))))

(defun sb-pcl::safe-code-p (&optional env)
  (let* ((lexenv (or env (make-null-lexenv)))
         (policy (lexenv-policy lexenv)))
    (eql (cdr (assoc 'safety policy)) 3)))

(define-source-context defmethod (name &rest stuff)
  (let ((arg-pos (position-if #'listp stuff)))
    (if arg-pos
        `(defmethod ,name ,@(subseq stuff 0 arg-pos)
           ,(handler-case
                (nth-value 2 (sb-pcl::parse-specialized-lambda-list
                              (elt stuff arg-pos)))
              (error () "<illegal syntax>")))
        `(defmethod ,name "<illegal syntax>"))))

(defvar sb-pcl::*internal-pcl-generalized-fun-name-symbols* nil)

(defmacro define-internal-pcl-function-name-syntax (name &body body)
  `(progn
     (define-function-name-syntax ,name ,@body)
     (pushnew ',name sb-pcl::*internal-pcl-generalized-fun-name-symbols*)))

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

(define-internal-pcl-function-name-syntax sb-pcl::ctor (list)
  (valid-function-name-p (cadr list)))

(defun sb-pcl::random-documentation (name type)
  (cdr (assoc type (info :random-documentation :stuff name))))

(defun sb-pcl::set-random-documentation (name type new-value)
  (let ((pair (assoc type (info :random-documentation :stuff name))))
    (if pair
        (setf (cdr pair) new-value)
        (push (cons type new-value)
              (info :random-documentation :stuff name))))
  new-value)

(defsetf sb-pcl::random-documentation sb-pcl::set-random-documentation)

;;;; SLOT-VALUE optimizations

(defknown slot-value (t symbol) t (any))
(defknown sb-pcl::set-slot-value (t symbol t) t (any))

(defun pcl-boot-state-complete-p ()
  (eq 'sb-pcl::complete sb-pcl::*boot-state*))

;;; These essentially duplicate what the compiler-macros in slots.lisp
;;; do, but catch more cases. We retain the compiler-macros since they
;;; can be used during the build, and because they catch common cases
;;; slightly more cheaply then the transforms. (Transforms add new
;;; lambdas, which requires more work by the compiler.)

(deftransform slot-value ((object slot-name))
  "optimize"
  (let (c-slot-name)
    (if (and (pcl-boot-state-complete-p)
             (constant-lvar-p slot-name)
             (setf c-slot-name (lvar-value slot-name))
             (sb-pcl::interned-symbol-p c-slot-name))
        `(sb-pcl::accessor-slot-value object ',c-slot-name)
        (give-up-ir1-transform "Slot name is not constant."))))

(deftransform sb-pcl::set-slot-value ((object slot-name new-value)
                                      (t symbol t) t
                                      ;; Safe code wants to check the
                                      ;; type, and the global accessor
                                      ;; won't do that. Also see the
                                      ;; comment in the
                                      ;; compiler-macro.
                                      :policy (< safety 3))
  "optimize"
  (let (c-slot-name)
    (if (and (pcl-boot-state-complete-p)
             (constant-lvar-p slot-name)
             (setf c-slot-name (lvar-value slot-name))
             (sb-pcl::interned-symbol-p c-slot-name))
        `(sb-pcl::accessor-set-slot-value object ',c-slot-name new-value)
        (give-up-ir1-transform "Slot name is not constant."))))

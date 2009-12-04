;;;; some basic PRINT-OBJECT functionality

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.

;;;; This software is derived from software originally released by Xerox
;;;; Corporation. Copyright and release statements follow. Later modifications
;;;; to the software are in the public domain and are provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for more
;;;; information.

;;;; Some of the text in this file was originally taken from various files of
;;;; the PCL system from Xerox Corporation, which carried the following
;;;; copyright information:
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

;;;; the PRINT-OBJECT generic function

;;; Blow away the old non-generic function placeholder which was used
;;; by the printer doing bootstrapping, and immediately replace it
;;; with some new printing logic, so that the Lisp printer stays
;;; crippled only for the shortest necessary time.
(/show0 "about to replace placeholder PRINT-OBJECT with DEFGENERIC")
(let (;; (If we don't suppress /SHOW printing while the printer is
      ;; crippled here, it becomes really easy to crash the bootstrap
      ;; sequence by adding /SHOW statements e.g. to the compiler,
      ;; which kinda defeats the purpose of /SHOW being a harmless
      ;; tracing-style statement.)
      #+sb-show (*/show* nil)
      ;; (another workaround for the problem of debugging while the
      ;; printer is disabled here)
      (sb-impl::*print-object-is-disabled-p* t))
  (fmakunbound 'print-object)
  (defgeneric print-object (object stream))
  (defmethod print-object ((x t) stream)
    (if *print-pretty*
        (pprint-logical-block (stream nil)
          (print-unreadable-object (x stream :type t :identity t)))
        (print-unreadable-object (x stream :type t :identity t)))))
(/show0 "done replacing placeholder PRINT-OBJECT with DEFGENERIC")

;;;; a hook called by the printer to take care of dispatching to PRINT-OBJECT
;;;; for appropriate FUNCALLABLE-INSTANCE objects

;;; Now that CLOS is working, we can replace our old temporary placeholder code
;;; for writing funcallable instances with permanent code:
(defun sb-impl::printed-as-funcallable-standard-class (object stream)
  (when (funcallable-standard-class-p (class-of object))
    (print-object object stream)
    t))

;;;; PRINT-OBJECT methods for objects from PCL classes
;;;;
;;;; FIXME: Perhaps these should be moved back alongside the definitions of
;;;; the classes they print. (Bootstrapping problems could be avoided by
;;;; using DEF!METHOD to do this.)

(defmethod print-object ((method standard-method) stream)
  (print-unreadable-object (method stream :type t :identity t)
    (if (slot-boundp method '%generic-function)
        (let ((generic-function (method-generic-function method)))
          (format stream "~S ~{~S ~}~:S"
                  (and generic-function
                       (generic-function-name generic-function))
                  (method-qualifiers method)
                  (if generic-function
                      (unparse-specializers generic-function (method-specializers method))
                      (method-specializers method))))
        ;; FIXME: Why do we do CALL-NEXT-METHOD in this method (and
        ;; in the PRINT-OBJECT STANDARD-ACCESSOR-METHOD method too)?
        (call-next-method))))

(defmethod print-object ((method standard-accessor-method) stream)
  (print-unreadable-object (method stream :type t :identity t)
    (if (slot-boundp method '%generic-function)
        (let ((generic-function (method-generic-function method)))
          (format stream "~S, slot:~S, ~:S"
                  (and generic-function
                       (generic-function-name generic-function))
                  (accessor-method-slot-name method)
                  (if generic-function
                      (unparse-specializers generic-function (method-specializers method))
                      (method-specializers method))))
        (call-next-method))))

(defmethod print-object ((mc standard-method-combination) stream)
  (print-unreadable-object (mc stream :type t :identity t)
    (format stream
            "~S ~S"
            (slot-value-or-default mc 'type-name)
            (slot-value-or-default mc 'options))))

(defun named-object-print-function (instance stream
                                    &optional (extra nil extra-p))
  (let ((name (slot-value-or-default instance 'name)))
    (print-unreadable-object (instance stream :type t :identity (not name))
      (if extra-p
          (format stream "~S ~:S" name extra)
          (format stream "~S" name)))))

(defmethod print-object ((class class) stream)
  (named-object-print-function class stream))

(defmethod print-object ((slotd slot-definition) stream)
  (named-object-print-function slotd stream))

(defmethod print-object ((generic-function standard-generic-function) stream)
  (named-object-print-function
    generic-function
    stream
    (if (slot-boundp generic-function 'methods)
        (list (length (generic-function-methods generic-function)))
        "?")))

(defmethod print-object ((cache cache) stream)
  (print-unreadable-object (cache stream :type t :identity t)
    (multiple-value-bind (lines-used lines-total max-depth depth-limit)
        (cache-statistics cache)
      (format stream
              "~D key~P, ~:[no value~;value~], ~D/~D lines, depth ~D/~D"
              (cache-key-count cache)
              (cache-key-count cache)
              (cache-value cache)
              lines-used
              lines-total
              max-depth
              depth-limit))))

(defmethod print-object ((wrapper wrapper) stream)
  (print-unreadable-object (wrapper stream :type t :identity t)
    (prin1 (wrapper-class wrapper) stream)))

(defmethod print-object ((dfun-info dfun-info) stream)
  (declare (type stream stream))
  (print-unreadable-object (dfun-info stream :type t :identity t)))

(defmethod print-object ((ctor ctor) stream)
  (print-unreadable-object (ctor stream :type t)
    (format stream "~S ~:S" (ctor-class-or-name ctor) (ctor-initargs ctor)))
  ctor)

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
(unless (sb-impl::!c-runtime-noinform-p)
  (write-string "; Removing placeholder PRINT-OBJECT ...")
  (force-output))
(let ((*print-pretty* t)) ; use pretty printer dispatch table, not PRINT-OBJECT
  (fmakunbound 'print-object)
  (defgeneric print-object (object stream))
  (!install-cross-compiled-methods 'print-object))
(unless (sb-impl::!c-runtime-noinform-p)
  (write-string " done
"))

;;;; PRINT-OBJECT methods for objects from PCL classes
;;;;

(defmethod print-object ((object standard-object) stream)
  (print-unreadable-object (object stream :type t :identity t)))

(defmethod print-object ((object funcallable-standard-object) stream)
  (print-unreadable-object (object stream :type t :identity t)))

(defmethod print-object ((method standard-method) stream)
  (if (slot-boundp method '%generic-function)
      (print-unreadable-object (method stream :type t :identity t)
        (let ((generic-function (method-generic-function method))
              (*print-length* 50))
          (format stream "~:[~*~;~/sb-ext:print-symbol-with-prefix/ ~]~{~S ~}~:S"
                  generic-function
                  (and generic-function
                       (generic-function-name generic-function))
                  (method-qualifiers method)
                  (if generic-function
                      (unparse-specializers generic-function (method-specializers method))
                      (method-specializers method)))))
      (call-next-method)))

(defmethod print-object ((method standard-accessor-method) stream)
  (if (slot-boundp method '%generic-function)
      (print-unreadable-object (method stream :type t :identity t)
        (let ((generic-function (method-generic-function method)))
          (format stream "~/sb-ext:print-symbol-with-prefix/, slot:~S, ~:S"
                  (and generic-function
                       (generic-function-name generic-function))
                  (accessor-method-slot-name method)
                  (if generic-function
                      (unparse-specializers generic-function (method-specializers method))
                      (method-specializers method)))))
      (call-next-method)))

(defmethod print-object ((mc standard-method-combination) stream)
  (print-unreadable-object (mc stream :type t :identity t)
    (format stream "~S ~:S"
            (slot-value-for-printing mc 'type-name)
            (slot-value-for-printing mc 'options))))

(defun named-object-print-function (instance stream
                                    &optional (properly-named-p t)
                                              (extra nil extra-p))
  (cond ((slot-boundp instance 'name) ; case (1): named
         (let ((name (slot-value instance 'name)))
           (print-unreadable-object
               (instance stream :type t :identity (not properly-named-p))
             (format stream "~/sb-ext:print-symbol-with-prefix/~:[~:; ~:S~]"
                     name extra-p extra))))
        ((not extra-p) ; case (2): empty body to avoid an extra space
         (print-unreadable-object (instance stream :type t :identity t)))
        (t ; case (3). no name, but extra data - show #<unbound slot> and data
         (print-unreadable-object (instance stream :type t :identity t)
           (format stream "#<unbound slot> ~:S" extra)))))

(defmethod print-object ((class class) stream)
  ;; Use a similar concept as in OUTPUT-FUN.
  (if (slot-boundp class 'name)
      (let* ((name (class-name class))
             (proper-p (and (symbolp name) (eq (find-class name nil) class))))
        (print-unreadable-object (class stream :type t :identity (not proper-p))
          (print-symbol-with-prefix stream name)))
      ;; "#<CLASS #<unbound slot> {122D1141}>" is ugly. Don't show that.
      (print-unreadable-object (class stream :type t :identity t))))

(defmethod print-object ((slotd slot-definition) stream)
  (named-object-print-function slotd stream))

(defmethod print-object ((generic-function standard-generic-function) stream)
  (multiple-value-call 'named-object-print-function
    generic-function
    stream
    (and (slot-boundp generic-function 'name)
         (let ((name (slot-value generic-function 'name)))
           (and (legal-fun-name-p name)
                (fboundp name)
                (eq (fdefinition name) generic-function))))
    (if (slot-boundp generic-function 'methods)
        (list (length (generic-function-methods generic-function)))
        (values))))

(defmethod print-object ((cache cache) stream)
  (print-unreadable-object (cache stream :type t :identity t)
    (multiple-value-bind (lines-used lines-total) (cache-statistics cache)
      (format stream
              "~D key~:P~:[~;, value~], ~D/~D lines~@[ (LF ~,,2F%)~], depth ~D/~D"
              (cache-key-count cache)
              (cache-value cache)
              lines-used
              lines-total
              (when (plusp lines-total) (/ lines-used lines-total))
              (cache-depth cache)
              (cache-limit cache)))))

(defmethod print-object ((dfun-info dfun-info) stream)
  (declare (type stream stream))
  (print-unreadable-object (dfun-info stream :type t :identity t)))

(defmethod print-object ((ctor ctor) stream)
  (print-unreadable-object (ctor stream :type t)
    (format stream "~S ~:S" (ctor-class-or-name ctor) (ctor-initargs ctor)))
  ctor)

(defmethod print-object ((obj class-precedence-description) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~D" (cpd-count obj))))

(defmethod print-object ((self specializer-with-object) stream)
  (if (and (slot-exists-p self 'object) (slot-boundp self 'object))
      (print-unreadable-object (self stream :type t)
        (write (slot-value self 'object) :stream stream))
      (print-unreadable-object (self stream :type t :identity t))))

sb-c::
(defmethod print-object ((self policy) stream)
  (if *print-readably*
      (call-next-method)
      (print-unreadable-object (self stream :type t)
        (write (policy-to-decl-spec self) :stream stream))))

sb-kernel::(progn
(defmethod print-object ((condition type-error) stream)
  (if (and *print-escape*
           (slot-boundp condition 'expected-type)
           (slot-boundp condition 'datum))
      (flet ((maybe-string (thing)
               (ignore-errors
                 (write-to-string thing :lines 1 :readably nil :array nil :pretty t))))
        (let ((type (maybe-string (type-error-expected-type condition)))
              (datum (maybe-string (type-error-datum condition))))
          (if (and type datum)
              (print-unreadable-object (condition stream :type t)
                (format stream "~@<expected-type: ~A ~_datum: ~A~:@>"
                        type datum))
              (call-next-method))))
      (call-next-method)))

(defmethod print-object ((condition cell-error) stream)
  (if (and *print-escape* (slot-boundp condition 'name))
      (print-unreadable-object (condition stream :type t :identity t)
        (princ (cell-error-name condition) stream))
      (call-next-method)))

(defmethod print-object :around ((o reference-condition) s)
  (call-next-method)
  (unless (or *print-escape* *print-readably*)
    (when (and *print-condition-references*
               (reference-condition-references o))
      (format s "~&See also:~%")
      (pprint-logical-block (s nil :per-line-prefix "  ")
        (do* ((rs (reference-condition-references o) (cdr rs))
              (r (car rs) (car rs)))
             ((null rs))
          (print-reference r s)
          (unless (null (cdr rs))
            (terpri s)))))))) ; end PROGN

;;; Ordinary DEFMETHOD should be used from here on out.
;;; This variable actually has some semantics to being unbound.
;;; FIXME: see if we can eliminate the associated hack in 'methods.lisp'
(makunbound '*!delayed-defmethod-args*)

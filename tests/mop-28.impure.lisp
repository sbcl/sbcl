;;;; miscellaneous side-effectful tests of the MOP

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; While most of SBCL is derived from the CMU CL system, the test
;;;; files (like this one) were written from scratch after the fork
;;;; from CMU CL.
;;;;
;;;; This software is in the public domain and is provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for
;;;; more information.

;;; a test of a non-standard specializer class and non-standard
;;; generic function class, which nevertheless admit the cacheing
;;; strategy implicit in the second return value of
;;; compute-applicable-methods-using-classes.

(load "assertoid.lisp")

(defpackage "OR-SPECIALIZER-TEST"
  (:use "CL" "SB-MOP" "ASSERTOID"))

(in-package "OR-SPECIALIZER-TEST")

(defclass or-specializer (specializer)
  ((classes :initform nil :reader or-specializer-classes :initarg :classes)
   (direct-methods :initform nil :reader specializer-direct-methods)))

(defvar *or-specializer-table* (make-hash-table :test 'equal))

(defun ensure-or-specializer (&rest classes)
  ;; FIXME: duplicate hash values
  (let* ((cs (mapcar (lambda (x) (if (symbolp x) (find-class x) x)) classes))
         (sorted-classes (sort cs #'< :key #'sxhash)))
    (or (gethash sorted-classes *or-specializer-table*)
        (setf (gethash sorted-classes *or-specializer-table*)
              (make-instance 'or-specializer :classes sorted-classes)))))

(defclass gf-with-or (standard-generic-function) ()
  (:metaclass funcallable-standard-class))

(defmethod compute-applicable-methods-using-classes
    ((generic-function gf-with-or) classes)
  ;; FIXME: assume one-argument for now
  (let (applicable-methods)
    (let ((methods (generic-function-methods generic-function)))
      (dolist (m methods)
        (let ((specializer (car (method-specializers m)))
              (class (car classes)))
          (typecase specializer
            (class (when (subtypep class specializer)
                     (push m applicable-methods)))
            (eql-specializer
             (when (eql (class-of (eql-specializer-object specializer))
                        class)
               (return-from compute-applicable-methods-using-classes
                 (values nil nil))))
            (or-specializer
             (dolist (c (or-specializer-classes specializer))
               (when (subtypep class c)
                 (push m applicable-methods))))))))
    ;; FIXME: sort the methods
    (values applicable-methods t)))

(defmethod compute-applicable-methods
    ((generic-function gf-with-or) arguments)
  ;; FIXME: assume one-argument for now
  (let (applicable-methods)
    (let ((methods (generic-function-methods generic-function)))
      (dolist (m methods)
        (let ((specializer (car (method-specializers m)))
              (argument (car arguments)))
          (typecase specializer
            (class (when (typep argument specializer)
                     (push m applicable-methods)))
            (eql-specializer
             (when (eql (eql-specializer-object specializer) argument)
               (push m applicable-methods)))
            (or-specializer
             (dolist (c (or-specializer-classes specializer))
               (when (typep argument c)
                 (push m applicable-methods))))))))
    ;; FIXME: sort the methods
    applicable-methods))

(defmethod add-direct-method ((specializer or-specializer) method)
  (pushnew method (slot-value specializer 'direct-methods)))

(defmethod remove-direct-method ((specializer or-specializer) method)
  (setf (slot-value specializer 'direct-methods)
        (remove method (slot-value specializer 'direct-methods))))

;;; FIXME: write SPECIALIZER-DIRECT-GENERIC-FUNCTIONS method

(defclass class1 () ())
(defclass class2 () ())
(defclass class3 () ())
(defclass class4 (class1) ())

(defgeneric foo (x)
  (:generic-function-class gf-with-or))

(let ((specializer (ensure-or-specializer 'class1 'class2)))
  (eval `(defmethod foo ((x ,specializer)) t)))

(assert (foo (make-instance 'class1)))
(assert (foo (make-instance 'class2)))
(assert (raises-error? (foo (make-instance 'class3))))
(assert (foo (make-instance 'class4)))

;;; check that we are actually cacheing effective methods.  If the
;;; representation in PCL changes, this test needs to change too.
(assert (typep (cddr (sb-pcl::gf-dfun-state #'foo)) 'sb-pcl::caching))

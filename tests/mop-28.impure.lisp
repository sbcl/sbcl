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

(defclass or-specializer (sb-mop:specializer)
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
  (:metaclass sb-mop:funcallable-standard-class))

(defmethod sb-pcl:specializer-type-specifier
    ((proto-generic-function gf-with-or)
     (proto-method t)
     (specializer or-specializer))
  `(or ,@(or-specializer-classes specializer)))

(defmethod sb-mop:compute-applicable-methods-using-classes
    ((generic-function gf-with-or) classes)
  ;; FIXME: assume one-argument for now
  (let (applicable-methods)
    (let ((methods (sb-mop:generic-function-methods generic-function)))
      (dolist (m methods)
        (let ((specializer (first (sb-mop:method-specializers m)))
              (class (first classes)))
          (typecase specializer
            (class (when (subtypep class specializer)
                     (push m applicable-methods)))
            (sb-mop:eql-specializer
             (when (eql (class-of (sb-mop:eql-specializer-object specializer))
                        class)
               (return-from sb-mop:compute-applicable-methods-using-classes
                 (values nil nil))))
            (or-specializer
             (dolist (c (or-specializer-classes specializer))
               (when (subtypep class c)
                 (push m applicable-methods))))))))
    ;; FIXME: sort the methods
    (values applicable-methods t)))

(defmethod sb-mop:compute-applicable-methods
    ((generic-function gf-with-or) arguments)
  ;; FIXME: assume one-argument for now
  (let (applicable-methods)
    (let ((methods (sb-mop:generic-function-methods generic-function)))
      (dolist (m methods)
        (let ((specializer (first (sb-mop:method-specializers m)))
              (argument (first arguments)))
          (typecase specializer
            (class (when (typep argument specializer)
                     (push m applicable-methods)))
            (sb-mop:eql-specializer
             (when (eql (sb-mop:eql-specializer-object specializer) argument)
               (push m applicable-methods)))
            (or-specializer
             (dolist (c (or-specializer-classes specializer))
               (when (typep argument c)
                 (push m applicable-methods))))))))
    ;; FIXME: sort the methods
    applicable-methods))

(defmethod sb-mop:add-direct-method ((specializer or-specializer) method)
  (pushnew method (slot-value specializer 'direct-methods)))

(defmethod sb-mop:remove-direct-method ((specializer or-specializer) method)
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

(with-test (:name (:mop-28 1))
  (assert (foo (make-instance 'class1)))
  (assert (foo (make-instance 'class2)))
  (assert-error (foo (make-instance 'class3)))
  (assert (foo (make-instance 'class4))))

;;; check that we are actually cacheing effective methods.  If the
;;; representation in PCL changes, this test needs to change too.
(with-test (:name (:mop-28 2))
  (assert (typep (cddr (sb-pcl::gf-dfun-state #'foo)) 'sb-pcl::caching)))

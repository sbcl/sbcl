;;;; Tests of SLOT-VALUE

(load "compiler-test-util.lisp")
(defpackage "SLOT-VALUE-TEST"
  (:use "CL" "SB-MOP" "ASSERTOID" "TEST-UTIL"))

(in-package "SLOT-VALUE-TEST")

(defclass a-class ()
  ((x :initform 123)))

(defun a-class-x-0 (a)
  (slot-value a 'x))

(defmethod a-class-x-1 ((a a-class))
  (slot-value a 'x))

(defmethod a-class-x-2 ((a a-class))
  (let ((%a a))
    (slot-value %a 'x)))

(defmethod a-class-x-3 ((a t))
  (slot-value a 'x))

(with-test (:name (slot-value defun))
  (assert (= (a-class-x-0 (make-instance 'a-class)) 123)))

(with-test (:name (slot-value defmethod))
  (assert (= (a-class-x-1 (make-instance 'a-class)) 123)))

(with-test (:name (slot-value defmethod let))
  (assert (= (a-class-x-2 (make-instance 'a-class)) 123)))

(with-test (:name (slot-value defmethod t))
  (assert (= (a-class-x-3 (make-instance 'a-class)) 123)))

(defun read-slot-way1 (instance slot)
  (slot-value instance slot))
(defun read-slot-way2 (instance slot)
  (slot-value (the structure-object instance) slot))
(compile 'read-slot-way1)
(compile 'read-slot-way2)

;;; Collect up a bunch of instances that are subtypes of STRUCTURE-OBJECT
(with-test (:name :fast-structure-slot-value)
  (let ((instances
         (sb-vm:list-allocated-objects
          :all :type sb-vm:instance-widetag
          :test (lambda (x)
                  (and (typep x 'structure-object)
                       ;; want to ensure the instances under test are mostly immutable.
                       ;; This is no guarantee, but it works.
                       (eq (sb-kernel:generation-of x)
                           sb-vm:+pseudo-static-generation+)))
          :count 10000)))
    (dolist (x instances)
      (let* ((layout (sb-kernel:%instance-layout x))
             (dd (sb-kernel:layout-dd layout)))
        (dolist (dsd (sb-kernel:dd-slots dd))
          (let ((slot-name (sb-kernel:dsd-name dsd)))
            ;; some slots may be raw, don't worry about being EQ
            (assert (eql (read-slot-way1 x slot-name)
                         (read-slot-way2 x slot-name)))))))))

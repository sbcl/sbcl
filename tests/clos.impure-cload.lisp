;;;; miscellaneous side-effectful tests of CLOS and file-compiler
;;;; optimizations

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

;;; Fix due to pmai, ported from CMUCL, regarding
;;; MAKE-INSTANCES-OBSOLETE:
(defclass mio-test ()
  ((test :initarg :test)))

(defun mio-demo ()
  (let ((x (make-instance 'mio-test :test 42)))
    (incf (slot-value x 'test))))

(defun mio-test ()
  (mio-demo)
  (make-instances-obsolete 'mio-test)
  (mio-demo))

(mio-test)

;;; Some tests of bits of optimized MAKE-INSTANCE that were hopelessly
;;; wrong until Gerd's ctor MAKE-INSTANCE optimization was ported.
(defvar *d-i-s-e-count* 0)
(defclass default-initargs-side-effect ()
  ((x :initarg :x))
  (:default-initargs :x (incf *d-i-s-e-count*)))
(defun default-initargs-side-effect ()
  (make-instance 'default-initargs-side-effect))
(assert (= *d-i-s-e-count* 0))
(default-initargs-side-effect)
(assert (= *d-i-s-e-count* 1))
(make-instance 'default-initargs-side-effect)
(assert (= *d-i-s-e-count* 2))
(make-instance 'default-initargs-side-effect :x 3)
(assert (= *d-i-s-e-count* 2))

(defclass class-allocation ()
  ((x :allocation :class :initarg :x :initform 3)))
(defun class-allocation-reader ()
  (slot-value (make-instance 'class-allocation) 'x))
(defun class-allocation-writer (value)
  (setf (slot-value (make-instance 'class-allocation) 'x) value))
(assert (= (class-allocation-reader) 3))
(class-allocation-writer 4)
(assert (= (class-allocation-reader) 4))

;;; from James Anderson via Gerd Moellmann: defining methods with
;;; specializers with forward-referenced superclasses used not to
;;; work.
(defclass specializer1 () ())
(defclass specializer2 (forward-ref1) ())
(defmethod baz ((x specializer2)) x)
(defmethod baz ((x specializer1)) x)
(assert (typep (baz (make-instance 'specializer1)) 'specializer1))

;;; in a similar vein, we should be able to define methods on classes
;;; that are effectively unknown to the type system:
(sb-mop:ensure-class 'unknown-type)
(defmethod method ((x unknown-type)) x)
;;; (we can't call it without defining methods on allocate-instance
;;; etc., but we should be able to define it).

;;; the ctor MAKE-INSTANCE optimizer used not to handle duplicate
;;; initargs.
(defclass dinitargs-class1 ()
  ((a :initarg :a)))
(assert (= (slot-value (make-instance 'dinitargs-class1 :a 1 :a 2) 'a) 1))

(defclass dinitargs-class2 ()
  ((b :initarg :b1 :initarg :b2)))
(assert (= (slot-value (make-instance 'dinitargs-class2 :b2 3 :b1 4) 'b) 3))

;;; success
(sb-ext:quit :unix-status 104)
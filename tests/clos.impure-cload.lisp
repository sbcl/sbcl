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

;;; success
(sb-ext:quit :unix-status 104)
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

;;;; Note that the MOP is not in a supported state. Package issues
;;;; (both MOP/SB-PCL and CL/SB-PCL) have yet to be resolved, and
;;;; there is likely to be missing functionality.  However, this seems
;;;; a good a way as any of ensuring that we have no regressions.

(defpackage "MOP-TEST"
  ;; eventually, we might want "MOP" as well here.
  (:use "CL"))

(in-package "MOP-TEST")

;;; Readers for Generic Function Metaobjects (pp. 216--218 of AMOP)
(defgeneric fn-with-odd-arg-precedence (a b c)
  (:argument-precedence-order b c a))

(assert (equal
	 (sb-pcl:generic-function-lambda-list #'fn-with-odd-arg-precedence)
	 '(a b c)))
(assert (equal
	 (sb-pcl:generic-function-argument-precedence-order #'fn-with-odd-arg-precedence)
	 '(b c a)))
;;; Test for DOCUMENTATION's order, which was wrong until sbcl-0.7.8.39
(assert (equal
	 (sb-pcl:generic-function-argument-precedence-order #'documentation)
	 (let ((ll (sb-pcl:generic-function-lambda-list #'documentation)))
	   (list (nth 1 ll) (nth 0 ll)))))

(assert (null
	 (sb-pcl:generic-function-declarations #'fn-with-odd-arg-precedence)))
(defgeneric gf-with-declarations (x)
  (declare (optimize (speed 3)))
  (declare (optimize (safety 0))))
(let ((decls (sb-pcl:generic-function-declarations #'gf-with-declarations)))
  (assert (= (length decls) 2))
  (assert (member '(optimize (speed 3)) decls :test #'equal))
  (assert (member '(optimize (safety 0)) decls :test #'equal)))

;;; Readers for Slot Definition Metaobjects (pp. 221--224 of AMOP)

;;; Ensure that SLOT-DEFINITION-ALLOCATION returns :INSTANCE/:CLASS as
;;; appropriate.
(defclass sdm-test-class ()
  ((an-instance-slot :accessor an-instance-slot)
   (a-class-slot :allocation :class :accessor a-class-slot)))
(dolist (m (list (list #'an-instance-slot :instance)
		 (list #'a-class-slot :class)))
  (let ((methods (sb-pcl:generic-function-methods (car m))))
    (assert (= (length methods) 1))
    (assert (eq (sb-pcl:slot-definition-allocation
		 (sb-pcl:accessor-method-slot-definition
		  (car methods)))
		(cadr m)))))

;;; Class Finalization Protocol (see section 5.5.2 of AMOP)
(let ((finalized-count 0))
  (defmethod sb-pcl:finalize-inheritance :after ((x sb-pcl::standard-class))
    (incf finalized-count))
  (defun get-count () finalized-count))
(defclass finalization-test-1 () ())
(make-instance 'finalization-test-1)
(assert (= (get-count) 1))
(defclass finalization-test-2 (finalization-test-3) ())
(assert (= (get-count) 1))
(defclass finalization-test-3 () ())
(make-instance 'finalization-test-3)
(assert (or (= (get-count) 2) (= (get-count) 3)))
(make-instance 'finalization-test-2)
(assert (= (get-count) 3))

;;;; success
(sb-ext:quit :unix-status 104)

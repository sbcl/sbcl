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

;;;; Note that the MOP is not in an entirely supported state.
;;;; However, this seems a good a way as any of ensuring that we have
;;;; no regressions.

(defpackage "MOP-TEST"
  (:use "CL" "SB-MOP"))

(in-package "MOP-TEST")

;;; Readers for Class Metaobjects (pp. 212--214 of AMOP)
(defclass red-herring (forward-ref) ())

(assert (null (class-direct-slots (find-class 'forward-ref))))
(assert (null (class-direct-default-initargs
	       (find-class 'forward-ref))))

;;; Readers for Generic Function Metaobjects (pp. 216--218 of AMOP)
(defgeneric fn-with-odd-arg-precedence (a b c)
  (:argument-precedence-order b c a))

(assert (equal
	 (generic-function-lambda-list #'fn-with-odd-arg-precedence)
	 '(a b c)))
(assert (equal
	 (generic-function-argument-precedence-order #'fn-with-odd-arg-precedence)
	 '(b c a)))
;;; Test for DOCUMENTATION's order, which was wrong until sbcl-0.7.8.39
(assert (equal
	 (generic-function-argument-precedence-order #'documentation)
	 (let ((ll (generic-function-lambda-list #'documentation)))
	   (list (nth 1 ll) (nth 0 ll)))))

(assert (null
	 (generic-function-declarations #'fn-with-odd-arg-precedence)))
(defgeneric gf-with-declarations (x)
  (declare (optimize (speed 3)))
  (declare (optimize (safety 0))))
(let ((decls (generic-function-declarations #'gf-with-declarations)))
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
  (let ((methods (generic-function-methods (car m))))
    (assert (= (length methods) 1))
    (assert (eq (slot-definition-allocation
		 (accessor-method-slot-definition
		  (car methods)))
		(cadr m)))))

;;; Class Finalization Protocol (see section 5.5.2 of AMOP)
(let ((finalized-count 0))
  (defmethod finalize-inheritance :after ((x standard-class))
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

;;; Bits of FUNCALLABLE-STANDARD-CLASS are easy to break; make sure
;;; that it is at least possible to define classes with that as a
;;; metaclass.
(defclass gf-class (standard-generic-function) ()
  (:metaclass sb-pcl::funcallable-standard-class))
(defgeneric g (a b c)
  (:generic-function-class gf-class))

;;; until sbcl-0.7.12.47, PCL wasn't aware of some direct class
;;; relationships.  These aren't necessarily true, but are probably
;;; not going to change often.
(dolist (x '(number array sequence character symbol))
  (assert (eq (car (class-direct-superclasses (find-class x)))
	      (find-class t)))
  (assert (member (find-class x)
		  (class-direct-subclasses (find-class t)))))

;;; the class-prototype of the NULL class used to be some weird
;;; standard-instance-like thing.  Make sure it's actually NIL.
;;;
;;; (and FIXME: eventually turn this into asserting that the prototype
;;; of all built-in-classes is of the relevant type)
(assert (null (class-prototype (find-class 'null))))

;;; simple consistency checks for the SB-PCL (perhaps AKA SB-MOP)
;;; package: all of the functionality specified in AMOP is in
;;; functions:
(assert (null (loop for x being each external-symbol in "SB-PCL"
		    unless (fboundp x) collect x)))
;;; and all generic functions in SB-PCL have at least one specified
;;; method, except for UPDATE-DEPENDENT
(assert (null (loop for x being each external-symbol in "SB-PCL"
		    unless (or (eq x 'update-dependent)
			       (not (typep (fdefinition x) 'generic-function))
			       (> (length (generic-function-methods
					   (fdefinition x)))
				  0))
		    collect x)))

;;; make sure that ENSURE-CLASS-USING-CLASS's arguments are the right
;;; way round (!)
(defvar *e-c-u-c-arg-order* nil)
(defmethod ensure-class-using-class :after
    (class (name (eql 'e-c-u-c-arg-order)) &key &allow-other-keys)
  (setf *e-c-u-c-arg-order* t))
(defclass e-c-u-c-arg-orderoid () ())
(assert (null *e-c-u-c-arg-order*))
(defclass e-c-u-c-arg-order () ())
(assert (eq *e-c-u-c-arg-order* t))
;;;; success
(sb-ext:quit :unix-status 104)

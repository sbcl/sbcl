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

;;; this file tests that FINALIZE-INHERITANCE behaves intuitively:
;;; that when FINALIZE-INHERITANCE is called on a class, it returns
;;; before subclasses are finalized.

(defclass mop-15-class (standard-class) ())

(defmethod sb-mop:validate-superclass ((s mop-15-class) (super standard-class))
  t)

(defvar *count* 0)
(defvar *max-count* 0)

(defmethod sb-mop:finalize-inheritance ((c mop-15-class))
  (let ((*count* (1+ *count*)))
    (when (> *count* *max-count*)
      (setf *max-count* *count*))
    (call-next-method)))

(defclass sub (super)
  ()
  (:metaclass mop-15-class))

(defclass super ()
  ()
  (:metaclass mop-15-class))

(with-test (:name :mop-15)
  (sb-mop:finalize-inheritance (find-class 'super))
  (sb-mop:finalize-inheritance (find-class  'sub))

  (assert (= *max-count* 1)))

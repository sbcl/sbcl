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

;;; This isn't really a test of the MOP per se.  PCL historically has
;;; a CLASS-EQ specializer, which it uses internally to achieve
;;; certain effects.  There's no particular reason that it should be
;;; exposed to the user, except that some people have asked for it at
;;; some point; however, there is also no particular reason that code
;;; using it should be gratuitously broken, as it was for a while by
;;; the SB-PCL::PARAMETER-SPECIALIZER-DECLARATION-IN-DEFMETHOD
;;; function.  So it's fine if this test starts failing, as long as
;;; it's deliberate.

(defclass super () ())
(defclass sub (super) ())

(defgeneric test (x))

(defmethod test ((x t)) nil)
(let ((spec (sb-pcl::class-eq-specializer (find-class 'super))))
  (eval `(defmethod test ((x ,spec)) t)))

(with-test (:name (:mop-26 1))
  (assert (test (make-instance 'super)))
  (assert (null (test (make-instance 'sub)))))

(let ((spec (sb-pcl::class-eq-specializer (find-class 't))))
  (eval `(defmethod test ((x ,spec)) (class-of x))))

(with-test (:name (:mop-26 2))
  (assert (test (make-instance 'super)))
  (assert (null (test (make-instance 'sub)))))

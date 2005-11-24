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

;;; A distilled test case from cmucl-imp for Kevin Rosenberg's
;;; hyperobject.  Fix from Gerd Moellmann.
(defclass hyperobject-class (standard-class)
  ((user-name :initarg :user-name :type (or null string) :initform nil
              :accessor user-name
              :documentation "User name for class")))

(defclass hyperobject-dsd (standard-direct-slot-definition)
  ())

(defclass hyperobject-esd (standard-effective-slot-definition)
  ((vc :initform 42)))

(defmethod validate-superclass ((class hyperobject-class)
                                (superclass standard-class))
  t)

(defmethod compute-effective-slot-definition :around
    ((cl hyperobject-class) name dsds)
  (let ((ia (sb-pcl::compute-effective-slot-definition-initargs cl dsds)))
    (apply #'make-instance 'hyperobject-esd ia)))

(defmethod (setf slot-value-using-class) :around
    (new-value (cl hyperobject-class) obj (slot hyperobject-esd))
  (format t "~s ~s ~s~%" cl obj slot)
  (slot-value slot 'vc))

(defclass hyperobject ()
  ()
  (:metaclass hyperobject-class))

(defclass person (hyperobject)
  ((name :initarg :name :accessor person-name))
  (:metaclass hyperobject-class))


(eval '(make-instance 'person :name t))

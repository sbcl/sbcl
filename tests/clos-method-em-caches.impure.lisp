;;;; testing the caching of effective methods

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

(with-test (:name (defmethod (defgeneric &allow-other-keys) funcall))
  (defmethod foo1 (&key x) x)
  (defgeneric foo1 (&key &allow-other-keys))
  (assert (null (foo1 :y 234))))

(with-test (:name (defmethod funcall (defgeneric &allow-other-keys) funcall))
  (defmethod foo2 (&key x) x)
  (assert-error (foo2 :y 234) program-error)
  (defgeneric foo2 (&key &allow-other-keys))
  (assert (null (foo2 :y 234))))

(with-test (:name (defgeneric defmethod funcall (defgeneric &allow-other-keys) funcall))
  (defgeneric foo3 (&key))
  (defmethod foo3 (&key x))
  (assert-error (foo3 :y 234) program-error)
  (defgeneric foo3 (&key &allow-other-keys))
  (assert (null (foo3 :y 234))))

(with-test (:name ((defgeneric &allow-other-keys) defmethod funcall defgeneric funcall))
  (defgeneric foo4 (&key &allow-other-keys))
  (defmethod foo4 (&key x))
  (assert (null (foo4 :y 234)))
  (defgeneric foo4 (&key))
  (assert-error (foo4 :y 234) program-error))

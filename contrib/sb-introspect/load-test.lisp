;;;
;;; The order of the forms must not change, as the order is checked in
;;; `test-driver.lisp'. Thus do not alter this file unless you edit
;;; test-driver.lisp to match.
;;;

(declaim (optimize (debug 3)))
(in-package :cl-user)

(eval-when (:compile-toplevel)
  (error "load-test.lisp needs to be loaded as source"))

(defun loaded-as-source-fun ()
  t)

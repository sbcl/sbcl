(defpackage "PACKAGE-CTOR-BUG"
  (:use "CL")
  (:export "TEST"))

(in-package "PACKAGE-CTOR-BUG")

(defclass fooass ()
  ((flot :initarg :flot :reader flot)))

(defun test ()
  (flot (make-instance 'fooass :flot 3)))

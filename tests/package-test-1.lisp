(defpackage "FOO"
  (:use "CL"))

(in-package "FOO")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun baz ()
    :bad))

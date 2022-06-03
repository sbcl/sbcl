(defpackage "FOO"
  (:use "CL")
  (:export "BAR" "BAZ"))

(in-package "FOO")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun baz ()
    1)
  (defmacro bar (&rest args)
    `(baz ,@args)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "COMPILE-TWICE")
    (make-package "COMPILE-TWICE" :use '("CL"))))

(in-package "COMPILE-TWICE")

(defstruct bar-compile-twice (foo))

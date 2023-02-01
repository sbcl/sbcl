(eval-when (:compile-toplevel :load-toplevel :execute)
  (cond
    ((find-package "FOO")
     (rename-package "FOO" "BAR" '("FOO")))
    ((not (find-package "BAR"))
     (make-package "BAR" :use '("CL") :nicknames '("FOO"))
     (export (list (intern "BAZ" "BAR")) "BAR"))))

(in-package "BAR")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun baz ()
    :good))

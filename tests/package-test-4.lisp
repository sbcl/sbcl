(eval-when (:compile-toplevel :load-toplevel :execute)
  (cond
    ((find-package "FOO")
     (rename-package "FOO" "FOO-NEW" '("FOO")))
    ((not (find-package "FOO-NEW"))
     (make-package "FOO-NEW" :use '("CL") :nicknames '("FOO"))
     (export (list (intern "BAR" "FOO-NEW") (intern "BAZ" "FOO-NEW")) "FOO-NEW"))))

(in-package "FOO-NEW")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro bar (&rest args)
    `(baz ,@args)))

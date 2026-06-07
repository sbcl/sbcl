(locally (declare (sb-ext:muffle-conditions sb-int:package-at-variance))
  (handler-bind ((sb-int:package-at-variance #'muffle-warning))
    (defpackage :sb-manual
      (:use :cl :sb-alien))))

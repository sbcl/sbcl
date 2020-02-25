(defpackage :sb-cltl2
  (:use :cl :sb-c :sb-int :sb-kernel)
  (:export #:compiler-let
           #:macroexpand-all
           ;; environment access
           #:variable-information
           #:function-information
           #:declaration-information
           #:augment-environment
           #:define-declaration
           #:parse-macro
           #:enclose
           ))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (sb-int:system-package-p (find-package "SB-CLTL2")) t))

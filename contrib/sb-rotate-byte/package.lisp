(defpackage "SB-ROTATE-BYTE"
  (:use "CL" "SB-C" "SB-VM" "SB-INT" "SB-KERNEL" "SB-ASSEM"
        "SB-BIGNUM")
  (:export "ROTATE-BYTE"))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (sb-int:system-package-p (find-package "SB-ROTATE-BYTE")) t))

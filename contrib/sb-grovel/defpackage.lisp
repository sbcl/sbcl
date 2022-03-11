(defpackage "SB-GROVEL"
  (:export "GROVEL-CONSTANTS-FILE")
  (:shadow "TYPE" "UNION")
  ;; FIXME: This is a really quick and dirty package lock compliance
  ;; fix, that should be redone. Specifically, this is needed to address the
  ;; nasty things done with SB-ALIEN:STRUCT.
  (:implement "SB-ALIEN")
  (:use "COMMON-LISP" "SB-ALIEN" #+asdf "ASDF" #+asdf "UIOP"))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (sb-int:system-package-p (find-package "SB-GROVEL")) t))

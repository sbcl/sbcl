(defpackage "SB-GROVEL"
  (:export "GROVEL-CONSTANTS-FILE")
  (:shadow "TYPE" "UNION")
  ;; FIXME: This is a really quick and dirty package lock compliance
  ;; fix, that should be redone. Specifically, this is needed to address the
  ;; nasty things done with SB-ALIEN:STRUCT.
  #+sb-package-locks
  (:implement "SB-ALIEN")
  (:use "COMMON-LISP" "SB-ALIEN" "ASDF" "SB-EXT"))

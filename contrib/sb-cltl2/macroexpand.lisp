(in-package :sb-cltl2)

(defun macroexpand-all (form &optional environment)
  (let ((sb-walker::*walk-form-expand-macros-p* t))
    (sb-walker:walk-form form environment)))

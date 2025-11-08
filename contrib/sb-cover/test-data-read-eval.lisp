(in-package sb-cover-test)

(defun read-eval (z w)
  #.(generate-code 'z 'w))

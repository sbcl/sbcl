(in-package sb-cover-test)

(defun read-eval-cdr (z w)
  (progn . #.(list (generate-code 'z 'w))))

(in-package sb-cover-test)

(defun test-if (x)
  (if (evenp x)
      (1+ x)
      (1- x)))

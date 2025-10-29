(in-package sb-cover-test)

(defun test2 (x)
  (let ((a 0))
    (when (plusp x)
      (incf a))
    a))

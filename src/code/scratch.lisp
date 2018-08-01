(defun foo (x)
  (declare (type (simple-array fixnum (*)) x))
  (aref x 1))

(in-package sb-cover-test)

(defun sharp-c (c)
  (let ((y #c(1 2)))
    (expt c y)))

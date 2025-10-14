(in-package sb-cover-test)

#+sbcl
(defun sharp-plus-minus (x)
  (let ((y (1+ x)))
    (1+ y)))

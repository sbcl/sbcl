(in-package sb-cover-test)

(defun comma (x)
  `(0 1 ,(1+ x) 3 . ,(if (evenp x) (list 4) (list 5))))

(in-package sb-cover-test)

(defun quote-comma (x)
  `(1 ',(if (evenp x) (1+ x) (+ x 2)) ,(+ x 3)))

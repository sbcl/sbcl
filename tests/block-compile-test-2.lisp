(defun foo1 (x y)
  (bar2 x y))

(defun bar2 (x y)
  (if (zerop x)
      y
      (foo1 (1- x) (* y x))))

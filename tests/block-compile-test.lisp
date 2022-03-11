;; test forward reference
(defun foo2 (x)
  (bar2 x))

(defun bar2 (x)
  (1+ (baz2 (+ 3 x))))

;; test backward reference
(defun baz2 (x)
  (if (zerop x)
      (foo2 x)
      x))

(defun foo.delete-package-redefpackage ()
  (let ((package (find-package :bar-drrfl)))
    (when package
      (let ((count 0))
        (do-symbols (sym package)
          (incf count))
        (assert (= count 2))
        (delete-package package))))
  (make-package "BAR-DRRFL" :use ())
  (let ((package (find-package :bar-drrfl))
        (count 0))
    (do-symbols (sym package)
      (incf count))
    (assert (= count 0))
    (eval (read-from-string
           "(progn
              (defun bar-drrfl::a ())
              (defun bar-drrfl::b ()))"))))

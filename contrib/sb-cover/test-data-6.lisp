(declaim (optimize sb-c:store-coverage-data))

(defun nlx-from-flet ()
  (flet ((foo (x)
           (return-from nlx-from-flet x)))
    (foo 3)
    (print "reached")
    (foo 4)))

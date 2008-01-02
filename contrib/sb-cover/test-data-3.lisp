(declaim (optimize sb-c::store-coverage-data))

(defun test-1 ()
  (print '((1 3 1))))

(defun test-2 ()
  (assert (equal (test-1)
                 (list (list 1 3 1)))))

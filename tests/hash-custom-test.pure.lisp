(with-test (:name (:hash-table :custom-hash-function :closure))
  (flet ((equal-p (left right)
           (equal left right))
         (hash (item)
           (sxhash item)))
    (let ((table (make-hash-table :test #'equal-p
                                  :hash-function #'hash)))
      (loop for i from 0 to 100
            do (progn
                 (setf (gethash i table) i)
                 (assert (= (gethash i table) i))))
      (assert (= (hash-table-count table) 101))
      (assert (eq (hash-table-test table) #'equal-p)))))

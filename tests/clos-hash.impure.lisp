(test-util:with-test (:name :very-random-hashes)
  (dotimes (i 10000) (fmakunbound 'foo) (eval '(defgeneric foo (a b c)))))

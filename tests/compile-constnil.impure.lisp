(with-test (:name :nil-special-case-unsafe)
  (proclaim '(optimize (safety 0)))
  (assert (eq (compile nil '(lambda ())) (compile nil '(lambda () nil))))
  (assert (eq (compile nil '(lambda ())) (compile nil '(lambda () 'nil))))
  (let ((f (compile nil '(lambda ()))))
    (assert (null (funcall f 3)))))

(with-test (:name :nil-special-case-safe)
  (proclaim '(optimize (safety 1)))
  (assert (eq (compile nil '(lambda ())) (compile nil '(lambda () nil))))
  (assert (eq (compile nil '(lambda ())) (compile nil '(lambda () 'nil))))
  (let ((f (compile nil '(lambda ()))))
    (assert (null (funcall f)))
    (assert-error (funcall f 3)))) ; arg count error

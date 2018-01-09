(load "compiler-test-util.lisp")

(with-test (:name :fop-compile-bad-macro)
  (assert (nth-value 1 (ctu:file-compile '((let () (do)))))))

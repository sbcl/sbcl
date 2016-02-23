(in-package :cl-user)

(with-test (:name :constantp-conservatively-nil)
  (assert (not (constantp '(if))))
  (assert (not (constantp '(if . 1))))
  (assert (not (constantp '(if 1))))
  (assert (not (constantp '(if 1 . 2))))
  (assert (not (constantp '(if 1 2 . 3))))
  (assert (not (constantp '(if 1 2 3 4)))))

;;;; Test of non-toplevel DEFMACRO
(cl:in-package :cl-user)

(eval-when (:compile-toplevel)
  (defun defmacro-test-aux (x)
    (setq *defmacro-test-status* `(function ,x))
    nil))

(let ((z 'z-value))
  (defmacro defmacro-test-aux (x)
    (setq *defmacro-test-status* `(macro ,x ,z))
    `(setq *defmacro-test-status* '(expanded ,x ,z))))

(eval-when (:compile-toplevel)
  (defmacro-test-aux 'a))

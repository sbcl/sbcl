(in-package "SB!IMPL")

(locally
    (declare (optimize (speed 3)
                       (safety 0)))
  (defun mv-test-callee ()
    (values 0 1 2 3 4 5))
  (defun mv-test-caller ()
    (multiple-value-bind (v0 v1 v2 v3 v4 v5)
        (mv-test-callee)
      (values v5 v4 v3 v2 v1 v0)))
  (defun !cold-init ()
    (mv-test-caller)
    nil))

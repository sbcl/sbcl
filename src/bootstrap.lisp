(in-package "SB!KERNEL")

(defun bar (x)
  (values x #. (coerce "Hello World" 'simple-base-string)))

(defun !cold-init ()
  (cons 20 30))

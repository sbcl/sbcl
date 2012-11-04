(in-package "SB!IMPL")

(defun test-closure (closure)
  (declare (function closure))
  (funcall closure)
  nil)

(defun test-uwp ()
  (let ((value 3))
    (unwind-protect
         (test-closure (lambda ()
                         (setf value 7)))
      (setf value (logand value 4)))
    value))

(defun !cold-init ()
  (test-uwp))

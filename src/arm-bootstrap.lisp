(in-package "SB!IMPL")

(defun test-closures-receiver (fun)
  (declare (type function fun))
  (funcall fun))

(defun test-closures (value)
  (test-closures-receiver
   (lambda ()
     value)))

(defun !cold-init ()
  (test-closures 14))

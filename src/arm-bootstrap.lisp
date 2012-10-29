(in-package "SB!IMPL")

(defun test-closures-receiver (fun)
  (declare (type function fun))
  (funcall fun))

(defun test-closures (value)
  (flet ((closure-test ()
           (setf value 27)
           (return-from test-closures
             13)))
    (declare (dynamic-extent #'closure-test))
    (test-closures-receiver #'closure-test)
    value))

(defun !cold-init ()
  (test-closures 14))

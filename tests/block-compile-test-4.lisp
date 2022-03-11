;;; From the CMU CL user manual.
(declaim (sb-ext:start-block fun1 fun3))

(defun fun1 (x)
  (print x))

(defun fun2 ()
  (1+ (fun1 6)))

(defun fun3 (x)
  (if x
      (fun1 3)
      (fun2)))

(declaim (sb-ext:end-block))

(defun fun4 (z)
  (+ 2 (fun1 z)))

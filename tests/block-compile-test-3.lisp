(declaim (inline inl))
(defun inl (x) (1- x))
(defun a (x)
  (declare (type (unsigned-byte 64) x))
  (logcount (inl x)))

(defun foo2 (x) (1+ (bar2 x)))
(declaim (inline bar2))
(defun bar2 (x) (1+ x))

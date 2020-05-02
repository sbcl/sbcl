(declaim (start-block gamma1))

(defun zeta1 (x)
  (print (+ 1 x)))

(declaim (inline zeta2))
(defun zeta2 (x)
  (zeta1 x)
  (zeta1 x))

(defun gamma1 (x)
  (+ (zeta2 x)
     (zeta2 x)))

(declaim (end-block))

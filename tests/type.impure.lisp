(in-package :cl-user)

(let ((types '(character
	       integer fixnum (integer 0 10)
	       single-float (single-float -1.0 1.0) (single-float 0.1)
	       (real 4 8) (real -1 7) (real 2 11)
	       (member #\a #\b #\c) (member 1 #\a) (member 3.0 3.3))))
  (dolist (i types)
    (format t "type I=~S~%" i)
    (dolist (j types)
      (format t "  type J=~S~%" j)
      (assert (subtypep i `(or ,i ,j)))
      (assert (subtypep i `(or ,j ,i)))
      (assert (subtypep i `(or ,i ,i ,j)))
      (assert (subtypep i `(or ,j ,i))))))

(defun type-evidently-= (x y)
  (and (subtypep x y)
       (subtypep y x)))

(assert (subtypep 'single-float 'float))

(assert (type-evidently-= '(integer 0 10) '(or (integer 0 5) (integer 4 10))))

;;; success
(quit :unix-status 104)

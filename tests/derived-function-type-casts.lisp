(defun derived-function-type-casts-make-fun () (lambda ()))

(defun derived-function-type-casts ()
  (let ((a (derived-function-type-casts-make-fun))
        (b (derived-function-type-casts-make-fun)))
    (funcall a)
    (funcall b)))


(defun tryit (fname)
  (sb-int:encapsulate
   fname
   'foomfa
   (lambda (realfun &rest args)
     (apply realfun args)))
  (assert (sb-int:encapsulated-p fname 'foomfa))
  (assert (not (sb-int:encapsulated-p fname 'nope)))
  (sb-int:unencapsulate fname 'no-such-encapsulation)
  (assert (sb-int:encapsulated-p fname 'foomfa))
  (sb-int:unencapsulate fname 'foomfa)
  (assert (not (sb-int:encapsulated-p fname 'foomfa))))

(with-test (:name :encapsulated-p-simple-fun)
  (tryit 'ed))

(defgeneric zerk (arg))
(with-test (:name :encapsulated-p-gf)
  (tryit 'zerk))

(with-test (:name :encapsulated-p-closure)
  (setf (symbol-function 'fleem)
        (locally (declare (notinline constantly))
          (constantly 'gazonk)))
  (assert (sb-kernel:closurep (symbol-function 'fleem)))
  (tryit 'fleem))

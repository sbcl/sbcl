;;; These two tests are by themselves because they consume 2x the run time
;;; of all other tests in arith.pure combined.
(with-test (:name (logand :complicated-identity))
  (loop for k from -8 upto 8 do
    (loop for min from -16 upto 16 do
      (loop for max from min upto 16 do
        (let ((f (checked-compile `(lambda (x)
                                     (declare (type (integer ,min ,max) x))
                                     (logand x ,k)))))
          (loop for x from min upto max do
            (assert (eql (logand x k) (funcall f x)))))))))

(with-test (:name (logior :complicated-identity))
  (loop for k from -8 upto 8 do
    (loop for min from -16 upto 16 do
      (loop for max from min upto 16 do
        (let ((f (checked-compile `(lambda (x)
                                     (declare (type (integer ,min ,max) x))
                                     (logior x ,k)))))
          (loop for x from min upto max do
            (assert (eql (logior x k) (funcall f x)))))))))

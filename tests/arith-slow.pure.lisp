;;; These two tests are by themselves because they consume more run time
;;; than all other tests in arith.pure combined.

(defmacro test-guts ()
  #+sb-thread '(let ((t1 (sb-thread:make-thread #'doit :arguments '(-8 0)))
                     (t2 (sb-thread:make-thread #'doit :arguments '(1 8))))
                (sb-thread:join-thread t1)
                (sb-thread:join-thread t2))
  #-sb-thread '(doit -8 8))

(with-test (:name (logand :complicated-identity))
  (flet ((doit (k-lo k-hi)
  (loop for k from k-lo upto k-hi do
    (loop for min from -16 upto 16 do
      (loop for max from min upto 16 do
        (let ((f (checked-compile `(lambda (x)
                                     (declare (type (integer ,min ,max) x))
                                     (logand x ,k)))))
          (loop for x from min upto max do
            (assert (eql (logand x k) (funcall f x))))))))))
    (test-guts)))

(with-test (:name (logior :complicated-identity))
  (flet ((doit (k-lo k-hi)
  (loop for k from k-lo upto k-hi do
    (loop for min from -16 upto 16 do
      (loop for max from min upto 16 do
        (let ((f (checked-compile `(lambda (x)
                                     (declare (type (integer ,min ,max) x))
                                     (logior x ,k)))))
          (loop for x from min upto max do
            (assert (eql (logior x k) (funcall f x))))))))))
    (test-guts)))

;;; These two tests are by themselves because they consume more run time
;;; than all other tests in arith.pure combined.

(defmacro test-guts ()
  #+sb-thread '(let ((t1 (sb-thread:make-thread #'doit :arguments '(-8 0)))
                     (t2 (sb-thread:make-thread #'doit :arguments '(1 8))))
                (sb-thread:join-thread t1)
                (sb-thread:join-thread t2))
  #-sb-thread '(doit -8 8))

(with-test (:name (logand :complicated-identity)
                  :skipped-on :mips) ; too slow
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

(with-test (:name (logior :complicated-identity)
                  :skipped-on :mips) ; too slow
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

(defun type-derivation (op u-l u-h s-l s-h)
  (let ((interval (sb-c::numeric-type->interval
                   (sb-kernel:specifier-type (cadr (caddr (sb-kernel:%simple-fun-type
                                                           (compile nil `(lambda (u s)
                                                                           (,op (truly-the (integer ,u-l ,u-h) u)
                                                                                (truly-the (integer ,s-l ,s-h) s)))))))))))
    (values (loop for u from u-l to u-h
                  minimize (loop for s from s-l to s-h
                                 minimize (funcall op u s)))
            (loop for u from u-l to u-h
                  maximize (loop for s from s-l to s-h
                                 maximize (funcall op u s)))
            (sb-c::interval-low interval)
            (sb-c::interval-high interval))))

(with-test (:name :logical-type-derivation)
  (loop
    for low1 from -4 to 5
    do
    (loop
      for high1 from low1 to 5
      do
      (loop
        for low2 from -4 to 5
        do
        (loop
          for high2 from low2 to 5
          do
          (loop for op in '(logand logior logxor)
                do
                (multiple-value-bind (low high r-low r-high)
                    (type-derivation op low1 high1 low2 high2)
                  (assert (>= low r-low))
                  (assert (<= high r-high)))))))))

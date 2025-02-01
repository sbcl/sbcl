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
  (let ((type (sb-kernel:specifier-type (cadr (caddr (sb-kernel:%simple-fun-type
                                                      (compile nil `(lambda (u s)
                                                                      (,op (truly-the (integer ,u-l ,u-h) u)
                                                                           (truly-the (integer ,s-l ,s-h) s))))))))))
    (loop for u from u-l to u-h
          do (loop for s from s-l to s-h
                   for r = (funcall op u s)
                   do
                   (assert (sb-kernel:ctypep (funcall op u s) type)
                           () "(~a ~a ~a) =>  ~a /= ~a ~a"
                           op u s r type `(lambda (u s)
                                            (,op (truly-the (integer ,u-l ,u-h) u)
                                                 (truly-the (integer ,s-l ,s-h) s))))))))

(defun type-derivation1 (op l h)
  (let ((interval (sb-c::numeric-type->interval
                   (sb-kernel:specifier-type (cadr (caddr (sb-kernel:%simple-fun-type
                                                           (compile nil `(lambda (u)
                                                                           (,op (truly-the (integer ,l ,h) u)))))))))))
    (values (loop for u from l to h
                  minimize (funcall op u))
            (loop for u from l to h
                  maximize (funcall op u))
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
                (type-derivation op low1 high1 low2 high2)))))))

(with-test (:name :bit-type-derivation)
  (loop
    for low from -18 to 17
    do
    (loop
      for high from low to 17
      do (loop for op in '(logcount integer-length)
               do
               (multiple-value-bind (low high r-low r-high) (type-derivation1 op low high)
                 (assert (>= low r-low))
                 (assert (<= high r-high)))))))

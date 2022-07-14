(macrolet ((define-c-wrapper (lisp-name c-name)
             `(defun ,lisp-name (key array)
                (declare (sb-vm:word key))
                (sb-sys:with-pinned-objects (array)
                  (let ((result
                         (sb-alien:alien-funcall
                          (sb-alien:extern-alien ,c-name
                                                 (function int sb-alien:unsigned
                                                           system-area-pointer int))
                                        key
                                        (sb-sys:vector-sap array) (length array))))
                    (unless (eql result -1)
                      (values result (aref array result))))))))
  (define-c-wrapper c-bsearch<= "bsearch_lesseql_uword")
  (define-c-wrapper c-bsearch>= "bsearch_greatereql_uword"))

(defun test-c-bsearch-sorted-vector ()
  (dotimes (vector-len 51) ; test the edge case of an empty vector
    (let ((maxval 10000)
          (a (make-array vector-len :initial-element 0
                                    :element-type 'sb-vm:word)))
      (dotimes (i vector-len)
        (let ((val (1+ (random maxval))))
          (loop while (find val a) ; repeat until unique
                do (setq val (1+ (random maxval))))
          (setf (aref a i) val)))
      (setq a (sort a #'<))
      (map-into a (lambda (x) (* x 5)) a)
      ;; Keys less than the lowest in the array should not be found by <=
      (assert (not (c-bsearch<= 0 a)))
      (when (plusp vector-len)
        (assert (not (c-bsearch<= (1- (aref a 0)) a))))
      ;; Keys larger than the largest item should not be found by >=
      (assert (not (c-bsearch>= most-positive-fixnum a)))
      (assert (not (c-bsearch>= most-positive-word a)))
      (when (plusp vector-len)
        (let ((greatest (aref a (1- vector-len))))
          (loop for i from 1 to 20
                do (assert (not (c-bsearch>= (+ greatest i) a))))))
      (dotimes (i vector-len)
        (let ((k (aref a i)))
          (assert (= (c-bsearch<= k a) i)) ; exact match
          (assert (= (c-bsearch>= k a) i)) ; exact match
          ;; Any key between K up to but excluding A[i+1] should find I by <=
          (assert (= (c-bsearch<= (1+ k) a) i))
          (let ((higher (if (< (1+ i) vector-len)
                            (1- (aref a (1+ i)))
                            (+ k 10000))))
            (assert (= (c-bsearch<= higher a) i)))
          ;; Any key down to A[i-1] should find I by >=
          (assert (= (c-bsearch>= (1- k) a) i))
          (let ((lower (if (>= (1- i) 0)
                           (1+ (aref a (1- i)))
                           0)))
            (assert (= (c-bsearch>= lower a) i))))))))

(compile 'test-c-bsearch-sorted-vector)

(with-test (:name :c-bsearch-sorted-vector)
  (test-c-bsearch-sorted-vector))

(defun test-c-bsearch-sorted-vector ()
  (flet ((bsearch<= (item array)
           (sb-sys:with-pinned-objects (array)
             (let ((answer
                    (alien-funcall
                     (extern-alien "bsearch_lesseql"
                                   (function int unsigned system-area-pointer int))
                     item (sb-sys:vector-sap array) (length array))))
               (unless (eql answer -1) answer)))))
    (loop for vector-len from 1 to 40
          do
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
         ;; Keys less than the lowest in the array should not be found
         (assert (not (bsearch<= 0 a)))
         (assert (not (bsearch<= (1- (aref a 0)) a)))
         (dotimes (i vector-len)
           (let ((k (aref a i)))
             (assert (= (bsearch<= k a) i)) ; exact match
             ;; Any key between K up to but excluding K[i+1] should find I
             (assert (= (bsearch<= (1+ k) a) i))
             (let ((higher (if (< (1+ i) vector-len)
                               (1- (aref a (1+ i)))
                               (+ k 10000))))
               (assert (= (bsearch<= higher a) i)))))))))
(compile 'test-c-bsearch-sorted-vector)

(with-test (:name :c-bsearch-sorted-vector)
  (test-c-bsearch-sorted-vector))

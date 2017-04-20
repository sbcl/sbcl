
;; The debug version of my C code asserts that the guard elements
;; in the vector are not touched.
(defparameter n-guard-words 4)

(defun effective-length (vector) (- (length vector) (* 2 n-guard-words)))

(defun make-test-vector (n-items)
  (make-array (+ n-items (* 2 n-guard-words))
              :element-type `(unsigned-byte ,sb-vm:n-word-bits)
              :initial-element 0))

(defun my-sort-vector (vector)
  (declare (type (simple-array (unsigned-byte #.sb-vm:n-word-bits) (*))
                 vector))
  (sb-sys:with-pinned-objects (vector)
    (alien-funcall (extern-alien "gc_heapsort_uwords"
                                 (function void system-area-pointer int))
      (sb-sys:sap+ (sb-sys:vector-sap vector)
                   (* n-guard-words sb-vm:n-word-bytes))
      (effective-length vector)))
  vector)

(defun assert-sorted (vector)
  (declare (type (simple-array (unsigned-byte #.sb-vm:n-word-bits) (*))
                 vector))
  (let ((n-items (effective-length vector)))
    (when (> n-items 1)
      (let ((predecessor (aref vector n-guard-words)))
        (loop for i from 1 below n-items
              for thing = (aref vector (+ n-guard-words i))
              do (assert (>= thing predecessor))
                 (setq predecessor thing))))))

(defun randomly-pound-on-heapsort (&key (n-iter 100)
                                        (min-size 0)
                                        (max-size 10000))
  (dotimes (i n-iter)
    (let* ((range (1+ (- max-size min-size)))
           (n-items (+ min-size (random range)))
           (vector (make-test-vector n-items)))
      (dotimes (i n-items)
        (setf (aref vector (+ i n-guard-words))
              (1+ (random most-positive-word)))) ; never 0
      (assert-sorted (my-sort-vector vector)))))

(defun perms (vector)
  (declare (vector vector))
  (if (= (length vector) 1)
      (return-from perms (list (list (elt vector 0)))))
  (loop for i from 0 below (length vector)
        for item = (elt vector i)
        for others = (perms (concatenate 'vector
                                         (subseq vector 0 i)
                                         (subseq vector (1+ i))))
        nconc (mapcar (lambda (other) (cons item other))
                      others)))

(defun fact (n)
  (if (= n 0)
      1
      (* n (fact (1- n)))))

(defun try-permutations ()
  (loop for n from 1 to 8
        for items = (subseq #(10 20 30 40 50 60 70 80) 0 n)
        for perms = (perms items)
        for vector = (make-test-vector n)
        do (assert (= (length perms) (fact n)))
           (dolist (perm (perms items))
             (replace vector perm :start1 n-guard-words)
             (assert-sorted (my-sort-vector vector)))))

(with-test (:name :c-heapsort-smoke-test)
  (try-permutations))
(with-test (:name :c-heapsort-random-test)
  (randomly-pound-on-heapsort))

(import sb-c::'(make-sset sset-adjoin sset-delete copy-sset
                sset-union sset-intersection sset-difference
                sset-member sset-empty
                sset-vector sset-count))

(defstruct (dummy-test-element (:include sb-c::sset-element)
                               (:constructor make-dummy-test-element (number))))

(with-test (:name :new-sset-basic-operations)
  (let ((set (make-sset))
        (e1 (make-dummy-test-element 10))
        (e2 (make-dummy-test-element 20))
        (e3 (make-dummy-test-element 30))
        (e4 (make-dummy-test-element 40)))
    (assert (sset-empty set))
    (assert (not (sset-member e1 set)))

    ;; Adjoin
    (assert (sset-adjoin e1 set))
    (assert (not (sset-adjoin e1 set))) ; already present
    (assert (sset-adjoin e2 set))
    (assert (sset-adjoin e3 set))
    (assert (= (sset-count set) 3))
    (assert (sset-member e1 set))
    (assert (sset-member e2 set))
    (assert (sset-member e3 set))
    (assert (not (sset-member e4 set)))

    ;; Check no tombstones (-1) exist in vector
    (loop for x across (sset-vector set)
          do (assert (not (eql x -1))))

    ;; Delete
    (assert (sset-delete e2 set))
    (assert (not (sset-delete e2 set))) ; already deleted
    (assert (= (sset-count set) 2))
    (assert (sset-member e1 set))
    (assert (not (sset-member e2 set)))
    (assert (sset-member e3 set))

    ;; Check no tombstones (-1) exist in vector after delete
    (loop for x across (sset-vector set)
          do (assert (not (eql x -1))))

    (assert (sset-delete e1 set))
    (assert (sset-delete e3 set))
    (assert (sset-empty set))
    (loop for x across (sset-vector set)
          do (assert (not (eql x -1))))))

(with-test (:name :new-sset-shift-deletion-stress)
  (let ((set (make-sset))
        (elems (loop for i from 1 to 100 collect (make-dummy-test-element i))))
    (dolist (e elems)
      (sset-adjoin e set))
    (assert (= (sset-count set) 100))
    ;; Delete every second element
    (loop for e in elems by #'cddr
          do (assert (sset-delete e set)))
    (assert (= (sset-count set) 50))
    (loop for x across (sset-vector set)
          do (assert (not (eql x -1))))
    (loop for e in elems
          for i from 0
          do (if (evenp i)
                 (assert (not (sset-member e set)))
                 (assert (sset-member e set))))
    (loop for e in (cdr elems) by #'cddr
          do (assert (sset-delete e set)))
    (assert (sset-empty set))
    (loop for x across (sset-vector set)
          do (assert (not (eql x -1))))))

(with-test (:name :new-sset-set-operations)
  (let ((s1 (make-sset))
        (s2 (make-sset))
        (e1 (make-dummy-test-element 1))
        (e2 (make-dummy-test-element 2))
        (e3 (make-dummy-test-element 3))
        (e4 (make-dummy-test-element 4)))
    (sset-adjoin e1 s1)
    (sset-adjoin e2 s1)
    (sset-adjoin e3 s1)

    (sset-adjoin e2 s2)
    (sset-adjoin e3 s2)
    (sset-adjoin e4 s2)

    ;; Union
    (let ((u (copy-sset s1)))
      (sset-union u s2)
      (assert (= (sset-count u) 4))
      (assert (sset-member e4 u))
      (loop for x across (sset-vector u) do (assert (not (eql x -1)))))

    ;; Intersection
    (let ((i (copy-sset s1)))
      (sset-intersection i s2)
      (assert (= (sset-count i) 2))
      (assert (not (sset-member e1 i)))
      (assert (sset-member e2 i))
      (assert (sset-member e3 i))
      (loop for x across (sset-vector i) do (assert (not (eql x -1)))))

    ;; Difference
    (let ((d (copy-sset s1)))
      (sset-difference d s2)
      (assert (= (sset-count d) 1))
      (assert (sset-member e1 d))
      (assert (not (sset-member e2 d)))
      (assert (not (sset-member e3 d)))
      (loop for x across (sset-vector d) do (assert (not (eql x -1)))))))

(with-test (:name :sset-growth-optimization)
  (let ((set (make-sset))
        (e1 (make-dummy-test-element 1))
        (e2 (make-dummy-test-element 2)))
    ;; Initially empty, limit is 0
    (assert (zerop (sb-c::sset-limit set)))
    (assert (= (length (sset-vector set)) 0))

    ;; Adjoin e1. Should grow to size 2, and count reaches limit.
    (assert (sset-adjoin e1 set))
    (assert (= (sb-c::sset-count set) (sb-c::sset-limit set)))
    (assert (= (length (sset-vector set)) 2))

    ;; Adjoin e1 again. Should NOT grow.
    (assert (not (sset-adjoin e1 set)))
    (assert (= (sb-c::sset-count set) (sb-c::sset-limit set)))
    (assert (= (length (sset-vector set)) 2))

    ;; Adjoin e2. Should grow to size 4.
    (assert (sset-adjoin e2 set))
    (assert (= (length (sset-vector set)) 4))))

(with-test (:name :sset-difference-comprehensive)
  (let ((elems (loop for i from 1 to 20 collect (make-dummy-test-element i))))
    ;; 1. Small set2, large set1 (Branch 1: set2 <= 2*set1)
    (let ((s1 (make-sset))
          (s2 (make-sset)))
      (dolist (e (subseq elems 0 10)) (sset-adjoin e s1))
      (dolist (e (subseq elems 3 6))  (sset-adjoin e s2)) ; s2 has 3 elements
      (assert (sset-difference s1 s2)) ; modified = t
      (assert (= (sset-count s1) 7))
      (assert (not (sset-member (elt elems 3) s1)))
      (assert (not (sset-member (elt elems 4) s1)))
      (assert (not (sset-member (elt elems 5) s1)))
      (assert (sset-member (elt elems 0) s1))
      ;; No modification when subtracting disjoint set
      (let ((s3 (make-sset)))
        (sset-adjoin (elt elems 3) s3)
        (assert (not (sset-difference s1 s3))) ; modified = nil
        (assert (= (sset-count s1) 7))))

    ;; 2. Large set2, small set1 (Branch 2: set2 > 2*set1)
    (let ((s1 (make-sset))
          (s2 (make-sset)))
      (dolist (e (subseq elems 0 3))  (sset-adjoin e s1)) ; s1 has 3 elements
      (dolist (e (subseq elems 2 15)) (sset-adjoin e s2)) ; s2 has 13 elements
      (assert (sset-difference s1 s2)) ; modified = t
      (assert (= (sset-count s1) 2)) ; element 2 removed
      (assert (not (sset-member (elt elems 2) s1)))
      (assert (sset-member (elt elems 0) s1))
      (assert (sset-member (elt elems 1) s1)))

    ;; 3. (sset-difference s s) - self difference triggers aver error
    (let ((s (make-sset)))
      (dolist (e (subseq elems 0 5)) (sset-adjoin e s))
      (assert-error (sset-difference s s)))

    ;; 4. Empty set operations
    (let ((s1 (make-sset))
          (s2 (make-sset)))
      (sset-adjoin (elt elems 0) s1)
      ;; Subtract empty from non-empty
      (assert (not (sset-difference s1 s2))) ; modified = nil
      (assert (= (sset-count s1) 1))
      ;; Subtract non-empty from empty
      (assert (not (sset-difference s2 s1))) ; modified = nil
      (assert (sset-empty s2)))))

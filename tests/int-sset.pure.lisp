#-adaptive-conset (invoke-restart 'run-tests::skip-file)

(import sb-integer-sparse-set::'(make-int-sset int-sset-adjoin int-sset-delete copy-int-sset
                                 int-sset-union int-sset-intersection int-sset-difference
                                 int-sset-member int-sset-empty
                                 int-sset-vector int-sset-count int-sset-limit
                                 do-int-sset-elements int-sset-vector-smallp))

(with-test (:name :new-int-sset-basic-operations)
  (let ((set (make-int-sset))
        (e1 10)
        (e2 20)
        (e3 30)
        (e4 40))
    (assert (int-sset-empty set))
    (assert (not (int-sset-member e1 set)))

    ;; Adjoin
    (assert (int-sset-adjoin e1 set))
    (assert (not (int-sset-adjoin e1 set))) ; already present
    (assert (int-sset-adjoin e2 set))
    (assert (int-sset-adjoin e3 set))
    (assert (= (int-sset-count set) 3))
    (assert (int-sset-member e1 set))
    (assert (int-sset-member e2 set))
    (assert (int-sset-member e3 set))
    (assert (not (int-sset-member e4 set)))

    ;; Delete
    (assert (int-sset-delete e2 set))
    (assert (not (int-sset-delete e2 set))) ; already deleted
    (assert (= (int-sset-count set) 2))
    (assert (int-sset-member e1 set))
    (assert (not (int-sset-member e2 set)))
    (assert (int-sset-member e3 set))

    (assert (int-sset-delete e1 set))
    (assert (int-sset-delete e3 set))
    (assert (int-sset-empty set))))

(with-test (:name :new-int-sset-shift-deletion-stress)
  (let ((set (make-int-sset))
        (elems (loop for i from 1 to 100 collect i)))
    (dolist (e elems)
      (int-sset-adjoin e set))
    (assert (= (int-sset-count set) 100))
    ;; Delete every second element
    (loop for e in elems by #'cddr
          do (assert (int-sset-delete e set)))
    (assert (= (int-sset-count set) 50))
    (loop for e in elems
          for i from 0
          do (if (evenp i)
                 (assert (not (int-sset-member e set)))
                 (assert (int-sset-member e set))))
    (loop for e in (cdr elems) by #'cddr
          do (assert (int-sset-delete e set)))
    (assert (int-sset-empty set))))

(with-test (:name :new-int-sset-set-operations)
  (let ((s1 (make-int-sset))
        (s2 (make-int-sset))
        (e1 1)
        (e2 2)
        (e3 3)
        (e4 4))
    (int-sset-adjoin e1 s1)
    (int-sset-adjoin e2 s1)
    (int-sset-adjoin e3 s1)

    (int-sset-adjoin e2 s2)
    (int-sset-adjoin e3 s2)
    (int-sset-adjoin e4 s2)

    ;; Union
    (let ((u (copy-int-sset s1)))
      (int-sset-union u s2)
      (assert (= (int-sset-count u) 4))
      (assert (int-sset-member e4 u)))

    ;; Intersection
    (let ((i (copy-int-sset s1)))
      (int-sset-intersection i s2)
      (assert (= (int-sset-count i) 2))
      (assert (not (int-sset-member e1 i)))
      (assert (int-sset-member e2 i))
      (assert (int-sset-member e3 i)))

    ;; Difference
    (let ((d (copy-int-sset s1)))
      (int-sset-difference d s2)
      (assert (= (int-sset-count d) 1))
      (assert (int-sset-member e1 d))
      (assert (not (int-sset-member e2 d)))
      (assert (not (int-sset-member e3 d))))))

(with-test (:name :int-sset-growth-optimization)
  (let ((set (make-int-sset))
        (e1 1)
        (e2 2))
    ;; Initially empty, limit is 0
    (assert (zerop (int-sset-limit set)))
    (assert (= (length (int-sset-vector set)) 0))

    ;; Adjoin e1. Should grow to size 2, and count reaches limit.
    (assert (int-sset-adjoin e1 set))
    (assert (= (int-sset-count set) (int-sset-limit set)))
    (assert (= (length (int-sset-vector set)) 2))

    ;; Adjoin e1 again. Should NOT grow.
    (assert (not (int-sset-adjoin e1 set)))
    (assert (= (int-sset-count set) (int-sset-limit set)))
    (assert (= (length (int-sset-vector set)) 2))

    ;; Adjoin e2. Should grow to size 4.
    (assert (int-sset-adjoin e2 set))
    (assert (= (length (int-sset-vector set)) 4))))

(with-test (:name :int-sset-difference-comprehensive)
  (let ((elems (loop for i from 1 to 20 collect i)))
    ;; 1. Small set2, large set1 (Branch 1: set2 <= 2*set1)
    (let ((s1 (make-int-sset))
          (s2 (make-int-sset)))
      (dolist (e (subseq elems 0 10)) (int-sset-adjoin e s1))
      (dolist (e (subseq elems 3 6))  (int-sset-adjoin e s2)) ; s2 has 3 elements
      (assert (int-sset-difference s1 s2)) ; modified = t
      (assert (= (int-sset-count s1) 7))
      (assert (not (int-sset-member (elt elems 3) s1)))
      (assert (not (int-sset-member (elt elems 4) s1)))
      (assert (not (int-sset-member (elt elems 5) s1)))
      (assert (int-sset-member (elt elems 0) s1))
      ;; No modification when subtracting disjoint set
      (let ((s3 (make-int-sset)))
        (int-sset-adjoin (elt elems 3) s3)
        (assert (not (int-sset-difference s1 s3))) ; modified = nil
        (assert (= (int-sset-count s1) 7))))

    ;; 2. Large set2, small set1 (Branch 2: set2 > 2*set1)
    (let ((s1 (make-int-sset))
          (s2 (make-int-sset)))
      (dolist (e (subseq elems 0 3))  (int-sset-adjoin e s1)) ; s1 has 3 elements
      (dolist (e (subseq elems 2 15)) (int-sset-adjoin e s2)) ; s2 has 13 elements
      (assert (int-sset-difference s1 s2)) ; modified = t
      (assert (= (int-sset-count s1) 2)) ; element 2 removed
      (assert (not (int-sset-member (elt elems 2) s1)))
      (assert (int-sset-member (elt elems 0) s1))
      (assert (int-sset-member (elt elems 1) s1)))

    ;; 3. (int-sset-difference s s) - self difference triggers aver error
    (let ((s (make-int-sset)))
      (dolist (e (subseq elems 0 5)) (int-sset-adjoin e s))
      (assert-error (int-sset-difference s s)))

    ;; 4. Empty set operations
    (let ((s1 (make-int-sset))
          (s2 (make-int-sset)))
      (int-sset-adjoin (elt elems 0) s1)
      ;; Subtract empty from non-empty
      (assert (not (int-sset-difference s1 s2))) ; modified = nil
      (assert (= (int-sset-count s1) 1))
      ;; Subtract non-empty from empty
      (assert (not (int-sset-difference s2 s1))) ; modified = nil
      (assert (int-sset-empty s2)))))

(with-test (:name :int-sset-intersection-comprehensive)
  (let ((elems (loop for i from 1 to 20 collect i)))
    ;; 1. Small set2, large set1 (Branch 1: set2 <= set1/2)
    (let ((s1 (make-int-sset))
          (s2 (make-int-sset)))
      (dolist (e (subseq elems 0 10)) (int-sset-adjoin e s1)) ; s1 has 10 elements
      (dolist (e (subseq elems 3 6))  (int-sset-adjoin e s2)) ; s2 has 3 elements
      (assert (int-sset-intersection s1 s2)) ; modified = t
      (assert (= (int-sset-count s1) 3))
      (assert (int-sset-member (elt elems 3) s1))
      (assert (int-sset-member (elt elems 4) s1))
      (assert (int-sset-member (elt elems 5) s1))
      (assert (not (int-sset-member (elt elems 0) s1)))
      (assert (not (int-sset-member (elt elems 9) s1)))

      ;; Intersecting with small disjoint set empties s1
      (let ((s3 (make-int-sset)))
        (dolist (e (subseq elems 15 17)) (int-sset-adjoin e s3))
        (assert (int-sset-intersection s1 s3)) ; modified = t
        (assert (int-sset-empty s1))))

    ;; 2. Large set2, small set1 (Branch 2: set2 > set1/2)
    (let ((s1 (make-int-sset))
          (s2 (make-int-sset)))
      (dolist (e (subseq elems 0 4))  (int-sset-adjoin e s1)) ; s1 has 4 elements
      (dolist (e (subseq elems 2 12)) (int-sset-adjoin e s2)) ; s2 has 10 elements
      (assert (int-sset-intersection s1 s2)) ; modified = t
      (assert (= (int-sset-count s1) 2))
      (assert (int-sset-member (elt elems 2) s1))
      (assert (int-sset-member (elt elems 3) s1))
      (assert (not (int-sset-member (elt elems 0) s1)))

      ;; Intersecting when set2 contains all elements of set1 (unmodified = nil)
      (let ((s4 (make-int-sset)))
        (dolist (e (subseq elems 0 10)) (int-sset-adjoin e s4)) ; contains all of s1
        (assert (not (int-sset-intersection s1 s4))) ; modified = nil
        (assert (= (int-sset-count s1) 2))))

    ;; 3. (eq set1 set2) returns nil (unmodified)
    (let ((s1 (make-int-sset)))
      (dolist (e (subseq elems 0 5)) (int-sset-adjoin e s1))
      (assert (not (int-sset-intersection s1 s1)))
      (assert (= (int-sset-count s1) 5)))

    ;; 4. Empty set operations
    (let ((s1 (make-int-sset))
          (s2 (make-int-sset)))
      (int-sset-adjoin (elt elems 0) s1)
      ;; Intersect non-empty with empty (s2 <= s1/2 => returns t, s1 becomes empty)
      (assert (int-sset-intersection s1 s2))
      (assert (int-sset-empty s1))
      ;; Intersect empty with non-empty (returns nil)
      (int-sset-adjoin (elt elems 0) s2)
      (assert (not (int-sset-intersection s1 s2)))
      (assert (int-sset-empty s1)))))

(with-test (:name :int-sset-zero-element)
  (let ((set (make-int-sset)))
    (assert (int-sset-empty set))
    (assert (not (int-sset-member 0 set)))
    ;; Adjoin 0
    (assert (int-sset-adjoin 0 set))
    (assert (find 1 (int-sset-vector set)))
    (assert (not (int-sset-adjoin 0 set))) ; already present
    (assert (= (int-sset-count set) 1))
    (assert (int-sset-member 0 set))
    (assert (not (int-sset-empty set)))

    ;; do-int-sset-elements yields 0 (biased down by 1)
    (let (collected)
      (do-int-sset-elements (e set)
        (push e collected))
      (assert (equal collected '(0))))

    ;; Adjoin other elements alongside 0
    (assert (int-sset-adjoin 10 set))
    (assert (int-sset-adjoin 20 set))
    (assert (= (int-sset-count set) 3))
    (assert (int-sset-member 0 set))
    (assert (int-sset-member 10 set))
    (assert (int-sset-member 20 set))

    ;; Set operations with 0
    (let ((s2 (make-int-sset)))
      (int-sset-adjoin 0 s2)
      (int-sset-adjoin 30 s2)
      (let ((u (copy-int-sset set)))
        (int-sset-union u s2)
        (assert (int-sset-member 0 u))
        (assert (= (int-sset-count u) 4)))
      (let ((i (copy-int-sset set)))
        (int-sset-intersection i s2)
        (assert (int-sset-member 0 i))
        (assert (= (int-sset-count i) 1)))
      (let ((d (copy-int-sset set)))
        (int-sset-difference d s2)
        (assert (not (int-sset-member 0 d)))
        (assert (= (int-sset-count d) 2))))

    ;; Delete 0
    (assert (int-sset-delete 0 set))
    (assert (not (int-sset-member 0 set)))
    (assert (= (int-sset-count set) 2))))

(with-test (:name :int-sset-array-type-upgrade)
  (let ((set (make-int-sset)))
    ;; 1. Small elements and #xFFFE (whose stored bit pattern is #xFFFF) fit in (unsigned-byte 16)
    (int-sset-adjoin 0 set)
    (int-sset-adjoin 42 set)
    (int-sset-adjoin #xFFFE set)
    (assert (int-sset-vector-smallp (int-sset-vector set)))
    (assert (typep (int-sset-vector set) '(simple-array (unsigned-byte 16) (*))))
    (assert (int-sset-member 0 set))
    (assert (int-sset-member 42 set))
    (assert (int-sset-member #xFFFE set))

    ;; 2. Inserting #xFFFF (logically 65535, stored as #x10000) exceeds (unsigned-byte 16) storage
    ;; and transparently upgrades the backing array to (unsigned-byte 32).
    (assert (int-sset-adjoin #xFFFF set))
    (assert (not (int-sset-vector-smallp (int-sset-vector set))))
    (assert (typep (int-sset-vector set) '(simple-array (unsigned-byte 32) (*))))
    (assert (int-sset-member 0 set))
    (assert (int-sset-member 42 set))
    (assert (int-sset-member #xFFFE set))
    (assert (int-sset-member #xFFFF set))

    ;; 3. Further insertions of large numbers into upgraded set
    (assert (int-sset-adjoin #x12345678 set))
    (assert (int-sset-adjoin #xFFFFFFFE set))
    (assert (int-sset-member #x12345678 set))
    (assert (int-sset-member #xFFFFFFFE set))
    (assert (= (int-sset-count set) 6))

    ;; 4. Directly inserting large numbers into a fresh empty set
    (let ((set2 (make-int-sset)))
      (assert (int-sset-adjoin #xFFFF set2))
      (assert (not (int-sset-vector-smallp (int-sset-vector set2))))
      (assert (typep (int-sset-vector set2) '(simple-array (unsigned-byte 32) (*))))
      (assert (int-sset-member #xFFFF set2)))

    ;; 5. Union of (unsigned-byte 16) set with (unsigned-byte 32) set upgrades the target set
    (let ((small-set (make-int-sset))
          (large-set (make-int-sset)))
      (int-sset-adjoin 1 small-set)
      (assert (int-sset-vector-smallp (int-sset-vector small-set)))
      (int-sset-adjoin #xFFFF large-set)
      (int-sset-union small-set large-set)
      (assert (not (int-sset-vector-smallp (int-sset-vector small-set))))
      (assert (int-sset-member 1 small-set))
      (assert (int-sset-member #xFFFF small-set)))))

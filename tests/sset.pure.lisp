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

(in-package "SB-IMPL")

(defun tree-count (tree)
  (if tree
      (+ 1 (tree-count (node-left tree)) (tree-count (node-right tree)))
      0))

(defun verify-tree-invariants (root)
  (when root
    (named-let recurse ((tree root)
                        (min most-negative-fixnum)
                        (max most-positive-fixnum))
      (let ((key (node-key tree)))
        (unless (<= min key max)
          (error "~S is not a binary search tree" root))
        (awhen (node-left tree)
          (recurse it min (1- key)))
        (awhen (node-right tree)
          (recurse it (1+ key) max))))))

(defun verify-heap-invariants (root)
  (when root
    (named-let recurse ((tree root))
      (let ((p (node-priority tree))
            (left-child (node-left tree))
            (right-child (node-right tree)))
        ;; Parent priority has to be not less than childrens' priorities.
        (when (or (and left-child (< p (node-priority left-child)))
                  (and right-child (< p (node-priority right-child))))
          (error "Heap invariant violated at node ~S" tree))
        (when left-child (recurse left-child))
        (when right-child (recurse right-child))))))

(defun generate-events (max-items mode)
  (let ((elts (make-array max-items :fill-pointer 0 :initial-element nil))
        (i 0)
        (ops))
    (loop
     (if (= (length elts) max-items) (return (nreverse ops)))
     (let ((val (ecase mode
                 (:sequential (incf i 10))
                 (:random
                  (let ((v (random 100000)))
                    (loop (if (find v elts)
                              (setq v (random 100000))
                              (return v))))))))
       (vector-push-extend val elts)
       (push `(:insert . ,val) ops)
       (when (< (random 10) 3)
         (let* ((elt-to-remove (random (length elts)))
                (val (aref elts elt-to-remove)))
           (replace elts elts
                    :start1 elt-to-remove
                    :start2 (1+ elt-to-remove))
           (decf (fill-pointer elts))
           (push `(:delete . ,val) ops)))))))

(defun treap-test (n-trials max-items mode)
  (dotimes (i n-trials)
    (let ((ops (generate-events max-items mode))
          (hash (make-hash-table))
          (tree))
      (dolist (op ops)
        (let ((val (cdr op)))
          (ecase (car op)
           (:insert
            (setq tree (sb-impl::treap-insert tree val (- val)))
            (setf (gethash val hash) t))
           (:delete
            (setq tree (sb-impl::treap-delete val tree))
            (remhash val hash))))
        (assert (= (tree-count tree) (hash-table-count hash)))
        (verify-tree-invariants tree)
        (verify-heap-invariants tree)
        (loop for k being each hash-key of hash
              do (assert (eql (bst-node-data (sb-int:bst-find k tree))
                              (- k))))))))

(test-util:with-test (:name :negative-test-verify-invariant)
  (flet ((node (key &key left right)
           (let ((n (make-node key key)))
             (setf (node-left n) left
                   (node-right n) right)
             n)))

    ;; This tree fails to be a binary search tree:
    ;;
    ;;        19
    ;;      /
    ;;    5
    ;;   /  \
    ;; 3     80

    (let ((tree (node 19 :left (node 5 :left (node 3) :right (node 80)))))
      (handler-case (verify-tree-invariants tree)
        (error (e) (progn e) nil)
        (:no-error () (error "Expected an error"))))))

(test-util:with-test (:name :treap-insert/delete-random)
  (treap-test 1 400 :random))
(test-util:with-test (:name :treap-insert/delete-sequential)
  (treap-test 1 400 :sequential))

(defun tree-height (tree)
  (if tree
      (1+ (max (tree-height (node-left tree))
               (tree-height (node-right tree))))
    0))
(defun balancing-test (&optional print)
  (loop for i from 1 to 10 do
    (let (tree)
      (dotimes (j (1- (ash 1 i)))
        (setq tree (treap-insert tree j (- j)))
        (verify-tree-invariants tree)
        (verify-heap-invariants tree))
      (let ((ratio (/ (tree-height tree) i)))
        (when print
          (format t "count=~5d expected-height=~2d actual=~2d ratio=~f~%"
                  (tree-count tree)
                  i (tree-height tree) ratio))
        (assert (< ratio 2.8))))))

(test-util:with-test (:name :treap-balance)
  (balancing-test))

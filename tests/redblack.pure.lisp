;;;; Tests

(add-package-local-nickname "REDBLACK" "SB-RBTREE")
(use-package 'sb-int)
(import 'redblack::(red black redp blackp
                    color-of left right
                    node-key node-value data
                    find= find<=))
(load "bbtree-test-util.lisp")

(defun verify-invariants (root print)
  (unless root
    (return-from verify-invariants))
  ;; Binary search tree invariant
  (named-let recurse ((node root)
                             (min most-negative-fixnum)
                             (max most-positive-fixnum))
    (assert (< min (node-key node) max))
    ;; every descendant to the left has a key strictly less than this key
    (awhen (left node) (recurse it min (node-key node)))
    ;; every descendant to the right has a key strictly more than this key
    (awhen (right node) (recurse it (node-key node) max)))
  ;; Red/black tree invariants
  (assert (eq (color-of root) 'black))
  (let ((h (make-hash-table :test #'eq))
        (count-red 0)
        (count-black 0)
        (count-nil 0))
    (setf (gethash nil h) 0)
    (labels ((black-height (node)
               (or (gethash node h)
                   (setf (gethash node h)
                         (let ((l (black-height (left node)))
                               (r (black-height (right node))))
                           (assert (= l r))
                           (if (eq (color-of node) 'black) (1+ l) l))))))
      ;; Child colors and black height
      (named-let recurse ((node root))
        (ecase (color-of node)
          (red (incf count-red))
          (black (incf count-black)))
        ;; If a node is red then both its children are black
        (when (redp node)
          (awhen (left node)  (assert (blackp it)))
          (awhen (right node) (assert (blackp it))))
        (acond ((left node) (recurse it)) (t (incf count-nil)))
        (acond ((right node) (recurse it)) (t (incf count-nil))))
      (when print
        (format t "red=~3d black=~3d nil=~3d black-height = ~3d~%"
                count-red count-black count-nil
                (black-height root))))))
(compile 'verify-invariants)

(defun random-integer-seq (n &aux seq)
  (let ((a (make-array n :fill-pointer n)))
    (dotimes (i n) (setf (aref a i) i))
    (dotimes (i n seq)
      (let* ((random-index (random (length a)))
             (item (aref a random-index)))
        (push item seq)
        (setf (aref a random-index) (aref a (1- (length a))))
        (decf (fill-pointer a))))))

(defun buildtree (n &optional verify &aux tree inserted-so-far)
  (dolist (item (random-integer-seq n))
    (setq tree (redblack:insert tree item (- item)))
    (push item inserted-so-far)
    (when verify
      (verify-invariants tree t)
      (dolist (k inserted-so-far)
        (assert (eql (node-value (find= k tree)) (- k))))))
  tree)

(defun inorder-traverse (tree)
  (collect ((items))
    (named-let recurse ((node tree))
      (awhen (left node) (recurse it))
      (items (data node))
      (awhen (right node) (recurse it)))
    (items)))

(with-test (:name :redblack-basic-insertion)
  (let* ((n 1000)
         (tree (buildtree n)))
    (let ((list (inorder-traverse tree)))
      (dotimes (i n)
        (let ((item (pop list)))
          (assert (eql (car item) i))
          (assert (eql (cdr item) (- i))))))
    (let ((original-tree tree) strings)
      ;; Change some nodes
      (dotimes (i 50)
        (let ((v (random n)))
          (push v strings)
          (setq tree (redblack:insert tree v (format nil "str~d" v)))))
      ;; Find everything again
      (dotimes (i n)
        (let ((v (node-value (find= i tree))))
          (if (member i strings)
              (assert (string= v (format nil "str~d" i)))
              (assert (eql v (- i))))))
      ;; Nodes are immutable, so the original tree is unchanged
      (dotimes (i n)
        (assert (= (node-value (find= i original-tree)) (- i)))))))

(defun tree-count (tree)
  (labels ((recurse (tree)
             (+ 1
                (acond ((left tree) (tree-count it)) (t 0))
                (acond ((right tree) (tree-count it)) (t 0)))))
    (if tree (recurse tree) 0)))

(defun test-deletion (tree n-trials &optional print)
  (let* ((keys (mapcar 'car (inorder-traverse tree)))
         (n (length keys)))
    (dotimes (i n-trials)
      (let ((deletion-sequence (shuffle keys))
            (tree tree)
            (n n))
        (case print
          (:verbose
           (format t "deletion sequence:~%~s~%" deletion-sequence))
          ((t)
           (format t "~aTrial ~d" #\return (1+ i))
           (force-output)))
        (loop
          (when (null deletion-sequence) (return))
          (let ((item (pop deletion-sequence)))
            (setq tree (redblack:delete item tree))
            (decf n)
            (assert (= (tree-count tree) n))
            (verify-invariants tree nil)
            (dolist (item (cdr deletion-sequence))
              ;; ensure that everything not deleted is found
              (assert (= (node-value (find= item tree))
                         (- item))))))))))


(with-test (:name :redblack-basic-deletion)
  (let* ((n 500)
         (tree (buildtree n)))
    (test-deletion tree 5)))

(defun test-randomly (&optional (n 10) print &aux (trial 0))
  (loop repeat n do
    (when print
      (format t "trial ~d~%" (incf trial)))
    (let ((tree)
          (list)
          (size 0))
      (loop
        (if (or (null tree) (< (random 100) 65))
            ;; insert
            (let ((r (random (ash 1 16))))
              (unless (find= r tree)
                (setq tree (redblack:insert tree r (- r)))
                (verify-invariants tree nil)
                (push r list)
                (when (= (incf size) 2000) (return))
                ;;(format t " +~d" r)
                (when print (format t "+") (force-output))
                ))
            (let ((key-to-remove (nth (random (length list)) list)))
              (aver key-to-remove)
              (setq tree (redblack:delete key-to-remove tree))
              (setq list (cl:delete key-to-remove list))
              ;;(format t " -~d" key-to-remove)
              (when print (format t "-") (force-output))
              (verify-invariants tree nil)
              (dolist (item list)
                (assert (= (node-value (find= item tree)) (- item))))
              (decf size))))
      ;(print *cases*)
      ;(terpri)
      (loop while tree
            do (setq tree (redblack:delete (node-key tree) tree))
               (verify-invariants tree nil)))))

(with-test (:name :redblack-random-test)
  (test-randomly 5))

(with-test (:name :redblack-find-less-eql)
  (let ((tree nil))
    ;; insert keys 0, 10, 20, ..., 1000
    (dotimes (i 101)
      (setq tree (redblack:insert tree (* i 10) i)))
    (loop for key from -10 to 1100
          do
       (let ((found (find<= key tree)))
         (cond ((not found)
                (assert (< key 0)))
               ((>= key 1000)
                (assert (= (node-key found) 1000)))
               (t
                (assert (= (node-key found)
                           (* (floor key 10) 10)))))))))

(defun big-tree (&optional (n 100000))
  (let ((tree nil))
    (dotimes (i n tree)
      (setq tree (redblack:insert tree i (- i))))))

;;; 2.2 seconds to insert 10,000 items
;;; 9.1 seconds to insert 20,000 items
(defun try-avl (n-items &aux tree)
  (dotimes (i n-items)
    (setq tree (sb-thread::avl-insert tree i i)))
  tree)
;; .016 seconds to insert 10,000 items
;; .024 seconds to insert 20,000 items
(defun try-rb (n-items &aux tree)
  (dotimes (i n-items)
    (setq tree (redblack:insert tree i i)))
  tree)

(defun test-rb-find-inexact (n-nodes n-iterations)
  (bbtree-test:test-find-inexact-macro redblack:insert
                                       find<= redblack:find>= node-key))

(test-util:with-test (:name :rb-find-inexact)
  (bbtree-test:exercise-find-inexact 'test-rb-find-inexact))

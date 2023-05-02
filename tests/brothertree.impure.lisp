#+interpreter (invoke-restart 'run-tests::skip-file)

(let ((*evaluator-mode* :compile))
  (handler-bind ((warning #'muffle-warning))
    (unless (member "SB-XC" (package-nicknames "CL") :test #'string=)
      (unlock-package "CL")
      (rename-package "COMMON-LISP" "COMMON-LISP" '("CL" "SB-XC")))
    (load "../src/code/brothertree.lisp")))

(in-package sb-brothertree)

(defun verify-invariants (root &optional print
                               &aux leaf-depth
                                    (n-binary-internal 0)
                                    (n-binary-leaf 0)
                                    (n-unary-internal 0)
                                    (n-unary-leaf 0))
  (sb-int:named-let recurse ((depth 0) (node root)
                             (min -1)
                             (max (1+ sb-ext:most-positive-word)))
    (etypecase node
     (unary-node
      (assert (not (unary-node-p (child node))))
      (if (child node)
          (incf n-unary-internal)
          (incf n-unary-leaf))
      (recurse (1+ depth) (child node) min max))
     (binary-node
      (if (fringe-binary-node-p node)
          (incf n-binary-leaf)
          (incf n-binary-internal))
      (multiple-value-bind (left key right) (binary-node-parts node)
        ;; binary search tree invariant
        (assert (< min key max))
        ;; brother tree invariant
        (when (typep left 'unary-node) (assert (typep right 'binary-node)))
        (when (typep right 'unary-node) (assert (typep left 'binary-node)))
        ;; each descendant to the left has a key strictly less than this key
        (recurse (1+ depth) left min key)
        ;; each descendant to the right has a key strictly more than this key
        (recurse (1+ depth) right key max)))
     (null ; every leaf is at the same depth
      (if leaf-depth
          (assert (= depth leaf-depth))
          (setq leaf-depth depth)))))
  (when print
    (format t " depth   : ~d~%" leaf-depth)
    (format t " binary  : ~d internal + ~d leaf~%" n-binary-internal n-binary-leaf)
    (format t " unary   : ~d internal + ~d leaf~%" n-unary-internal n-unary-leaf)
    ;; The byte count is only correct if #+compact-instance-header
    (format t " memory  : ~d bytes~%"
            (* (+ (* n-binary-internal 4)
                  (* (+ n-binary-leaf n-unary-internal n-unary-leaf) 2))
               sb-vm:n-word-bytes)))
  (values leaf-depth (+ n-unary-internal n-unary-leaf)))
(compile 'verify-invariants)

(defun height (tree &aux (n 0))
  (loop
     (unless tree (return n))
     (incf n)
     (typecase tree
       (binary-node (setq tree (values (binary-node-parts tree))))
       (unary-node  (setq tree (child tree))))))

(defun tree-count (tree)
  (named-let recurse ((node tree))
    (typecase node
      (binary-node
       (multiple-value-bind (left key right) (binary-node-parts node)
         (declare (ignore key))
         (+ 1 (recurse left) (recurse right))))
      (unary-node (recurse (child node)))
      (t 0))))

(defun tree-from-list (list &optional (verify nil))
  (let ((tree))
    (dolist (item list tree)
      (setq tree (insert item tree))
      (when verify
        (verify-invariants tree)))))

(defun tree-to-list (tree)
  (let (result)
    (named-let visit ((node tree))
      (typecase node
        (unary-node
         (visit (child node)))
        (binary-node
         (multiple-value-bind (left key right) (binary-node-parts node)
           (visit right)
           (push key result)
           (visit left)))))
    result))

(defun try-delete-everything (input)
  (fill *cases* 0)
  (format t "~&Pass 1: delete 1 item from original tree")
  (let ((original-list (tree-to-list input)))
    (dolist (item (test-util:shuffle (copy-list original-list)))
      (let* ((new-tree (delete item input))
             (new-list (tree-to-list new-tree)))
        ;;(format t "After deleting ~d: ~S~%" item *cases*)
        (verify-invariants new-tree)
        (assert (equal new-list (remove item original-list)))))
    ;(print *cases*)
    (fill *cases* 0)
    (format t "~&Pass 2: delete all items from original tree")
    (let ((deletion-order (test-util:shuffle (copy-list original-list)))
          (tree input)
          (list (tree-to-list input)))
      (loop
         (unless deletion-order (return))
         (let* ((item (pop deletion-order))
                (new-tree (delete item tree)))
           (verify-invariants new-tree)
           (setq list (cl:delete item list))
           (assert (equal (tree-to-list new-tree) list))
           (setq tree new-tree))))
    ;(print *cases*)
    nil))

(defun random-big-list (count &optional (maxval (ash 1 30)))
  (let ((h (make-hash-table)))
    (loop
       (when (zerop count) (return (loop for k being each hash-key of h collect k)))
       (let ((n (random (min most-positive-fixnum maxval))))
        (unless (gethash n h)
          (setf (gethash n h) t)
          (decf count))))))

(defun insert-sequence (n &optional (verify t))
  (let ((tree))
    (loop for i from 1 to n
       do (setq tree (insert (* i 10) tree))
          (when verify
            (verify-invariants tree)))
    tree))
(defun insert-sequence-backwards (n &optional (verify t))
  (let ((tree))
    (loop for i downfrom n to 1
       do (setq tree (insert (* i 10) tree))
          (when verify
            (verify-invariants tree)))
    tree))

;; INSERT-SEQUENCE-BACKWARDS for N = (1- (expt 2 i))
;; should never need unary nodes. Because magic.
(defun try-powers-of-2 (&optional (max-expt 18) print)
  (loop for n = 1 then (* n 2) repeat (1+ max-expt)
     do
       (when print (format t "~&Trying ~d ..." n))
       (binding* ((tree (insert-sequence-backwards (1- n) nil))
                  ((depth n-unary-nodes) (verify-invariants tree)))
        (when print
          (format t " depth=~d, ~d unary nodes~%"
                  depth n-unary-nodes))
        (assert (zerop n-unary-nodes)))))

(defmacro microseconds-elapsed (form)
  #+win32
  `(values 0 ,form)
  #-win32
  `(multiple-value-bind (sec0 nsec0) (sb-unix:clock-gettime sb-unix:clock-realtime)
     (let ((result ,form))
       (multiple-value-bind (sec1 nsec1) (sb-unix:clock-gettime sb-unix:clock-realtime)
         (let ((abstime-before-usec (+ (* sec0 1000000) (floor nsec0 1000)))
               (abstime-after-usec  (+ (* sec1 1000000) (floor nsec1 1000))))
           (values (- abstime-after-usec abstime-before-usec)
                   result))))))

(defun tree-from-codeblobs ()
  (sb-sys:without-gcing
      (let (blobs)
        (sb-vm:map-code-objects
         (lambda (x)
           (push (- (sb-kernel:get-lisp-obj-address x) sb-vm:other-pointer-lowtag)
                 blobs)))
        (format t "~&Done mapping~%")
        (binding* (((et1 tree1) (microseconds-elapsed (tree-from-list blobs)))
                   ((et2 tree2) (microseconds-elapsed (tree-from-list (nreverse blobs)))))
          (format t "~&Reverse: elapsed=~D~%" et1)
          (verify-invariants tree1 t)
          (format t "~&Forward: elapsed=~D~%" et2)
          (terpri)
          (verify-invariants tree2 t)))))

(defun find-experiment (n)
  (let* ((insertion-order (loop for i from 1 repeat n collect (* i 100)))
         (tree))
#|
    ;; The AVL code is too slow to stress-test
    (when (< n 4000)
      (format t "~&AVL:")
      (gc :full t)
      (time (setq tree (build-avltree-from-ints insertion-order)))
      (time (loop for i from 150 by 100 repeat n
               do (let ((node (sb-thread:avl-find<= i tree)))
                    (assert (and node (= (sb-thread::avlnode-key node)
                                         (- i 50))))))))
    (format t "~&Red/black:")
    (gc :full t)
    (time (setq tree (build-redblack-from-ints insertion-order)))
    (time (loop for i from 150 by 100 repeat n
             do (let ((node (sb-rbtree.word::find<= i tree)))
                  (assert (and node (= (sb-rbtree.word::node-key node)
                                       (- i 50)))))))
    (time (dolist (item insertion-order)
            (setq tree (sb-rbtree.word:delete item tree))))
    (assert (null tree))
|#
    (format t "~&Brother:~%")
    (gc :full t)
    (time (setq tree (tree-from-list insertion-order)))
    (time (loop for i from 150 by 100 repeat n
             do (let ((node (find<= i tree)))
                  (assert (and node (= (binary-node-key node)
                                       (- i 50)))))))
    (time (dolist (item insertion-order)
            (setq tree (delete item tree))))
    (assert (null tree))
    ))

(defun delete-from-brothertree (tree list)
  (dolist (item list tree)
    (setq tree (delete item tree))))
(defun insertion-deletion-experiment (n)
  (let* ((insertion-order
          (test-util:shuffle (loop for i below n collect i)))
         (deletion-order
          (test-util:shuffle (copy-list insertion-order)))
         (tree))
#|
    (format t "~&Red/black:")
    (time (setq tree (build-redblack-from-ints insertion-order)))
    (format t "~&Height=~D~%" (redblack-tree-height tree))
    (time (setq tree (delete-from-redblack tree deletion-order)))
    (assert (null tree))
|#
    (format t "~&Brother:~%")
    (time (setq tree (tree-from-list insertion-order)))
    (format t "~&Height=~D~%" (verify-invariants tree))
    (time (setq tree (delete-from-brothertree tree deletion-order)))
    (assert (null tree))
    ))

(macrolet ((define-c-wrapper (lisp-name c-name)
             `(defun ,lisp-name (key tree)
                (declare (sb-vm:word key))
                (sb-sys:with-pinned-objects (tree)
                  (let ((result
                         (sb-alien:alien-funcall
                          (sb-alien:extern-alien ,c-name
                                                 (function sb-alien:unsigned sb-alien:unsigned
                                                           sb-alien:unsigned))
                                        key
                                        (sb-kernel:get-lisp-obj-address tree))))
                    (unless (eql result sb-vm:nil-value)
                      ;; have to use *UNSAFE* make-lisp-obj here because "safe" make-lisp-obj
                      ;; will not allow you to see an object in an open allocation region.
                      ;; (could we close the region flushing the trees nodes to the page table?)
                      (sb-kernel:%make-lisp-obj result)))))))
  (define-c-wrapper c-find<= "brothertree_find_lesseql")
  (define-c-wrapper c-find>= "brothertree_find_greatereql"))

(test-util:with-test (:name :find-inequality)
  (dotimes (i 10) ; try with various trees resulting from different shuffles
    (let* ((list (test-util:shuffle (loop for i from 100 by 100 repeat 25 collect i)))
           (tree (tree-from-list list)))
      (assert (not (find<= 99 tree)))
      (assert (not (c-find<= 99 tree)))
      (assert (not (find>= 10000 tree)))
      (assert (not (c-find>= 10000 tree)))
      (loop for key from 100 by 100 repeat 25
            do
         (let ((node (find<= key tree)))
           (assert (= (binary-node-key node) key))
           (assert (eq (c-find<= key tree) node)))
         (let ((node (find>= key tree)))
           (assert (= (binary-node-key node) key))
           (assert (eq (c-find>= key tree) node)))
         (let ((node (find<= (1+ key) tree)))
           (assert (= (binary-node-key node) key))
           (assert (eq (c-find<= (1+ key) tree) node)))
         (let ((node (find>= (1- key) tree)))
           (assert (= (binary-node-key node) key))
           (assert (eq (c-find>= (1- key) tree) node)))
         (let ((node (find<= (+ key 99) tree)))
           (assert (= (binary-node-key node) key))
           (assert (eq (c-find<= (+ key 99) tree) node)))
         (let ((node (find>= (- key 99) tree)))
           (assert (= (binary-node-key node) key))
           (assert (eq (c-find>= (- key 99) tree) node)))))))

(test-util:with-test (:name :insert-delete)
  (try-delete-everything (tree-from-list (random-big-list 2500))))

(test-util:with-test (:name :powers-of-2)
  (try-powers-of-2))

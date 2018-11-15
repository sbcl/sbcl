;;;; Binary Search Trees
;;;; implemented as randomly balanced 'treap' (what a horrible name).

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

(define-load-time-global *treap-random-state* (make-random-state))
(defstruct (treap-node
            (:conc-name node-)
            (:constructor make-node
             (key data &aux (priority
                             (random sb!xc:most-positive-fixnum
                                     *treap-random-state*)))))
  key data
  left right
  priority)
(declaim (inline bst-node-data))
(defun bst-node-data (x) (node-data x))

(macrolet ((rotate-left (node)
             `(let ((child (node-right ,node)))
                (shiftf (node-right ,node) (node-left child) ,node)
                child))
           (rotate-right (node)
             `(let ((child (node-left ,node)))
                (shiftf (node-left ,node) (node-right child) ,node)
                child)))

  ;; Maintain BST invariant and also heap invariant
  ;; that PRIORITY is greater than that of both children.
  (defun treap-insert (root key data &aux insertedp)
    (values
     (named-let recurse ((tree root))
       (if (not tree)
           (progn (setq insertedp t)
                  (make-node key data))
           (let ((node-key (node-key tree)))
             (cond ((> key node-key)
                    (if (> (node-priority (setf (node-right tree)
                                                (recurse (node-right tree))))
                           (node-priority tree))
                        (rotate-left tree)
                        tree))
                   ((< key node-key)
                    (if (> (node-priority (setf (node-left tree)
                                                (recurse (node-left tree))))
                           (node-priority tree))
                        (rotate-right tree)
                        tree))
                   (t tree)))))
     insertedp))

  (defun treap-delete (key root)
    (multiple-value-bind (node parent)
      (named-let find-in ((tree root) (parent nil))
        (if tree
            (let ((node-key (node-key tree)))
              (cond ((> key node-key) (find-in (node-right tree) tree))
                    ((< key node-key) (find-in (node-left tree) tree))
                    (t (values tree parent))))
            (error "Key not found: ~x" key)))
      (let ((left-child (node-left node))
            (right-child (node-right node)))
        (unless parent ; Deleting the root of the tree.
          (when (or (not left-child) (not right-child))
            ;; If this node is childless or has exactly one child,
            ;; return whichever child exists, if either, as the new root.
            (return-from treap-delete (or left-child right-child)))
          ;; Bring the higher priority child to the top.
          (setq root (if (> (node-priority left-child) (node-priority right-child))
                         (rotate-right node)
                         (rotate-left node))
                parent root)))
      (loop
       (let ((left-child (node-left node))
             (right-child (node-right node))
             (leftp (eq node (node-left parent))))
         (when (and (not left-child) (not right-child)) ; Cut the leaf off.
           (if leftp
               (setf (node-left parent) nil)
               (setf (node-right parent) nil))
           (return root))
         ;; If node has one child, we can terminate in just one more rotation.
         ;; Otherwise rotate the higher priority child up and keep going.
         (let ((r (if (or (not right-child)
                          (and left-child
                               (> (node-priority left-child)
                                  (node-priority right-child))))
                      (rotate-right node)
                      (rotate-left node))))
           (if leftp
               (setf (node-left parent) r)
               (setf (node-right parent) r))
           (setq parent r)))))))

;;; Find node with KEY in binary search tree rooted at ROOT.
(defun bst-find (key root)
  (named-let find-in ((tree root))
    (and tree
         (let ((node-key (node-key tree)))
           (cond ((> key node-key) (find-in (node-right tree)))
                 ((< key node-key) (find-in (node-left tree)))
                 (t tree))))))

;;; Find nearest node whose key is <= specified KEY.
(defun bst-find<= (key root)
  (named-let find-in ((tree root) (result nil))
    (if tree
        (let ((node-key (node-key tree)))
          (cond ((> key node-key) (find-in (node-right tree) tree))
                ((< key node-key) (find-in (node-left tree) result))
                (t tree)))
      result)))

;; For debugging only
(defun tree-keys (tree)
  (when tree
    (append (tree-keys (node-left tree))
            (list (node-key tree))
            (tree-keys (node-right tree)))))

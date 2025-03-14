
(in-package sb-thread)

(sb-xc:defstruct (avlnode (:constructor avlnode (key data left right)))
  (left  nil :read-only t)
  (right nil :read-only t)
  (key   0   :read-only t :type sb-vm:word)
  data)

;;; The remainder of this file is not for the host.
#-sb-xc-host
(progn
;;; Return the difference in left and right heights. Some people do the subtraction
;;; the other way around (like on Wikipedia). Oh well, it is what it is.
(defun avl-balance-factor (node)
  ;; would keeping the balance factor in each node would entail more consing? unsure.
  (labels ((height (node)
             (if node
                 (1+ (max (height (avlnode-left node)) (height (avlnode-right node))))
                 0)))
    (- (height (avlnode-left node)) (height (avlnode-right node)))))

(defun avl-rotate-left (node)
  (let ((right (avlnode-right node)))
    (avlnode (avlnode-key right) (avlnode-data right)
             (avlnode (avlnode-key node) (avlnode-data node) (avlnode-left node)
                      (avlnode-left right))
             (avlnode-right right))))

(defun avl-rotate-right (node)
  (let ((left (avlnode-left node)))
    (avlnode (avlnode-key left) (avlnode-data left) (avlnode-left left)
             (avlnode (avlnode-key node) (avlnode-data node)
                      (avlnode-right left) (avlnode-right node)))))

(defun avl-findmin (tree)
  (acond ((avlnode-left tree) (avl-findmin it))
         (t tree)))

(defun avl-count (tree)
  (named-let recurse ((node tree))
    (if node
        (+ 1 (recurse (avlnode-left node)) (recurse (avlnode-right node)))
        0)))

;;; There are 4 possible balance-restoring rotations, which some people
;;; term as LL, LR, RL, RR depending on the direction of the edges leading
;;; to the newly inserted node. Those lend themselves to the names of
;;; the rotations. A picture is worth a thousand words:
;;;
;;;        *         *       *        *
;;;       /         /         \        \
;;;      *         *           *        *
;;;     /           \         /          \
;;;
;;;     case A    case B    case C    case D

(defun avl-insertion-rebalance (node)
  (ecase (avl-balance-factor node)
    (+2 ; left heavy
     (avl-rotate-right
      (ecase (avl-balance-factor (avlnode-left node))
       (+1 node) ; case A
       (-1 (avlnode (avlnode-key node) (avlnode-data node) ; case B
                    (avl-rotate-left (avlnode-left node))
                    (avlnode-right node))))))
    (-2 ; right heavy
     (avl-rotate-left
      (ecase (avl-balance-factor (avlnode-right node))
        (+1 (avlnode (avlnode-key node) (avlnode-data node) ; case C
                     (avlnode-left node)
                     (avl-rotate-right (avlnode-right node))))
        (-1 node)))) ; case D
    ((-1 0 1) node)))

;;; Restoring balance after deletion is just like after insertion,
;;; but the node can be left-heavy whilst its left child can be in balance,
;;; respectively right-heavy whilst its right child is in balance.
;;; So the major reason for a separate function is to enforce a stronger
;;; invariant in the case of insertion (witness the ECASE differences).
(defun avl-deletion-rebalance (node)
  (ecase (avl-balance-factor node)
    (+2 ; left heavy
     (avl-rotate-right
      (ecase (avl-balance-factor (avlnode-left node))
       ((+1 0) node)
       (-1 (avlnode (avlnode-key node) (avlnode-data node)
                    (avl-rotate-left (avlnode-left node))
                    (avlnode-right node))))))
    (-2 ; right heavy
     (avl-rotate-left
      (ecase (avl-balance-factor (avlnode-right node))
        (+1 (avlnode (avlnode-key node) (avlnode-data node)
                     (avlnode-left node)
                     (avl-rotate-right (avlnode-right node))))
        ((-1 0) node))))
    ((-1 0 1) node)))

(defun avl-insert (tree key value)
  (declare (sb-vm:word key))
  (if (not tree)
      (avlnode key value nil nil)
      (avl-insertion-rebalance
       (cond ((< key (avlnode-key tree))
              (avlnode (avlnode-key tree) (avlnode-data tree)
                       (avl-insert (avlnode-left tree) key value)
                       (avlnode-right tree)))
             ((> key (avlnode-key tree))
              (avlnode (avlnode-key tree) (avlnode-data tree)
                       (avlnode-left tree)
                       (avl-insert (avlnode-right tree) key value)))
             (t
              (bug "Duplicate key ~S" key))))))

(defun avl-delete (key tree)
  (declare (sb-vm:word key))
  (when (null tree)
    (return-from avl-delete tree))
  (let* ((left (avlnode-left tree))
         (right (avlnode-right tree))
         (node-key (avlnode-key tree))
         (data (avlnode-data tree))
         (tree
          (cond ((and (not left) (not right)) ; leaf
                 (if (= node-key key) nil tree))
                ((not right) ; left child only
                 (cond ((= node-key key) left)
                       ((< node-key key) tree)
                       (t (avlnode node-key data (avl-delete key left) nil))))
                ((not left) ; right child only
                 (cond ((= node-key key) right)
                       ((> node-key key) tree)
                       (t (avlnode node-key data nil (avl-delete key right)))))
                ((= (avlnode-key tree) key)
                 (let* ((smallest (avl-findmin right))
                        (min-key (avlnode-key smallest))
                        (min-data (avlnode-data smallest)))
                   (avlnode min-key min-data left (avl-delete min-key right))))
                ((< key node-key)
                 (avlnode node-key data (avl-delete key left) right))
                (t
                 (avlnode node-key data left (avl-delete key right))))))
    (when tree
      (avl-deletion-rebalance tree))))

(defun avl-find (key root)
  (declare (sb-vm:word key))
  (named-let find-in ((tree root))
    (and tree
         (let ((node-key (avlnode-key tree)))
           (cond ((> key node-key) (find-in (avlnode-right tree)))
                 ((< key node-key) (find-in (avlnode-left tree)))
                 (t tree))))))

;;; Find a node with key less than or equal to KEY and as near it as possible.
(defun avl-find<= (key root)
  (declare (sb-vm:word key))
  (named-let find-in ((tree root) (result nil))
    (if tree
        (let ((node-key (avlnode-key tree)))
          (cond ((> key node-key) (find-in (avlnode-right tree) tree))
                ((< key node-key) (find-in (avlnode-left tree) result))
                (t tree)))
        result)))

;;; Find a node with key greater than or equal to KEY and as near it as possible.
(defun avl-find>= (key root)
  (declare (sb-vm:word key))
  (named-let find-in ((tree root) (result nil))
    (if tree
        (let ((node-key (avlnode-key tree)))
          (cond ((> key node-key) (find-in (avlnode-right tree) result))
                ((< key node-key) (find-in (avlnode-left tree) tree))
                (t tree)))
        result)))

(export '(avltree-list avl-find>= avl-find<=))
(defun avltree-list (tree &optional (transducer #'avlnode-data))
  (let (result)
    (named-let recurse ((node tree))
      (when node
        (push (funcall transducer node) result)
        (recurse (avlnode-left node))
        (recurse (avlnode-right node))))
    result))

(defun avltree-filter (predicate tree)
  (declare (dynamic-extent predicate))
  (let (result)
    (named-let recurse ((node tree))
      (when node
        (let ((value (funcall predicate node)))
          (when value
            (push value result)))
        (recurse (avlnode-left node))
        (recurse (avlnode-right node))))
    result))

(defmethod print-object ((self avlnode) stream)
  (format stream "#<avltree key=~s " (avlnode-key self))
  (let ((ct (avl-count self)))
    (cond ((< ct 20) ; print in order if not too many
           (let ((c #\[))
             (named-let recurse ((node self))
               (when node
                 (recurse (avlnode-left node))
                 (write-char c stream)
                 (write (avlnode-key node) :stream stream)
                 (setq c #\space)
                 (recurse (avlnode-right node)))))
           (write-char #\] stream))
          (t
           (format stream "~s ~d" :count ct))))
  (format stream " #x~x>" (get-lisp-obj-address self)))
) ; end PROGN

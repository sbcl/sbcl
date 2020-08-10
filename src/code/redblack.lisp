;;;; A pure functional red/black tree implementation

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-RBTREE")

;;; For testing
;;; (declaim (optimize sb-c:store-coverage-data))

;;; This file is translated from the Haskell code available at
;;; https://www.seas.upenn.edu/~cis552/13fa/lectures/RedBlack.html
;;;
;;; The insertion algorithm is that of Chris Okasaki
;;; and the deletion algorithm is that of Stefan Kahrs.
;;; An allegedly simpler deletion algorithm is presented in
;;; http://matt.might.net/papers/germane2014deletion.pdf
;;; which I found to be less simple when translated from Haskell.

;; This structure is exactly 4 words with #+compact-instance-header
(defstruct (rbnode (:conc-name "") (:constructor nil))
  (left  nil :read-only t :type (or rbnode null))
  (right nil :read-only t :type (or rbnode null))
  ;; It would be conventional to store key and value as two slots,
  ;; but using a cons is actually better - the table operations
  ;; create new RBNODEs, but we can share the immutable cons cells.
  (data  nil :read-only t :type cons))

(defstruct (red-node (:include rbnode)
                     (:predicate redp)
                     (:constructor red-node (left data right))
                     (:conc-name "")))
(defstruct (black-node (:include rbnode)
                       (:predicate blackp)
                       (:constructor black-node (left data right))
                       (:conc-name "")))

;; FIXME: should blackp return T of NIL?

(declaim (inline node-key node-value))
(defun node-key (node) (car (data node)))
(defun node-value (node) (cdr (data node)))

(defmethod print-object ((self rbnode) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~s" (data self))))

(defun rb-node (color left data right)
  (ecase color
    (red   (red-node left data right))
    (black (black-node left data right))))

(defun blacken (tree)
  (cond ((not tree) nil)
        ((blackp tree) tree)
        (t (black-node (left tree) (data tree) (right tree)))))

(defun redden (tree)
  (if (redp tree)
      tree
      (red-node (left tree) (data tree) (right tree))))

(defun color-of (node)
  (etypecase node
    (red-node 'red)
    (black-node 'black)))

;;; This implementation of BALANCE differs from the reference algorithm
;;; only in that by passing 4 arguments rather than an object we can
;;; sometimes avoid consing and immediately discarding one node.
(defun balance (color L data R)
  (when (eq color 'red)
    (return-from balance (red-node L data R)))
  (macrolet ((values* (&rest args)
               `(values ,@(mapcan (lambda (arg)
                                    (if (typep arg '(cons (eql node-slots)))
                                        (let ((node (cadr arg)))
                                          `((left ,node) (data ,node) (right ,node)))
                                        (list arg)))
                                  args))))
    (multiple-value-bind (a x b y c z d)
        (cond ((and (redp L) (redp (left L)))
               (let ((LL (left L)))
                 (values* (node-slots LL) (data L) (right L) data R)))
              ((and (redp L) (redp (right L)))
               (let ((RL (right L)))
                 (values* (left L) (data L) (node-slots RL) data R)))
              ((and (redp R) (redp (left R)))
               (let ((LR (left R)))
                 (values* L data (node-slots LR) (data R) (right R))))
              ((and (redp R) (redp (right R)))
               (let ((RR (right R)))
                 (values* L data (left R) (data R) (node-slots RR))))
              (t
               (return-from balance (rb-node color L data R))))
      (red-node (black-node a x b) y (black-node c z d)))))

(defun insert (tree key value)
  (let ((data (cons key value)))
    (labels ((ins (tree)
               (cond (tree
                      (let ((c (color-of tree))
                            (a (left tree))
                            (x (data tree))
                            (b (right tree)))
                        (cond ((< key (car x)) (balance c (ins a) x b))
                              ((< (car x) key) (balance c a x (ins b)))
                              (t (rb-node c a data b)))))
                     (t
                      (red-node nil data nil)))))
      (blacken (ins tree)))))

(defun delete (key tree)
  (labels ((del (tree)
             (when tree
               (binding* (((a y b) (node-slots tree)))
                 (cond ((< key (car y)) (delete-left a y b))
                       ((< (car y) key) (delete-right a y b))
                       (t (rb-merge a b))))))
           (delete-left (a y b)
             (funcall (if (blackp a) #'balance-left #'red-node) (del a) y b))
           (delete-right (a y b)
             (funcall (if (blackp b) #'balance-right #'red-node) a y (del b)))
           (sub1 (tree) ; decrease the black height by 1
             (aver (blackp tree))
             (redden tree))
           (balance-left (L data R)
             (cond ((redp L) (red-node (blacken L) data R))
                   ((blackp R) (balance 'black L data (redden R)))
                   (t
                    (aver (and (redp R) (blackp (left R))))
                    (binding* (((a y b) (node-slots (left R)))
                               (z (data R))
                               (c (right R)))
                      (red-node (black-node L data a) y (balance 'black b z (sub1 c)))))))
           (balance-right (L data R)
             (cond ((redp R) (red-node L data (blacken R)))
                   ((blackp L) (balance 'black (redden L) data R))
                   (t
                    (aver (and (redp L) (blackp (right L))))
                    (binding* ((a (left L))
                               (x (data L))
                               ((b y c) (node-slots (right L))))
                      (red-node (balance 'black (sub1 a) x b) y (black-node c data R))))))
           (rb-merge (L R)
             (cond ((null L) R)
                   ((null R) L)
                   ((eq (color-of L) (color-of R))
                    (binding* (((a x b) (node-slots L))
                               ((c y d) (node-slots R))
                               (bc (rb-merge b c)))
                      (if (redp L)
                          (if (redp bc)
                              (red-node (red-node a x (left bc)) (data bc)
                                        (red-node (right bc) y d))
                              (red-node a x (red-node bc y d)))
                          (if (redp bc)
                              (red-node (black-node a x (left bc)) (data bc)
                                        (black-node (right bc) y d))
                              (balance-left a x (black-node bc y d))))))
                   ((redp R)
                    (red-node (rb-merge L (left R)) (data R) (right R)))
                   (t
                    (aver (redp L))
                    (red-node (left L) (data L) (rb-merge (right L) R)))))
           (node-slots (x) (values (left x) (data x) (right x))))
    (declare (inline node-slots))
    (blacken (del tree))))

(defun find= (key tree)
  (loop
   (cond ((null tree) (return nil))
         ((< key (node-key tree)) (setq tree (left tree)))
         ((< (node-key tree) key) (setq tree (right tree)))
         (t (return tree)))))

;;; Find a node with key less than or equal to KEY and as near it as possible.
(defun find<= (key tree)
  (named-let recurse ((node tree) (best nil))
    (cond ((null node) best)
          ((< key (node-key node)) (recurse (left node) best))
          ((< (node-key node) key) (recurse (right node) node))
          (t node))))

;;; Find a node with key greater than or equal to KEY and as near it as possible.
(defun find>= (key tree)
  (named-let recurse ((node tree) (best nil))
    (cond ((null node) best)
          ((< key (node-key node)) (recurse (left node) node))
          ((< (node-key node) key) (recurse (right node) best))
          (t node))))

(export '(find>= find<=))

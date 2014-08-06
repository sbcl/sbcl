;;;; A union-find data structure for Unicode confusable handling

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!UNICODE")

(defstruct uf-forest mapping members)

(defstruct uf-node parent rank item index)

#!-sb-fluid
(declaim (inline uf-find))

(defun uf-find (item uf-forest)
  (let ((index (gethash item (uf-forest-mapping uf-forest))))
    (when index
      (%uf-find
       (aref
        (uf-forest-members uf-forest) index) uf-forest))))

(defun %uf-find (node uf-forest)
  (if (eql (uf-node-parent node) node)
      node
      (setf (uf-node-parent node) (%uf-find (uf-node-parent node) uf-forest))))

(defun uf-union (item1 item2 uf-forest)
  (let* ((node1 (uf-find item1 uf-forest))
         (node2 (uf-find item2 uf-forest))
         (rank1 (uf-node-rank node1))
         (rank2 (uf-node-rank node2)))
    (cond
      ((< rank1 rank2) (setf (uf-node-parent node1) node2))
      ((> rank1 rank2) (setf (uf-node-parent node2) node1))
      (t (incf (uf-node-rank node1))
         (setf (uf-node-parent node2) node1)))))

(defun create-union-find (sets &key (test #'equal))
  ;; Given a list of "sets" (lists), creates a union-find forest for them
  (let* ((total-len (loop for l in sets summing (length l)))
         (mapping (make-hash-table :test test))
         (members (make-array total-len :fill-pointer 0))
         (rep nil))
    (loop for set in sets do
         (loop for item in set
              for node = (make-uf-node :item item :rank 0 :parent NIL
                                       :index (fill-pointer members))
            do
              (setf (uf-node-parent node) node)
              (setf (gethash item mapping) (fill-pointer members))
              (vector-push node members)))
    (let ((forest (make-uf-forest :mapping mapping :members members)))
      (loop for set in sets do
           (loop for item in set do
                (if rep
                    (uf-union rep item forest)
                    (setf rep item)))
           (setf rep nil))
      forest)))

(defun uf-set-containing (item forest)
  ;; Effectively a noop if you pass a representative
  (let ((representative (uf-find item forest)))
    (unless representative (return-from uf-set-containing nil))
    (loop for node across (uf-forest-members forest)
         when (eql (%uf-find node forest) representative)
         collect (uf-node-item node))))

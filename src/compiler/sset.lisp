;;;; This file implements a sparse set abstraction, represented as a
;;;; custom lightweight hash-table. We don't use bit-vectors to
;;;; represent sets in flow analysis, since the universe may be quite
;;;; large but the average number of elements is small. We also don't
;;;; use sorted lists like in the original CMUCL code, since it had
;;;; bad worst-case performance (on some real-life programs the
;;;; hash-based sset gives a 20% compilation speedup). A custom
;;;; hash-table is used since the standard one is too heavy (locking,
;;;; memory use) for this use.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information. (This file no)

(in-package "SB-C")

;;; Each structure that may be placed in a SSET must include the
;;; SSET-ELEMENT structure. We allow an initial value of NIL to mean
;;; that no ordering has been assigned yet (although an ordering must
;;; be assigned before doing set operations.)
(defstruct (sset-element (:constructor nil)
                         (:copier nil))
  (number nil :type (or index null)))

(defstruct (sset (:copier nil)
                 (:constructor %make-sset (vector limit count)))
  ;; Vector containing the set values. 0 is used for empty (since
  ;; initializing a vector with 0 is cheaper than with NIL).
  (vector #() :type simple-vector)
  ;; The threshold count above which we double the vector.
  (limit 0 :type index)
  ;; How many elements are currently members of the set.
  (count 0 :type index))
(defun make-sset ()
  (declare (inline %make-sset))
  (%make-sset #() 0 0))

(declaim (freeze-type sset))

(defprinter (sset) vector)

;;; Iterate over the elements in SSET, binding VAR to each element in
;;; turn.
(defmacro do-sset-elements ((var sset &optional result) &body body)
  `(loop for ,var across (sset-vector ,sset)
         do (unless (fixnump ,var)
              ,@body)
         finally (return ,result)))

;;; There is no "Primary" or "Secondary" hash now. Just the hash.
;;; We use Linear Probing (step size = 1) with Knuth's Backward Shift Deletion
;;; algorithm instead of leaving tombstones. And we rely on MIX to produce
;;; a sufficiently good hash that linear probing is a reasonable strategy.
;;; (Any probing strategy other than linear does not so readily admit a
;;; deletion technique which shifts other elements on top of the deleted one.)
(declaim (inline sset-hash))
(defun sset-hash (element) (mix (sset-element-number element) 0))

;;; Rehash the sset when the proportion of free cells in the set is
;;; lower than this, the value is a reciprocal.
(defconstant +sset-rehash-threshold+ 4)

;;; Double the size of the hash vector of SET.
(defun sset-grow (set)
  (let* ((vector (sset-vector set))
         (length (* (length vector) 2))
         (new-vector (make-array length
                                 :initial-element 0))
         (new-limit (- length (truncate length +sset-rehash-threshold+))))
    (setf (sset-vector set) new-vector
          (sset-limit set) new-limit
          (sset-count set) 0)
    (loop for element across vector
          do (unless (fixnump element)
               (sset-adjoin element set)))))

;;; Destructively add ELEMENT to SET. If ELEMENT was not in the set,
;;; then we return true, otherwise we return false.
(declaim (ftype (sfunction (sset-element sset) boolean) sset-adjoin))
(defun sset-adjoin (element set)
  (when (zerop (length (sset-vector set)))
    (setf (sset-vector set) (make-array 2 :initial-element 0)
          (sset-limit set) 1))
  (loop with vector = (sset-vector set)
        with mask of-type index = (1- (length vector))
        for hash of-type index = (logand mask (sset-hash element)) then
          (logand mask (1+ hash))
        for current = (aref vector hash)
        do (cond ((eql current 0)
                  (setf (aref vector hash) element)
                  (when (> (incf (sset-count set)) (sset-limit set))
                    (sset-grow set))
                  (return t))
                 ((eq current element)
                  (return nil)))))

;;; Destructively remove ELEMENT from SET. If element was in the set,
;;; then return true, otherwise return false.
(declaim (ftype (sfunction (sset-element sset) boolean) sset-delete))
(defun sset-delete (element set)
  (when (zerop (sset-count set))
    (return-from sset-delete nil))
  (let ((vector (sset-vector set)))
    (loop with mask of-type index = (1- (length vector))
          for hash of-type index = (logand mask (sset-hash element)) then
            (logand mask (1+ hash))
          for current = (aref vector hash)
          do (cond ((eql current 0)
                    (return nil))
                   ((eq current element)
                    (decf (sset-count set))
                    (loop with i of-type index = hash
                          do (loop for j of-type index = (logand mask (1+ i)) then
                                     (logand mask (1+ j))
                                   for candidate = (aref vector j)
                                   do (cond ((eql candidate 0)
                                             (setf (aref vector i) 0)
                                             (return-from sset-delete t))
                                            ((>= (the fixnum
                                                      (logand mask
                                                              (- j (logand mask (sset-hash candidate)))))
                                                 (the fixnum
                                                      (logand mask (- j i))))
                                             (setf (aref vector i) candidate
                                                   i j)
                                             (return)))))
                    (return t))))))

;;; Return true if ELEMENT is in SET, false otherwise.
(declaim (ftype (sfunction (sset-element sset) boolean) sset-member))
(defun sset-member (element set)
  (when (zerop (sset-count set))
    (return-from sset-member nil))
  (loop with vector = (sset-vector set)
        with mask fixnum = (1- (length vector))
        for hash of-type index = (logand mask (sset-hash element)) then
          (logand mask (1+ hash))
        for current = (aref vector hash)
        do (cond ((eq current element) (return t))
                 ((eql current 0) (return nil)))))

(declaim (ftype (sfunction (sset sset) boolean) sset=))
(defun sset= (set1 set2)
  (unless (eql (sset-count set1)
               (sset-count set2))
    (return-from sset= nil))
  (do-sset-elements (element set1)
    (unless (sset-member element set2)
      (return-from sset= nil)))
  t)

;;; Return true if SET contains no elements, false otherwise.
(declaim (ftype (sfunction (sset) boolean) sset-empty))
(declaim (inline sset-empty))
(defun sset-empty (set)
  (zerop (sset-count set)))

;;; Return a new copy of SET.
(declaim (ftype (sfunction (sset) sset) copy-sset))
(defun copy-sset (set)
  (%make-sset (copy-seq (sset-vector set)) (sset-limit set) (sset-count set)))

;;; Perform the appropriate set operation on SET1 and SET2 by
;;; destructively modifying SET1. We return true if SET1 was modified,
;;; false otherwise.
(declaim (ftype (sfunction (sset sset) boolean) sset-union sset-intersection
                sset-difference))
(defun sset-union (set1 set2)
  (loop with modified = nil
        for element across (sset-vector set2)
        do (unless (fixnump element)
             (when (sset-adjoin element set1)
               (setf modified t)))
        finally (return modified)))

(defun sset-intersection (set1 set2)
  (cond
    ((or (sset-empty set1) (eq set1 set2)) nil)
    ;; Consing can always be bounded by the cardinality of the smaller set,
    ;; we just have to decide whether to collect items to ADJOIN vs DELETE.
    ;; In the situation where SET2 is empty (and SET1 is not, because empty was
    ;; ruled out above), this correctly pick the first of the following two COND
    ;; clauses, doing zero consing.
    ;; When SET2 is no more than half the size of SET1, collecting kept elements
    ;; by scanning SET2 conses at most |SET2| items, whereas scanning SET1
    ;; conses at least |SET1| - |SET2| items.
    ((<= (sset-count set2) (ash (sset-count set1) -1))
     (let ((to-keep nil))
       (do-sset-elements (element set2)
         (when (sset-member element set1)
           (push element to-keep)))
       (fill (sset-vector set1) 0)
       (setf (sset-count set1) 0)
       ;; Since |SET2| < |SET1|, SET1 is guaranteed to shrink, so we always return T.
       (dolist (element to-keep t)
         (sset-adjoin element set1))))
    (t
     (let ((to-delete nil))
       (do-sset-elements (element set1)
         (unless (sset-member element set2)
           (push element to-delete)))
       (when to-delete
         (dolist (element to-delete t)
           (sset-delete element set1)))))))

(defun sset-difference (set1 set2)
  ;; If sets are EQ, the algorithms below are either terribly broken (if you pick
  ;; the first cond clause) or terribly stupid (if you pick the second).
  ;; The result should technically be an empty set, but we don't need it.
  (aver (neq set1 set2))
  ;; If SET2 is smaller than SET1, or possibly even larger by an allowance,
  ;; we should prefer to scan all of it, calling SSET-DELETE on each item.
  ;; This technique never conses a list of items to delete.
  ;; When SET1 drives iteration, we can not both delete from and iterate over it,
  ;; so we necessarily cons an intermediate list.
  (cond ((<= (ash (sset-count set2) -1) (sset-count set1)) ; allow 2x larger SET2
         (let (modified)
           (do-sset-elements (element set2 modified)
             (when (sset-delete element set1)
               (setq modified t)))))
        (t
         (let ((to-delete nil))
           (do-sset-elements (element set1)
             (when (sset-member element set2)
               (push element to-delete)))
           (when to-delete
             (dolist (element to-delete t)
               (sset-delete element set1)))))))

;;; Destructively modify SET1 to include its union with the difference
;;; of SET2 and SET3. We return true if SET1 was modified, false
;;; otherwise.
(declaim (ftype (sfunction (sset sset sset) boolean) sset-union-of-difference))
(defun sset-union-of-difference (set1 set2 set3)
  (loop with modified = nil
        for element across (sset-vector set2)
        do (unless (fixnump element)
             (unless (sset-member element set3)
               (when (sset-adjoin element set1)
                 (setf modified t))))
        finally (return modified)))

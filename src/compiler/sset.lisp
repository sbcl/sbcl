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
                 (:constructor make-sset (&optional vector free count)))
  ;; Vector containing the set values. 0 is used for empty (since
  ;; initializing a vector with 0 is cheaper than with NIL), -1
  ;; is used to mark buckets that used to contain an element, but no
  ;; longer do.
  (vector #() :type simple-vector)
  ;; How many elements can be inserted before rehashing.
  ;; This is not the actual amount of free elements, but a ratio
  ;; calculated from +sset-rehash-threshold+.
  (free 0 :type index)
  ;; How many elements are currently members of the set.
  (count 0 :type index))

(declaim (freeze-type sset))

(defprinter (sset) vector)

;;; Iterate over the elements in SSET, binding VAR to each element in
;;; turn.
(defmacro do-sset-elements ((var sset &optional result) &body body)
  `(loop for ,var across (sset-vector ,sset)
         do (unless (fixnump ,var)
              ,@body)
         finally (return ,result)))

;;; Primary hash.
(declaim (inline sset-hash1))
(defun sset-hash1 (element)
  #+sb-xc-host
  (let ((result (sset-element-number element)))
    ;; This is performance critical, and it's not certain that the host
    ;; compiler does modular arithmetic optimization. Instad use
    ;; something that most CL implementations will do efficiently.
    (the fixnum (logxor (the fixnum result)
                        (the fixnum (ash result -9))
                        (the fixnum (ash result -5)))))
  #-sb-xc-host
  (let ((result (sset-element-number element)))
    (declare (type sb-vm:word result))
    ;; We only use the low-order bits.
    (macrolet ((set-result (form)
                 `(setf result (ldb (byte #.sb-vm:n-word-bits 0) ,form))))
      (set-result (+ result (ash result -19)))
      (set-result (logxor result (ash result -13)))
      (set-result (+ result (ash result -9)))
      (set-result (logxor result (ash result -5)))
      (set-result (+ result (ash result -2)))
      (logand most-positive-fixnum result))))

;;; Secondary hash (for double hash probing). Needs to return an odd
;;; number.
(declaim (inline sset-hash2))
(defun sset-hash2 (element)
  (let ((number (sset-element-number element)))
    (declare (fixnum number))
    (logior 1 number)))

;;; Rehash the sset when the proportion of free cells in the set is
;;; lower than this, the value is a reciprocal.
(defconstant +sset-rehash-threshold+ 4)

;;; Double the size of the hash vector of SET.
(defun sset-grow (set)
  (let* ((vector (sset-vector set))
         (length (if (zerop (length vector))
                     2
                     (* (length vector) 2)))
         (new-vector (make-array length
                                 :initial-element 0)))
    (setf (sset-vector set) new-vector
          ;; SSET-ADJOIN below will decrement this and shouldn't reach zero
          (sset-free set) length
          (sset-count set) 0)
    (loop for element across vector
          do (unless (fixnump element)
               (sset-adjoin element set)))
    ;; Now the real amount of elements which can be inserted before rehashing
    (setf (sset-free set) (- (sset-free set)
                             (max 1 (truncate length
                                              +sset-rehash-threshold+))))))


;;; Destructively add ELEMENT to SET. If ELEMENT was not in the set,
;;; then we return true, otherwise we return false.
(declaim (ftype (sfunction (sset-element sset) boolean) sset-adjoin))
(defun sset-adjoin (element set)
  (when (= (sset-free set) 0)
    (sset-grow set))
  (loop with vector = (sset-vector set)
        with mask of-type fixnum = (1- (length vector))
        with secondary-hash = (sset-hash2 element)
        with deleted-index
        for hash of-type index = (logand mask (sset-hash1 element)) then
          (logand mask (+ hash secondary-hash))
        for current = (aref vector hash)
        do (cond ((eql current 0)
                  (incf (sset-count set))
                  (cond (deleted-index
                         (setf (aref vector deleted-index) element))
                        (t
                         (decf (sset-free set))
                         (setf (aref vector hash) element)))
                  (return t))
                 ((eql current -1)
                  (setf deleted-index hash))
                 ((eq current element)
                  (return nil)))))

;;; Destructively remove ELEMENT from SET. If element was in the set,
;;; then return true, otherwise return false.
(declaim (ftype (sfunction (sset-element sset) boolean) sset-delete))
(defun sset-delete (element set)
  (when (zerop (length (sset-vector set)))
    (return-from sset-delete nil))
  (loop with vector = (sset-vector set)
        with mask fixnum = (1- (length vector))
        with secondary-hash = (sset-hash2 element)
        for hash of-type index = (logand mask (sset-hash1 element)) then
          (logand mask (+ hash secondary-hash))
        for current = (aref vector hash)
        do (cond ((eql current 0)
                  (return nil))
                 ((eq current element)
                  (decf (sset-count set))
                  (setf (aref vector hash) -1)
                  (return t)))))

;;; Return true if ELEMENT is in SET, false otherwise.
(declaim (ftype (sfunction (sset-element sset) boolean) sset-member))
(defun sset-member (element set)
  (when (zerop (length (sset-vector set)))
    (return-from sset-member nil))
  (loop with vector = (sset-vector set)
        with mask fixnum = (1- (length vector))
        with secondary-hash = (sset-hash2 element)
        for hash of-type index = (logand mask (sset-hash1 element)) then
          (logand mask (+ hash secondary-hash))
        for current = (aref vector hash)
        do (cond ((eql current 0)
                  (return nil))
                 ((eq current element)
                  (return t)))))

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
(defun sset-empty (set)
  (zerop (sset-count set)))

;;; Return a new copy of SET.
(declaim (ftype (sfunction (sset) sset) copy-sset))
(defun copy-sset (set)
  (make-sset (let* ((vector (sset-vector set))
                    (new-vector (make-array (length vector))))
               (declare (type simple-vector vector new-vector)
                        (optimize speed (safety 0)))
               ;; There's no REPLACE deftransform for simple-vectors.
               (dotimes (i (length vector))
                 (setf (aref new-vector i)
                       (aref vector i)))
               new-vector)
             (sset-free set)
             (sset-count set)))

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
  (loop with modified = nil
        for element across (sset-vector set1)
        for index of-type index from 0
        do (unless (fixnump element)
             (unless (sset-member element set2)
               (decf (sset-count set1))
               (setf (aref (sset-vector set1) index) -1
                     modified t)))
        finally (return modified)))
(defun sset-difference (set1 set2)
  (loop with modified = nil
        for element across (sset-vector set1)
        for index of-type index from 0
        do (unless (fixnump element)
             (when (sset-member element set2)
               (decf (sset-count set1))
               (setf (aref (sset-vector set1) index) -1
                     modified t)))
        finally (return modified)))

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

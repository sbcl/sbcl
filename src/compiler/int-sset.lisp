;;;; This file implements a sparse set abstraction capable of storing only
;;;; small non-negative integers using heuristics to choose from several
;;;; representations with the goal of minimizing computational complexity.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-INTEGER-SPARSE-SET")

;;; These won't be needed after combining this package into SB-C
(defconstant +sset-rehash-threshold+ 4)
(declaim (inline sset-hash))
(defun sset-hash (element-number) (mix element-number 0))

;;; INTEGER-SSETs can logically store 0, but 0 is the physical sentinel value.
;;; The trick is that we increment the user's value by 1 when storing,
;;; and decrement when reading out of the array during iteration.
(deftype int-sset-element () '(integer 0 #xFFFFFFFE))
(deftype int-sset-stored-value () '(integer 1 #xFFFFFFFF))
(defmacro int-sset-elt-encode (e) `(1+ ,e))
(defmacro int-sset-elt-decode (e) `(1- ,e))

(defstruct (integer-sset (:conc-name int-sset-)
                         (:copier nil)
                         (:constructor %make-int-sset (vector %bounds inline-bits)))
  ;; Vector containing the set values.
  ;; Using 0 as the empty value is convenient both from a perspective of being the
  ;; default memory fill value, but also not having to pick different sentinels
  ;; (such as #xFFFF and #xFFFFFFFF) for the two array specializations allows logic
  ;; in the algebraic operations to be shared for either specialization.
  (vector #* :type (or (simple-array (unsigned-byte 16) 1)
                       (simple-array (unsigned-byte 32) 1)
                       simple-bit-vector)) ; not used yet
  (%bounds 0 :type sb-vm:word)
  (inline-bits 0 :type sb-vm:word))

(declaim (freeze-type integer-sset))

(defmacro int-sset-limit (x) `(ldb (byte 32 0) (int-sset-%bounds ,x)))
(defmacro int-sset-count (x) `(ldb (byte 32 32) (int-sset-%bounds ,x)))

(defun make-int-sset ()
  (declare (inline %make-int-sset))
  (%make-int-sset #.(sb-xc:make-array 0 :element-type '(unsigned-byte 16)) 0 0))

(declaim (inline int-sset-vector-smallp))
(defun int-sset-vector-smallp (v)
  (typep v '(simple-array (unsigned-byte 16))))

;;; Iterate over the elements in SSET, binding VAR to each element in
;;; turn.
(defmacro do-int-sset-elements ((var sset &optional result) &body body)
  (let ((v '#:v) (small'#:small) (i '#:i) (elt '#:e))
    `(let* ((,v (int-sset-vector ,sset))
            (,small (int-sset-vector-smallp ,v)))
       (do ((,i (1- (length ,v)) (1- ,i)))
           ((minusp ,i) ,result)
         (declare (sb-kernel:index-or-minus-1 ,i)
                  (optimize (sb-c::insert-array-bounds-checks 0)))
         (let ((,elt (if ,small
                         (aref (truly-the (simple-array (unsigned-byte 16) 1) ,v) ,i)
                         (aref (truly-the (simple-array (unsigned-byte 32) 1) ,v) ,i))))
           (unless (eql ,elt 0)
             (let ((,var (int-sset-elt-decode ,elt))) ,@body)))))))

;; This variant purposely inserts the body twice, which is generally frowned upon
;; if arbitrary user code is allowed in the body, but is reasonable within
;; the context of implementating the operations on int-ssets.
(defmacro unswitched-do-int-sset-elt ((var sset) &body body)
  (let ((vector '#:vector))
    `(int-sset-loop-unswitch (,vector (int-sset-vector ,sset))
       (dovector (,var ,vector) (unless (eql ,var 0) ,@body)))))
(defmacro int-sset-loop-unswitch ((var expr) &body body)
  `(let ((,var ,expr))
     (if (int-sset-vector-smallp ,var)
         (let ((,var (truly-the (simple-array (unsigned-byte 16) 1) ,var))) ,@body)
         (let ((,var (truly-the (simple-array (unsigned-byte 32) 1) ,var))) ,@body))))

;;; Double the size of the hash vector of SET.
(defun int-sset-grow (set)
  (let* ((vector (int-sset-vector set))
         (length (* (length vector) 2))
         (new-vector
          (if (int-sset-vector-smallp vector)
              (make-array length :element-type '(unsigned-byte 16) :initial-element 0)
              (make-array length :element-type '(unsigned-byte 32) :initial-element 0)))
         (new-limit (- length (truncate length +sset-rehash-threshold+))))
    (setf (int-sset-vector set) new-vector
          (int-sset-limit set) new-limit
          (int-sset-count set) 0)
    ;; Can't use UNSWITCHED-DO-INT-SSET-ELT here because we're scanning the _old_ vector!
    (int-sset-loop-unswitch (old-vector vector)
      (dovector (e old-vector)
        (unless (eql e 0) (%iss-adjoin e set))))))

;;; Destructively add ELEMENT to SET. If ELEMENT was not in the set,
;;; then we return true, otherwise we return false.
(defun %iss-adjoin (element set)
  (declare (int-sset-stored-value element))
  #-sb-xc-host (declare (optimize (insert-array-bounds-checks 0)))
  (let ((vector (int-sset-vector set)))
    (if (<= element #xFFFF) ; almost always true
        (when (zerop (length vector))
          (setf vector (make-array 2 :element-type '(unsigned-byte 16) :initial-element 0)
                (int-sset-vector set) vector
                (int-sset-limit set) 1))
        ;; Ensure the vector can hold (UNSIGNED-BYTE 32).  No extra test is needed for 0-length
        ;; vectors because they are always of element-type (UNSIGNED-BYTE 16).
        (when (int-sset-vector-smallp vector)
          (let ((new-vector (make-array (max 2 (length vector)) :element-type '(unsigned-byte 32)
                                        :initial-element 0)))
            (setf vector (cond ((plusp (length vector)) (replace new-vector vector))
                               (t (setf (int-sset-limit set) 1) new-vector))
                  (int-sset-vector set) new-vector))))
    (int-sset-loop-unswitch (vector vector)
     (loop with mask = (truly-the index (1- (length vector)))
           for hash of-type index = (logand mask (sset-hash element)) then (logand mask (1+ hash))
           for current = (aref vector hash)
           do (cond ((eql current 0)
                     (setf (aref vector hash) element)
                     (when (> (incf (int-sset-count set)) (int-sset-limit set))
                       (int-sset-grow set))
                     (return t))
                    ((eql current element)
                     (return nil)))))))
(defun int-sset-adjoin (element set)
  (declare (type int-sset-element element))
  (%iss-adjoin (int-sset-elt-encode element) set))

;;; Destructively remove ELEMENT from SET. If element was in the set,
;;; then return true, otherwise return false.
(defun %iss-delete (element set)
  (declare (int-sset-stored-value element))
  #-sb-xc-host (declare (optimize (insert-array-bounds-checks 0)))
  (when (zerop (int-sset-count set))
    (return-from %iss-delete nil))
  (int-sset-loop-unswitch (vector (int-sset-vector set))
    (loop with mask = (truly-the index (1- (length vector)))
          for hash = (logand mask (sset-hash element)) then (logand mask (1+ hash))
          for current = (aref vector hash)
          do (cond ((eql current 0)
                    (return nil))
                   ((eql current element)
                    ;; COUNT was nonzero, so it can't become negative.
                    ;; Compiler isn't inferring that, so inform it.
                    (setf (int-sset-count set) (truly-the index (1- (int-sset-count set))))
                    (do ((i hash)) (nil)
                      (do ((j (logand mask (1+ i)) (logand mask (1+ j)))) (nil)
                        (declare (index j))
                        (let ((candidate (aref vector j)))
                          (when (eql candidate 0)
                            (setf (aref vector i) 0)
                            (return-from %iss-delete t))
                          (let ((n candidate))
                            (when (>= (logand mask (- j (logand mask (sset-hash n))))
                                      (logand mask (- j i)))
                              (setf (aref vector i) candidate i j)
                              (return)))))))))))

(defun int-sset-delete (element set)
  (declare (type int-sset-element element))
  (%iss-delete (int-sset-elt-encode element) set))

;;; Return true if ELEMENT is in SET, false otherwise.
(defun %iss-member (element set)
  (declare (int-sset-stored-value element))
  #-sb-xc-host (declare (optimize (insert-array-bounds-checks 0)))
  (int-sset-loop-unswitch (vector (int-sset-vector set))
     (loop with mask = (truly-the index (1- (length vector)))
           for hash of-type index = (logand mask (sset-hash element)) then
             (logand mask (1+ hash))
           for current = (aref vector hash)
           do (cond ((eql current element) (return t))
                    ((eql current 0) (return nil))))))

(defun int-sset-member (element set)
  (declare (type int-sset-element element))
  (and (/= 0 (int-sset-count set))
       (%iss-member (int-sset-elt-encode element) set)))

(defun int-sset= (set1 set2)
  (unless (eql (int-sset-count set1) (int-sset-count set2))
    (return-from int-sset= nil))
  (unswitched-do-int-sset-elt (e set1)
    (unless (%iss-member e set2)
      (return-from int-sset= nil)))
  t)

;;; Return true if SET contains no elements, false otherwise.
(declaim (inline int-sset-empty))
(defun int-sset-empty (set) (= 0 (int-sset-count set)))

;;; Return a new copy of SET.
(defun copy-int-sset (set)
  (%make-int-sset (copy-seq (int-sset-vector set))
                  (int-sset-%bounds set) (int-sset-inline-bits set)))

;;; Perform the appropriate set operation on SET1 and SET2 by
;;; destructively modifying SET1. We return true if SET1 was modified,
;;; false otherwise.
(defun int-sset-union (set1 set2 &aux modified)
  (unswitched-do-int-sset-elt (element set2)
    (when (%iss-adjoin element set1)
      (setf modified t)))
  modified)

(defun int-sset-intersection (set1 set2)
  (cond
    ((or (int-sset-empty set1) (eq set1 set2)) nil)
    ;; Consing can always be bounded by the cardinality of the smaller set,
    ;; we just have to decide whether to collect items to ADJOIN vs DELETE.
    ;; In the situation where SET2 is empty (and SET1 is not, because empty was
    ;; ruled out above), this correctly pick the first of the following two COND
    ;; clauses, doing zero consing.
    ;; When SET2 is no more than half the size of SET1, collecting kept elements
    ;; by scanning SET2 conses at most |SET2| items, whereas scanning SET1
    ;; conses at least |SET1| - |SET2| items.
    ((<= (int-sset-count set2) (ash (int-sset-count set1) -1))
     (let ((to-keep nil))
       (unswitched-do-int-sset-elt (element set2)
         (when (%iss-member element set1)
           (push element to-keep)))
       (fill (int-sset-vector set1) 0)
       (setf (int-sset-count set1) 0)
       ;; Since |SET2| < |SET1|, SET1 is guaranteed to shrink, so we always return T.
       (dolist (element to-keep t)
         (%iss-adjoin element set1))))
    (t
     (let ((to-delete nil))
       (unswitched-do-int-sset-elt (element set1)
         (unless (%iss-member element set2)
           (push element to-delete)))
       (when to-delete
         (dolist (element to-delete t)
           (%iss-delete element set1)))))))

(defun int-sset-difference (set1 set2)
  ;; If sets are EQ, the algorithms below are either terribly broken (if you pick
  ;; the first cond clause) or terribly stupid (if you pick the second).
  ;; The result should technically be an empty set, but we don't need it.
  (aver (neq set1 set2))
  ;; If SET2 is smaller than SET1, or possibly even larger by an allowance,
  ;; we should prefer to scan all of it, calling SSET-DELETE on each item.
  ;; This technique never conses a list of items to delete.
  ;; When SET1 drives iteration, we can not both delete from and iterate over it,
  ;; so we necessarily cons an intermediate list.
  (cond ((<= (ash (int-sset-count set2) -1) (int-sset-count set1)) ; allow 2x larger SET2
         (let (modified)
           (unswitched-do-int-sset-elt (element set2)
             (when (%iss-delete element set1)
               (setq modified t)))
           modified))
        (t
         (let ((to-delete nil))
           (unswitched-do-int-sset-elt (element set1)
             (when (%iss-member element set2)
               (push element to-delete)))
           (when to-delete
             (dolist (element to-delete t)
               (%iss-delete element set1)))))))

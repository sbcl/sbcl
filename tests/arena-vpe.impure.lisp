#-system-tlabs (invoke-restart 'run-tests::skip-file)

(in-package sb-vm)

(defvar *arena* (new-arena))

(defun test-vpe-heap-vector (vector count &aux grown)
  (with-arena (*arena*)
    (assert (not (heap-allocated-p (cons 1 2)))) ; assert arena is in use
    (dotimes (i count)
      (let ((old-data (%array-data vector)))
        (vector-push-extend i vector)
        (let ((new-data (%array-data vector)))
          (unless (eq new-data old-data)
            (assert (heap-allocated-p new-data))
            (setq grown t))))))
  (assert grown)) ; make sure the test proved something

(defun test-vpe-arena-vector (count &aux grown)
  (with-arena (*arena*)
    (let ((v (make-array 4 :fill-pointer 0 :adjustable t)))
      (assert (not (heap-allocated-p v)))
      (assert (not (heap-allocated-p (%array-data v))))
      (dotimes (i count)
        (let ((old-data (%array-data v)))
          (vector-push-extend i v)
          (let ((new-data (%array-data v)))
            (unless (eq new-data old-data)
              (assert (not (heap-allocated-p new-data)))
              (setq grown t)))))))
  (assert grown))

(defun test-puthash-heap-table (table count &aux grown)
  (assert (sb-impl::hash-table-hash-vector table)) ; require a hash vector
  (with-arena (*arena*)
    (assert (not (heap-allocated-p (cons 1 2))))
    (dotimes (i count)
      (let ((old-data (sb-impl::hash-table-pairs table)))
        (setf (gethash i table) i)
        (let ((new-data (sb-impl::hash-table-pairs table)))
          (unless (eq new-data old-data)
            (assert (heap-allocated-p new-data))
            (assert (heap-allocated-p (sb-impl::hash-table-hash-vector table)))
            (assert (heap-allocated-p (sb-impl::hash-table-index-vector table)))
            (assert (heap-allocated-p (sb-impl::hash-table-next-vector table)))
            (setq grown t))))))
  (assert grown))

(defun test-puthash-arena-table (count &aux grown)
  (with-arena (*arena*)
    (let ((table (make-hash-table :test 'equal)))
      (assert (sb-impl::hash-table-hash-vector table)) ; require a hash vector
      (assert (not (heap-allocated-p table)))
      (dotimes (i count)
        (let ((old-data (sb-impl::hash-table-pairs table)))
          (setf (gethash i table) i)
          (let ((new-data (sb-impl::hash-table-pairs table)))
            (unless (eq new-data old-data)
              (assert (not (heap-allocated-p new-data)))
              (assert (not (heap-allocated-p (sb-impl::hash-table-hash-vector table))))
              (assert (not (heap-allocated-p (sb-impl::hash-table-index-vector table))))
              (assert (not (heap-allocated-p (sb-impl::hash-table-next-vector table))))
              (setq grown t)))))))
  (assert grown))

;;; There is a case that this doesn't assert anything about, which is that
;;; an arena-allocated table or vector which grows while *not* in the scope of
;;; a WITH-ARENA (or inside a nested WITHOUT-ARENA) will go to the dynamic space.
;;; I think that is the right behavior: you can't force an object to be arena-allocated
;;; within a dynamic controls that asks for no arena allocation.
;;; I can't see how such a situation would legitimately arise,
;;; and it's probably only through application programmer error.

(test-util:with-test (:name :vector-push-extend-heap-vector)
  (test-vpe-heap-vector (make-array 4 :fill-pointer 0 :adjustable t) 100))

(test-util:with-test (:name :vector-push-extend-arena-vector)
  (test-vpe-arena-vector 100))

(test-util:with-test (:name :puthash-heap-table)
  (test-puthash-heap-table (make-hash-table :test 'equal) 100))

(test-util:with-test (:name :puthash-arena-table)
  (test-puthash-arena-table 100))

#+(or gc-stress ;; c-find-heap->arena is not gc-safe
      (not system-tlabs) interpreter) (invoke-restart 'run-tests::skip-file)

(test-util:with-test (:name :puthash-heap-table-addr-hash-realloc)
  (let ((a (sb-vm:new-arena 1048576))
        (old-threshold (sb-ext:bytes-consed-between-gcs)))
    (unwind-protect
         (progn
           ;; Low GC threshold ensures GC occurs during realloc inside grow-hash-table
           (setf (sb-ext:bytes-consed-between-gcs) 4096)
           (let ((table (make-hash-table :test 'eq :size 128 :rehash-size 2)))
             (assert (not (sb-impl::flat-hash-table-p table)))
             (sb-vm:with-arena (a)
               (dotimes (i 1000)
                 (let ((k (sb-vm:without-arena (cons i i))))
                   (setf (gethash k table) i))
                 (assert (sb-ext:heap-allocated-p
                          (sb-impl::hash-table-index-vector table))))))
           (assert (null (sb-vm:c-find-heap->arena a))))
      (setf (sb-ext:bytes-consed-between-gcs) old-threshold)
      (sb-vm:destroy-arena a))))

(with-test (:name :stream-layout-bits)
  (loop for wrapper being each hash-value
        of (sb-kernel:classoid-subclasses (sb-kernel:find-classoid 't))
        do (flet ((check-bit (bit ancestor-type)
                    (let ((ancestor (sb-kernel:find-layout ancestor-type)))
                      (when (or (eq wrapper ancestor)
                                (find ancestor (sb-kernel:wrapper-inherits wrapper)))
                        (assert (logtest bit (sb-kernel:wrapper-flags wrapper)))))))
              (check-bit sb-kernel:+stream-layout-flag+ 'stream)
              (check-bit sb-kernel:+string-stream-layout-flag+ 'string-stream)
              (check-bit sb-kernel:+file-stream-layout-flag+ 'file-stream))))

(with-test (:name :boxed-layout-bits)
  ;; Negative test
  (dolist (name '(hash-table sb-thread:thread sb-thread::avlnode))
    (let ((layout (sb-kernel:find-layout name)))
      (assert (not (logtest (sb-kernel:wrapper-flags layout)
                            sb-kernel:+strictly-boxed-flag+)))))
  ;; Positive test, just a small sampling
  (dolist (name '(condition warning error
                  pathname logical-pathname
                  sb-impl::string-output-stream
                  structure-object sb-c::node
                  fundamental-stream))
    (let ((layout (sb-kernel:find-layout name)))
      (assert (logtest (sb-kernel:wrapper-flags layout)
                       sb-kernel:+strictly-boxed-flag+)))))

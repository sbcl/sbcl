(with-test (:name :bug-981106
            :broken-on :freebsd)
  (gc :full t)
  (assert (eq :ok
               (handler-case
                   (dotimes (runs 100 :ok)
                     (let* ((n (truncate (dynamic-space-size) 1200))
                            (len (length
                                  (with-output-to-string (string)
                                    (dotimes (i n)
                                      (write-sequence "hi there!" string))))))
                       (assert (eql len (* n (length "hi there!"))))))
                 (storage-condition ()
                   :oom)))))

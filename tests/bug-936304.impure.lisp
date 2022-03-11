(defun stress-gc ()
  ;; Kludge or not?  I don't know whether the smaller allocation size
  ;; for sb-safepoint is a legitimate correction to the test case, or
  ;; rather hides the actual bug this test is checking for...  It's also
  ;; not clear to me whether the issue is actually safepoint-specific.
  ;; But the main problem safepoint-related bugs tend to introduce is a
  ;; delay in the GC triggering -- and if bug-936304 fails, it also
  ;; causes bug-981106 to fail, even though there is a full GC in
  ;; between, which makes it seem unlikely to me that the problem is
  ;; delay- (and hence safepoint-) related. --DFL
  (let* ((x (make-array (truncate #-sb-safepoint (* 0.2 (dynamic-space-size))
                                  #+sb-safepoint (* 0.1 (dynamic-space-size))
                                  sb-vm:n-word-bytes))))
    (setf (elt x 0) t)
    (elt x 0)))

(with-test (:name :bug-936304)
  (gc :full t)
  (assert (eq :ok (handler-case
                       (progn
                         (loop repeat 50 do (stress-gc))
                         :ok)
                     (storage-condition ()
                       :oom)))))

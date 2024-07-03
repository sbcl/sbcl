#+(or (not system-tlabs) interpreter) (invoke-restart 'run-tests::skip-file)

;; Test based on example from Andreas Franke

(defun make-unbounded-arena (chunk-size)
  (sb-vm:new-arena chunk-size
             chunk-size
             (truncate (- (dynamic-space-size) chunk-size)
                       chunk-size)))

(defvar *my-arena* (make-unbounded-arena (* 64 1024)))

(dotimes (i 200)
  (sb-ext:gc)
  (write-char #\.)(force-output)
  (sb-vm:with-arena (*my-arena*)
    (dotimes (j 32768)
      (let* ((cell (cons nil nil)))
        (setf (car cell) (list 42)))) ;; list is crucial here
    nil))



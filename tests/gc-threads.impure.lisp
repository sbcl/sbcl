#-sb-thread (invoke-restart 'run-tests::skip-file)

(with-test (:name (:two-threads-running-gc)
                  )
  (let (a-done b-done)
    (make-join-thread (lambda ()
                        (dotimes (i 100)
                          (sb-ext:gc) (princ "\\") (force-output))
                        (setf a-done t)))
    (make-join-thread (lambda ()
                        (dotimes (i 25)
                          (sb-ext:gc :full t)
                          (princ "/") (force-output))
                        (setf b-done t)))
    (loop
      (when (and a-done b-done) (return))
      (sleep 1))))

(defun waste (&optional (n 1000))
  (loop repeat n do (test-util:opaque-identity (make-string 16384))))

(compile 'waste)

(with-test (:name (:one-thread-runs-gc-while-other-conses))
  (loop for i below 100 do
        (princ "!")
        (force-output)
        (make-join-thread
         #'(lambda ()
             (waste)))
        (waste)
        (sb-ext:gc)))

(defparameter *aaa* nil)
(with-test (:name (:one-thread-runs-gc-while-other-conses :again))
  (loop for i below 100 do
        (princ "!")
        (force-output)
        (make-join-thread
         #'(lambda ()
             (let ((*aaa* (waste)))
               (waste))))
        (let ((*aaa* (waste)))
          (waste))
        (sb-ext:gc)))

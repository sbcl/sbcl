#-sb-thread (sb-ext:exit :code 104)

;; test that SLEEP actually sleeps for at least the given time, even
;; if interrupted by another thread exiting/a gc/anything
(with-test (:name (sleep :continue-sleeping-after-interrupt))
  (let ((start-time (get-universal-time)))
    (make-join-thread (lambda () (sleep 1) (sb-ext:gc :full t)))
    (sleep 5)
    (assert (>= (get-universal-time) (+ 5 start-time)))))

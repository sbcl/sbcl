(use-package "SB-THREAD")

(with-test (:name (:join-thread :timeout)
                  :skipped-on (not :sb-thread))
  (assert-error
   (join-thread (make-join-thread (lambda () (sleep 10))) :timeout 0.01)
   join-thread-error)
  (let ((cookie (cons t t)))
    (assert (eq cookie
                (join-thread (make-join-thread (lambda () (sleep 10)))
                             :timeout 0.01
                             :default cookie)))))

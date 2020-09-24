(use-package "SB-THREAD")

(with-test (:name (:join-thread :timeout)
                  :skipped-on (not :sb-thread))
  (let ((begin (get-internal-real-time)))
    (assert-error
     (join-thread (make-kill-thread (lambda () (sleep 10))) :timeout 0.01)
     join-thread-error)
    ;; should not have taken more than 1/10th sec. (and that's being generous)
    (assert (< (- (get-internal-real-time) begin)
               (/ internal-time-units-per-second 10))))
  (let ((cookie (cons t t))
        (begin (get-internal-real-time)))
    (assert (eq cookie
                (join-thread (make-kill-thread (lambda () (sleep 10)))
                             :timeout 0.01
                             :default cookie)))
    (assert (< (- (get-internal-real-time) begin)
               (/ internal-time-units-per-second 10)))))

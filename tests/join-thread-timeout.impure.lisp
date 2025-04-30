(use-package "SB-THREAD")

(with-test (:name (:join-thread :timeout)
            :broken-on :sb-safepoint
            :skipped-on (or (not :sb-thread) :gc-stress))
  (macrolet ((delta-t () '(/ (- (get-internal-real-time) begin)
                             internal-time-units-per-second)))
    (let ((thr (sb-thread:make-thread (lambda () (sleep 10)) :name "thr1"))
          (begin (get-internal-real-time)))
      (assert-error(join-thread thr :timeout 0.01) join-thread-error)
      ;; should not have taken more than 1 sec. (and that's being generous)
      (assert (< (delta-t) 1))
      (sb-thread:terminate-thread thr))
    (let ((cookie (cons t t))
          (thr (sb-thread:make-thread (lambda () (sleep 10)) :name "thr2"))
          (begin (get-internal-real-time)))
      (assert (eq cookie (join-thread thr :timeout 0.01 :default cookie)))
      (assert (< (delta-t) 1))
      (sb-thread:terminate-thread thr)))
  ;; KLUDGE: JOIN-THREAD would signal an error if the victim threads already indicated
  ;; "aborted" status (by failing to store a list of values), so just give them time
  ;; to relax and unwind and remove themselves from *ALL-THREADS*
  (sleep .25))

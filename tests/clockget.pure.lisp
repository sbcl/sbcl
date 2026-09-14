#+linux ; the CLOCK- symbols may not exist
(with-test (:name :clock-gettime)
  (sb-unix:clock-gettime sb-unix:clock-monotonic-raw)
  (sb-unix:clock-gettime sb-unix:clock-monotonic-coarse)
  (sb-unix:clock-gettime sb-unix:clock-monotonic)
  #+sb-thread
  (with-alien ((pthread-getcpuclockid (function int unsigned (* int)) :extern)
               (custom-clockid int))
    (when (zerop (alien-funcall pthread-getcpuclockid
                            (sb-thread::thread-os-thread sb-thread:*current-thread*)
                            (addr custom-clockid)))
      (flet ((time-in-microsec (clockid)
               (multiple-value-bind (sec nsec) (sb-unix:clock-gettime clockid)
                 (+ (* sec 1000000) (ceiling nsec 1000)))))
        ;; The custom clock is really the same clock as for myself,
        ;; so check that they're basically in agreement.
        (let ((t1 (time-in-microsec sb-unix:clock-thread-cputime-id))
              (t2 (time-in-microsec custom-clockid))
              (t3 (time-in-microsec sb-unix:clock-thread-cputime-id)))
          (assert (<= t1 t2 t3)))))
    (let* ((s (sb-thread:make-semaphore))
           (thread (sb-thread:make-thread
                    (lambda () (sb-thread:wait-on-semaphore s) :hooray)))
           (result (alien-funcall pthread-getcpuclockid
                                  (sb-thread::thread-os-thread thread)
                                  (addr custom-clockid))))
      (assert (zerop result)) ; got a clockid
      (sb-thread:signal-semaphore s) ; now we can let the thread go away
      (sb-thread:join-thread thread)
      ;; The Lisp thread is done, but the OS thread and pthread are semi-alive.
      (sb-thread::%dispose-thread-structs) ; Force resource freeing (pthread_join)
      (multiple-value-bind (sec nsec) (sb-unix:clock-gettime custom-clockid)
        ;; and now we should safely get NIL and NIL for the sec + nsec
        (assert (and (null sec) (null nsec)))))))

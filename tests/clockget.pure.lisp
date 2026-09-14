#+linux
(defun os-tid-existsp (tid)
  ;; "Note: glibc provides no wrapper for tkill(), necessitating the use of syscall(2)."
  ;; but also: "[[deprecated]] int syscall(SYS_tkill, pid_t tid, int sig);"
  ;; so it might have a stub.
  (if (sb-sys:find-foreign-symbol-address "tgkill")
      ;; the usual trick to find out whether a pid/tid exists- deliver signal 0
      (zerop (alien-funcall (extern-alien "tgkill" (function int int int int))
                            (sb-unix:unix-getpid) tid 0))
      :maybe))

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
    (let* ((alive (sb-thread:make-semaphore))
           (stop (sb-thread:make-semaphore))
           (thread (sb-thread:make-thread
                    (lambda ()
                      (sb-thread:signal-semaphore alive)
                      (sb-thread:wait-on-semaphore stop)
                      :hooray)))
           (result (alien-funcall pthread-getcpuclockid
                                  (sb-thread::thread-os-thread thread)
                                  (addr custom-clockid)))
           (tid))
      (assert (zerop result)) ; got a clockid
      (sb-thread:wait-on-semaphore alive)
      (setq tid (sb-thread::thread-os-tid thread))
      (assert (not (zerop tid)))
      (assert (os-tid-existsp tid))
      (sb-thread:signal-semaphore stop) ; now we can let the thread go away
      (sb-thread:join-thread thread)
      ;; The Lisp thread is done, but the OS thread and pthread are semi-alive.
      (sb-thread::%dispose-thread-structs) ; Force resource freeing (pthread_join)
      ;; can't do the negative test unless the TID is actually not valid
      (if (os-tid-existsp tid)
          (warn "Can't test for clock-gettime returnin NIL")
          (multiple-value-bind (sec nsec) (sb-unix:clock-gettime custom-clockid)
            (format t "::: (ran nonexistent TID check)~%")
            ;; and now we should safely get NIL and NIL for the sec + nsec
            (assert (and (null sec) (null nsec))))))))

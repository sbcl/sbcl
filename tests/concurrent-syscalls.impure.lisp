#-sb-thread (sb-ext:exit :code 104)
(use-package "SB-THREAD")

(defun exercise-syscall (fn reference-errno)
  (make-kill-thread
   (lambda ()
     (loop do
          (funcall fn)
          (let ((errno (sb-unix::get-errno)))
            (sleep (random 0.1d0))
            (unless (eql errno reference-errno)
              (format t "Got errno: ~A (~A) instead of ~A~%"
                      errno
                      (sb-unix::strerror)
                      reference-errno)
              (force-output)
              (abort-thread)))))))

;; (nanosleep -1 0) does not fail on FreeBSD
(with-test (:name (:exercising-concurrent-syscalls)
            :broken-on :win32)
  (let* (#-freebsd
         (nanosleep-errno (progn
                            (sb-unix:nanosleep -1 0)
                            (sb-unix::get-errno)))
         (open-errno (progn
                       (open "no-such-file"
                             :if-does-not-exist nil)
                       (sb-unix::get-errno)))
         (threads
          (list
           #-freebsd
           (exercise-syscall (lambda () (sb-unix:nanosleep -1 0)) nanosleep-errno)
           (exercise-syscall (lambda () (open "no-such-file"
                                              :if-does-not-exist nil))
                             open-errno)
           (make-join-thread (lambda () (loop (sb-ext:gc) (sleep 1)))))))
    (sleep 10)
    (princ "terminating threads")
    (dolist (thread threads)
      (terminate-thread thread))))

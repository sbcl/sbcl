;;; This is in its own file to avoid being the recipient
;;; of unanticipated effects on the file descriptor space
;;; due to co-located tests.

;;; don't know how to check validity of a HANDLE in the win32 api
#-unix (sb-ext:exit :code 104)

(defvar *streams*
  (let (l)
    (dotimes (i 6 l)
      (setq l (nconc (list (open "test-util.lisp")
                           (open "assertoid.lisp"))
                     l)))))
(defvar *fds* (mapcar 'sb-impl::fd-stream-fd *streams*))
(dolist (fd *fds*)
  (let ((errno (nth-value 1 (sb-unix:unix-lseek fd 0 sb-unix:l_incr))))
    (assert (zerop errno))))
(setq *streams* nil)
(sb-sys:scrub-control-stack)
(gc)
;;; The file descriptors should cease to be valid.
;;; Obviously it wouldn't do any good to hold on to the lisp streams
;;; to check that the FD slot gets clobbered once and only once,
;;; but perhaps someone could rig up a test involving strace of the
;;; close() system call.
(with-test (:name :finalizer-closes-fdstream)
  (let ((countbad 0))
    (dolist (fd *fds*)
      (let ((errno (nth-value 1 (sb-unix:unix-lseek fd 0 sb-unix:l_incr))))
        (unless (= errno 0)
          (assert (= errno sb-unix:ebadf))
          (incf countbad))))
    ;; I'm seeing all descriptors opened in this test get closed, but ymmv.
    ;; Let's say 90% of them is passing.
    (assert (>= countbad 10))))

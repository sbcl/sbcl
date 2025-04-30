;;; This is in its own file to avoid being the recipient
;;; of unanticipated effects on the file descriptor space
;;; due to co-located tests.

;;; don't know how to check validity of a HANDLE in the win32 api
#-unix (invoke-restart 'run-tests::skip-file)

(defun fd-has-finalizer-p (fd)
  (labels
      ((checkit (thing)
           ;; Return T if THING is a closure created in MAKE-FD-STREAM
           ;; (which is assumed to be a stream finalizer) whose payload
           ;; contains the integer FD.
           (when (and (sb-kernel:closurep thing)
                      (eql (sb-kernel:%closure-index-ref thing 0) fd))
             (let ((underlying (sb-kernel:%closure-fun thing)))
               (when (and (sb-kernel:simple-fun-p underlying)
                          (equal (sb-kernel:%simple-fun-name underlying)
                                 '(lambda () :in sb-sys:make-fd-stream)))
                 (return-from fd-has-finalizer-p t)))))
       (scan-actions (actions)
         (dolist (f (sb-int:ensure-list actions))
           (checkit (if (functionp f) f (sb-kernel:value-cell-ref f))))))
    #+weak-vector-readbarrier
    (dolist (cell sb-impl::**finalizer-store**)
      (scan-actions (cdr cell)))
    #-weak-vector-readbarrier
    (sb-lockless:so-maplist
     (lambda (node) (scan-actions (sb-lockless:so-data node)))
     sb-impl::**finalizer-store**)))

(defvar *fds*)
(defun make-streams ()
  (let (streams)
    (dotimes (i 6)
      (push (open "autoclose-stream.impure.lisp") streams))
    ;; fd-has-finalizer-p is unreliable
    #+nil
    (dolist (stream streams)
      (assert (fd-has-finalizer-p (sb-impl::fd-stream-fd stream))))
    (setq *fds*
          (mapcar (lambda (x)
                    (cons (sb-impl::fd-stream-fd x) (make-weak-pointer x)))
                  streams))
    (dolist (fd *fds*)
      (let ((errno (nth-value 1 (sb-unix:unix-lseek (car fd) 0 sb-unix:l_incr))))
        (assert (zerop errno))))))

(defun assert-invalid-file-descriptor (fd)
  (let ((errno (nth-value 1 (sb-unix:unix-lseek fd 0 sb-unix:l_incr))))
    (assert (= errno sb-unix:ebadf))))

(compile 'make-streams)

(with-test (:name :finalizer-closes-fdstream)
  (make-streams)
  (gc)
  (sb-kernel:run-pending-finalizers)
  #+sb-thread
  (sb-impl::finalizer-thread-stop)
  (sb-sys:scrub-control-stack)
  (let ((nsmashed 0))
    (dolist (entry *fds*)
      (let ((fd (car entry))
            (wp (cdr entry)))
        (unless (weak-pointer-value wp)
          (incf nsmashed)
          (assert-invalid-file-descriptor fd))))
    (format t "::: INFO: ~d streams closed~%" nsmashed)))

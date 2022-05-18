;;; This is in its own file to avoid being the recipient
;;; of unanticipated effects on the file descriptor space
;;; due to co-located tests.

;;; don't know how to check validity of a HANDLE in the win32 api
#-unix (sb-ext:exit :code 104)

(defun fd-has-finalizer-p (fd)
  ;; Finalizer function can be:
  ;; 1. a function - finalizer that was not created with :DONT-SAVE.
  ;; 2. a single list of a function - a finalizer that was created with :DONT-SAVE
  ;; 3. a vector - more than one finalizer, each being form 1 or form 2.
  (flet ((checkit (thing)
           (when (and (sb-kernel:closurep thing)
                      (eql (sb-kernel:%closure-index-ref thing 0) fd))
             (let ((underlying (sb-kernel:%closure-fun thing)))
               (when (and (sb-kernel:simple-fun-p underlying)
                          (equal (sb-kernel:%simple-fun-name underlying)
                                 '(lambda () :in sb-sys:make-fd-stream)))
                 (return-from fd-has-finalizer-p t))))))
    (let ((v sb-impl::**finalizer-store**))
      (loop for i from 3 below (length v)
            do
         (let ((entry (aref v i)))
           (typecase entry
             (list (checkit (car entry)))
             (function (checkit entry))
             (vector
              (sb-int:dovector (entry entry)
                (checkit (if (listp entry) (car entry) entry))))))))))

(defvar *fds*)
(defun threadfun ()
  (let (streams)
    (dotimes (i 6)
      (setq streams (nconc (list (open "test-util.lisp")
                                 (open "assertoid.lisp"))
                           streams)))
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

;;; A separate thread should always leave this thread with
;;; a clean stack. But the interpreter might retain garbage.
;;; So if we think garbage might be retained, don't require
;;; that finalization happened, but do check that if the weak-pointer
;;; to a stream got smashed, then the FD is invalid.
#+(or :interpreter (not :sb-thread))
(progn (threadfun)
       ;; This FMAKUNBOUND definitely seems to be needed
       ;; for sb-fasteval.
       (fmakunbound 'threadfun)
       (sb-sys:scrub-control-stack)
       (gc)
       ;; I have no idea how this write-to-string is helping,
       ;; but without it the test becomes unreliable.
       (write-to-string *fds*)
       (dolist (entry *fds*)
         (let ((fd (car entry))
               (wp (cdr entry)))
           (unless (weak-pointer-value wp)
             (assert-invalid-file-descriptor fd))))
       (sb-ext:exit :code 104))

(compile 'threadfun)

(let ((thread (sb-thread:make-thread #'threadfun)))
  (sb-thread:join-thread thread))
(gc)
;;; The file descriptors should cease to be valid.
;;; Obviously it wouldn't do any good to hold on to the lisp streams
;;; to check that the FD slot gets clobbered once and only once,
;;; but perhaps someone could rig up a test involving strace of the
;;; close() system call.
(with-test (:name :finalizer-closes-fdstream)
  (let ((countbad 0))
    (dolist (fd *fds*)
      (let ((errno (nth-value 1 (sb-unix:unix-lseek (car fd)
                                                    0 sb-unix:l_incr))))
        (unless (= errno 0)
          (assert (= errno sb-unix:ebadf))
          (incf countbad))))
    ;; I'm seeing all descriptors opened in this test get closed, but ymmv.
    ;; Let's say 90% of them is passing.
    (assert (>= countbad 10))))

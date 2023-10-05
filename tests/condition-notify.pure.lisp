#-sb-thread (invoke-restart 'run-tests::skip-file)
(use-package "SB-THREAD")

;;; A few remarks about this:
;;; 1) this test is quite strange/bad because it relies on luck and timing.
;;;    The linux FUTEX_WAIT manual entry says "wakes at most val of the waiters"
;;;    meaning 'val' is an upper bound on the count of awakened threads.
;;;    The kernel might decide to cap 'val' to the number of hardware threads,
;;;    as more threads can't be scheduled concurrently.
;;;    The lower bound on awakened threads is 1, without which there would literally
;;;    be no reason to call FUTEX_WAKE. So why does this test expect that all threads
;;;    are awakened? Because I think in practice the only cases the Linux kernel
;;;    actually implements are 1 and all.  I would guess that other OSes behave similarly.
;;;    Absence of any criterion guarded by the mutex makes this usage of a condition
;;;    var highly dubious in the first place, as normally any consumer of the event
;;;    signaled would be allowed to consume more than one event. Hence the validity
;;;    of waking fewer threads than 'val'.
;;;
;;; 2) win32 didn't always have #+sb-futex but now it does, so maybe this test should
;;;    pass there. However I tried it once (and only once) and got
;;;    "LEFTOVER-THREAD" from the test runner, which is a different problem.
;;;    TERMINATE-THREAD is horrible. Some win32 contributor will have to fix this
;;;    by making the test have a better termination criterion.
;;;
;;; When executed on macOS, each waiter tends to (or does) print its output message
;;; exactly once, but on Linux I've seen threads print more than once, which
;;; unquestionably indicates spurious wakeup.
(defparameter nthreads 10)
(with-test (:name (:condition-variable :notify-multiple)
                  :broken-on :win32)
  (flet ((tester (name notify-fun)
           (format t "~&Exercising ~A~%" name)
           (let ((queue (make-waitqueue :name "queue"))
                 (lock (make-mutex :name "lock"))
                 (start-sem (make-semaphore))
                 (waiting (1- (ash 1 nthreads))))
             (labels ((test (x &aux signaled-start-sem)
                        ;; This is an extremely atypical (dare I say "bogus") use of cv-wait
                        ;; in that there is no concrete test that it is waiting to satisfy.
                        ;; So it can't distinguish spurious wakeup from being signaled
                        ;; which sort of negates the very purpose of the test.
                        (loop
                         (let ((initial-waiting 0))
                           (with-mutex (lock)
                             (setq initial-waiting waiting)
                             ;; (format t "condition-wait ~a~%" x) (force-output)
                             (unless signaled-start-sem
                               (signal-semaphore start-sem)
                               (setq signaled-start-sem t))
                             (condition-wait queue lock)
                             ;; clear my bit. mutext is held, so this is safe
                             (setf waiting (logandc2 waiting (ash 1 x))))
                           ;; for any given thread, say that its bit was cleared once only
                           ;; (to try to see any problems)
                           ;; This can be outside the lock, but try to issue a single write()
                           ;; to avoid interleaved output.
                           (let ((str (format nil "worker ~a got cv notification (repeated=~D)~%" x
                                              (not (logbitp x initial-waiting)))))
                             (sb-sys:with-pinned-objects (str)
                               (sb-unix:unix-write 2 str 0 (length str))))))))
               (let ((threads (loop for x from 0 below (integer-length waiting)
                                    collect (make-kill-thread
                                             #'test :arguments (list x)
                                             :name (format nil "worker~D" x)))))
                 (wait-on-semaphore start-sem :n nthreads)
                 (format t "~&All threads indicate ready~%")
                 (with-mutex (lock)
                   (funcall notify-fun queue))
                 ;; give a few seconds for every thread to decide that it was woken,
                 ;; and stop as soon as they all are
                 (loop repeat 30
                       do (when (zerop waiting) (return))
                          (sleep .1))
                 (mapcar #'terminate-thread threads)
                 ;; Check that all threads woke up at least once
                 (assert (zerop waiting)))))))
    ;; the BROADCAST test is reasonable, the NOTIFY test is not.
    (tester "condition-broadcast"
            (lambda (queue) (condition-broadcast queue)))
    (tester "condition-notify"
            (lambda (queue) (condition-notify queue nthreads)))))

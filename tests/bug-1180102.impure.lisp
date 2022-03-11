(use-package "SB-THREAD")

;; SB-THREAD:MAKE-THREAD used to lock SB-THREAD:*MAKE-THREAD-LOCK*
;; before entering WITHOUT-INTERRUPTS. When a thread which was
;; executing SB-THREAD:MAKE-THREAD was interrupted with code which
;; also called SB-THREAD:MAKE-THREAD, it could happen that the first
;; thread already owned SB-THREAD:*MAKE-THREAD-LOCK* and the
;; interrupting code thus made a recursive lock attempt.

;; This test runs excruciatingly slowly on win32 without futexes.
;; Using n-threads = 100 with sb-futex, each trial took between .1 and .2
;; seconds, so 200 trials took ~ 30 seconds.
;; Without sb-futex and that same number of threads, each trial took
;; between 1.5 and 2.5 seconds which would be 400 seconds total.
(defparameter *test-params* (or #+(and win32 (not sb-futex)) '(10 . 20)
                                '(100 . 100)))

(with-test (:name (:timer :dispatch-thread :make-thread :bug-1180102)
            :skipped-on (not :sb-thread))
  (flet ((test (thread)
           (let ((timer (make-timer (lambda ()) :thread thread)))
             (schedule-timer timer .01 :repeat-interval 0.1)
             (dotimes (i (car *test-params*))
               (let ((threads '())
                     (start (get-internal-real-time)))
                 (declare (ignorable start))
                 (dotimes (i (cdr *test-params*))
                   (push (sb-thread:make-thread (lambda () (sleep .01)))
                         threads))
                 (mapc #'sb-thread:join-thread threads)
                 #+nil (format t "Trial ~d: ~f sec~%" i
                               (/ (- (get-internal-real-time) start)
                                  internal-time-units-per-second))))
             (unschedule-timer timer))))
    (test t)
    (test sb-thread:*current-thread*)))

(with-test (:name (:make-thread :interrupt-with :make-thread :bug-1180102)
            :skipped-on (not :sb-thread)
            :broken-on :sb-safepoint)
  (fresh-line)
  (write-string "; ")
  (force-output)
  (dotimes (i 100)
    (let (outer-threads
          (inner-threads (list nil))
          (parent *current-thread*))
      (dotimes (i 100)
        (push (make-thread
               (lambda ()
                 (interrupt-thread
                  parent
                  (lambda () (atomic-push (make-thread (lambda ()))
                                          (car inner-threads))))))
              outer-threads)
        (push (make-thread (lambda ())) outer-threads))
      (mapc #'join-thread outer-threads)
      (mapc #'join-thread (car inner-threads)))
    (write-char #\.)
    (force-output)))

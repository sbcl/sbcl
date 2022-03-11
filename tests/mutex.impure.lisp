#-sb-thread (sb-ext:exit :code 104)

(use-package "SB-THREAD")

;;; This test takes at least 6 seconds because each thread wants to
;;; grab and hold the mutex for a total of 3 seconds.
(with-test (:name (:mutex :contention))
  (let ((mutex (make-mutex :name "contended")))
    (labels ((run ()
               (let ((me *current-thread*))
                 (dotimes (i 100)
                   (with-mutex (mutex)
                     (sleep .03)
                     (assert (eql (mutex-owner mutex) me)))
                   (assert (not (eql (mutex-owner mutex) me))))
                 (format t "done ~A~%" *current-thread*))))
      (let ((kid1 (make-thread #'run))
            (kid2 (make-thread #'run)))
        (format t "contention ~A ~A~%" kid1 kid2)
        (wait-for-threads (list kid1 kid2))))))

(with-test (:name (interrupt-thread :interrupt-mutex-acquisition)
                  :broken-on :win32)
  (let ((lock (make-mutex :name "loctite"))
        child)
    (with-mutex (lock)
      (setf child (test-interrupt
                   (lambda ()
                     (with-mutex (lock)
                       (assert (eql (mutex-owner lock) *current-thread*)))
                     (assert (not (eql (mutex-owner lock) *current-thread*)))
                     (sleep 10))))
      ;;hold onto lock for long enough that child can't get it immediately
      (sleep 5)
      (interrupt-thread child (lambda () (format t "l ~A~%" (mutex-owner lock))))
      (format t "parent releasing lock~%"))
    (process-all-interrupts child)
    (terminate-thread child)
    (wait-for-threads (list child))))

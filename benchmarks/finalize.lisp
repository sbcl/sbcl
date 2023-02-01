(defun make-threads (semaphore nwriters nobjects)
  (loop for i below nwriters
        collect
        (let ((list (loop repeat nobjects for j from 1
                            collect (cons i j))))
          (sb-thread:make-thread
           (lambda (things)
             (sb-thread:wait-on-semaphore semaphore)
             (dolist (thing things)
               (finalize thing #'+)) ; a no-op finalizer
             (mapc #'cancel-finalization things))
           :arguments (list list)
           :name (format nil "worker ~D" i)))))

(defun test-finalize+cancel (ntrials nwriters nobjects)
  (dotimes (i ntrials)
    (let* ((sem (sb-thread:make-semaphore))
           (threads (make-threads sem nwriters nobjects)))
      (sb-thread:signal-semaphore sem nwriters)
      (mapc #'sb-thread:join-thread threads))))

(time (test-finalize+cancel 100 4 10000)) ; 100 trials, 4 threads, 10k objects per thread
#|
;; Old:
Evaluation took:
  4.100 seconds of real time
  10.704615 seconds of total run time (10.585181 user, 0.119434 system)
  [ Run times consist of 0.017 seconds GC time, and 10.688 seconds non-GC time. ]
  261.10% CPU
  9,841,747,312 processor cycles
  203,244,640 bytes consed

;; New:
Evaluation took:
  1.179 seconds of real time
  2.874184 seconds of total run time (2.756504 user, 0.117680 system)
  [ Run times consist of 0.041 seconds GC time, and 2.834 seconds non-GC time. ]
  243.77% CPU
  2,830,553,292 processor cycles
  353,720,608 bytes consed
|#

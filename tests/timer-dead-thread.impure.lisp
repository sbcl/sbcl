(with-test (:name (:with-timeout :dead-thread) :skipped-on (not :sb-thread))
  (make-join-thread
   (lambda ()
     (let ((timer (make-timer (lambda ()))))
       (schedule-timer timer 3)
       (assert t))))
  (sleep 6)
  (assert t))

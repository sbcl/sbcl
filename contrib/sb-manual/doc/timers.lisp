(in-package :sb-manual)

(defsection @timers (:title "Timers")
  "SBCL supports a system-wide event scheduler implemented on top of
  `setitimer(2)` that also works with threads but does not require a
  separate scheduler thread.

  The following example schedules a timer that writes `Hello, world`
  after two seconds.

      (schedule-timer (make-timer (lambda ()
                                    (write-line \"Hello, world\")
                                    (force-output)))
                      2)

  It should be noted that writing timer functions requires special
  care, as the dynamic environment in which they run is unpredictable:
  dynamic variable bindings, locks held, etc, all depend on whatever
  code was running when the timer fired. The following example should
  serve as a cautionary tale:

      (defvar *foo* nil)

      (defun show-foo ()
        (format t \"~&foo=~S~%\" *foo*)
        (force-output t))

      (defun demo ()
        (schedule-timer (make-timer #'show-foo) 0.5)
        (schedule-timer (make-timer #'show-foo) 1.5)
        (let ((*foo* t))
          (sleep 1.0))
        (let ((*foo* :surprise!))
          (sleep 2.0)))"
  (sb-ext:timer structure)
  (sb-ext:make-timer function)
  (sb-ext:timer-name function)
  (sb-ext:timer-scheduled-p function)
  (sb-ext:schedule-timer function)
  (sb-ext:unschedule-timer function)
  (sb-ext:list-all-timers function))

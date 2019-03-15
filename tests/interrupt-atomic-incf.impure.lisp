#-sb-thread (sb-ext:exit :code 104)

(use-package "SB-THREAD")

(defstruct counter (n 0 :type sb-vm:word))
(defvar *interrupt-counter* (make-counter))

(declaim (notinline check-interrupt-count))
(defun check-interrupt-count (i)
  (declare (optimize (debug 1) (speed 1)))
  ;; This used to lose if eflags were not restored after an interrupt.
  (unless (typep i 'fixnum)
    (error "!!!!!!!!!!!")))

(with-test (:name (interrupt-thread :interrupt-ATOMIC-INCF)
                  :broken-on :win32)
  (let ((c (make-thread
            (lambda ()
              (handler-bind ((error #'(lambda (cond)
                                        (princ cond)
                                        (sb-debug:print-backtrace
                                         :count most-positive-fixnum))))
                (loop (check-interrupt-count
                       (counter-n *interrupt-counter*))))))))
    (let ((func (lambda ()
                  (princ ".")
                  (force-output)
                  (sb-ext:atomic-incf (counter-n *interrupt-counter*)))))
      (setf (counter-n *interrupt-counter*) 0)
      (dotimes (i 100)
        (sleep (random 0.1d0))
        (interrupt-thread c func))
      (loop until (= (counter-n *interrupt-counter*) 100) do (sleep 0.1))
      (terminate-thread c)
      (wait-for-threads (list c)))))

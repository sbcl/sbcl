(defmacro assert-timeout ((expected-message) form)
  (let ((ok (gensym "OK")))
    `(let ((,ok ',ok))
       (unless (eq ,ok
                   (handler-case ,form
                     (timeout (condition)
                       (assert (string= ,expected-message
                                        (princ-to-string condition)))
                       ,ok)))
         (error "No timeout from form:~%  ~S" ',form)))))

(defun run-sleep (seconds)
  (sb-ext:run-program "sleep" (list (format nil "~D" seconds))
                      :search t :wait t))

(with-test (:name (sb-sys:decode-timeout :large-values :lp-1727789))
  (flet ((test (seconds)
           (assert (not (sb-sys:decode-timeout seconds)))))
    (test (expt 2 64))
    (test (1+ sb-kernel:internal-seconds-limit))
    (test (float (1+ sb-kernel:internal-seconds-limit) 1.0f0))
    (test (float (1+ sb-kernel:internal-seconds-limit) 1.0d0))))

(with-test (:name (sb-sys:with-deadline :large-values :lp-1727789))
  (flet ((test (seconds)
           (let ((sem (sb-thread:make-semaphore :count 0)))
             (assert-timeout ("A deadline was reached after 0 seconds.")
               (sb-sys:with-deadline (:seconds seconds)
                 (sb-sys:with-deadline (:seconds 0)
                   (sb-thread:wait-on-semaphore sem)))))))
    (test (1+ most-positive-fixnum))
    (test (1+ sb-kernel:internal-seconds-limit))
    (test (float (1+ sb-kernel:internal-seconds-limit) 1.0f0))
    (test (float (1+ sb-kernel:internal-seconds-limit) 1.0d0))))

(with-test (:name (:deadline sb-ext:run-program :trivial) :fails-on :win32)
  (assert-timeout ("A deadline was reached after 1 second.")
    (sb-sys:with-deadline (:seconds 1)
      (run-sleep 3))))

(with-test (:name (:deadline sb-sys:defer-deadline 1) :fails-on :win32)
  (let ((n 0))
    (assert-timeout ("A deadline was reached after 0.1 seconds.")
      (handler-bind ((sb-sys:deadline-timeout
                      (lambda (c)
                        (when (< n 2)
                          (incf n)
                          (sb-sys:defer-deadline 0.1 c)))))
        (sb-sys:with-deadline (:seconds 1)
          (run-sleep 2))))
    (assert (= n 2))))

(with-test (:name (:deadline sb-sys:defer-deadline 2) :fails-on :win32)
  (let ((n 0)
        (final nil))
    (handler-case
        (handler-bind ((sb-sys:deadline-timeout
                        (lambda (c)
                          (incf n)
                          (sb-sys:defer-deadline 0.1 c))))
          (sb-sys:with-deadline (:seconds 1)
            (run-sleep 2)))
      (sb-sys:deadline-timeout (c)
        (setf final c)))
    (assert (plusp n))
    (assert (not final))))

(with-test (:name (:deadline sb-sys:defer-deadline 3) :fails-on :win32)
  (let ((n 0))
    (assert-timeout ("A deadline was reached after 0.1 seconds.")
      (handler-bind ((sb-sys:deadline-timeout
                      (lambda (condition)
                        (declare (ignore condition))
                        (when (< n 2)
                          (incf n)
                          (invoke-restart 'sb-sys:defer-deadline)))))
        (sb-sys:with-deadline (:seconds .1)
          (run-sleep 3))))
    (assert (plusp n))))

(with-test (:name (:deadline sb-sys:cancel-deadline) :fails-on :win32)
  (let ((n 0)
        (final nil))
    (handler-case
        (handler-bind ((sb-sys:deadline-timeout
                        (lambda (c)
                          (incf n)
                          (sb-sys:cancel-deadline c))))
          (sb-sys:with-deadline (:seconds 1)
            (run-sleep 2)))
      (sb-sys:deadline-timeout (c)
        (setf final c)))
    (assert (= n 1))
    (assert (not final))))

(with-test (:name (:deadline sb-thread:grab-mutex)
                  :skipped-on (not :sb-thread))
  (assert-timeout ("A deadline was reached after 1 second.")
    (let ((lock (sb-thread:make-mutex))
          (waitp t))
      (make-join-thread (lambda ()
                          (sb-thread:grab-mutex lock)
                          (setf waitp nil)
                          (sleep 5)))
      (loop while waitp do (sleep 0.01))
      (sb-sys:with-deadline (:seconds 1)
        (sb-thread:grab-mutex lock)))))

(with-test (:name (:deadline sb-thread:wait-on-semaphore)
                  :skipped-on (not :sb-thread))
  (assert-timeout ("A deadline was reached after 1 second.")
    (let ((sem (sb-thread:make-semaphore :count 0)))
      (sb-sys:with-deadline (:seconds 1)
        (sb-thread:wait-on-semaphore sem)))))

(with-test (:name (:deadline sb-thread:join-thread)
            :skipped-on (not :sb-thread)
            :broken-on :win32)
  (assert-timeout ("A deadline was reached after 1 second.")
    (sb-sys:with-deadline (:seconds 1)
      (sb-thread:join-thread
       (make-kill-thread (lambda () (loop (sleep 1))))))))

(with-test (:name (:deadline :futex-wait-eintr)
            :skipped-on (not :sb-thread)
            :broken-on :win32)
  (let ((lock (sb-thread:make-mutex))
        (waitp t))
    (make-join-thread (lambda ()
                        (sb-thread:grab-mutex lock)
                        (setf waitp nil)
                        (sleep 5)))
    (loop while waitp do (sleep 0.01))
    (let ((thread (make-join-thread
                   (lambda ()
                     (let ((start (get-internal-real-time)))
                       (handler-case
                           (sb-sys:with-deadline (:seconds 1)
                             (sb-thread:grab-mutex lock))
                         (sb-sys:deadline-timeout (x)
                           (declare (ignore x))
                           (let ((end (get-internal-real-time)))
                             (float (/ (- end start)
                                       internal-time-units-per-second)
                                    0.0)))))))))
      (sleep 0.3)
      (sb-thread:interrupt-thread thread (lambda () 42))
      (let ((seconds-passed (sb-thread:join-thread thread)))
        (assert (< seconds-passed 1.2))))))

;;;; Sleep

(with-test (:name (sb-sys:with-deadline sleep :smoke))
  (assert-timeout ("A deadline was reached after 0.1 seconds.")
    (sb-sys:with-deadline (:seconds .1) (sleep 1)))

  (assert-no-signal
      (sb-sys:with-deadline (:seconds .2) (sleep .1))
    sb-sys:deadline-timeout))

(with-test (:name (sb-sys:with-deadline sleep :long-sleep))
  (assert-timeout ("A deadline was reached after 0.1 seconds.")
    (sb-sys:with-deadline (:seconds .1)
      (sleep (1+ sb-kernel:internal-seconds-limit)))))

(with-test (:name (sb-sys:with-deadline sleep :no-sleep))
  ;; When SLEEP is called in the context of an expired deadline, the
  ;; DEADLINE-TIMEOUT must be signaled even if there is no sleeping to
  ;; be done.
  (assert-timeout ("A deadline was reached after 0.1 seconds.")
    (sb-sys:with-deadline (:seconds .1)
      (let ((sb-impl::*deadline* nil)) (sleep .2))
      (sleep 0))))

(with-test (:name (sb-sys:with-deadline sleep sb-sys:defer-deadline))
  (let ((n 0))
    (assert-no-signal
        (handler-bind ((sb-sys:deadline-timeout
                        (lambda (condition)
                          (incf n)
                          (sb-sys:defer-deadline .1 condition))))
          (sb-sys:with-deadline (:seconds .1) (sleep .5)))
      sb-sys:deadline-timeout)
    (assert (plusp n))))

(with-test (:name (sb-sys:with-deadline sleep sb-sys:cancel-deadline))
  (assert-no-signal
      (handler-bind ((sb-sys:deadline-timeout
                      (lambda (condition)
                        (sb-sys:cancel-deadline condition))))
        (sb-sys:with-deadline (:seconds .1) (sleep 1)))
    sb-sys:deadline-timeout))

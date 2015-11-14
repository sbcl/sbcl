(in-package :cl-user)

(use-package :test-util)

(defmacro assert-timeout (form)
  (let ((ok (gensym "OK")))
    `(let ((,ok ',ok))
       (unless (eq ,ok
                   (handler-case ,form
                     (timeout ()
                       ,ok)))
         (error "No timeout from form:~%  ~S" ',form)))))

(defun run-sleep (seconds)
  (sb-ext:run-program "sleep" (list (format nil "~D" seconds))
                      :search t :wait t))

(with-test (:name (:deadline sb-ext:run-program :trivial) :fails-on :win32)
  (assert-timeout (sb-sys:with-deadline (:seconds 1)
                    (run-sleep 3))))

(with-test (:name (:deadline sb-sys:defer-deadline 1) :fails-on :win32)
  (let ((n 0)
        (final nil))
    (handler-case
        (handler-bind ((sb-sys:deadline-timeout
                        (lambda (c)
                          (when (< n 2)
                            (incf n)
                            (sb-sys:defer-deadline 0.1 c)))))
          (sb-sys:with-deadline (:seconds 1)
            (run-sleep 2)))
      (sb-sys:deadline-timeout (c)
        (setf final c)))
    (assert (= n 2))
    (assert final)))

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
                  :skipped-on '(not :sb-thread))
  (assert-timeout
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
                  :skipped-on '(not :sb-thread))
  (assert-timeout
   (let ((sem (sb-thread:make-semaphore :count 0)))
     (sb-sys:with-deadline (:seconds 1)
       (sb-thread:wait-on-semaphore sem)))))

(with-test (:name (:deadline sb-thread:join-thread)
                  :skipped-on '(not :sb-thread))
  (assert-timeout
   (sb-sys:with-deadline (:seconds 1)
     (sb-thread:join-thread
      (make-kill-thread (lambda () (loop (sleep 1))))))))

(with-test (:name (:deadline :futex-wait-eintr) :skipped-on '(not :sb-thread))
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

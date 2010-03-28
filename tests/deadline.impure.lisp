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

(with-test (:name (:deadline :run-program :trivial))
  (assert-timeout (sb-sys:with-deadline (:seconds 1)
                    (run-sleep 3))))

(with-test (:name (:deadline :defer-deadline-1))
  (let ((n 0)
        (final nil))
    (handler-case
        (handler-bind ((sb-sys:deadline-timeout
                        #'(lambda (c)
                            (when (< n 2)
                              (incf n)
                              (sb-sys:defer-deadline 0.1 c)))))
          (sb-sys:with-deadline (:seconds 1)
            (run-sleep 2)))
      (sb-sys:deadline-timeout (c)
        (setf final c)))
    (assert (= n 2))
    (assert final)))

(with-test (:name (:deadline :defer-deadline-2))
  (let ((n 0)
        (final nil))
    (handler-case
        (handler-bind ((sb-sys:deadline-timeout
                        #'(lambda (c)
                            (incf n)
                            (sb-sys:defer-deadline 0.1 c))))
          (sb-sys:with-deadline (:seconds 1)
            (run-sleep 2)))
      (sb-sys:deadline-timeout (c)
        (setf final c)))
    (assert (plusp n))
    (assert (not final))))

(with-test (:name (:deadline :cancel-deadline))
  (let ((n 0)
        (final nil))
    (handler-case
        (handler-bind ((sb-sys:deadline-timeout
                        #'(lambda (c)
                            (incf n)
                            (sb-sys:cancel-deadline c))))
          (sb-sys:with-deadline (:seconds 1)
            (run-sleep 2)))
      (sb-sys:deadline-timeout (c)
        (setf final c)))
    (assert (= n 1))
    (assert (not final))))

#+(and sb-thread (not sb-lutex))
(progn

  (with-test (:name (:deadline :get-mutex))
    (assert-timeout
     (let ((lock (sb-thread:make-mutex))
           (waitp t))
       (sb-thread:make-thread (lambda ()
                                (sb-thread:get-mutex lock)
                                (setf waitp nil)
                                (sleep 5)))
       (loop while waitp do (sleep 0.01))
       (sb-sys:with-deadline (:seconds 1)
         (sb-thread:get-mutex lock)))))

  (with-test (:name (:deadline :wait-on-semaphore))
    (assert-timeout
     (let ((sem (sb-thread::make-semaphore :count 0)))
       (sb-sys:with-deadline (:seconds 1)
         (sb-thread::wait-on-semaphore sem)))))

  (with-test (:name (:deadline :join-thread))
    (assert-timeout
     (sb-sys:with-deadline (:seconds 1)
       (sb-thread:join-thread
        (sb-thread:make-thread (lambda () (loop (sleep 1))))))))

  (with-test (:name (:deadline :futex-wait-eintr))
    (let ((lock (sb-thread:make-mutex))
          (waitp t))
      (sb-thread:make-thread (lambda ()
                               (sb-thread:get-mutex lock)
                               (setf waitp nil)
                               (sleep 5)))
      (loop while waitp do (sleep 0.01))
      (let ((thread (sb-thread:make-thread
                     (lambda ()
                       (let ((start (get-internal-real-time)))
                         (handler-case
                             (sb-sys:with-deadline (:seconds 1)
                               (sb-thread:get-mutex lock))
                           (sb-sys:deadline-timeout (x)
                             (declare (ignore x))
                             (let ((end (get-internal-real-time)))
                               (float (/ (- end start)
                                         internal-time-units-per-second)
                                      0.0)))))))))
        (sleep 0.3)
        (sb-thread:interrupt-thread thread (lambda () 42))
        (let ((seconds-passed (sb-thread:join-thread thread)))
          (format t "Deadline in ~S~%" seconds-passed)
          (assert (< seconds-passed 1.2)))))))
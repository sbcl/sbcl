(defmacro assert-timeout (form)
  (let ((ok (gensym "OK")))
    `(let ((,ok ',ok))
       (unless (eq ,ok
                   (handler-case ,form
                     (timeout ()
                       ,ok)))
         (error "No timeout from form:~%  ~S" ',form)))))


(assert-timeout
 (sb-sys:with-deadline (:seconds 1)
   (run-program "sleep" '("3") :search t :wait t)))

(let ((n 0)
      (final nil))
  (handler-case
      (handler-bind ((sb-sys:deadline-timeout (lambda (c)
                                                (when (< n 2)
                                                  (incf n)
                                                  (sb-sys:defer-deadline 0.1 c)))))
        (sb-sys:with-deadline (:seconds 1)
          (run-program "sleep" '("2") :search t :wait t)))
    (sb-sys:deadline-timeout (c)
      (setf final c)))
  (assert (= n 2))
  (assert final))

(let ((n 0)
      (final nil))
  (handler-case
      (handler-bind ((sb-sys:deadline-timeout (lambda (c)
                                                (incf n)
                                                (sb-sys:defer-deadline 0.1 c))))
        (sb-sys:with-deadline (:seconds 1)
          (run-program "sleep" '("2") :search t :wait t)))
    (sb-sys:deadline-timeout (c)
      (setf final c)))
  (assert (plusp n))
  (assert (not final)))

#+(and sb-thread (not sb-lutex))
(progn
  (assert-timeout
   (let ((lock (sb-thread:make-mutex))
         (waitp t))
     (sb-thread:make-thread (lambda ()
                              (sb-thread:get-mutex lock)
                              (setf waitp nil)
                              (sleep 5)))
     (loop while waitp do (sleep 0.01))
     (sb-impl::with-deadline (:seconds 1)
       (sb-thread:get-mutex lock))))

  (assert-timeout
   (let ((sem (sb-thread::make-semaphore :count 0)))
     (sb-impl::with-deadline (:seconds 1)
       (sb-thread::wait-on-semaphore sem))))

  (assert-timeout
   (sb-impl::with-deadline (:seconds 1)
     (sb-thread:join-thread
      (sb-thread:make-thread (lambda () (loop (sleep 1))))))))

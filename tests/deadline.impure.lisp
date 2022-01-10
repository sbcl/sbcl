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

(defmacro assert-timeout (form)
  (let ((ok (gensym "OK")))
    `(let ((,ok ',ok))
       (unless (eq ,ok
                   (handler-case ,form
                     (timeout ()
                       ,ok)))
         (error "No timeout from form:~%  ~S" ',form)))))


(assert-timeout
 (sb-impl::with-deadline (:seconds 1)
   (run-program "sleep" '("5") :search t :wait t)))

#+(and sb-thread (not sb-lutex))
(progn
  (assert-timeout
   (let ((lock (sb-thread:make-mutex)))
     (sb-thread:make-thread (lambda () (sb-thread:get-mutex lock) (sleep 5)))
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

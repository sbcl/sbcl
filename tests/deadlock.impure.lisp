#-sb-thread (sb-ext:exit :code 104)

(with-test (:name (:deadlock-detection :gc))
  ;; To semi-reliably trigger the error (in SBCL's where)
  ;; it was present you had to run this for > 30 seconds,
  ;; but that's a bit long for a single test.
  (let* ((stop (+ 5 (get-universal-time)))
         (m1 (sb-thread:make-mutex :name "m1"))
         (t1 (sb-thread:make-thread
              (lambda ()
                (loop until (> (get-universal-time) stop)
                      do (sb-thread:with-mutex (m1)
                           (eval `(make-array 24))))
                :ok)))
         (t2 (sb-thread:make-thread
              (lambda ()
                (loop until (> (get-universal-time) stop)
                      do (sb-thread:with-mutex (m1)
                           (eval `(make-array 24))))
                :ok))))
    (let ((res (list (sb-thread:join-thread t1)
                     (sb-thread:join-thread t2))))
      (assert (equal '(:ok :ok) res)))))

(with-test (:name :gc-deadlock
            :broken-on :win32)
  (write-line "WARNING: THIS TEST WILL HANG ON FAILURE!")
  ;; Prior to 0.9.16.46 thread exit potentially deadlocked the
  ;; GC due to *all-threads-lock* and session lock. On earlier
  ;; versions and at least on one specific box this test is good enough
  ;; to catch that typically well before the 1500th iteration.
  (loop
     with i = 0
     with n = 3000
     while (< i n)
     do
       (incf i)
       (when (zerop (mod i 100))
         (write-char #\.)
         (force-output))
       (handler-case
           (if (oddp i)
               (make-join-thread
                (lambda ()
                  (sleep (random 0.001)))
                :name (format nil "SLEEP-~D" i))
               (make-join-thread
                (lambda ()
                  ;; KLUDGE: what we are doing here is explicit,
                  ;; but the same can happen because of a regular
                  ;; MAKE-THREAD or LIST-ALL-THREADS, and various
                  ;; session functions.
                  (sb-thread::with-all-threads-lock
                    (sb-thread::with-session-lock (sb-thread::*session*)
                      (sb-ext:gc))))
                :name (format nil "GC-~D" i)))
         (error (e)
           (format t "~%error creating thread ~D: ~A -- backing off for retry~%" i e)
           (sleep 0.1)
           (incf i)))))

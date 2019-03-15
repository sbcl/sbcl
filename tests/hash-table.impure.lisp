;;; HASH TABLES

#-sb-thread (sb-ext:exit :code 104)
(use-package "SB-THREAD")
(use-package "SB-SYS")

(defvar *errors* nil)

(defun oops (e)
  (setf *errors* e)
  (format t "~&oops: ~A in ~S~%" e *current-thread*)
  (sb-debug:print-backtrace)
  (catch 'done))

(with-test (:name (hash-table :unsynchronized)
                  ;; FIXME: This test occasionally eats out craploads
                  ;; of heap instead of expected error early. Not 100%
                  ;; sure if it would finish as expected, but since it
                  ;; hits swap on my system I'm not likely to find out
                  ;; soon. Disabling for now. -- nikodemus
            :broken-on :sbcl)
  ;; We expect a (probable) error here: parellel readers and writers
  ;; on a hash-table are not expected to work -- but we also don't
  ;; expect this to corrupt the image.
  (let* ((hash (make-hash-table))
         (*errors* nil)
         (threads (list (make-kill-thread
                         (lambda ()
                           (catch 'done
                             (handler-bind ((serious-condition 'oops))
                               (loop
                                 ;;(princ "1") (force-output)
                                 (setf (gethash (random 100) hash) 'h)))))
                         :name "writer")
                        (make-kill-thread
                         (lambda ()
                           (catch 'done
                             (handler-bind ((serious-condition 'oops))
                               (loop
                                 ;;(princ "2") (force-output)
                                 (remhash (random 100) hash)))))
                         :name "reader")
                        (make-kill-thread
                         (lambda ()
                           (catch 'done
                             (handler-bind ((serious-condition 'oops))
                               (loop
                                 (sleep (random 1.0))
                                 (sb-ext:gc :full t)))))
                         :name "collector"))))
    (unwind-protect
         (sleep 10)
      (mapc #'terminate-thread threads))))

(with-test (:name (hash-table :synchronized)
            :broken-on :win32)
  (let* ((hash (make-hash-table :synchronized t))
         (*errors* nil)
         (threads (list (make-join-thread
                         (lambda ()
                           (catch 'done
                             (handler-bind ((serious-condition 'oops))
                               (loop
                                 ;;(princ "1") (force-output)
                                 (setf (gethash (random 100) hash) 'h)))))
                         :name "writer")
                        (make-join-thread
                         (lambda ()
                           (catch 'done
                             (handler-bind ((serious-condition 'oops))
                               (loop
                                 ;;(princ "2") (force-output)
                                 (remhash (random 100) hash)))))
                         :name "reader")
                        (make-join-thread
                         (lambda ()
                           (catch 'done
                             (handler-bind ((serious-condition 'oops))
                               (loop
                                 (sleep (random 1.0))
                                 (sb-ext:gc :full t)))))
                         :name "collector"))))
    (unwind-protect
         (sleep 10)
      (mapc #'terminate-thread threads))
    (assert (not *errors*))))

(with-test (:name (hash-table :parallel-readers)
                  :broken-on :win32)
  (let ((hash (make-hash-table))
        (*errors* nil))
    (loop repeat 50
          do (setf (gethash (random 100) hash) 'xxx))
    (let ((threads (list (make-kill-thread
                          (lambda ()
                            (catch 'done
                              (handler-bind ((serious-condition 'oops))
                                (loop
                                      until (eq t (gethash (random 100) hash))))))
                          :name "reader 1")
                         (make-kill-thread
                          (lambda ()
                            (catch 'done
                              (handler-bind ((serious-condition 'oops))
                                (loop
                                      until (eq t (gethash (random 100) hash))))))
                          :name "reader 2")
                         (make-kill-thread
                          (lambda ()
                            (catch 'done
                              (handler-bind ((serious-condition 'oops))
                                (loop
                                      until (eq t (gethash (random 100) hash))))))
                          :name "reader 3")
                         (make-kill-thread
                          (lambda ()
                            (catch 'done
                              (handler-bind ((serious-condition 'oops))
                               (loop
                                 (sleep (random 1.0))
                                 (sb-ext:gc :full t)))))
                          :name "collector"))))
      (unwind-protect
           (sleep 10)
        (mapc #'terminate-thread threads))
      (assert (not *errors*)))))

(with-test (:name (hash-table :single-accessor :parallel-gc)
                  :broken-on :win32)
  (let ((hash (make-hash-table))
        (*errors* nil))
    (let ((threads (list (make-kill-thread
                          (lambda ()
                            (handler-bind ((serious-condition 'oops))
                              (loop
                                (let ((n (random 100)))
                                  (if (gethash n hash)
                                      (remhash n hash)
                                      (setf (gethash n hash) 'h))))))
                          :name "accessor")
                         (make-kill-thread
                          (lambda ()
                            (handler-bind ((serious-condition 'oops))
                              (loop
                                (sleep (random 1.0))
                                (sb-ext:gc :full t))))
                          :name "collector"))))
      (unwind-protect
           (sleep 10)
        (mapc #'terminate-thread threads))
      (assert (not *errors*)))))

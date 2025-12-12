(require :sb-posix)
#-win32
(with-test (:name (open :interrupt)
                  :skipped-on (or :win32 (:and :darwin :sb-safepoint)))
  (let ((to 0))
    (with-scratch-file (fifo)
           ;; Make a FIFO
           (sb-posix:mkfifo fifo (logior sb-posix:s-iwusr sb-posix:s-irusr))
           ;; Try to open it (which hangs), and interrupt ourselves with a timer,
           ;; continue (this used to result in an error due to open(2) returning with
           ;; EINTR, then interupt again and unwind.
           (handler-case
               (with-timeout 2
                 (handler-bind ((timeout (lambda (c)
                                           (when (eql 1 (incf to))
                                             (continue c)))))
                   (with-timeout 1
                     (with-open-file (f fifo :direction :input)
                       :open))))
             (timeout ()
               (if (eql 2 to)
                   :timeout
                   :wtf))
             (error (e)
               e)))))

#-sb-thread (sb-ext:exit :code 104)

(use-package "SB-THREAD")

(defun alloc-stuff () (copy-list '(1 2 3 4 5)))

(with-test (:name (:interrupt-thread :interrupt-consing-child)
                  :broken-on :win32)
  (let* ((thread (make-thread (lambda () (loop (alloc-stuff)))))
         (killer (make-thread
                  (lambda ()
                    (loop repeat 100 do
                          (sleep (random 0.1d0))
                          (princ ".")
                          (force-output)
                          (process-all-interrupts thread)
                          (interrupt-thread thread (lambda ())))))))
    (wait-for-threads (list killer))
    (process-all-interrupts thread)
    (terminate-thread thread)
    (wait-for-threads (list thread)))
  (sb-ext:gc :full t))

#+(or x86 x86-64) ;; x86oid-only, see internal commentary.
(with-test (:name (:interrupt-thread :interrupt-consing-child :again)
                  :broken-on :win32)
  (let ((c (make-thread (lambda () (loop (alloc-stuff))))))
    ;; NB this only works on x86: other ports don't have a symbol for
    ;; pseudo-atomic atomicity
    (dotimes (i 100)
      (sleep (random 0.1d0))
      (process-all-interrupts c)
      (interrupt-thread c
                        (lambda ()
                          (princ ".")
                          (force-output)
                          (assert (thread-alive-p *current-thread*))
                          (assert
                           (not (logbitp 0 SB-KERNEL:*PSEUDO-ATOMIC-BITS*))))))
    (process-all-interrupts c)
    (terminate-thread c)
    (wait-for-threads (list c))))

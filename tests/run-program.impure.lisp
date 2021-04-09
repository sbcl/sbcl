;;;; various RUN-PROGRAM tests with side effects

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; While most of SBCL is derived from the CMU CL system, the test
;;;; files (like this one) were written from scratch after the fork
;;;; from CMU CL.
;;;;
;;;; This software is in the public domain and is provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for
;;;; more information.

(cl:in-package :cl-user)
(defun malloc ()
  (let ((x (sb-alien:make-alien int 10000)))
    (sb-alien:free-alien x)
    1))

(with-test (:name (run-program :malloc-deadlock)
            :broken-on :sb-safepoint
            :skipped-on (or (not :sb-thread) :win32))
  (let* (stop
         (delay-between-gc
          (or #+freebsd
              (let* ((p (run-program "sysctl" '("hw.model") :search t :output :stream))
                     (output (process-output p))
                     (result (read-line output)))
                (close output)
                ;; With the default delay of 0 using FreeBSD on QEMU this test never
                ;; finished because the RUN-PROGRAM thread would never get scheduled
                ;; after its first entry into the loop.
                ;; It wasn't hung, it just wasn't getting CPU time. For me anyway.
                (when (search "QEMU" result)
                  .00000001)) ; 10 nanoseconds
              #+(and darwin arm64)
               0.01
              0))
         (threads (list*
                   (sb-thread:make-thread (lambda ()
                                            (loop until (progn (sb-thread:barrier (:read))
                                                               stop)
                                                  do
                                                  (sleep delay-between-gc)
                                                  (gc :full t))))
                   (loop repeat 3
                         collect
                         (sb-thread:make-thread
                          (lambda ()
                            (loop until (progn (sb-thread:barrier (:read))
                                               stop)
                                  do (malloc))))))))
    (loop with dot = (get-internal-real-time)
          with end = (+ dot (* internal-time-units-per-second 4))
          for time = (get-internal-real-time)
          while (> end time)
          do
          (when (> (- time dot)
                   (/ internal-time-units-per-second 10))
            (setf dot time)
            (write-char #\.)
            (finish-output))
          (with-output-to-string (str)
            (run-program "uname" ()
                         :output str
                         :search t
                         :wait t)))
    (setf stop t)
    (sb-thread:barrier (:write))
    (mapc #'sb-thread:join-thread threads)))



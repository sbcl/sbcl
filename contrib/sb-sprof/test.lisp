(cl:defpackage #:sb-sprof-test
  (:use #:cl #:sb-sprof)
  (:export #:run-tests))

(cl:in-package #:sb-sprof-test)

;#+sb-fasteval (setq sb-ext:*evaluator-mode* :compile)

;;; silly examples

(defun test-0 (n &optional (depth 0))
  (declare (optimize (debug 3)))
  (when (< depth n)
    (dotimes (i n)
      (test-0 n (1+ depth))
      (test-0 n (1+ depth)))))

(defun test ()
  (with-profiling (:reset t :max-samples 1000 :report :graph)
    (test-0 7)))

(defun consalot ()
  (let ((junk '()))
    (loop repeat 10000 do
         (push (make-array 10) junk))
    junk))

(defun consing-test ()
  ;; 0.0001 chosen so that it breaks rather reliably when sprof does not
  ;; respect pseudo atomic.
  (with-profiling (:reset t
                          ;; setitimer with small intervals
                          ;; is broken on FreeBSD 10.0
                          ;; And ARM targets are not fast in
                          ;; general, causing the profiling signal
                          ;; to be constantly delivered without
                          ;; making any progress.
                          #-(or freebsd arm) :sample-interval
                          #-(or freebsd arm) 0.0001
                          #+arm :sample-interval #+arm 0.1
                          :report :graph)
    (loop with target = (+ (get-universal-time) 3)
          while (< (get-universal-time) target)
          do (consalot))))

(defun run-tests ()
  (let ((*standard-output* (make-broadcast-stream)))
    (test)
    (consing-test)
    #+sb-thread
    (let* ((sem (sb-thread:make-semaphore))
           (some-thread (sb-thread:make-thread #'sb-thread:wait-on-semaphore :arguments sem)))
      (sb-sprof:stop-sampling some-thread)
      (sb-sprof:start-sampling some-thread)
      (sb-thread:signal-semaphore sem))
    t))

;; For debugging purposes, print output for visual inspection to see if
;; the allocation sequence gets hit in the right places (i.e. not at all
;; in traditional builds, and everywhere if SB-SAFEPOINT is enabled.)
#+nil (disassemble #'consalot)


;;; This exists outside of the unit test in sb-sprof so that you can execute
;;; it with parallel-exec specifying an arbitrarily huge --runs_per_test.
;;; It is uncharacteristically verbose in its output for my liking,
;;; but I need to try to see it behaving badly (if it does),
;;; and there's really no other way than to watch for bad output.

#+(or win32 sparc) (invoke-restart 'run-tests::skip-file)

(require :sb-sprof)

;;; silly examples

(defun test-0 (n &optional (depth 0))
  (declare (optimize (debug 3)))
  (when (< depth n)
    (dotimes (i n)
      (test-0 n (1+ depth))
      (test-0 n (1+ depth))))
  (values 'a 'b 'c))

(defun test ()
  (sb-sprof:with-profiling (:reset t :max-samples 1000 :report :graph)
    (test-0 6)))
(compile 'test-0)
(compile 'test)
(with-test (:name :with-profiling-return-value)
  (let ((answer
         ;; don't want to actually see the report
         (let ((*standard-output* (make-broadcast-stream)))
           (multiple-value-list (test)))))
    (assert (equal answer '(a b c)))  ))

(defun consalot ()
  (let ((junk '()))
    (loop repeat 10000 do
         (push (make-array 10) junk))
    junk))
(compile 'consalot)
(defun consing-test ()
  ;; This used to test that rapid consing didn't improperly interrupt pseudo-atomic.
  ;; But now that the profiling signal isn't deferrable, I don't really think
  ;; this tests anything.
  (sb-sprof:with-profiling (:reset t
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
    (loop with target = (+ (get-universal-time) 2)
          while (< (get-universal-time) target)
          do (consalot))))
(compile 'consing-test)
(with-test (:name :sprof-consing-test)
  ;; again don't want to actually see the report
  (let ((*standard-output* (make-broadcast-stream)))
    (consing-test))
  ;; For debugging purposes, print output for visual inspection to see where
  ;; the allocation sequence gets hit.
  ;; It can be interrupted even inside pseudo-atomic now.
  (disassemble #'consalot :stream *error-output*))

(load "../contrib/sb-sprof/test.lisp")

(with-test (:name :sprof)
  (with-scratch-file (f "fasl")
    (setq sb-sprof-test::*compiler-input* "../contrib/sb-sprof/graph.lisp"
          sb-sprof-test::*compiler-output* f
          ;; It was supposed to be 100 before I decreased it.
          ;; surely more samples is better, right?
          sb-sprof-test::*sprof-loop-test-max-samples* 100)
    (sb-sprof-test:run-tests)))

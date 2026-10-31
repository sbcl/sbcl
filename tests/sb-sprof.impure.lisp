
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

;;; This test _nearly_ always failed without the corresponding fix to target-thread.
;;; It doesn't definitely fail, but it's better than the 1-in-100 failure rate that
;;; arena.impure had. And amusingly it fails less often under parallel-exec
;; new-arena doesn't exist maybe
#+(and sb-thread arena-allocator (not (or interpreter gc-stress)))
(test-util:with-test (:name :sprof-thread-exit-in-arena)
  (setf (extern-alien "pre_verify_gen_0" int) 1) ; stays until EOF and I intend that
  (let ((arena (sb-vm:new-arena 65536)))
    (unwind-protect
         (progn
           (sb-sprof:reset)
           (sb-sprof:start-profiling :sample-interval 0.001)
           ;; Close the main thread's TLAB and promote *SAMPLES* so the child
           ;; THREAD lands on a fresh nursery page with no caller stack roots.
           (sb-ext:gc)
           (flet ((run-kid ()
                    (let ((th (sb-vm:with-arena (arena)
                                (sb-thread:make-thread
                                 (lambda ()
                                   ;; If we don't get a SIGPROF in 10 million iterations,
                                   ;; just give up trying.
                                   (loop repeat 10000000
                                         while (zerop
                                                (sb-sys:sap-int
                                                 (sb-vm::current-thread-offset-sap
                                                  sb-vm:thread-sprof-data-slot)))))))))
                      (sb-thread:join-thread th)
                      nil)))
             (declare (notinline run-kid))
             (run-kid))
           (sb-sprof:stop-profiling)
           ;; Try to limit ambiguous stack refs that pin things under test
           (sb-sys:scrub-control-stack)
           ;; Rewinding resets freeptr -> allocator_base so GC won't scavenge
           ;; ARENA, while keeping the memory mapped.
           (sb-vm:rewind-arena arena)
           (sb-ext:gc)
           ;; Change (make-broadcast-stream) to *standard-output* if you also
           ;; want to see #<SB-KERNEL:INSTANCE {120...}> or other garbage printed
           ;; from sb-sprof's "Sampled threads:"
           (sb-sprof:report :type :flat :stream (make-broadcast-stream))
           (sb-ext:gc :full t)
           (sb-sprof:reset))
      (sb-vm:destroy-arena arena))))

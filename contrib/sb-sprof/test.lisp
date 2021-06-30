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

;; This has been failing on Sparc/SunOS for a while,
;; having nothing to do with the rewrite of sprof's
;; data collector into C. Maybe it works on Linux
;; but the less fuss about Sparc, the better.
#+sparc (defun run-tests () t)

(defvar *compiler-input* "graph")
(defvar *compiler-output* "./foo.fasl")
(defvar *sprof-loop-test-max-samples* 50)

#-sparc
(defun run-tests ()
  (proclaim '(sb-ext:muffle-conditions style-warning))
  (sb-sprof:with-profiling (:max-samples *sprof-loop-test-max-samples*
                            :report :flat :loop t :show-progress t)
    ;; Notice that "./foo.fasl" writes into this directory, whereas simply "foo.fasl"
    ;; would write into "../../src/code/"
    ;; Notice also that our file I/O routines are so crappy that 15% of the test
    ;; is spent in lseek, and 12% in write. Just wow!
    ;;            Self        Total        Cumul
    ;;   Nr  Count     %  Count     %  Count     %    Calls  Function
    ;; ------------------------------------------------------------------------
    ;;    1     15  15.0     15  15.0     15  15.0        -  foreign function __lseek
    ;;    2     12  12.0     12  12.0     27  27.0        -  foreign function write
    ;;    3      7   7.0      7   7.0     34  34.0        -  foreign function __pthread_sigmask

    ;;
    (compile-file *compiler-input* :output-file *compiler-output* :print nil))
  (delete-file *compiler-output*)
  (let ((*standard-output* (make-broadcast-stream)))
    (test)
    (consing-test)
    ;; This test shows that STOP-SAMPLING and START-SAMPLING on a thread do something.
    ;; Based on rev b6bf65d9 it would seem that the API got broken a little.
    ;; The thread doesn't do a whole lot, which is fine for what it is.
    #+sb-thread
    (let* ((sem (sb-thread:make-semaphore))
           (some-thread (sb-thread:make-thread #'sb-thread:wait-on-semaphore :arguments sem
                                               :name "donothing")))
      (sb-sprof:stop-sampling some-thread)
      (sb-sprof:start-sampling some-thread)
      (sb-thread:signal-semaphore sem)
      ;; Join because when run by run-tests.sh, it's an error to have random leftover threads
      (sb-thread:join-thread some-thread))
    ;; For debugging purposes, print output for visual inspection to see where
    ;; the allocation sequence gets hit.
    ;; It can be interrupted even inside pseudo-atomic now.
    (disassemble #'consalot :stream *error-output*))
  t)

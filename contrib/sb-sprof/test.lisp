(cl:defpackage #:sb-sprof-test
  (:use #:cl #:sb-sprof)
  (:export #:run-tests))

(cl:in-package #:sb-sprof-test)

;#+sb-fasteval (setq sb-ext:*evaluator-mode* :compile)

(defvar *compiler-input* "../contrib/sb-sprof/graph.lisp")
(defvar *compiler-output* "./foo.fasl")
(defvar *sprof-loop-test-max-samples* 50)

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
      (sb-thread:join-thread some-thread)))
  t)

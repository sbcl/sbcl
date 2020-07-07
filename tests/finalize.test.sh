#!/bin/sh
#
# This test is as convoluted as it is to avoid having failing tests
# hang the test-suite, as the typical failure mode used to be SBCL
# hanging uninterruptible in GC.

. ./subr.sh

use_test_subdirectory

echo //entering finalize.test.sh

# $! is not set correctly when calling run_sbcl, do it directly
"$SBCL_RUNTIME" --core "$SBCL_CORE" $SBCL_ARGS <<EOF > /dev/null &
(defvar *tmp* 0.0) ; don't remove - used by the setq below
(defglobal *count* 0)
(declaim (fixnum *count*))

(defun foo (_)
  (declare (ignore _))
  nil)

(defglobal *maxdepth* 0)
;; Be gentler on 32-bit platforms
(defglobal *n-finalized-things* #+64-bit 20000 #-64-bit 10000)

#+sb-thread ; Check that we do want to start a thread, and it hasn't been started.
(assert (eq sb-impl::*finalizer-thread* t))

(let ((junk (mapcar (compile nil '(lambda (_)
                                   (declare (ignore _))
                                   (let ((x (gensym)))
                                     (finalize x (lambda ()
                                                   (setq *maxdepth*
                                                         (max sb-kernel:*free-interrupt-context-index*
                                                              *maxdepth*))
                                                   ;; cons 640K in the finalizer for #+64-bit,
                                                   ;; or 80K for #-64-bit
                                                   (setf *tmp* (make-list #+64-bit 40000
                                                                          #-64-bit 10000))
                                                   (sb-ext:atomic-incf *count*)))
                                     x)))
                    (make-list *n-finalized-things*))))
    (setf junk (foo junk))
    (foo junk))

;;(format *error-output* "About to GC~%")
(gc :full t)

#+sb-thread
(progn
  ;; Verify that the thread was started.
  (assert (typep sb-impl::*finalizer-thread* 'sb-thread::thread))
  ;; We're asserting on an exact count of finalizers that ran, which is a bit sketchy.
  ;; It would mostly be fine to that is within some fudge factor of the expected number,
  ;; however, the purpose of this "stress test" is to verify that we don't miss any
  ;; any finalizations when under stress.
  ;; So to do that, call SCAN-FINALIZERS which actually does the work of helping out
  ;; the finalizer thread. RUN-PENDING-FINALIZERS is not enough - it would just kick
  ;; the thread and return right away, failing our requirement for being synchronous.
  ;; Also, there's no reason to pre- or post-check how may finalizer are pending
  ;; right now- it may be all of them, none of them, or anything in between,
  ;; depending on what the thread has gotten to so far.
  ;; This also shows that it works to run finalization actions in two threads -
  ;; the finalizer thread and this. Hence the need for ATOMIC-INCF on *count*.
  (sb-impl::scan-finalizers))

;;; This was failing with something like:
;;;  The assertion (<= *MAXDEPTH* 1) failed with *MAXDEPTH* = 232.
;;; The test parameters for 64-bit are quite severe, but should not exhaust the heap.
(assert (<= *maxdepth* 1))

(if (= *count* *n-finalized-things*)
    (with-open-file (f "finalize-test-passed" :direction :output)
      (write-line "OK" f))
    (with-open-file (f "finalize-test-failed" :direction :output)
      (format f "OOPS: ~A~%" *count*)
      (sb-kernel:run-pending-finalizers)
      (format f "After sb-kernel:run-pending-finalizers: ~A~%" *count*)))

(sb-ext:quit)
EOF

SBCL_PID=$!
WAITED=x

echo "Waiting for SBCL to finish stress-testing finalizers"
while true; do
    if [ -f finalize-test-passed ]; then
        echo "OK"
        rm finalize-test-passed
        exit $EXIT_TEST_WIN
    elif [ -f finalize-test-failed ]; then
        wait
        cat finalize-test-failed
        rm finalize-test-failed
        exit $EXIT_LOSE
    fi
    sleep 1
    WAITED="x$WAITED"
    if [ $WAITED = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" ]; then
        echo
        echo "timeout, killing SBCL"
        kill -9 $SBCL_PID
        exit $EXIT_LOSE # Failure, SBCL probably hanging in GC
    fi
done


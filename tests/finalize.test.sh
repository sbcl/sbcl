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
(defvar *tmp* 0.0)
(defvar *count* 0)

(defun foo (_)
  (declare (ignore _))
  nil)

(let ((junk (mapcar (compile nil '(lambda (_)
                                   (declare (ignore _))
                                   (let ((x (gensym)))
                                     (finalize x (lambda ()
                                                   ;; cons in finalizer
                                                   (setf *tmp* (make-list 10000))
                                                   (incf *count*)))
                                     x)))
                    (make-list 10000))))
    (setf junk (foo junk))
    (foo junk))

(gc :full t)
(gc :full t)

(if (= *count* 10000)
    (with-open-file (f "finalize-test-passed" :direction :output)
      (write-line "OK" f))
    (with-open-file (f "finalize-test-failed" :direction :output)
      (format f "OOPS: ~A~%" *count*)))

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
        echo "Failed"
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


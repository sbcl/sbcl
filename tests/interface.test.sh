#!/bin/sh

# tests for problems in the interface presented to the user/programmer

# This software is part of the SBCL system. See the README file for
# more information.
#
# While most of SBCL is derived from the CMU CL system, the test
# files (like this one) were written from scratch after the fork
# from CMU CL.
#
# This software is in the public domain and is provided with
# absolutely no warranty. See the COPYING and CREDITS files for
# more information.

. ./subr.sh

use_test_subdirectory

tmpscript=$TEST_FILESTEM.lisp-script

# bug 881445
case "$SBCL_MACHINE_TYPE" in
    X86-64)
        cat > $tmpscript <<EOF
(let ((x (make-array (1- (expt 2 32)) :element-type '(unsigned-byte 8))))
  (assert (> (sb-kernel:dynamic-usage) (length x)))
  ;; prevent compiler from getting too smart...
  (eval x)
  (sb-ext:exit :code $EXIT_LISP_WIN))
EOF
        run_sbcl_with_args --dynamic-space-size 5GB $SBCL_ARGS \
            --eval "(setf sb-ext:*evaluator-mode* :${TEST_SBCL_EVALUATOR_MODE:-compile})" \
            --load $tmpscript
        check_status_maybe_lose "bug 881445" $?
        ;;
esac

run_sbcl --eval '(sb-ext:exit)'
check_status_maybe_lose "simple exit" $? 0 "ok"

run_sbcl --eval '(sb-ext:exit :code 42)'
check_status_maybe_lose "exit with code" $? 42 "ok"

run_sbcl --eval '(progn (defvar *exit-code* 100) (push (lambda () (exit :code (decf *exit-code*))) *exit-hooks*) #+sb-thread (sb-thread:make-thread (lambda () (exit :code 13))) #-sb-thread (exit :code 13))'
check_status_maybe_lose "exit with code" $? 99 "ok"

run_sbcl --eval '(unwind-protect (sb-ext:exit :code 13 :abort t) (sb-ext:exit :code 7 :abort t))'
check_status_maybe_lose "exit with abort" $? 13 "ok"

run_sbcl --eval '(unwind-protect (sb-ext:exit :code 0 :abort t) (sb-ext:exit :code 7 :abort t))'
check_status_maybe_lose "exit with abort and code 0" $? 0 "ok"

run_sbcl --eval '(unwind-protect (sb-ext:exit :code 0 :abort nil) (sb-ext:exit :code 7))'
check_status_maybe_lose "exit with abort and code 0" $? 7 "ok"

exit $EXIT_TEST_WIN

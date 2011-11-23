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

printenv

# bug 881445
case "$SBCL_MACHINE_TYPE" in
    X86-64)
        cat > $tmpscript <<EOF
(let ((x (make-array (1- (expt 2 32)) :element-type '(unsigned-byte 8))))
  (assert (> (sb-kernel:dynamic-usage) (length x)))
  ;; prevent compiler from getting too smart...
  (eval x)
  (sb-ext:quit :unix-status $EXIT_LISP_WIN))
EOF
        run_sbcl_with_args --dynamic-space-size 5GB $SBCL_ARGS --load $tmpscript
        check_status_maybe_lose "bug 881445" $?
        ;;
esac

exit $EXIT_TEST_WIN

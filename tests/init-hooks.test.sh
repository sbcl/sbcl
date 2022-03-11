#!/bin/sh

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

tmpcore="init-hook-test.core"

run_sbcl <<EOF
  (push 'check-no-threads sb-ext:*init-hooks*)
  (defun check-no-threads ()
    (sb-sys:os-exit (if (sb-thread::thread-p sb-impl::*finalizer-thread*) 1 0)))
  (save-lisp-and-die "$tmpcore")
EOF
if [ $? -ne 0 ]; then
    echo "failure saving core"
    exit 1
fi
run_sbcl_with_core "$tmpcore" --noinform --disable-debugger
check_status_maybe_lose "init-hooks execution order" $? 0 "(passed)"

exit $EXIT_TEST_WIN

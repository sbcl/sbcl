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

run_sbcl <<EOF
    (compile-file "./stress-gc.lisp")
    (load *)
    (time (stress-gc ${1:-100000} ${2:-3000}))
    (format t "~&test completed successfully~%")
    (exit :code $EXIT_LISP_WIN)
EOF
check_status_maybe_lose "stress-gc" $?
exit $EXIT_TEST_WIN
#!/bin/sh

# testing ROOM in a fresh SBCL.

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
  (dotimes (i 10)
    (dotimes (j 10)
      (let ((*standard-output* (make-string-output-stream))) (room)))
    #+nil (gc))
  (sb-ext:quit :unix-status $EXIT_LISP_WIN)
EOF
check_status_maybe_lose "room test" $?
# success convention for script
exit $EXIT_TEST_WIN

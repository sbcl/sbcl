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

tmpfilename="$TEST_FILESTEM.lisp"

cat > $tmpfilename <<EOF
    (in-package :cl-user)
    (if (equal (concatenate 'string "Bivalent *STANDARD-INPUT*" (string #\newline))
               (with-output-to-string (s)
                 (loop for byte = (read-byte *standard-input* nil)
                       while byte do (write-char (code-char byte) s))))
        (exit :code $EXIT_LISP_WIN)
        (exit :code $EXIT_LOSE))
EOF
run_sbcl --disable-debugger --load $tmpfilename <<EOF
Bivalent *STANDARD-INPUT*
EOF
check_status_maybe_lose bivalent-standard-input $?

cat > $tmpfilename <<EOF
    (in-package :cl-user)
    (loop for char across "Bivalent *STANDARD-OUTPUT*"
          do (write-byte (char-code char) *standard-output*))
    (terpri *standard-output*)
    (exit :code $EXIT_LISP_WIN)
EOF
run_sbcl --disable-debugger --load $tmpfilename > $tmpfilename.out
check_status_maybe_lose bivalent-standard-output $?
test_output=`cat $tmpfilename.out`
rm -f $tmpfilename.out
if [ 'Bivalent *STANDARD-OUTPUT*' != "$test_output" ]; then
    echo "bad test output: '$test_output'"
    exit $EXIT_LOSE
fi

cat > $tmpfilename <<EOF
    (in-package :cl-user)
    (loop for char across "Bivalent *ERROR-OUTPUT*"
          do (write-byte (char-code char) *error-output*))
    (terpri *error-output*)
    (exit :code $EXIT_LISP_WIN)
EOF
run_sbcl --disable-debugger --load $tmpfilename 2> $tmpfilename.out
check_status_maybe_lose bivalent-error-output $?
test_output=`cat $tmpfilename.out`
rm -f $tmpfilename.out
if [ 'Bivalent *ERROR-OUTPUT*' != "$test_output" ]; then
    echo "bad test output: '$test_output'"
    exit $EXIT_LOSE
fi

# success
exit $EXIT_TEST_WIN

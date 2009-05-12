#!/bin/sh

# tests related to --script

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

echo '(quit :unix-status 7)' > $tmpscript
run_sbcl --script $tmpscript
check_status_maybe_lose "--script exit status from QUIT" $? 7 "(quit status good)"

echo '(error "oops")' > $tmpscript
run_sbcl --script $tmpscript
check_status_maybe_lose "--script exit status from ERROR" $? 1 "(error implies 1)"

echo 'nil'> $tmpscript
run_sbcl --script $tmpscript
check_status_maybe_lose "--script exit status from normal exit" $? 0 "(everything ok)"

rm -f $tmpscript

exit $EXIT_TEST_WIN

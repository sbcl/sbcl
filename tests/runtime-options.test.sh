#!/bin/sh

# tests related to miscellaneous runtime option behavior.

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

tmpout=$TEST_FILESTEM.lisp-out
tmperr=$TEST_FILESTEM.lisp-err

# Test that under --lose-on-corruption and --disable-ldb, in case of
# corruption, (a) the process exits non-zero, (b) all
# lose-on-corruption diagnostics go to stderr, and (c) not stdout.
run_sbcl --noinform --lose-on-corruption --disable-ldb --eval '(labels ((foo () (list (foo)))) (foo))' >$tmpout 2>$tmperr
# default_lossage_handler() calls exit(1).
check_status_maybe_lose "--script exit after corruption" $? 1 "(ok)"
[ -s $tmperr ]
check_status_maybe_lose "--script corruption lossage to stderr" $? 0 "(ok)"
[ -s $tmpout ]
check_status_maybe_lose "--script corruption lossage not to stdout" $? 1 "(ok)"

rm -f $tmpout $tmperr

exit $EXIT_TEST_WIN

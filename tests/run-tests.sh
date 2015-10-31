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

if [ "$1" = "--help" ]; then
    cat <<EOF
Run the regression tests in this directory.

Usage: $0 [OPTIONS] [files]

Options:

  --evaluator-mode              Either compile or interpret. Set the
                                value SB-EXT:*EVALUATOR-MODE* while
                                running tests.
  --break-on-failure            Break into the debugger when a test fails
                                unexpectedly
  --break-on-expected-failure   Break into the debugger when any test fails
  --report-skipped-tests        Include tests :skipped-on target SBCL in
                                the test report.
  --no-color                    Disable coloring of results.

If no test files are specified, runs all tests.
EOF
    exit 0
fi


. ./subr.sh

echo /running tests on \'$SBCL_RUNTIME --core $SBCL_CORE $SBCL_ARGS\'

tenfour () {
    if [ $1 = $EXIT_TEST_WIN ]; then
        echo ok
    else
        echo test failed, expected $EXIT_TEST_WIN return code, got $1
        exit 1
    fi
}
set +u
run_sbcl \
    --eval '(with-compilation-unit () (load "run-tests.lisp"))' \
    --eval '(run-tests::run-all)' $*

tenfour $?

echo '//apparent success (reached end of run-tests.sh normally)'
date

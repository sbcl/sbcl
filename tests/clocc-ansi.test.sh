#!/bin/sh

# Run clocc's ansi-test suite on SBCL (if you set the appropriate
# environment variable so that the test suite, a separate piece of
# software, can be found).
#
# This is implemented as a shell script because ansi-test likes to
# report its errors on standard output and it's convenient to use the
# *nix shell tools to extract them.

# clocc = Common Lisp Open Code Collection, available on
#         <http://clocc.sourceforge.net/>
# ansi-test = one of the subdirectories in clocc, containing lotso tests
#             for ANSI compliance (and the occasional test for CLISP
#             compatibility too:-)

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

# Find clocc ansi-test (or just punt, returning success).
set +u
if [ "$SBCL_CLOCC_ANSI_TEST" = "" ] ; then
    echo //punting clocc ansi-test because SBCL_CLOCC_ANSI_TEST is undefined
    exit $EXIT_TEST_WIN
else
    echo //going on to run clocc ansi-test in $SBCL_CLOCC_ANSI_TEST
    cd $SBCL_CLOCC_ANSI_TEST
fi
set -u

# The condition system is for the weak.
tmpprefix="${TMPDIR:-/tmp}/sbcl-clocc-ansi-test-$$"
rawfilename="$tmpprefix-raw.tmp"
bugsfilename="$tmpprefix-bugs.tmp"

# Go SBCL go.
run_sbcl <<EOF >$rawfilename
(in-package :cl-user)
;;; Tell ansi-test about our known bugs.
(load "$SBCL_PWD/clocc-ansi-test-known-bugs.lisp")
;;; Actually run ansi-test.
(load "tests.lisp")
;;; Return a special status code to show that we reached the end
;;; normally instead of taking a dirt nap.
(print "back from ansi-test tests.lisp")
(sb-ext:quit :unix-status 52)
EOF
if [ $? != 52 ]; then
    echo "failure: SBCL didn't finish running clocc ansi-test."
    exit 1
fi

# Klingon programmers handle errors by recognizing error strings
# in standard output.
if egrep 'ERROR!!' $rawfilename > $bugsfilename; then
    # new bugs, better luck next time
    cat $bugsfilename
    exit 1
else
    # only known bugs, happy happy joy joy
    rm $rawfilename $bugsfilename
    exit 104
fi

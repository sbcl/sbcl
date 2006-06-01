#!/bin/sh

# Run the regression tests in this directory.
#
# Usage: run-tests.sh [--break-on-failure] [--break-on-expected-failure] [files]
#  --break-on-failure            Break into the debugger when a test fails
#                                unexpectedly
#  --break-on-expected-failure   Break into the debugger when any test fails
#
# If no test files are specified, runs all tests.

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

# how we invoke SBCL in the tests
#
# Until sbcl-0.6.12.8, the shell variable SBCL was bound to a relative
# pathname, but now we take care to bind it to an absolute pathname (still
# generated relative to `pwd` in the tests/ directory) so that tests
# can chdir before invoking SBCL and still work.
. ../sbcl-pwd.sh
sbcl_pwd

SBCL_HOME=$SBCL_PWD/../contrib
export SBCL_HOME
sbclstem=$SBCL_PWD/../src/runtime/sbcl

SBCL="$sbclstem --core $SBCL_PWD/../output/sbcl.core --noinform --sysinit /dev/null --userinit /dev/null --noprint --disable-debugger"
export SBCL
echo /running tests on SBCL=\'$SBCL\'
# more or less like SBCL, but without enough grot removed that appending
# a --core command line argument works
#
# (KLUDGE: and also without any magic to suppress --userinit and
# --sysinit, so if you use it in a test, you need to add those
# yourself if you want things to be clean. If many tests start using
# this, we can redo it as a shell function or something so that the
# magic can be done once and only once.). Not used in this file, but
# exists for the benefit of the *.test.sh files that can be started by
# run-tests.lisp
SBCL_ALLOWING_CORE=$sbclstem
export SBCL_ALLOWING_CORE
echo /with SBCL_ALLOWING_CORE=\'$SBCL_ALLOWING_CORE\'

LANG=C
LC_ALL=C
export LANG
export LC_ALL

# "Ten four" is the closest numerical slang I can find to "OK", so
# it's the Unix status value that we expect from a successful test.
# (Of course, zero is the usual success value, but we don't want to
# use that because SBCL returns that by default, so we might think
# we passed a test when in fact some error caused us to exit SBCL
# in a weird unexpected way. In contrast, 104 is unlikely to be
# returned unless we exit through the intended explicit "test
# successful" path.
tenfour () {
    if [ $1 = 104 ]; then
        echo ok
    else
        echo test $2 failed, expected 104 return code, got $1
        exit 1
    fi
}

$SBCL --eval '(with-compilation-unit () (load "run-tests.lisp"))' \
  --eval '(run-tests::run-all)' $*

tenfour $?

echo '//apparent success (reached end of run-tests.sh normally)'
date

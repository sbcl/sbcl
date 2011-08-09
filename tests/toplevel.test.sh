#!/bin/sh

# tests related to the toplevel interface: command line parsing
# and outer REPL

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

# Until sbcl-0.pre8, all --eval arguments were parsed before any of
# them were executed, making it impossible for --eval forms to refer
# to packages created by --eval forms.
run_sbcl --eval "(defpackage :foo)" --eval "(print 'foo::bar)" \
  < /dev/null > $TEST_FILESTEM
if [ "`grep -c FOO::BAR $TEST_FILESTEM`" != 1 ] ; then
    echo failed DEFPACKAGE-then-PRINT from --eval form
    exit $EXIT_LOSE
fi

# --quit
run_sbcl --eval "(defpackage :foo)" --eval "(print 'foo::bar)" --quit \
    > $TEST_FILESTEM
if [ "`grep -c FOO::BAR $TEST_FILESTEM`" != 1 ] ; then
    echo failed DEFPACKAGE-then-PRINT with --quit
    exit $EXIT_LOSE
fi
# --quit gets delayed
run_sbcl --eval "(defpackage :foo)" --quit --eval "(print 'foo::bar)" \
    > $TEST_FILESTEM
if [ "`grep -c FOO::BAR $TEST_FILESTEM`" != 1 ] ; then
    echo failed DEFPACKAGE-then-PRINT with delayed --quit
    exit $EXIT_LOSE
fi

# --non-interactive
run_sbcl --eval "(defpackage :foo)" \
    --non-interactive \
    --eval "(print 'foo::bar)" \
    > $TEST_FILESTEM
if [ "`grep -c FOO::BAR $TEST_FILESTEM`" != 1 ] ; then
    echo failed DEFPACKAGE-then-PRINT with --non-interactive
    exit $EXIT_LOSE
fi

# disable the use of --disable-debugger through run_sbcl for the rest of
# this file
SBCL_ARGS="--noinform --no-sysinit --no-userinit --noprint"

# --non-interactive with error
#
# (This test would hang if --non-interactive did not work properly.)
run_sbcl --eval "(defpackage :foo)" \
    --non-interactive \
    --eval "(print 'foo::bar)" \
    --eval '(error "fail-safe")' \
    >$TEST_FILESTEM 2>&1
if [ "`grep -c FOO::BAR $TEST_FILESTEM`" != 1 ] ; then
    echo failed DEFPACKAGE-then-PRINT with --non-interactive and error
    exit $EXIT_LOSE
fi
if [ "`grep -c -i SIMPLE-ERROR $TEST_FILESTEM`" -lt 1 ] ; then
    echo no error seen in --non-interactive test
    exit $EXIT_LOSE
fi

exit $EXIT_TEST_WIN

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

# --script
run_sbcl --script script-test.lisp --eval foo \
  < /dev/null > $TEST_FILESTEM
if [ "`grep -c :SCRIPT-OK $TEST_FILESTEM`" != 1 ] ; then
   echo "failed --script test"
   exit $EXIT_LOSE
fi
exit $EXIT_TEST_WIN

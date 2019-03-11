#!/bin/sh

# tests related to ELFinated .core files

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

run_sbcl --noinform <<EOF
  #+(and x86-64 linux immobile-code) (exit :code 0) ; good
 (exit :code 2) ; otherwise
EOF
status=$?
if [ $status != 0 ]; then # test can't be executed
    # we don't have a way to exit shell tests with "inapplicable" as the result
    exit $EXIT_TEST_WIN
fi
set -e # fail if Make fails
(cd ../src/runtime ; make shrinkwrap-sbcl)

../src/runtime/shrinkwrap-sbcl \
  --disable-debugger \
  --eval '(dotimes (i 100000) (sb-vm::alloc-immobile-fdefn))' \
  --quit

# reaching here means no crash happened in the allocator
exit $EXIT_TEST_WIN

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
  #+(and x86-64 linux immobile-code sb-dynamic-core) (exit :code 0) ; good
 (exit :code 2) ; otherwise
EOF
status=$?
if [ $status != 0 ]; then # test can't be executed
    # we don't have a way to exit shell tests with "inapplicable" as the result
    exit $EXIT_TEST_WIN
fi

# If running in a test sandbox that doesn't provide Make but has already
# provided the necessary executable file, just use that.
# Unfortunately this removes enforcement that the test binary be up-to-date
# with respect to sources for local builds, but I think the greater evil
# would be to spuriously pass by exiting with success if we couldn't build.
if [ ! -x $SBCL_PWD/../src/runtime/shrinkwrap-sbcl ]
then
  set -e # fail if Make fails
  (cd $SBCL_PWD/../src/runtime ; make shrinkwrap-sbcl)
fi

$SBCL_PWD/../src/runtime/shrinkwrap-sbcl \
  --disable-debugger \
  --eval '(dotimes (i 100000) (sb-vm::alloc-immobile-fdefn))' \
  --quit

echo ok
# reaching here means no crash happened in the allocator
exit $EXIT_TEST_WIN

#!/bin/sh

# tests related to compressed .core files

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

tmpcore=$TEST_FILESTEM.core

run_sbcl <<EOF
  (save-lisp-and-die "$tmpcore" :toplevel (lambda () 42)
                      :compression (and (member :sb-core-compression *features*) t))
EOF
run_sbcl_with_core "$tmpcore" --noinform --no-userinit --no-sysinit \
    --eval "(setf sb-ext:*evaluator-mode* :${TEST_SBCL_EVALUATOR_MODE:-compile})"
check_status_maybe_lose "SAVE-LISP-AND-DIE :COMPRESS" $? 0 "(compressed saved core ran)"

rm "$tmpcore"
run_sbcl <<EOF
  (save-lisp-and-die "$tmpcore" :toplevel (lambda () 42) :executable t
                     :compression (and (member :sb-core-compression *features*) t))
EOF
chmod u+x "$tmpcore"
./"$tmpcore" --no-userinit --no-sysinit
check_status_maybe_lose "SAVE-LISP-AND-DIE :EXECUTABLE-COMPRESS" $? 0 "(executable compressed saved core ran)"

exit $EXIT_TEST_WIN

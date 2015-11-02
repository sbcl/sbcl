#!/bin/sh

# tests related to loading init files

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

tmpcore="init-test.core"

run_sbcl <<EOF
  (require :sb-introspect)
  (defun custom-userinit-pathname ()
     "$SBCL_PWD/custom-userinit.lisp")
  (defun custom-sysinit-pathname ()
     "$SBCL_PWD/custom-sysinit.lisp")
  (setf sb-impl::*userinit-pathname-function* 'custom-userinit-pathname
        sb-impl::*sysinit-pathname-function* 'custom-sysinit-pathname)
  (save-lisp-and-die "$tmpcore")
EOF
if [ $? != 0 ]; then
    echo "failure saving core"
    exit 1
fi
run_sbcl_with_core "$tmpcore" --disable-debugger \
    --eval "(setf sb-ext:*evaluator-mode* :${TEST_SBCL_EVALUATOR_MODE:-compile})" \
    <<EOF
  (assert (string= (custom-sysinit-pathname)
                   (namestring
                    (sb-introspect:definition-source-pathname
                     (car (sb-introspect:find-definition-sources-by-name
                           'sysinit-21 :function))))))
  (userinit-quit (sysinit-21))
EOF
check_status_maybe_lose "userinit and sysinit loading" $? 21 "(loading worked)"

exit $EXIT_TEST_WIN

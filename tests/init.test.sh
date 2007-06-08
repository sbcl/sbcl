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

tmpcore="init-test-sh-$$.core"
rm -f $tmpcore

$SBCL <<EOF
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
$SBCL_ALLOWING_CORE --core "$tmpcore" --disable-debugger <<EOF
  (userinit-quit (sysinit-21))
EOF
if [ $? = 21 ]; then
    echo "/Default userinit and sysinit loading worked, good"
else
    echo "failure loading user/sysinit files: $?"
    exit 1
fi

rm -f $tmpcore
echo "/returning success from init.test.sh"
exit 104

#!/bin/sh

# tests related to foreign function interface and LOAD-FOREIGN

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

testfilestem=$TMPDIR/sbcl-foreign-test-$$

echo 'int summish(int x, int y) { return 1 + x + y; }' > $testfilestem.c
make $testfilestem.o
ld -shared -o $testfilestem.so $testfilestem.o

${SBCL:-sbcl} <<EOF
  (when (fboundp 'load-foreign) ; not necessarily supported on all OSes..
    (load-foreign '("$testfilestem.so"))
    (def-alien-routine summish int (x int) (y int))
    (assert (= (summish 10 20) 31)))
  (sb-ext:quit :unix-status 52) ; success convention for Lisp program
EOF
if [ $? != 52 ]; then
    echo test failed: $?
    exit 1
fi

# FIXME: I rewrote the handling of ENV/ENVIRONMENT arguments for
# LOAD-FOREIGN, but I can't think of a nice way to test it. (Kent Beck
# would cry. If he didn't keel over on the spot and then commence
# rolling over in his grave.:-) It would be good to make a test case
# for it..

# success convention for script
exit 104

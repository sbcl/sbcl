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

echo //entering foreign.test.sh

testfilestem=${TMPDIR:-/tmp}/sbcl-foreign-test-$$

# Make a little shared object file to test with.
echo 'int summish(int x, int y) { return 1 + x + y; }' > $testfilestem.c
cc -c $testfilestem.c -o $testfilestem.o
ld -shared -o $testfilestem.so $testfilestem.o

# Test interaction with the shared object file.
${SBCL:-sbcl} <<EOF
  (define-alien-variable environ (* c-string))
  (defvar *environ* environ)
  (handler-case 
      (load-foreign '("$testfilestem.so"))
    (sb-int:unsupported-operator ()
     ;; At least as of sbcl-0.7.0.5, LOAD-FOREIGN isn't supported
     ;; on every OS. In that case, there's nothing to test, and we
     ;; can just fall through to success.
     (sb-ext:quit :unix-status 52))) ; success convention for Lisp program
  ;; Test that loading an object file didn't screw up our records
  ;; of variables visible in runtime. (This was a bug until 
  ;; Nikodemus Siivola's patch in sbcl-0.8.5.50.)
  (assert (= (sb-sys:sap-int (alien-sap *environ*))
             (sb-sys:sap-int (alien-sap environ))))
  (define-alien-routine summish int (x int) (y int))
  (assert (= (summish 10 20) 31))
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

echo //cleanup: removing $testfilestem.*
rm $testfilestem.*

# success convention for script
exit 104 

#!/bin/sh

# tests related to foreign function interface and loading of shared
# libraries

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

# simple way to make sure we're not punting by accident:
# setting PUNT to anything other than 104 will make non-dlopen
# and non-linkage-table platforms fail this
PUNT=104

testfilestem=${TMPDIR:-/tmp}/sbcl-foreign-test-$$

# Make a little shared object file to test with.
echo 'int summish(int x, int y) { return 1 + x + y; }' > $testfilestem.c
echo 'int numberish = 42;' >> $testfilestem.c
echo 'int nummish(int x) { return numberish + x; }' >> $testfilestem.c
cc -c $testfilestem.c -o $testfilestem.o
ld -shared -o $testfilestem.so $testfilestem.o

# Foreign definitions & load
cat > $testfilestem.def.lisp <<EOF
  (define-alien-variable environ (* c-string))
  (defvar *environ* environ)
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (handler-case 
        (load-shared-object "$testfilestem.so")
      (sb-int:unsupported-operator ()
        ;; At least as of sbcl-0.7.0.5, LOAD-SHARED-OBJECT isn't
        ;; supported on every OS. In that case, there's nothing to test,
        ;; and we can just fall through to success.
        (sb-ext:quit :unix-status 22)))) ; catch that
  (define-alien-routine summish int (x int) (y int))
  (define-alien-variable numberish int)
  (define-alien-routine nummish int (x int))

  ;; Test that loading an object file didn't screw up our records
  ;; of variables visible in runtime. (This was a bug until 
  ;; Nikodemus Siivola's patch in sbcl-0.8.5.50.)
  ;;
  ;; This cannot be tested in a saved core, as there is no guarantee
  ;; that the location will be the same.
  (assert (= (sb-sys:sap-int (alien-sap *environ*))
             (sb-sys:sap-int (alien-sap environ))))
EOF

# Test code
cat > $testfilestem.test.lisp <<EOF
  (assert (= (summish 10 20) 31))
  (assert (= 42 numberish))
  (setf numberish 13)
  (assert (= 13 numberish))
  (assert (= 14 (nummish 1)))
  (sb-ext:quit :unix-status 52) ; success convention for Lisp program
EOF

${SBCL:-sbcl} --eval "(progn (compile-file #p\"$testfilestem.def.lisp\") (sb-ext:quit :unix-status 52))"
if [ $? = 52 ] ; then :
else
    # we can't compile the test file. something's wrong.
    rm $testfilestem.*
    echo test failed: $?
    exit 1
fi

${SBCL:-sbcl} --load $testfilestem.def.fasl --load $testfilestem.test.lisp
RET=$?
if [ $RET = 22 ]; then
    rm $testfilestem.*
    exit $PUNT # success -- load-shared-object not supported
elif [ $RET != 52 ]; then
    rm $testfilestem.*
    echo test failed: $?
    exit 1 
fi

${SBCL:-sbcl} --load $testfilestem.def.fasl --eval "(when (member :linkage-table *features*) (save-lisp-and-die \"$testfilestem.core\"))" <<EOF
  (sb-ext:quit :unix-status 22) ; catch this
EOF
if [ $? = 22 ]; then
    rm $testfilestem.*
    exit $PUNT # success -- linkage-table not available
fi

$SBCL_ALLOWING_CORE --core $testfilestem.core --sysinit /dev/null --userinit /dev/null --load $testfilestem.test.lisp
if [ $? != 52 ]; then
    rm $testfilestem.*
    echo test failed: $?
    exit 1 # Failure
fi

rm $testfilestem.*

# success convention for script
exit 104 

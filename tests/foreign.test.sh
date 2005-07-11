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

## Make a little shared object files to test with.

build_so() {
  echo building $1.so
  if [ "$(uname -m)" = x86_64 ]; then
    CFLAGS="$CFLAGS -fPIC"
  fi
  if [ "$(uname)" = Darwin ]; then
    SO_FLAGS="-bundle"
  else
    SO_FLAGS="-shared"
  fi
  cc -c $1.c -o $1.o $CFLAGS
  ld $SO_FLAGS -o $1.so $1.o
}
    
echo 'int summish(int x, int y) { return 1 + x + y; }' > $testfilestem.c
echo 'int numberish = 42;' >> $testfilestem.c
echo 'int nummish(int x) { return numberish + x; }' >> $testfilestem.c
echo 'short negative_short() { return -1; }' >> $testfilestem.c
echo 'int negative_int()     { return -2; }' >> $testfilestem.c
echo 'long negative_long()   { return -3; }' >> $testfilestem.c
build_so $testfilestem

echo 'int foo = 13;' > $testfilestem-b.c
echo 'int bar() { return 42; }' >> $testfilestem-b.c
build_so $testfilestem-b

echo 'int foo = 42;' > $testfilestem-b2.c
echo 'int bar() { return 13; }' >> $testfilestem-b2.c
build_so $testfilestem-b2

echo 'int late_foo = 43;' > $testfilestem-c.c
echo 'int late_bar() { return 14; }' >> $testfilestem-c.c
build_so $testfilestem-c

## Foreign definitions & load

cat > $testfilestem.def.lisp <<EOF
  (define-alien-variable environ (* c-string))
  (defvar *environ* environ)
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (handler-case
        (progn
          (load-shared-object "$testfilestem.so")
          (load-shared-object "$testfilestem-b.so"))
      (sb-int:unsupported-operator ()
        ;; At least as of sbcl-0.7.0.5, LOAD-SHARED-OBJECT isn't
        ;; supported on every OS. In that case, there's nothing to test,
        ;; and we can just fall through to success.
        (sb-ext:quit :unix-status 22)))) ; catch that
  (define-alien-routine summish int (x int) (y int))
  (define-alien-variable numberish int)
  (define-alien-routine nummish int (x int))
  (define-alien-variable "foo" int)
  (define-alien-routine "bar" int)

  (define-alien-routine "negative_short" short)
  (define-alien-routine "negative_int" int)
  (define-alien-routine "negative_long" long)

  ;; compiling this gets us the FOP-FOREIGN-DATAREF-FIXUP on
  ;; linkage-table ports
  (defvar *extern* (extern-alien "negative_short" short))

  ;; Test that loading an object file didn't screw up our records
  ;; of variables visible in runtime. (This was a bug until 
  ;; Nikodemus Siivola's patch in sbcl-0.8.5.50.)
  ;;
  ;; This cannot be tested in a saved core, as there is no guarantee
  ;; that the location will be the same.
  (assert (= (sb-sys:sap-int (alien-sap *environ*))
             (sb-sys:sap-int (alien-sap environ))))

  ;; automagic restarts
  (setf *debugger-hook* 
        (lambda (condition hook)
          (print (list :debugger-hook condition))
          (let ((cont (find-restart 'continue condition)))
            (when cont
              (invoke-restart cont)))
          (print :fell-through)
          (invoke-debugger condition)))
EOF

# Test code
cat > $testfilestem.test.lisp <<EOF
  (assert (= (summish 10 20) 31))
  (assert (= 42 numberish))
  (setf numberish 13)
  (assert (= 13 numberish))
  (assert (= 14 (nummish 1)))

  (assert (= -1 (negative-short)))
  (assert (= -2 (negative-int)))
  (assert (= -3 (negative-long)))

  (print :stage-1)

  ;; test realoading object file with new definitions
  (assert (= 13 foo))
  (assert (= 42 (bar)))
  (rename-file "$testfilestem-b.so" "$testfilestem-b.bak")
  (rename-file "$testfilestem-b2.so" "$testfilestem-b.so")
  (load-shared-object "$testfilestem-b.so")
  (assert (= 42 foo))
  (assert (= 13 (bar)))
  (rename-file "$testfilestem-b.so" "$testfilestem-b2.so")
  (rename-file "$testfilestem-b.bak" "$testfilestem-b.so")

  (print :stage-2)

  ;; test late resolution
  (define-alien-variable late-foo int)
  (define-alien-routine late-bar int)
  (multiple-value-bind (val err) (ignore-errors late-foo)
    (assert (not val))
    (assert (typep err 'undefined-alien-error)))
  (multiple-value-bind (val err) (ignore-errors (late-bar))
    (assert (not val))
    (assert (typep err 'undefined-alien-error)))
  (load-shared-object "$testfilestem-c.so")
  (assert (= 43 late-foo))
  (assert (= 14 (late-bar)))

  (print :stage-3)

  (sb-ext:quit :unix-status 52) ; success convention for Lisp program
EOF

${SBCL:-sbcl} --eval "(progn (load (compile-file #p\"$testfilestem.def.lisp\")) (sb-ext:quit :unix-status 52))"
if [ $? = 52 ]; then
    true # nop
else
    # we can't compile the test file. something's wrong.
    rm $testfilestem.*
    echo test failed: $?
    exit 1
fi

echo compile ok

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

echo load ok

${SBCL:-sbcl} --load $testfilestem.def.fasl --eval "(when (member :linkage-table *features*) (save-lisp-and-die \"$testfilestem.core\"))" <<EOF
  (sb-ext:quit :unix-status 22) ; catch this
EOF
if [ $? = 22 ]; then
    rm $testfilestem.*
    exit $PUNT # success -- linkage-table not available
fi

echo table ok

${SBCL_ALLOWING_CORE:-sbcl} --core $testfilestem.core --sysinit /dev/null --userinit /dev/null --load $testfilestem.test.lisp
if [ $? != 52 ]; then
    rm $testfilestem.*
    echo test failed: $?
    exit 1 # Failure
fi

echo start ok

# missing object file
rm $testfilestem-b.so $testfilestem-b2.so
${SBCL_ALLOWING_CORE:-sbcl} --core $testfilestem.core --sysinit /dev/null --userinit /dev/null <<EOF
  (assert (= 22 (summish 10 11)))
  (multiple-value-bind (val err) (ignore-errors (eval 'foo))
    (assert (not val))
    (assert (typep err 'undefined-alien-error)))
  (multiple-value-bind (val err) (ignore-errors (eval '(bar)))
    (assert (not val))
    (assert (typep err 'undefined-alien-error)))
  (quit :unix-status 52)
EOF
if [ $? != 52 ]; then
    rm $testfilestem.*
    echo test failed: $?
    exit 1 # Failure
fi

echo missing ok

rm -f $testfilestem.* $testfilestem-*

# success convention for script
exit 104 

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

## Make some shared object files to test with.

build_so() {
  echo building $1.so
  if [ "`uname -m`" = x86_64 ]; then
    CFLAGS="$CFLAGS -fPIC"
  fi
  if [ "`uname`" = Darwin ]; then
    SO_FLAGS="-bundle"
  else
    SO_FLAGS="-shared"
  fi
  cc -c $1.c -o $1.o $CFLAGS
  ld $SO_FLAGS -o $1.so $1.o  
}

cat > $testfilestem.c <<EOF
int summish(int x, int y) { return 1 + x + y; }

int numberish = 42;

int nummish(int x) { return numberish + x; }

short negative_short() { return -1; }
int negative_int()     { return -2; }
long negative_long()   { return -3; }

long long powish(unsigned int x, unsigned int y) {
  long long acc = 1;
  long long xx = (long long) x;
  for(; y != 1; y /= 2) {
    if (y & 1) {
      acc *= xx;
      y -= 1;
    }
    xx *= xx;
  }
  return xx*acc;
}

float return9th(float f1, float f2, float f3, float f4, float f5, 
		float f6, float f7, float f8, float f9, float f10, 
		float f11, float f12) { 
    return f9; 
}

double return9thd(double f1, double f2, double f3, double f4, double f5, 
		  double f6, double f7, double f8, double f9, double f10,
		  double f11, double f12) { 
    return f9; 
}

int long_test8(int a1, int a2, int a3, int a4, int a5, 
	       int a6, int a7, long long l1) { 
    return (l1 == powish(2,34));
}

int long_test9(int a1, int a2, int a3, int a4, int a5, 
	       int a6, int a7, long long l1, int a8) { 
    return (l1 == powish(2,35));
}

int long_test2(int i1, int i2, int i3, int i4, int i5, int i6,
	       int i7, int i8, int i9, long long l1, long long l2) {
    return (l1 == (1 + powish(2,37)));
}

long long return_long_long() {
    return powish(2,33);
}
EOF

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

cat > $testfilestem.base.lisp <<EOF
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

  (define-alien-routine return9th float (input1 float) (input2 float) (input3 float) (input4 float) (input5 float) (input6 float) (input7 float) (input8 float) (input9 float) (input10 float) (input11 float) (input12 float))
  (define-alien-routine return9thd double (f1 double) (f2 double) (f3 double) (f4 double) (f5 double) (f6 double) (f7 double) (f8 double) (f9 double) (f10 double) (f11 double) (f12 double))

  (define-alien-routine long-test8 int (int1 int) (int2 int) (int3 int) (int4 int) (int5 int) (int6 int) (int7 int) (long1 (integer 64)))
  (define-alien-routine long-test9 int (int1 int) (int2 int) (int3 int) (int4 int) (int5 int) (int6 int) (int7 int) (long1 (integer 64)) (int8 int))
  (define-alien-routine long-test2 int (int1 int) (int2 int) (int3 int) (int4 int) (int5 int) (int6 int) (int7 int) (int8 int) (int9 int) (long1 (integer 64)) (long2 (integer 64)))
  (define-alien-routine return-long-long (integer 64))

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
  (enable-debugger)
  ;; automagic restarts
  (setf *debugger-hook*
        (lambda (condition hook)
          (princ condition)
          (let ((cont (find-restart 'continue condition)))
            (when cont
              (invoke-restart cont)))
          (print :fell-through)
          (invoke-debugger condition)))
EOF

echo "(declaim (optimize speed))" > $testfilestem.fast.lisp
cat $testfilestem.base.lisp >> $testfilestem.fast.lisp

echo "(declaim (optimize space))" > $testfilestem.small.lisp
cat $testfilestem.base.lisp >> $testfilestem.small.lisp

# Test code
cat > $testfilestem.test.lisp <<EOF
  (assert (= 31 (summish 10 20)))
  (assert (= 42 numberish))
  (setf numberish 13)
  (assert (= 13 numberish))
  (assert (= 14 (nummish 1)))

  (assert (= -1 (negative-short)))
  (assert (= -2 (negative-int)))
  (assert (= -3 (negative-long)))

  (assert (= 9.0s0 (return9th 1.0s0 2.0s0 3.0s0 4.0s0 5.0s0 6.0s0 7.0s0 8.0s0 9.0s0 10.0s0 11.0s0 12.0s0)))
  (assert (= 9.0d0 (return9thd 1.0d0 2.0d0 3.0d0 4.0d0 5.0d0 6.0d0 7.0d0 8.0d0 9.0d0 10.0d0 11.0d0 12.0d0)))

  (assert (= 1 (long-test8 1 2 3 4 5 6 7 (ash 1 34))))
  (assert (= 1 (long-test9 1 2 3 4 5 6 7 (ash 1 35) 8)))
  (assert (= 1 (long-test2 1 2 3 4 5 6 7 8 9 (+ 1 (ash 1 37)) 15)))
  (assert (= (ash 1 33) (return-long-long)))

  (print :stage-1)

  ;; test reloading object file with new definitions
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
  #+linkage-table
  (progn
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
    (assert (= 14 (late-bar))))

  (print :stage-3)

  (sb-ext:quit :unix-status 52) ; success convention for Lisp program
EOF

test_compile() {
    ${SBCL:-sbcl} --eval "(progn (load (compile-file #p\"$testfilestem.$1.lisp\")) (sb-ext:quit :unix-status 52))"
    if [ $? = 52 ]; then
        echo test compile $1 ok
    else
        # we can't compile the test file. something's wrong.
        # rm $testfilestem.*
        echo test compile $1 failed: $?
	exit 1
    fi
}

test_compile fast
test_compile small

test_use() {
    ${SBCL:-sbcl} --load $testfilestem.$1.fasl --load $testfilestem.test.lisp
    RET=$?
    if [ $RET = 22 ]; then
	rm $testfilestem.*
	exit $PUNT # success -- load-shared-object not supported
    elif [ $RET != 52 ]; then
	rm $testfilestem.*
	echo test use $1 failed: $?
	exit 1
    else
	echo test use $1 ok
    fi
}

test_use small
test_use fast

test_save() {
    ${SBCL:-sbcl} --load $testfilestem.$1.fasl --eval "(when (member :linkage-table *features*) (save-lisp-and-die \"$testfilestem.$1.core\"))" <<EOF
  (sb-ext:quit :unix-status 22) ; catch this
EOF
    if [ $? = 22 ]; then
	rm $testfilestem.*
	exit $PUNT # success -- linkage-table not available
    else
	echo save $1 ok
    fi
}

test_save small
test_save fast

test_start() {
    ${SBCL_ALLOWING_CORE:-sbcl} --core $testfilestem.$1.core --sysinit /dev/null --userinit /dev/null --load $testfilestem.test.lisp
    if [ $? != 52 ]; then
	rm $testfilestem.*
	echo test failed: $?
	exit 1 # Failure
    else
	echo test start $1 ok
    fi
}

test_start fast
test_start small

# missing object file
rm $testfilestem-b.so $testfilestem-b2.so
${SBCL_ALLOWING_CORE:-sbcl} --core $testfilestem.fast.core --sysinit /dev/null --userinit /dev/null <<EOF
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

echo missing .so ok

rm -f $testfilestem.* $testfilestem-*

# success convention for script
exit 104

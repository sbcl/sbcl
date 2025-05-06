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

run_compiler=`pwd`/run-compiler.sh
primary_c_source=`pwd`/foreigntest.c
. ./expect.sh
. ./subr.sh

use_test_subdirectory

echo //entering foreign.test.sh

## Make some shared object files to test with.

build_so() (
  if [ $# -eq 2 ]
  then
    echo building $1.so from $2
    /bin/sh ${run_compiler} -sbcl-pic -sbcl-shared "$2" -o "$1.so"
  else
    echo building $1.so
    /bin/sh ${run_compiler} -sbcl-pic -sbcl-shared "$1.c" -o "$1.so"
  fi
)

# We want to bail out in case any of these Unix programs fails.
set -e

build_so $TEST_FILESTEM $primary_c_source

echo 'int foo = 13;' > $TEST_FILESTEM-b.c
echo 'int bar() { return 42; }' >> $TEST_FILESTEM-b.c
build_so $TEST_FILESTEM-b

echo 'int foo = 42;' > $TEST_FILESTEM-b2.c
# Try not to produce two more-or-less physically identical '.so' files (modulo
# some constants in the functions), so that the test will fail if we accidentally
# call the new bar() at the old address - as might happen if the OS maps both '.so'
# files at the same place - as a consequence of failing to update the linkage table.
echo 'int bar(void);' >> $TEST_FILESTEM-b2.c
echo 'int somerandomfun(int x) { return x?-x:bar(); }' >> $TEST_FILESTEM-b2.c
echo 'int bar() { return 13; }' >> $TEST_FILESTEM-b2.c
build_so $TEST_FILESTEM-b2

echo 'int late_foo = 43;' > $TEST_FILESTEM-c.c
echo 'int late_bar() { return 14; }' >> $TEST_FILESTEM-c.c
build_so $TEST_FILESTEM-c

cat > $TEST_FILESTEM-noop-dlclose-test.c <<EOF
#ifdef _WIN32
int dlclose_is_noop() { return 1; }
#else
// Why is this test which establishes preconditions of the actual test
// expressed as a loadable library? It's ridiculousy obfuscated imho.
#include <dlfcn.h>
#include <stddef.h>

int dlclose_is_noop () {
#ifdef RTLD_NOLOAD
    void * handle = dlopen("./$TEST_FILESTEM-noop-dlclose-test-helper.so", RTLD_NOW | RTLD_GLOBAL);
    dlclose(handle);

    handle = dlopen("./$TEST_FILESTEM-noop-dlclose-test-helper.so", RTLD_NOW | RTLD_NOLOAD);
    if (handle != NULL) {
        return 1;
    }
#endif
    return 0;
}
#endif
EOF
build_so $TEST_FILESTEM-noop-dlclose-test

cat > $TEST_FILESTEM-noop-dlclose-test-helper.c <<EOF
int sbcl_dlclose_test = 42;
EOF
build_so $TEST_FILESTEM-noop-dlclose-test-helper

## Foreign definitions & load

cat > $TEST_FILESTEM.base.lisp <<EOF
  (declaim (muffle-conditions compiler-note))
  (define-alien-variable environ (* c-string))
  #+unix (defvar *environ* environ)
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (handler-case
        (progn
          (load-shared-object (truename "$TEST_FILESTEM.so"))
          (load-shared-object (truename "$TEST_FILESTEM-b.so")))
      (sb-int:unsupported-operator ()
        ;; At least as of sbcl-0.7.0.5, LOAD-SHARED-OBJECT isn't
        ;; supported on every OS. In that case, there's nothing to test,
        ;; and we can just fall through to success.
        (sb-ext:exit :code 22)))) ; catch that
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
  (define-alien-routine long-sap-test1 int (ptr1 int :copy) (long1 (integer 64)))
  (define-alien-routine long-sap-test2 int (ptr1 int :copy) (int1 int) (long1 (integer 64)))
  (define-alien-routine return-long-long (integer 64))
  (define-alien-routine return-schar-test char (p char :copy))
  (define-alien-routine return-uchar-test unsigned-char (p unsigned-char :copy))
  (define-alien-routine return-short-test short (p short :copy))
  (define-alien-routine return-ushort-test unsigned-short (p unsigned-short :copy))
  (define-alien-routine return-int-test int (p int :copy))
  (define-alien-routine return-uint-test unsigned-int (p unsigned-int :copy))

  ;; Test that loading an object file didn't screw up our records
  ;; of variables visible in runtime. (This was a bug until
  ;; Nikodemus Siivola's patch in sbcl-0.8.5.50.)
  ;;
  ;; This cannot be tested in a saved core, as there is no guarantee
  ;; that the location will be the same.
  #+unix
  (assert (= (sb-sys:sap-int (alien-sap *environ*))
             (sb-sys:sap-int (alien-sap environ))))
EOF

echo "(declaim (optimize speed))" | cat - $TEST_FILESTEM.base.lisp > $TEST_FILESTEM.fast.lisp

echo "(declaim (optimize space))" | cat - $TEST_FILESTEM.base.lisp > $TEST_FILESTEM.small.lisp

# Test code
cat > $TEST_FILESTEM.test.lisp <<EOF
  ;; FIXME: currently the start/small case fails on x86/Darwin. Moving
  ;; this NOTE definition to the base.lisp file fixes that, but obviously
  ;; it is better fo figure out what is going on instead of doing that...
  ;;
  ;; Other trivialish changes that mask the error include:
  ;; * loading the .lisp file instead of the .fasl at the save test
  ;; * --eval 'nil' before loading the .fasl at the save test
  ;;
  ;; HATE.
  (defun note (x)
     (write-string " | " *standard-output*)
     (write-line x *standard-output*)
     (force-output *standard-output*))
  (note "/initial assertions")
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
  (assert (= 1 (long-sap-test1 38 (+ 3 (ash 1 38)))))
  (assert (= 1 (long-sap-test2 38 1 (+ 3 (ash 1 38)))))
  (assert (= (ash 1 33) (return-long-long)))
  (assert (= -1 (return-schar-test -1)))
  (assert (= 1 (return-schar-test 1)))
  (assert (= 255 (return-uchar-test 255)))
  (assert (= 1 (return-uchar-test 1)))
  (assert (= -1 (return-short-test -1)))
  (assert (= 65535 (return-ushort-test 65535)))
  (assert (= -1 (return-int-test -1)))
  (assert (= #xffffffff (return-uint-test #xffffffff)))

  (note "/initial assertions ok")

  ;; determine if dlclose is a noop.
  (load-shared-object (truename "$TEST_FILESTEM-noop-dlclose-test.so"))
  (define-alien-routine dlclose-is-noop int)
  (defparameter *dlclose-noop-p* (plusp (dlclose-is-noop)))

  ;; test reloading object file with new definitions
  (assert (= 13 foo))
  (assert (= 42 (bar)))
  (note "/original definitions ok")
  (if *dlclose-noop-p*
      (note "/skipping reloading tests")
      (progn
        (rename-file "$TEST_FILESTEM-b.so" "$TEST_FILESTEM-b.bak")
        (rename-file "$TEST_FILESTEM-b2.so" "$TEST_FILESTEM-b.so")
        (load-shared-object (truename "$TEST_FILESTEM-b.so"))
        (note "/reloading ok")
        (assert (= 42 foo))
        (assert (= 13 (bar)))
        (note "/redefined versions ok")
        (rename-file "$TEST_FILESTEM-b.so" "$TEST_FILESTEM-b2.so")
        (rename-file "$TEST_FILESTEM-b.bak" "$TEST_FILESTEM-b.so")
        (note "/renamed back to originals")))

  ;; test late resolution
  (eval-when (:compile-toplevel :load-toplevel :execute)
   (setq *features* (union *features* sb-impl:+internal-features+)))
  (progn
    (note "/starting linkage table tests")
    (define-alien-variable late-foo int)
    (define-alien-routine late-bar int)
    (multiple-value-bind (val err) (ignore-errors late-foo)
      (assert (not val))
      (assert (typep err 'undefined-alien-error)))
    (multiple-value-bind (val err) (ignore-errors (late-bar))
      (assert (not val))
      (assert (typep err 'undefined-alien-error)))
    (load-shared-object (truename "$TEST_FILESTEM-c.so"))
    (assert (= 43 late-foo))
    (assert (= 14 (late-bar)))
    (if *dlclose-noop-p*
        (note "/skipping linkage table unloading tests")
        (progn
          (unload-shared-object (truename "$TEST_FILESTEM-c.so"))
          (multiple-value-bind (val err) (ignore-errors late-foo)
            (assert (not val))
            (assert (typep err 'undefined-alien-error)))
          (multiple-value-bind (val err) (ignore-errors (late-bar))
            (assert (not val))
            (assert (typep err 'undefined-alien-error)))))
    (note "/linkage table ok"))

  (sb-ext:exit :code $EXIT_LISP_WIN) ; success convention for Lisp program
EOF

# Files are now set up; toggle errexit off, since we use a custom exit
# convention.
set +e

test_compile() {
    x="$1"
    run_sbcl <<EOF
(progn (load (compile-file "$TEST_FILESTEM.$x.lisp" :verbose nil) :verbose nil)
(sb-ext:exit :code $EXIT_LISP_WIN))
EOF
    check_status_maybe_lose "compile $1" $?
}

test_compile fast
test_compile small
echo # blank line

test_use() {
    echo 'Testing' $1 '...'
    run_sbcl --load $TEST_FILESTEM.$1.fasl --load $TEST_FILESTEM.test.lisp
    check_status_maybe_lose "use $1" $? 22 "(load-shared-object not supported)"
}

test_use small
test_use fast
echo # blank line

test_save() {
    echo testing save $1
    x="$1"
    run_sbcl --load $TEST_FILESTEM.$1.fasl <<EOF
(eval-when (:compile-toplevel :load-toplevel :execute)
 (setq *features* (union *features* sb-impl:+internal-features+)))
(save-lisp-and-die "$TEST_FILESTEM.$x.core")
(sb-ext:exit :code 22) ; catch this
EOF
    check_status_maybe_lose "save $1" $? \
	0 "(successful save)" 22 "(linkage table not available)"
}

test_save small
test_save fast
echo # blank line

options="--noinform --no-sysinit --no-userinit"
test_start() {
    if [ -f $TEST_FILESTEM.$1.core ] ; then
        echo testing start $1
        run_sbcl_with_core $TEST_FILESTEM.$1.core $options \
            --eval "(setf sb-ext:*evaluator-mode* :${TEST_SBCL_EVALUATOR_MODE:-compile})" \
            --load $TEST_FILESTEM.test.lisp
        check_status_maybe_lose "start $1" $?
    else
        echo "not testing start $1 (no core file, possibly due to linkage table above)"
    fi
}

test_start fast
test_start small
echo # blank line

if [ -f $TEST_FILESTEM.fast.core ] ; then
# missing object file
    echo Building core for missing shared-object-file test
    run_sbcl_with_core $TEST_FILESTEM.fast.core $options --noprint \
        --eval "(setf sb-ext:*evaluator-mode* :${TEST_SBCL_EVALUATOR_MODE:-compile})" \
        <<EOF
  (setf *invoke-debugger-hook*
        (lambda (condition hook)
          (declare (ignore hook))
          (let ((cont (find-restart 'continue condition)))
            (when cont
              (invoke-restart cont)))
          (print :fell-through)
          (invoke-debugger condition)))
   (save-lisp-and-die "$TEST_FILESTEM.missing.core")
   (sb-ext:exit :code 22) ; catch this
EOF
    check_status_maybe_lose "saving-missing-so-core" $? \
	0 "(successful save)" 22 "(linkage table not available)"
fi

rm $TEST_FILESTEM-b.so $TEST_FILESTEM-b2.so
if [ -f $TEST_FILESTEM.missing.core ] ; then
    run_sbcl_with_core $TEST_FILESTEM.missing.core $options --noprint \
        --eval "(setf sb-ext:*evaluator-mode* :${TEST_SBCL_EVALUATOR_MODE:-compile})" \
        <<EOF
  (assert (= 22 (summish 10 11)))
  (multiple-value-bind (val err) (ignore-errors (eval 'foo))
    (assert (not val))
    (assert (typep err 'undefined-alien-error)))
  (multiple-value-bind (val err) (ignore-errors (eval '(bar)))
    (assert (not val))
    (assert (typep err 'undefined-alien-error)))
  (exit :code $EXIT_LISP_WIN)
EOF
    check_status_maybe_lose "missing-so" $?
else
    echo "skipping missing-so test (no core file, possibly due to linkage table above)"
fi

# ADDR of a heap-allocated object
cat > $TEST_FILESTEM.addr.heap.c <<EOF
  struct foo
  {
    int x, y;
  } a, *b;
EOF

build_so $TEST_FILESTEM.addr.heap

run_sbcl <<EOF
  (load-shared-object (truename "$TEST_FILESTEM.addr.heap.so"))
  (define-alien-type foo (struct foo (x int) (y int)))

  (define-alien-variable a foo)
  (define-alien-variable b (* foo))
  (funcall (compile nil '(lambda () (setq b (addr a)))))
  (assert (sb-sys:sap= (alien-sap a) (alien-sap (deref b))))
  (exit :code $EXIT_LISP_WIN)
EOF
check_status_maybe_lose "ADDR of a heap-allocated object" $?

run_sbcl <<EOF
  (define-alien-type inner (struct inner (var (unsigned 32))))
  (define-alien-type outer (struct outer (one inner) (two inner)))

  (defvar *outer* (make-alien outer))
  (defvar *inner* (make-alien inner))
  (setf (slot *inner* 'var) 20)
  (setf (slot *outer* 'one) (deref *inner*)) ; lp#1514023 (?)
  (assert (= (slot (slot *outer* 'one) 'var) 20))
  (setf (slot *inner* 'var) 40)
  (setf (slot *outer* 'two) (deref *inner*)) ; lp#1514023 (?)
  (assert (= (slot (slot *outer* 'two) 'var) 40))
  (exit :code $EXIT_LISP_WIN)
EOF
check_status_maybe_lose "struct offsets" $?

cat > $TEST_FILESTEM.alien.enum.lisp <<EOF
(define-alien-type foo-flag
  (enum foo-flag-
    (:a 1)
    (:b 2)))

(define-alien-type bar
  (struct bar
    (foo-flag foo-flag)))

(define-alien-type barp
  (* bar))

(defun foo (x)
  (declare (type (alien barp) x))
  x)

(defun bar (x)
  (declare (type (alien barp) x))
  x)
EOF
expect_clean_compile $TEST_FILESTEM.alien.enum.lisp

# success convention for script
exit $EXIT_TEST_WIN

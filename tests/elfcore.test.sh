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

export TEST_BASEDIR=${TMPDIR:-/tmp}
. ./subr.sh

run_sbcl --noinform <<EOF
  #+(and sb-dynamic-core elf sb-thread)
  (let ((s (find-symbol "IMMOBILE-SPACE-OBJ-P" "SB-KERNEL")))
    (when (and s (funcall s #'car)) (exit :code 0))) ; good
 (exit :code 2) ; otherwise
EOF
status=$?
if [ $status != 0 ]; then # test can't be executed
    # we don't have a way to exit shell tests with "inapplicable" as the result
    exit $EXIT_TEST_WIN
fi

set -e # exit on error

# If running in a test sandbox that doesn't provide Make but has already
# provided the necessary executable file, just use that.
# Unfortunately this removes enforcement that the test binary be up-to-date
# with respect to sources for local builds, but I think the greater evil
# would be to spuriously pass by exiting with success if we couldn't build.
if [ ! -x $SBCL_PWD/../src/runtime/shrinkwrap-sbcl ]
then
  (cd $SBCL_PWD/../src/runtime ; make shrinkwrap-sbcl)
fi

$SBCL_PWD/../src/runtime/shrinkwrap-sbcl \
  --disable-debugger \
  --eval '(dotimes (i 100000) (sb-vm::alloc-immobile-fdefn))' \
  --quit
# reaching here means no crash happened in the allocator
echo Basic smoke test: PASS

create_test_subdirectory
tmpcore=$TEST_DIRECTORY/$TEST_FILESTEM.tmpcore

run_sbcl --noinform <<EOF
  (setq sb-c:*compile-to-memory-space* :dynamic)
  ;; Call an assembly routine from dynamic space
  (defun f (x y z) (+ x (- y z)))
  (compile 'f)
  (save-lisp-and-die "${tmpcore}")
EOF

m_arg=`run_sbcl --eval '(progn #+sb-core-compression (princ " -lz") #+x86 (princ " -m32"))' --quit`

(cd $SBCL_PWD/../src/runtime ; make libsbcl.a)
run_sbcl --script ../tools-for-build/editcore.lisp split \
  ${tmpcore} $TEST_DIRECTORY/elfcore-test.s
# I guess we're going to have to hardwire the system libraries
# until I can figure out how to get a Makefile to work, which is fine
# for now because elfination is only supported on linux/x86-64.
./run-compiler.sh -no-pie -g -o $TEST_DIRECTORY/elfcore-test \
  $TEST_DIRECTORY/elfcore-test.s \
  $TEST_DIRECTORY/elfcore-test-core.o \
  $SBCL_PWD/../src/runtime/libsbcl.a -ldl -lm -lpthread ${m_arg}

$TEST_DIRECTORY/elfcore-test $SBCL_ARGS --eval '(assert (zerop (f 1 2 3)))' --quit
echo Custom core: PASS

exit $EXIT_TEST_WIN

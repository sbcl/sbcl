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
  #+(and elf sb-thread)
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

# Ensure that we're not running a stale shrinkwrap-sbcl
(cd $SBCL_PWD/../src/runtime ; rm -f shrinkwrap-sbcl ; make shrinkwrap-sbcl)

$SBCL_PWD/../src/runtime/shrinkwrap-sbcl --disable-debugger --noprint <<EOF
(dotimes (i 100000) (sb-vm::alloc-immobile-fdefn))
(let* ((code (sb-kernel:fun-code-header
              (sb-c::vop-info-generator-function
               (gethash 'print sb-c::*backend-template-names*))))
       (fixups (sb-vm::%code-fixups code)))
  (assert (typep fixups 'bignum))
  (assert (not (heap-allocated-p fixups))))
EOF
# reaching here means no crash happened in the allocator
# and that the fixups were rewritten into C data space
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

./run-compiler.sh -no-pie -g -o $TEST_DIRECTORY/relocating-elfcore-test \
  $TEST_DIRECTORY/elfcore-test.s \
  $TEST_DIRECTORY/elfcore-test-core.o \
  $SBCL_PWD/../tests/heap-reloc/fake-mman.c \
  $SBCL_PWD/../src/runtime/libsbcl.a -ldl -lm -lpthread ${m_arg}

export SBCL_FAKE_MMAP_INSTRUCTION_FILE=heap-reloc/fakemap
i=1
while [ $i -le 6 ]
do
  echo Trial $i
  i=`expr $i + 1`
  $TEST_DIRECTORY/relocating-elfcore-test $SBCL_ARGS --eval '(assert (zerop (f 1 2 3)))' --quit
done

exit $EXIT_TEST_WIN

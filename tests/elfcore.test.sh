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

. ./subr.sh

run_sbcl <<EOF
  #+(and linux elf sb-thread)
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
(cd $SBCL_PWD/../src/runtime ; rm -f shrinkwrap-sbcl* ; make shrinkwrap-sbcl)

# Prevent style-warnings in the editcore script, but don't assume that it
# can be compiled in the first place unless actually doing the ELFcore tests.
run_sbcl <<EOF
  (let ((*evaluator-mode* :interpret))
    (load "../tests/test-util")
    (load "../tools-for-build/corefile"))
  (test-util:with-scratch-file (fasl "fasl")
    (assert (not (nth-value 1
                  (compile-file "../tools-for-build/editcore"
                                :output-file fasl :print nil)))))
EOF

$SBCL_PWD/../src/runtime/shrinkwrap-sbcl --disable-debugger --no-sysinit --no-userinit --noprint <<EOF
#+x86-64 (sb-vm::%alloc-immobile-symbol "junk") ; crashed 'cause I forgot to use rip-relative-EA

;; Test that the link step did not use --export-dynamic
(assert (gethash '("TEXT_SPACE_START") (car sb-sys:*linkage-info*))) ; C symbol exists
; but dlsym() can't see it
(assert (not (sb-sys:find-dynamic-foreign-symbol-address "TEXT_SPACE_START")))
; but we can read the value
(assert (funcall (compile nil '(lambda () sb-vm:text-space-start))))

;; Test that CODE-SERIAL# is never 0 except for simple-fun-less objects
(sb-vm:map-allocated-objects
 (lambda (obj type size)
   (declare (ignore size))
   (when (and (= type sb-vm:code-header-widetag)
              (> (sb-kernel:code-n-entries obj) 0))
     (assert (/= (sb-kernel:%code-serialno obj) 0))))
 :all)
EOF
(cd $SBCL_PWD/../src/runtime ; rm -f shrinkwrap-sbcl shrinkwrap-sbcl.s shrinkwrap-sbcl-core.o shrinkwrap-sbcl.core)

# reaching here means no crash happened in the allocator
# and that the fixups were rewritten into C data space
echo Basic smoke test: PASS

create_test_subdirectory
tmpcore=$TEST_DIRECTORY/$TEST_FILESTEM.tmpcore

run_sbcl <<EOF
  (setq sb-c:*compile-to-memory-space* :dynamic)
  ;; Call an assembly routine from dynamic space
  (defun f (x y z) (+ x (- y z)))
  (compile 'f)
  ;; :AUTO physically allocates code in immobile space (unless it fills up)
  ;; using instruction forms that do not assume immobile space
  ;; (for that same reason) which caused a glitch in editcore.
  (setq sb-c:*compile-to-memory-space* :auto)
  (defun g (fun &rest args) (apply fun args)) ; exercise TAIL-CALL-VARIABLE
  (compile 'g)
  (save-lisp-and-die "${tmpcore}")
EOF

m_arg=`run_sbcl --eval '(progn #+sb-core-compression (princ " -lzstd") #+x86 (princ " -m32"))' --quit`

(cd $SBCL_PWD/../src/runtime ; rm -f libsbcl.a; make libsbcl.a)
run_sbcl --script ../tools-for-build/elftool.lisp split \
  ${tmpcore} $TEST_DIRECTORY/elfcore-test.s
# I guess we're going to have to hardwire the system libraries
# until I can figure out how to get a Makefile to work, which is fine
# for now because elfination is only supported on linux/x86-64.
./run-compiler.sh -no-pie -g -o $TEST_DIRECTORY/elfcore-test \
  $TEST_DIRECTORY/elfcore-test.s \
  $TEST_DIRECTORY/elfcore-test-core.o \
  $SBCL_PWD/../src/runtime/libsbcl.a -lm -lpthread ${m_arg}

$TEST_DIRECTORY/elfcore-test $SBCL_ARGS --eval '(assert (zerop (f 1 2 3)))' --quit
echo Custom core: PASS

./run-compiler.sh -no-pie -g -o $TEST_DIRECTORY/relocating-elfcore-test \
  $TEST_DIRECTORY/elfcore-test.s \
  $TEST_DIRECTORY/elfcore-test-core.o \
  $SBCL_PWD/../tests/heap-reloc/fake-mman.c \
  $SBCL_PWD/../src/runtime/libsbcl.a -lm -lpthread ${m_arg}

(cd $SBCL_PWD/../src/runtime ; rm -f libsbcl.a)

export SBCL_FAKE_MMAP_INSTRUCTION_FILE=heap-reloc/fakemap
i=1
while [ $i -le 6 ]
do
  echo Trial $i
  i=`expr $i + 1`
  $TEST_DIRECTORY/relocating-elfcore-test $SBCL_ARGS --eval '(assert (zerop (f 1 2 3)))' --quit
done

exit $EXIT_TEST_WIN

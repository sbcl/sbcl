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
  #+(and linux x86-64 sb-thread)
  (unless (member :immobile-space sb-impl:+internal-features+)
    (exit :code 0)) ; proceed with test
 (exit :code 2) ; otherwise skip the test
EOF
status=$?
if [ $status != 0 ]; then # test can't be executed
    # we don't have a way to exit shell tests with "inapplicable" as the result
    exit $EXIT_TEST_WIN
fi

set -e # exit on error

create_test_subdirectory
temp=$TEST_DIRECTORY/$TEST_FILESTEM

# TODO: make this one-step. Patching will need to be done offline
# and then we can wrap the different steps into one driver.
echo Step 1
run_sbcl --load ../tools-for-build/editcore \
  --eval '(sb-editcore:move-dynamic-code-to-text-space "../output/sbcl.core" "'${temp}'-split.core")' \
  --quit

echo Step 2
run_sbcl_with_core ${temp}-split.core --noinform --load ../tools-for-build/rewrite-asmcalls \
  --eval '(sb-core-rewriter:monkeypatch-all-calls)' \
  --eval '(save-lisp-and-die "'${temp}'-patched.core")'

echo Step 3
run_sbcl --load ../tools-for-build/editcore \
  --eval '(sb-editcore:split-core "'${temp}'-patched.core" "'${temp}'-src.s")' --quit

echo Step 4
(cd ../src/runtime ; make libsbcl.a)
exefile=$TEST_DIRECTORY/sbcl-new-elf
cc -no-pie -o ${exefile} -Wl,--export-dynamic -Wl,-no-as-needed \
   ${temp}-src.s ${temp}-src-core.o ../src/runtime/libsbcl.a -lm -ldl

result=`${exefile} --eval '(princ "Success")' --quit`
echo $result
if [ "$result" = Success ]
then
  echo "basic ELF: smoke test PASS"
else
  exit 1
fi
result=`${exefile} --eval '(defun fib (n) (if (<= n 1) 1 (+ (fib (- n 1)) (fib (- n 2)))))' \
  --eval "(compile 'fib)" \
  --eval "(if (equal (loop for i from 2 to 5 collect (fib i)) '(2 3 5 8)) (print 'ok))" --quit`
if [ $result = OK ]
then
  echo "COMPILE: PASS"
  exit $EXIT_TEST_WIN
else
  exit 1
fi

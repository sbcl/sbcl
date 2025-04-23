#!/bin/sh

. ./subr.sh

# The relocation test binary can only be built on linux.
# FIXME: This test _should_ work on any architecture, but it doesn't,
#        so there must have been a regression in the heap relocator.
data=`run_sbcl --eval '(progn #+linux(progn(princ "fakemap") #+64-bit(princ "_64")))' \
  --quit`
if [ -z "$data" ]
then
    # shell tests don't have a way of exiting as "not applicable"
    exit $EXIT_TEST_WIN
fi

test_sbcl=../src/runtime/heap-reloc-test

rm -f $test_sbcl

set -e
(cd ../src/runtime ; make heap-reloc-test)

# Exercise all the lines of 'fakemap' by starting up N times in a row.
# KLUDGE: assume N = 6
# FIXME: don't assume that N = 6

export SBCL_FAKE_MMAP_INSTRUCTION_FILE=`pwd`/heap-reloc/$data
i=1
while [ $i -le 6 ]
do
  export SBCL_FAKE_MMAP_INSTRUCTION_LINE=$i
  $test_sbcl --lose-on-corruption --disable-ldb --noinform --core ../output/sbcl.core \
              --no-sysinit --no-userinit --noprint --disable-debugger \
              --eval '(gc :full t)' \
              --eval '(defun fib (n) (if (<= n 1) 1 (+ (fib (- n 1)) (fib (- n 2)))))' \
              --eval "(compile 'fib)" -quit
  i=`expr $i + 1`
done

create_test_subdirectory
tmpcore=$TEST_DIRECTORY/$TEST_FILESTEM.core

run_sbcl <<EOF
  (defglobal original-static-space-bounds
    (cons sb-vm:static-space-start (sb-sys:sap-int sb-vm:*static-space-free-pointer*)))
  ;; there's no point in testing for #-x86-64. While arm64 allows #+relocatable-static-space
  ;; it only does so if #+immobile-space which is not the default config
  #+x86-64
  (when (member :alien-callbacks sb-impl:+internal-features+)
    (push :do-test *features*)
    (sb-alien:define-alien-callable foo int () 42))
  (save-lisp-and-die "$tmpcore")
EOF

$test_sbcl --lose-on-corruption --disable-ldb --noinform --core $tmpcore \
              --no-sysinit --no-userinit --noprint --disable-debugger <<EOF
#-do-test (quit)
;; check that static space relocation happened
(assert (not (eql sb-vm:static-space-start (car original-static-space-bounds))))
;; the identical alien is stored in two places (so there is 1 and only 1 SAP)
(assert (eq (aref sb-alien::*alien-callbacks* 0)
            (gethash 'foo sb-alien::*alien-callables*)))
;; the SAP points within static space
(let* ((alien (aref sb-alien::*alien-callbacks* 0))
       (sap (alien-sap alien)))
 (assert (sb-sys:sap>= sap (sb-sys:int-sap sb-vm:static-space-start)))
 (assert (sb-sys:sap< sap sb-vm:*static-space-free-pointer*)))
;; the callable doesn't crash
(let ((result (alien-funcall (sb-alien:alien-callable-function 'foo))))
  (assert (= result 42)))
(format t "~&I'm back!~%")
EOF

rm -f $tmpcore $test_sbcl

exit $EXIT_TEST_WIN

#!/bin/sh

. ./subr.sh

# The relocation test binary can only be built on linux.
# FIXME: This test _should_ work on any architecture, but it doesn't,
#        so there must have been a regression in the heap relocator.
run_sbcl --eval '(exit :code (or #+linux 0 1))'
if [ $? -eq 1 ]
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

export SBCL_FAKE_MMAP_INSTRUCTION_FILE=`pwd`/heap-reloc/fakemap
i=1
while [ $i -le 6 ]
do
  echo Trial $i
  i=`expr $i + 1`
  $test_sbcl --lose-on-corruption --disable-ldb --noinform --core ../output/sbcl.core \
              --no-sysinit --no-userinit --noprint --disable-debugger \
              --eval '(gc :full t)' --quit
done

rm -f $test_sbcl

exit $EXIT_TEST_WIN

#!/bin/sh

. ./subr.sh

# TODO: get this to run for all platforms that have a relocatable heap
# (I think that's everything that we care about these days?)

run_sbcl --eval '(exit :code (or #+(and (or x86 x86-64) linux) 0 1))'
if [ $? -eq 1 ]
then
    # shell tests don't have a way of exiting as "not applicable"
    exit $EXIT_TEST_WIN
fi

set -e
cd heap-reloc
source ./build-test-sbcl

# Exercise all the lines of 'fakemap' by starting up N times in a row.
# KLUDGE: assume N = 6
# FIXME: don't assume that N = 6
export SBCL_FAKE_MMAP_INSTRUCTION_FILE=`pwd`/fakemap
i=1
while [ $i -le 6 ]
do
  echo Trial $i
  i=`expr $i + 1`
  ./test-sbcl --noinform --core ../../output/sbcl.core --eval '(gc :full t)' --quit
done
rm ./test-sbcl

exit $EXIT_TEST_WIN

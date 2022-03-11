#!/bin/sh

source ./build-test-sbcl

export SBCL_FAKE_MMAP_INSTRUCTION_FILE=`pwd`/fakemap

./test-sbcl --core ../../output/sbcl.core \
  --eval '(setf (extern-alien "verify_gens" char) 0)' \
  --eval '(setf (extern-alien "gencgc_verbose" int) 1)' \
  --eval '(gc :full t)' \
  --eval '(exit)'

# TODO:
# 1. this needs to be run as part of the regression suite
# 2. and split into one test per shell file for parallelization

# There is a slight problem in that one of the tests in core.test.sh
# asserts something about dynamic space size, which due to relocation
# might come out one GC page too small.
# It happens when we see:
#   //Fuzzed 0x1000080000 into 0x200000a000 successfully
# which is not page-aligned.
# The base gets aligned up to 0x2000010000 and one page is subtracted
# from dynamic space size so not to overrun the memory.
# And this script does not actually exit with the correct status anyway.
(cd .. ; TEST_SBCL_RUNTIME=`pwd`/heap-reloc/test-sbcl ./run-tests.sh save*.test.sh)
rm test-sbcl

#!/bin/sh

make preload.so
export SBCL_FAKE_MMAP_INSTRUCTION_FILE=`pwd`/fakemap
LD_PRELOAD=./preload.so ../../run-sbcl.sh \
  --eval '(setf (extern-alien "verify_gens" char) 0)' \
  --eval '(setf (extern-alien "gencgc_verbose" int) 1)' \
  --eval '(gc :full t)' \
  --eval '(exit)'
(LD_PRELOAD=`pwd`/preload.so ; cd .. ; ./run-tests.sh core.test.sh)
rm preload.so

#!/bin/sh

# This script is not part of the build, but running it tells you
# whether each genesis headers can be included without fussing
# around with all sorts of other headers.
for i in src/runtime/genesis/*.h
do
  echo '#include "'$i'"' > tmp.c
  cc -Isrc/runtime -c tmp.c
done

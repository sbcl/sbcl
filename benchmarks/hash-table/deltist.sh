#!/bin/sh

set -eu

base=`dirname "$0"`

sbcl --dynamic-space-size 128 --disable-ldb --noinform --end-runtime-options \
     --disable-debugger --quit --noprint \
     --load "${base}/deltist.lisp" --eval "(deltist)" --end-toplevel-options \
     "$@"

#!/bin/sh

DIR="$HOME/src"

source ./macrobenchmark.sh --command-names "0 1 u f m s 2 3" \
       "$DIR/sbcl.0" "$DIR/sbcl.1" "$DIR/sbcl.u" "$DIR/sbcl.f" "$DIR/sbcl.m" \
       "$DIR/sbcl.s" "$DIR/sbcl.2" "$DIR/sbcl.3" | tee -a macrobenchmark.log

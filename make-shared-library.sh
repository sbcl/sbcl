#!/bin/sh

. output/build-config

echo //entering make-shared-library.sh
echo //building sbcl runtime into a shared library

$GNUMAKE -C src/runtime libsbcl.so

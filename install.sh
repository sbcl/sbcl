#!/bin/sh

# Install SBCL files into the usual places.

cp /usr/local/bin/sbcl /usr/local/bin/sbcl.old
cp /usr/local/lib/sbcl.core /usr/local/lib/sbcl.core.old

cp src/runtime/sbcl /usr/local/bin/
cp output/sbcl.core /usr/local/lib/
cp doc/sbcl.1 /usr/local/man/man1/

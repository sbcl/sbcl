#!/bin/sh

# Create a binary distribution. (make.sh should be run first to create
# the various binary files, and make-doc.sh, or possibly some other 
# DocBook-to-HTML converter, should also be run to create the 
# HTML version of the documentation.)

tar cf ../sbcl-x.y.z-binary.tar \
    output/sbcl.core src/runtime/sbcl \
    BUGS COPYING CREDITS INSTALL NEWS README \
    install.sh \
    doc/sbcl.1 doc/cmucl/cmu-user doc/*.htm* \
    pubring.pgp

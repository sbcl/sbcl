#!/bin/sh

# Create a binary distribution. (make.sh should be run first to create
# the various binary files, and make-doc.sh, or possibly some other 
# DocBook-to-HTML converter, should also be run to create the 
# HTML version of the documentation.)

# (Before sbcl-0.6.10, this was run in the sbcl/ directory and created
# a tar file with no directory prefixes. Since sbcl-0.6.10, we've
# switched over to trying to do this the way everyone else does.)

b=${1:?"missing base directory name argument"}
tar -cf $b-binary.tar \
    $b/output/sbcl.core $b/src/runtime/sbcl \
    $b/BUGS $b/COPYING $b/CREDITS $b/INSTALL $b/NEWS $b/README \
    $b/install.sh \
    $b/doc/sbcl.1 \
    $b/pubring.pgp \
    $b/contrib/asdf-module.mk \
    $b/contrib/vanilla-module.mk \
    `for dir in $b/contrib/*; do 
         if test -d $dir && test -f $dir/test-passed; then 
             echo $dir
         fi
     done`

#!/bin/sh
set -e

# Create a binary distribution. (make.sh should be run first to create
# the various binary files, and make-doc.sh should also be run to
# create the HTML version of the documentation.)

# (Before sbcl-0.6.10, this was run in the sbcl/ directory and created
# a tar file with no directory prefixes. Since sbcl-0.6.10, we've
# switched over to trying to do this the way everyone else does.)

b=${1:?"missing base directory name argument"}

tar -cf $b-binary.tar \
    $b/output/sbcl.core $b/src/runtime/sbcl $b/output/prefix.def \
    $b/src/runtime/sbcl.mk \
    `grep '^LIBSBCL=' $b/src/runtime/sbcl.mk | cut -d= -f2- | while read lib; do echo $b/src/runtime/$lib; done` \
    $b/BUGS $b/COPYING $b/CREDITS $b/INSTALL $b/NEWS $b/README \
    $b/install.sh $b/find-gnumake.sh $b/sbcl-pwd.sh $b/run-sbcl.sh \
    $b/doc/sbcl.1 \
    $b/pubring.pgp \
    $b/contrib/asdf-module.mk \
    `for contrib in $(cd $b/contrib && echo *); do
         src_dir=$b/contrib/$contrib
         cache_dir=$b/obj/asdf-cache/$contrib
         if test -d $src_dir && test -f $cache_dir/test-passed.test-report; then
             echo $src_dir/Makefile
             echo $cache_dir/test-passed.test-report
         fi
     done` \
    $b/obj/sbcl-home

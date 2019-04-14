#!/bin/sh
set -em

# This is a script to be run as part of make.sh. The only time you'd
# want to run it by itself is if you're trying to cross-compile the
# system or if you're doing some kind of troubleshooting.

# This software is part of the SBCL system. See the README file for
# more information.
#
# This software is derived from the CMU CL system, which was
# written at Carnegie Mellon University and released into the
# public domain. The software is in the public domain and is
# provided with absolutely no warranty. See the COPYING and CREDITS
# files for more information.

echo //entering make-genesis-2.sh

. output/build-config

# In a fresh host Lisp invocation, load the cross-compiler (in order
# to get various definitions that GENESIS needs, not in order to
# cross-compile GENESIS, then load and run GENESIS. (We use a fresh
# host Lisp invocation here for basically the same reasons we did
# before when loading and running the cross-compiler.)
#
# (Why do we need this second invocation of GENESIS? In order to
# create a .core file, as opposed to just a .h file, GENESIS needs
# symbol table data on the C runtime. And we can get that symbol
# data only after the C runtime has been built. Therefore, even
# though we ran GENESIS earlier, we couldn't get it to make a .core
# file at that time; but we needed to run it earlier in order to
# get to where we can write a .core file.)
echo //loading and running GENESIS to create cold-sbcl.core
echo '(load "loader.lisp") (load-sbcl-file "make-genesis-2.lisp")' | $SBCL_XC_HOST

echo //testing for consistency of first and second GENESIS passes
if ! command -v diff > /dev/null ; then
    echo //no diffutils, not testing
elif diff -r src/runtime/genesis output/genesis-2; then
    echo //header files match between first and second GENESIS -- good
else
    echo error: header files do not match between first and second GENESIS
    exit 1
fi

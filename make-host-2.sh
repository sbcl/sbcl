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

echo //entering make-host-2.sh

LANG=C
LC_ALL=C
export LANG LC_ALL

# Load our build configuration
. output/build-config

# In some cases, a debugging build of the system will creates a core
# file output/after-xc.core in the next step. In cases where it
# doesn't, it's confusing and basically useless to have any old copies
# lying around, so delete:
rm -f output/after-xc.core

# In a fresh host Lisp invocation, load and run the cross-compiler to
# create the target object files describing the target SBCL.
#
# (There are at least three advantages to running the cross-compiler in a
# fresh host Lisp invocation instead of just using the same Lisp invocation
# that we used to compile it:
#   (1) It reduces the chance that the cross-compilation process
#       inadvertently comes to depend on some weird compile-time
#       side effect.
#   (2) It reduces peak memory demand (because definitions wrapped in
#       (EVAL-WHEN (:COMPILE-TOPLEVEL :EXECUTE) ..) aren't defined
#       in the fresh image).
#   (3) It makes it easier to jump in and retry a step when tweaking
#       and experimenting with the bootstrap procedure.
# Admittedly, these don't seem to be enormously important advantages, but
# the only disadvantage seems to be the extra time required to reload
# the fasl files into the new host Lisp, and that doesn't seem to be
# an enormously important disadvantage, either.)
if [ $# -gt 0 ]
then
    files=\'\(\"$1\"\) # FIXME: create string-quoted list of each arg individually
    echo //compiling $files
    echo '(defvar *compile-files* '${files}')(load "make-host-2.lisp")' | $SBCL_XC_HOST
    exit
fi

echo //running cross-compiler to create target object files
echo '(load "loader.lisp") (load-sbcl-file "make-host-2.lisp")' | $SBCL_XC_HOST

# Run GENESIS (again) in order to create cold-sbcl.core. (The first
# time was before we ran the cross-compiler, in order to create the
# header file which was needed in order to run gcc on the runtime
# code.)
sh make-genesis-2.sh

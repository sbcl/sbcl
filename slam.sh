#!/bin/sh

# a quick and dirty way of partially rebuilding the system after a
# change
#
# This script is not a reliable way to build the system, but it is
# fast.:-| It can be useful if you are trying to debug a low-level
# problem, e.g. a problem in src/runtime/*.c or in src/code/unix.lisp,
# and you find yourself wanting to make a small change and test it
# without going through the entire build-the-system-from-scratch
# cycle.
#
# You probably don't want to be using this script unless you
# understand the system build process to be able to guess when it
# won't work.


# This software is part of the SBCL system. See the README file for
# more information.
#
# This software is derived from the CMU CL system, which was
# written at Carnegie Mellon University and released into the
# public domain. The software is in the public domain and is
# provided with absolutely no warranty. See the COPYING and CREDITS
# files for more information.


export SBCL_XC_HOST="${1:-sbcl --noprogrammer}"

# (We don't do make-host-1.sh at all. Hopefully nothing relevant has
# changed.)

sh make-target-1.sh || exit 1

# Instead of doing the full make-host-2.sh, we (1) use after-xc.core
# to rebuild only the specifically-requested Lisp files (or skip
# after-xc.core completely if no Lisp files are specifically
# requested), then (2) run GENESIS.
#
# Our command line arguments are the stems that we'll use
# after-xc.core to recompile. If there are no command line arguments,
# though, make a point of not calling after-xc.core, since it might
# not exist, and there's no point in causing a fatal failure (by
# unsuccessfully trying to execute it) unnecessarily.
if [ "$*" != "" ] ; then 
    # Actually, I wrote this script when I needed to do a lot of
    # tweaking in src/runtime/*.c, and I haven't tried to make it 
    # work for src/code/*.c yet. -- WHN 2001-05-12
    echo stub: no support yet for after-xc.core
    exit 1
fi
sh make-genesis-2.sh || exit 1 

sh make-target-2.sh || exit 1

echo /ordinary termination of slam.sh

#!/bin/sh

# ("smooth duct tape: the mark of a true craftsman":-)

# a quick and dirty way of partially rebuilding the system after a
# change
#
# This script is not a reliable way to build the system, but it is
# fast.:-| It can be useful if you are trying to debug a low-level
# problem, e.g. a problem in src/runtime/*.c or in
# src/code/cold-init.lisp, and you find yourself wanting to make a
# small change and test it without going through the entire
# build-the-system-from-scratch cycle.
#
# You probably don't want to be using this script unless you
# understand the system build process well enough to be able to guess
# when it won't work.

# This software is part of the SBCL system. See the README file for
# more information.
#
# This software is derived from the CMU CL system, which was
# written at Carnegie Mellon University and released into the
# public domain. The software is in the public domain and is
# provided with absolutely no warranty. See the COPYING and CREDITS
# files for more information.

if [ "" != "$*" ]; then
    echo no command line arguments supported in this version of slam
    exit 1
fi

# We don't try to be general about this in this script the way we are
# in make.sh, since the idiosyncrasies of SBCL command line argument
# order dependence, the meaninglessness of duplicate --core arguments,
# and the SBCL-vs-CMUCL dependence of --core/-core argument syntax
# make it too messy to try deal with arbitrary SBCL_XC_HOST variants.
# So you have no choice:
export SBCL_XC_HOST='sbcl --noprogrammer'

# (We don't do make-host-1.sh at all. Hopefully nothing relevant has
# changed.)

sh make-target-1.sh || exit 1

# Instead of doing the full make-host-2.sh, we (1) use after-xc.core
# to rebuild only obviously-out-of-date Lisp files, then (2) run
# GENESIS.
sbcl --core output/after-xc.core --sysinit /dev/null --userinit /dev/null <<'EOF' || exit 1
  (load "src/cold/slam.lisp")
EOF
# (This ^ used to be
#   for f in $*; do echo "(target-compile-stem \"$f\")"; done \
#     | sbcl --core output/after-xc.core || exit 1
# and perhaps we do something like this again, allowing explicit
# rebuild-this-stem requests on the command line to supplement
# the rebuild-obviously-outdated-stems logic above.)
#
sh make-genesis-2.sh || exit 1 

sh make-target-2.sh || exit 1

echo //ordinary termination of slam.sh
date

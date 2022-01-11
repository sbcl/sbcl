#!/bin/sh
set -e

# a quick and dirty way of partially rebuilding the system after a
# change
#
# ("smooth duct tape: the mark of a true craftsman":-)

# This software is part of the SBCL system. See the README file for
# more information.
#
# This software is derived from the CMU CL system, which was
# written at Carnegie Mellon University and released into the
# public domain. The software is in the public domain and is
# provided with absolutely no warranty. See the COPYING and CREDITS
# files for more information.

#######################################################################
# You probably don't want to be using this script unless you
# understand the ordinary system build process pretty well already.
#
# This script is not a reliable way to build the system, but it is
# fast.:-| It can be useful if you are trying to debug a low-level
# problem, e.g. a problem in src/runtime/*.c or in
# src/code/cold-init.lisp. Soon, you'll find yourself wanting to
# test a small change in a file compiled into cold-sbcl.core without
# redoing the entire rebuild-the-system-from-scratch process. You may be
# able to avoid a complete make-host-2.sh by just letting this script
# rebuild only files that have changed. On the other hand, it might
# not work...
#
# It's not anywhere rigorously correct for all small changes, much
# less for all large changes. It can't be, unless we either solve the
# halting problem or totally rearchitect the SBCL sources to support
# incremental recompilation. Beyond that fundamental limitation, even
# an easy special case might not work unless someone's paid attention
# to making it work. Here are some highlights to help you understand
# when it will work:
#  * It will rebuild a .fasl file when the corresponding
#    .lisp file is out of date.
#  * It rebuilds the src/runtime/ files completely, since that
#    doesn't take very long anyway.
#  * Apparently it will not rebuild assembly-code-in-.lisp files
#    even when the sources are out of date. This is probably not a
#    fundamental limitation, it's just that I (WHN 2002-01-16)
#    have made vanishingly nontrivial changes to assembler files,
#    so I'm not motivated. If you're motivated, please send a patch.
#  * It will not notice when you change something in one .lisp file
#    which should affect the compilation of code in another .lisp
#    file. E.g.
#    ** changing the definition of a macro used in another file (or a
#       function or a variable which is used at macroexpansion time)
#    ** changing the value of a DEFCONSTANT used in another file
#    ** changing the layout of a structure used in another file
#    ** changing the PROCLAIMed type of something used in another
#       file
#    Mostly it looks as though such limitations aren't fixable without
#    the aforementioned rearchitecting or solving the halting problem.
#
# To make this work, you need an after-xc.core file. To cause the
# system to generate an after-xc.core file, you need
# :SB-AFTER-XC-CORE in target features during an ordinary build.
# See the comments in base-target-features.lisp-expr for the
# recommended way to make that happen.
#######################################################################

warm_option=""
if [ "$1" == --load -o "$1" == --load-with-sb-devel ]; then
    warm_option="$1"
    shift
fi

# Load our build configuration
. output/build-config

HOST_TYPE="${1:-sbcl}"

echo //HOST_TYPE=\"$HOST_TYPE\"

# We don't try to be general about this in this script the way we are
# in make.sh, since the idiosyncrasies of SBCL command line argument
# order dependence, the meaninglessness of duplicate --core arguments,
# and the SBCL-vs-CMUCL dependence of --core/-core argument syntax
# make it too messy to try deal with arbitrary SBCL_XC_HOST variants.
# So you have no choice:
case "$HOST_TYPE" in
    cmucl) LISP="lisp -batch"
           INIT="-noinit"
           CORE="-core"
           ;;
    sbcl)  LISP="${SBCL_XC_HOST:-sbcl}"
           INIT="--no-sysinit --no-userinit"
           CORE="--core"
           ;;
    clisp) LISP="clisp"
           INIT="-norc"
           CORE="-M"
           ;;
    openmcl)
           LISP="openmcl"
           INIT="-b"
           CORE="-I"
           ;;
    *)     echo unknown host type: "$HOST_TYPE"
           echo should be one of "sbcl", "cmucl", or "clisp"
           exit 1
esac

SBCL_XC_HOST="$LISP ${XC_CORE:+$CORE $XC_CORE} $INIT"
export SBCL_XC_HOST

# (We don't do make-host-1.sh at all. Hopefully nothing relevant has
# changed.)

. ./find-gnumake.sh
find_gnumake

$GNUMAKE -C src/runtime all

# Instead of doing the full make-host-2.sh, we (1) use after-xc.core
# to rebuild only obviously-out-of-date Lisp files, then (2) run
# GENESIS.
$LISP $CORE output/after-xc.core $INIT <<'EOF'
  (load "src/cold/slam.lisp")
EOF
# (This ^ used to be
#   for f in $*; do echo "(target-compile-stem \"$f\")"; done \
#     | sbcl --core output/after-xc.core || exit 1
# and perhaps we do something like this again, allowing explicit
# rebuild-this-stem requests on the command line to supplement
# the rebuild-obviously-outdated-stems logic above.)
#
sh make-genesis-2.sh

sh make-target-2.sh "$warm_option"

echo //ordinary termination of slam.sh
date

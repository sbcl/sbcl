#!/bin/sh
set -e

# This is a script to be run as part of make.sh. The only time you'd
# probably want to run it by itself is if you're cross-compiling the
# system or doing some kind of troubleshooting.

# This software is part of the SBCL system. See the README file for
# more information.
#
# This software is derived from the CMU CL system, which was
# written at Carnegie Mellon University and released into the
# public domain. The software is in the public domain and is
# provided with absolutely no warranty. See the COPYING and CREDITS
# files for more information.

echo //entering make-target-contrib.sh

LANG=C
LC_ALL=C

# Just doing CC=${CC:-cc} may be enough, but it needs to be checked
# that cc is available on all platforms.
if [ -z "$CC" ]; then
    if [ -x "`command -v cc`" ]; then
        CC=cc
    else
        CC=gcc
    fi
fi

if [ -z "${LD}" ]; then
    LD=ld
fi

unset EXTRA_CFLAGS # avoid any potential interference 
export CC LD LANG LC_ALL

# Load our build configuration
. output/build-config

## All programs spawned by make-target-contrib.sh that use this
## variable or anything derived from it are started with CWD
## contrib/<contrib_name>/. Keeping this a relative pathname to the
## toplevel source directory makes the shell and make portions of the
## build system robust against funny stuff in PWD.
SBCL_TOP="../../"

SBCL_HOME="$SBCL_TOP/obj/sbcl-home"
export SBCL_HOME SBCL_TOP

SBCL="$SBCL_TOP/src/runtime/sbcl --noinform --core $SBCL_TOP/output/sbcl.core \
--lose-on-corruption --disable-debugger --no-sysinit --no-userinit"
export SBCL

set -e # exit with failure if any $GNUMAKE fails
if [ -z "$*" ]; then
    $GNUMAKE $SBCL_MAKE_JOBS -k -C contrib
else
    for x in "$@"; do
        $GNUMAKE -C contrib $x.fasl
    done
fi

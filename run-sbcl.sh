#!/bin/sh
# A simple shell-script to run the freshly build SBCL without
# installing it.

# This software is part of the SBCL system. See the README file for
# more information.
#
# This software is derived from the CMU CL system, which was
# written at Carnegie Mellon University and released into the
# public domain. The software is in the public domain and is
# provided with absolutely no warranty. See the COPYING and CREDITS
# files for more information.

set -e

if [ "$1" = "--help" ]; then
    echo "usage: run-sbcl.sh sbcl-options*"
    echo
    echo "Runs SBCL from the build directory or binary tarball without need for"
    echo "installation. Except for --help and --core, accepts all the same"
    echo "command-line options as SBCL does."
    echo
    exit 1
fi

BASE=`dirname "$0"`

if [ -x "$BASE"/src/runtime/sbcl -a -f "$BASE"/output/sbcl.core ]; then
    echo "(running SBCL from: $BASE)"
    SBCL_HOME="$BASE"/contrib \
        "$BASE"/src/runtime/sbcl --core "$BASE"/output/sbcl.core "$@"
else
    echo "No built SBCL here ($BASE): run 'sh make.sh' first!"
    exit 1
fi

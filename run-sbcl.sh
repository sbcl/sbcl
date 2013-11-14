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

this="$0"

# OSX 10.8 readlink doesn't have -f
while [ -h "$this" ]; do
    # [ -h should guarantee that readlink output will be non-null
    link=`readlink -n "$this"`
    # if absolute path
    if expr "$link" : '^/.*' > /dev/null; then
        this="$link"
    else
        this=`dirname "$this"`/"$link"
    fi
done
BASE=`dirname "$this"`

CORE_DEFINED=no

for arg in "$@"; do
    case "$arg" in
        --core)
          CORE_DEFINED=yes
          ;;
        --help)
          echo "usage: run-sbcl.sh sbcl-options*"
          echo
          echo "Runs SBCL from the build directory or binary tarball without need for"
          echo "installation. Except for --help, accepts all the same command-line options"
          echo "as SBCL does."
          echo
          exit 1
          ;;
    esac
done

ARGUMENTS=""

if [ "$CORE_DEFINED" = "no" ]; then
    ARGUMENTS="--core "$BASE"/output/sbcl.core"
fi

if [ -x "$BASE"/src/runtime/sbcl -a -f "$BASE"/output/sbcl.core ]; then
    echo "(running SBCL from: $BASE)" 1>&2
    SBCL_HOME="$BASE/obj/sbcl-home" exec "$BASE"/src/runtime/sbcl $ARGUMENTS "$@"
else
    echo "No built SBCL here ($BASE): run 'sh make.sh' first!"
    exit 1
fi

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

build_directory_p(){
    [ -x "$1"/src/runtime/sbcl -a -f "$1"/output/sbcl.core ];
}

# OSX 10.8 readlink doesn't have -f
while [ -h "$this" ]; do
    # Stop resolving symlinks when $this lives in a build tree.
    # (Build trees might consist of symlinks to something that doesn't
    # follow our repo layout.)
    if build_directory_p `dirname "$this"`; then
	break
    fi
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
# BASE can still be relative if $0 is a relative pathname naming a
# non-symlink, or if the last symlink visited in that loop has a
# relative target. We need BASE to be an absolute pathname in order to
# make MODULE-PROVIDE-CONTRIB work throughout the Lisp session, even
# after frobbing *DEFAULT-PATHNAME-DEFAULTS*.
if expr "$BASE" : '^/.*' > /dev/null; [ $? != 0 ]; then
    BASE=`cd "$BASE" && pwd`
fi

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

if [ "$CORE_DEFINED" = "no" ]; then
    CORE="$BASE"/output/sbcl.core
fi

if build_directory_p "$BASE"; then
    export SBCL_HOME
    if [ "$CORE_DEFINED" = "no" ]; then
	SBCL_HOME="$BASE"/obj/sbcl-home exec "$BASE"/src/runtime/sbcl --core "$CORE" "$@"
    else
	SBCL_HOME="$BASE"/obj/sbcl-home exec "$BASE"/src/runtime/sbcl "$@"
    fi
else
    echo "No built SBCL here ($BASE): run 'sh make.sh' first!"
    exit 1
fi

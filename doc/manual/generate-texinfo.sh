#!/bin/sh

# This software is part of the SBCL system. See the README file for
# more information.
#
# This software is in the public domain and is provided with
# absolutely no warranty. See the COPYING and CREDITS files for
# more information.

# how we invoke SBCL

# We create the documentation from the in-tree sbcl if it is found,
# else an installed sbcl is used.

if [ -z "$1" ] ; then
    SBCL_TOP=../..
    sbclsystem=$SBCL_TOP/src/runtime/sbcl
    sbclcore=$SBCL_TOP/output/sbcl.core
    if [ -f $sbclsystem ] && [ -f $sbclcore ]
    then
        SBCLRUNTIME="$sbclsystem --core $sbclcore"
        SBCL_HOME=$SBCL_TOP/obj/sbcl-home/; export SBCL_HOME
    else
        SBCLRUNTIME="`command -v sbcl`"
    fi
    . $SBCL_TOP/output/build-config
else
    SBCLRUNTIME="$1"
fi

${SBCLRUNTIME}                                                            \
    --noinform --no-sysinit --no-userinit --noprint --disable-debugger    \
    --eval '(require :sb-manual)' --eval '(sb-manual::generate-texinfo)' \
    --quit

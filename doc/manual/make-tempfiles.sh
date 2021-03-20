#!/bin/sh

# Create Texinfo snippets from the documentation of exported symbols.

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
        SBCL_BUILDING_CONTRIB="please asdf install your hook"; export SBCL_BUILDING_CONTRIB
    else
        SBCLRUNTIME="`command -v sbcl`"
    fi
else
    SBCLRUNTIME="$1"
    shift
fi

if [ -z "$1" ] ; then
    DOCSTRINGDIR="${DOCSTRINGDIR:-docstrings/}"
else
    DOCSTRINGDIR="$1"
    shift
fi

${SBCLRUNTIME}                                                          \
    --noinform --no-sysinit --no-userinit --noprint --disable-debugger  \
    --script generate-texinfo.lisp "${SBCLRUNTIME}" "${DOCSTRINGDIR}"

#!/bin/sh

# This software is part of the SBCL system. See the README file for
# more information.
#
# This software is in the public domain and is provided with
# absolutely no warranty. See the COPYING and CREDITS files for
# more information.

BASE=`dirname "$0"`

git_forge_uri="$1"

if [ -z "$2" ] ; then
    SBCL_TOP="$BASE/../.."
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
    SBCLRUNTIME="$2"
fi

# We *don't* add --no-sysinit and --no-userinit because we rely on the
# user to have set things up so that PAX can be loaded.
${SBCLRUNTIME} \
    --noinform --noprint --disable-debugger \
    --load ${BASE}/make-pax-docs.lisp \
    --eval "(sb-manual::make-pax-docs \"${git_forge_uri}\")" \
    --quit

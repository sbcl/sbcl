#!/bin/false
# Not a shell script, but something intended to be sourced from shell scripts

# This ensures that SBCL_PWD is a path understandable to SBCL.

sbcl_pwd() {
    if [ "${OSTYPE:-}" = "cygwin" ] ; then
	SBCL_PWD="`cygpath -m \"$(pwd)\"`"
    else
	SBCL_PWD="`pwd`"
    fi
    export SBCL_PWD
}

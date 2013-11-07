#!/bin/false
# Not a shell script, but something intended to be sourced from shell scripts

# This ensures that SBCL_PWD is a path understandable to SBCL.

sbcl_pwd() {
    case "${OSTYPE:-}" in
        cygwin)
            SBCL_PWD="`cygpath -m \"$(pwd)\"`" ;;
        msys)
            SBCL_PWD="`pwd -W`" ;;
        *)
            SBCL_PWD="`pwd`" ;;
    esac
    export SBCL_PWD
}

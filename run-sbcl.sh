# A simple shell-script to run the freshly build SBCL without
# installing it. Expects to be run from the top of the SBCL source
# tree.

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
    echo "Expects the current directory to be the topmost directory of the SBCL"
    echo "source tree or binary tarball."
    echo
    exit 1
fi

if [ -f sbcl-pwd.sh -a -x src/runtime/sbcl -a -f output/sbcl.core ]; then
    . sbcl-pwd.sh
    sbcl_pwd
    echo "(running SBCL from: $SBCL_PWD)"
    SBCL_HOME=$SBCL_PWD/contrib src/runtime/sbcl --core output/sbcl.core $@
elif [ -f run-sbcl.sh -a -f version.lisp-expr ]; then
    echo "No built SBCL here ($(pwd)): run 'sh make.sh' first!"
    exit 1
else
    echo "No SBCL here ($(pwd))!"
    echo
    echo "run-sbcl.sh needs to be run from the top of the SBCL source tree or"
    echo "binary tarball."
    exit 1
fi

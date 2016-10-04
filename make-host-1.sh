#!/bin/sh
set -em

# This is a script to be run as part of make.sh. The only time you'd
# want to run it by itself is if you're trying to cross-compile the
# system or if you're doing some kind of troubleshooting.

# This software is part of the SBCL system. See the README file for
# more information.
#
# This software is derived from the CMU CL system, which was
# written at Carnegie Mellon University and released into the
# public domain. The software is in the public domain and is
# provided with absolutely no warranty. See the COPYING and CREDITS
# files for more information.

echo //entering make-host-1.sh

LANG=C
LC_ALL=C
export LANG LC_ALL

# Load our build configuration
. output/build-config

# Compile and load the cross-compiler. (We load it here not because we're
# about to use it, but because it's written under the assumption that each
# file will be loaded before the following file is compiled.)
#
# Also take the opportunity to compile and load genesis, to create the
# header file sbcl.h which will be needed to create the C runtime
# environment.
echo //building cross-compiler, and doing first genesis
echo '(load "loader.lisp") (load-sbcl-file "make-host-1.lisp")' | $SBCL_XC_HOST

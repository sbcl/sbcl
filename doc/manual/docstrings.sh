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
sbclsystem=`pwd`/../../src/runtime/sbcl
if [ -e $sbclsystem ] 
then
SBCL="${1:-$sbclsystem --core `pwd`/../../output/sbcl.core}"
export SBCL_HOME=`pwd`/../../contrib
else
SBCL="${1:-`which sbcl`}"
fi

# List of package names that documentation will be created for.
PACKAGES=":SB-ALIEN :SB-EXT :SB-GRAY :SB-MOP :SB-PROFILE :SB-THREAD"

# Output directory.  This has to end with a slash (it's interpreted by
# Lisp's `pathname' function) or you lose.
DOCSTRINGDIR="${DOCSTRINGDIR:-docstrings/}"


echo /creating docstring snippets from SBCL=\'$SBCL\' for packages \'$PACKAGES\'
echo "(progn (load \"docstrings.lisp\") (docstrings-to-texinfo \"$DOCSTRINGDIR\" $PACKAGES) (sb-ext:quit))" | $SBCL --noinform --sysinit /dev/null --userinit /dev/null --noprint --disable-debugger

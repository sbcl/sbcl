#!/bin/sh

# Create Texinfo snippets from the documentation of exported symbols.
# Also create contrib-docs.texi-temp to include documentation in contrib/.

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

# Output directory.  This has to end with a slash (it's interpreted by
# Lisp's `pathname' function) or you lose.  This is normally set from
# Makefile.
DOCSTRINGDIR="${DOCSTRINGDIR:-docstrings/}"

# List of package names that documentation will be created for.  This
# is normally set from Makefile.
PACKAGES="${PACKAGES:-:COMMON-LISP :SB-ALIEN :SB-DEBUG :SB-EXT :SB-GRAY :SB-MOP :SB-PROFILE :SB-THREAD}"

echo /creating docstring snippets from SBCL=\'$SBCL\' for packages \'$PACKAGES\'
echo "(progn (load \"docstrings.lisp\") (docstrings-to-texinfo \"$DOCSTRINGDIR\" $PACKAGES) (sb-ext:quit))" | $SBCL --noinform --sysinit /dev/null --userinit /dev/null --noprint --disable-debugger

echo /creating contrib-docs.texi-temp
echo "(load \"create-contrib-doc-list.lisp\")" | $SBCL --noinform --sysinit /dev/null --userinit /dev/null --noprint --disable-debugger

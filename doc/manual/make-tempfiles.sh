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
sbclcore=`pwd`/../../output/sbcl.core
if [ -e $sbclsystem ] && [ -e $sbclcore ] 
then
    SBCL="${1:-$sbclsystem --core $sbclcore}"
    export SBCL_HOME=`pwd`/../../contrib
else
    SBCL="${1:-`which sbcl`}"
fi

# Output directory.  This has to end with a slash (it's interpreted by
# Lisp's `pathname' function) or you lose.  This is normally set from
# Makefile.
DOCSTRINGDIR="${DOCSTRINGDIR:-docstrings/}"

# List of contrib modules that docstring docs will be created for.
# This is normally set from Makefile.
#MODULES="${MODULES:-sb-md5 :sb-rotate-byte}"

# List of package names that documentation will be created for.  This
# is normally set from Makefile.
#PACKAGES="${PACKAGES:-:COMMON-LISP :SB-ALIEN :SB-DEBUG :SB-EXT :SB-GRAY :SB-MOP :SB-PROFILE :SB-THREAD}"

echo /creating docstring snippets from SBCL=\'$SBCL\' for packages \'$PACKAGES\'
echo "(progn (load \"docstrings.lisp\") (dolist (module (quote ($MODULES))) (require module)) (docstrings-to-texinfo \"$DOCSTRINGDIR\" $PACKAGES) (sb-ext:quit))" | $SBCL --noinform --sysinit /dev/null --userinit /dev/null --noprint --disable-debugger

echo /creating contrib-docs.texi-temp
echo "(load \"create-contrib-doc-list.lisp\")" | $SBCL --noinform --sysinit /dev/null --userinit /dev/null --noprint --disable-debugger

echo /creating package-locks.texi-temp
if $SBCL --noinform --sysinit /dev/null --userinit /dev/null --noprint --disable-debugger --eval '(quit :unix-status #+sb-package-locks 0 #-sb-package-locks 1)'
then
    cp package-locks-extended.texinfo package-locks.texi-temp
else
    cp package-locks-basic.texinfo package-locks.texi-temp
fi

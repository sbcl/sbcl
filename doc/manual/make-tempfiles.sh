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

if [ -z "$1" ]
then
    sbclsystem=`pwd`/../../src/runtime/sbcl
    sbclcore=`pwd`/../../output/sbcl.core
    if [ -e $sbclsystem ] && [ -e $sbclcore ]
    then
        SBCLRUNTIME="$sbclsystem --core $sbclcore"
        SBCL_HOME=`pwd`/../../contrib; export SBCL_HOME
    else
        SBCLRUNTIME="`which sbcl`"
    fi
else
    SBCLRUNTIME="$1"
fi

SBCL="$SBCLRUNTIME --noinform --sysinit /dev/null --userinit /dev/null --noprint --disable-debugger"

# extract version and date
VERSION=`$SBCL --eval '(write-line (lisp-implementation-version))' --eval '(sb-ext:quit)'`
MONTH=`date "+%Y-%m"`

sed -e "s/@VERSION@/$VERSION/" \
    -e "s/@MONTH@/$MONTH/" < variables.template > variables.texinfo || exit 1

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

echo /creating docstring snippets from SBCL=\'$SBCLRUNTIME\' for packages \'$PACKAGES\'
$SBCL <<EOF
(load "docstrings.lisp") 
(dolist (module (quote ($MODULES))) 
  (require module)) 
(sb-texinfo:generate-includes "$DOCSTRINGDIR" $PACKAGES) 
(sb-ext:quit))
EOF

echo /creating package-locks.texi-temp
if $SBCL --eval "(let ((plp (find-symbol \"PACKAGE-LOCKED-P\" :sb-ext))) (quit :unix-status (if (and plp (fboundp plp)) 0 1)))";
then
    cp package-locks-extended.texinfo package-locks.texi-temp
else
    cp package-locks-basic.texinfo package-locks.texi-temp
fi

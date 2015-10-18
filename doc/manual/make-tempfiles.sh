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
    . ../../sbcl-pwd.sh
    sbcl_pwd

    sbclsystem=$SBCL_PWD/../../src/runtime/sbcl
    sbclcore=$SBCL_PWD/../../output/sbcl.core
    if [ -f $sbclsystem ] && [ -f $sbclcore ]
    then
        SBCLRUNTIME="$sbclsystem --core $sbclcore"
        SBCL_HOME=$SBCL_PWD/../../obj/sbcl-home/; export SBCL_HOME
        SBCL_BUILDING_CONTRIB="please asdf install your hook"; export SBCL_BUILDING_CONTRIB
    else
        SBCLRUNTIME="`command -v sbcl`"
    fi
else
    SBCLRUNTIME="$1"
fi

SBCL="$SBCLRUNTIME --noinform --no-sysinit --no-userinit --noprint --disable-debugger"

# extract version and date
VERSION=`$SBCL --eval '(write-line (lisp-implementation-version))' --eval '(sb-ext:exit)'`
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
(require :asdf)
(dolist (module (quote ($MODULES)))
  (require module))
(sb-texinfo:generate-includes "$DOCSTRINGDIR" $PACKAGES)
(sb-ext:exit))
EOF

echo /creating package-locks.texi-temp
if $SBCL --eval "(let ((plp (find-symbol \"PACKAGE-LOCKED-P\" :sb-ext))) (exit :code (if (and plp (fboundp plp)) 0 1)))";
then
    cp package-locks-extended.texinfo package-locks.texi-temp
else
    cp package-locks-basic.texinfo package-locks.texi-temp
fi

echo /creating encodings.texi-temp
$SBCL <<EOF
(with-open-file (s "encodings.texi-temp" :direction :output :if-exists :supersede)
  (let (result)
    (sb-int:dohash ((key val) sb-impl::*external-formats*)
      (pushnew (sb-impl::ef-names val) result :test #'equal))
    (setq result (sort result #'string< :key #'car))
    (format s "@table @code~%~%")
    (loop for (cname . names) in result
          do (format s "@item ~S~%~{@code{~S}~^, ~}~%~%" cname names))
    (format s "@end table~%")))
EOF

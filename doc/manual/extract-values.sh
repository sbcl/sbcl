#!/bin/sh

# extracts values from the system for inclusion in the texinfo source.

VERSION=`eval echo $(grep '^"' ../../version.lisp-expr)`
MONTH=`date "+%Y-%m"`

sed -e "s/@VERSION@/$VERSION/" \
    -e "s/@MONTH@/$MONTH/"

#!/bin/sh

if [ "" != "$JADE" ]; then
    # The user has told us where to find jade, good.
    echo using $JADE
elif which openjade; then
    # OpenJade is the version of Jade which comes with OpenBSD 2.9, 
    # and I started using it in sbcl-0.pre7.x. -- WHN
    JADE=openjade
elif which jade; then
    # I used the original Jade until sbcl-0.pre7.x. It might still
    # work. -- WHN
    JADE=jade
else
    echo "can't find Jade, sorry"
    exit 1
fi

rm -f book1.htm
$JADE -t sgml -ihtml -d sbcl-html.dsl\#html user-manual.sgml
ln -sf book1.htm user-manual.html

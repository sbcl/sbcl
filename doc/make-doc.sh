#!/bin/sh

# Where is Jade? (i.e. James Clark's implementation of DSSSL, or
# something offsprung)
if [ "" != "$JADE" ]; then
    # The user has told us where to find jade, good.
    echo using JADE=$JADE
elif which openjade > /dev/null; then
    # OpenJade is the version of Jade which comes with OpenBSD 2.9, 
    # and I started using it in sbcl-0.pre7.x. -- WHN
    JADE=openjade
elif which jade > /dev/null; then
    # I used the original Jade until sbcl-0.pre7.x. It might still
    # work. -- WHN
    JADE=jade
else
    echo "can't find Jade, sorry"
    exit 1
fi

# Our hacked sbcl-html.dsl directs HTML output to html/. Make a clean slate.
rm -rf html
mkdir html

$JADE -t sgml -ihtml -d sbcl-html.dsl\#html user-manual.sgml

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

# Since Jade has strange ideas about the name of the top level output
# file, use a symlink as a workaround to provide a reasonable entry
# point.
#
# (KLUDGE: Why does the output always come out in book1.htm? According
# to the docs of OpenJade 1.3, it should be coming out in
# user-manual.htm by default, I think. And it should respect the -o
# option. But experimentally that seems not to be. -- WHN 2002-01-15)
rm -f book1.htm
$JADE -t sgml -ihtml -d sbcl-html.dsl\#html user-manual.sgml
ln -sf book1.htm user-manual.html

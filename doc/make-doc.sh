#!/bin/sh

. ../find-gnumake.sh
find_gnumake

# Where is xsltproc? 
if [ "" != "$XSLTPROC" ]; then
    # The user has told us where to find xsltproc, good.
    echo using XSLTPROC=$XSLTPROC
elif which xsltproc > /dev/null; then
    # We have found it ourselves.
    XSLTPROC=xsltproc
else
    echo "can't find xsltproc, sorry"
    exit 1
fi

export XSLTPROC
$GNUMAKE html

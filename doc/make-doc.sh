#!/bin/sh

. ../find-gnumake.sh
find_gnumake

# Where is xsltproc? 
if [ "" != "$XSLTPROC" ]; then
    # The user has told us where to find jade, good.
    echo using XSLTPROC=$XSLTPROC
elif which xsltproc > /dev/null; then
    # Openxsltproc is the version of xsltproc which comes with OpenBSD 2.9, 
    # and I started using it in sbcl-0.pre7.x. -- WHN
    XSLTPROC=xsltproc
else
    echo "can't find xsltproc, sorry"
    exit 1
fi

export XSLTPROC
$GNUMAKE html

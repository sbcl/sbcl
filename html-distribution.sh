#!/bin/sh

# Create a distribution containing the HTML versions of system
# documentation. (make-doc.sh needs to be run first, in order to
# compile the DocBook sources into HTML.)

# (Before sbcl-0.7.0, this functionality was part of
# binary-distribution.sh.)

b=${1:?missing base directory name argument}
tar cf $b-html.tar
    `find $b -name '*.htm*'`
    $b/COPYING $b/CREDITS $b/README \
    $b/pubring.pgp

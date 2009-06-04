#!/bin/sh
set -e

# Create a distribution containing the HTML versions of system
# documentation. (cd doc/manual && make needs to be run first, in order to
# compile the doc sources into HTML.)

b=${1:?missing base directory name argument}
tar cf $b-documentation-html.tar \
    `find $b -name '*.htm*'` \
    $b/COPYING $b/CREDITS $b/README \
    $b/pubring.pgp

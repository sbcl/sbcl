#!/bin/sh

. ../find-gnumake.sh
find_gnumake
$GNUMAKE clean

(cd manual; sh clean.sh)

#!/bin/sh

. ../find-gnumake.sh
find_gnumake

(cd ./manual ; $GNUMAKE html pdf info)

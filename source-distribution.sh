#!/bin/sh
set -e

# Create a source distribution. (You should run clean.sh first.)

b=${1:?"missing base directory name argument"}
tar cf $b-source.tar --exclude .git $b

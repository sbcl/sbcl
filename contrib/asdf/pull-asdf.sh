#!/bin/sh

# Get the current ASDF release from the upstream repo.

if test -d asdf-upstream
then
    cd asdf-upstream
    git checkout release
    git reset --hard release
    git pull -a
else
    git clone --branch release git://common-lisp.net/projects/asdf/asdf.git asdf-upstream
fi

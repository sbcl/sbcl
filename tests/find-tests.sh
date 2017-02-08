#!/bin/sh

# This software is part of the SBCL system. See the README file for
# more information.
#
# While most of SBCL is derived from the CMU CL system, the test
# files (like this one) were written from scratch after the fork
# from CMU CL.
#
# This software is in the public domain and is provided with
# absolutely no warranty. See the COPYING and CREDITS files for
# more information.

if [ "$1" = "--help" ] || [ -z "$1" ]; then
    cat <<EOF
Find test cases whose names match a given string.

Usage: $0 [OPTIONS] STRING

Options:

  --fuzzy  Enable substring matching, disregard packages.

EOF
    exit 0
fi

. ./subr.sh

set +u
run_sbcl --script find-tests.lisp $*

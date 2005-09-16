#!/bin/sh
set -e

# How big is this project anyway? Crudely estimate non-comment source lines..

echo -n "approximate Lisp source lines: "
find . -name "*.lisp" -print | xargs egrep -hs '^[ 	]*[^ 	;]' | wc -l
echo -n "approximate Lisp source non-whitespace chars: "
find . -name "*.lisp" -print | xargs egrep -hs '^[ 	]*[^ 	;]' \
  | perl -ne 's/\s//g ; print' | wc -c
# some errors in Lisp counting above:
#   * doesn't catch #| .. |#
#   * doesn't catch #+NIL convention for commenting out forms
#   * doesn't catch stale source files which are no longer used

echo -n "approximate C source lines: "
find . -name "*.[ch]" -print | xargs egrep -s '^[ 	]*[^ 	/*]' | wc -l
# errors:
#   * only crudely approximates "/*"-style commenting (using the assumption
#     that all lines beginning with "/" or "*" are beginning of comments or
#     continuation of comments respectively)
#   * doesn't catch #if 0 convention for commenting out blocks
#   * doesn't catch stale source files which are no longer used

echo "(ignoring .sh, .S, etc.)"

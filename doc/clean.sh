#!/bin/sh

# Remove directories which are known to be generated for output only.
rm -rf html/

# Remove files whose suffixes indicate that they're generated (e.g.
# HTML or PostScript) not source (DocBook/SGML, build scripts, etc.).
find . \( \
	-name '*.htm' -o \
	-name '*.html' \) -print | xargs rm -f

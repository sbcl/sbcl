#!/bin/sh

find . \( \
	-name '*.htm' -o \
	-name '*.html' \) -print | xargs rm -f

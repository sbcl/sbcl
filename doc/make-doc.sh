#!/bin/sh

rm -f book1.htm
jade -t sgml -ihtml -d sbcl-html.dsl\#html user-manual.sgml
ln -sf book1.htm user-manual.html

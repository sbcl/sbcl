MODULE=asdf
include ../vanilla-module.mk


test::
	true

up:
	cvs -d :pserver:anonymous@cclan.cvs.sourceforge.net:/cvsroot/cclan \
		co -kv -p asdf/asdf.lisp >/tmp/$$$$ &&\
		mv /tmp/$$$$ asdf.lisp
	cvs -d :pserver:anonymous@cclan.cvs.sourceforge.net:/cvsroot/cclan \
		co -kv -p asdf/README  >/tmp/$$$$ &&\
		mv /tmp/$$$$ README

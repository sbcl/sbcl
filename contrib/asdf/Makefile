MODULE=asdf
include ../vanilla-module.mk


test::
	true

up:
	cvs -d :pserver:anonymous@cvs.cclan.sf.net:/cvsroot/cclan \
		co -kv -p asdf/asdf.lisp >/tmp/$$$$ &&\
		mv /tmp/$$$$ asdf.lisp
	cvs -d :pserver:anonymous@cvs.cclan.sf.net:/cvsroot/cclan \
		co -kv -p asdf/README  >/tmp/$$$$ &&\
		mv /tmp/$$$$ README

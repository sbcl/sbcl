MODULE=asdf
include ../vanilla-module.mk


test::
	true

up:
	sh pull-asdf.sh
	(cd asdf-upstream; make build/asdf.lisp)
	cp asdf-upstream/build/asdf.lisp asdf.lisp
	cp asdf-upstream/doc/asdf.texinfo asdf.texinfo
	cp asdf-upstream/README README

MODULE=asdf
include ../vanilla-module.mk


test::
	true

up:
	sh pull-asdf.sh
	cp asdf-upstream/asdf.lisp asdf.lisp
	cp asdf-upstream/doc/asdf.texinfo asdf.texinfo
	cp asdf-upstream/README README
	cd asdf-upstream ; make archive
	cp asdf-upstream/LICENSE LICENSE 

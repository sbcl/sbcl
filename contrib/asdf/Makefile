MODULE=asdf
include ../vanilla-module.mk


test::
	true

up:
	test -d asdf-upstream || git clone http://common-lisp.net/project/asdf/asdf.git asdf-upstream
	cd asdf-upstream && git pull
	cp asdf-upstream/asdf.lisp asdf.lisp
	cp asdf-upstream/asdf.texinfo asdf.texinfo
	cp asdf-upstream/README README
	cp asdf-upstream/LICENSE LICENSE 

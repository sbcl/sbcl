MODULE=asdf
include ../vanilla-module.mk


test::
	true

up:
	test -d asdf-upstream || \
	git clone \
	git://common-lisp.net/projects/asdf/asdf.git asdf-upstream
	: Using development ASDF. For stable ASDF, should use --branch release above.
	cd asdf-upstream && git pull
	cp asdf-upstream/asdf.lisp asdf.lisp
	cp asdf-upstream/doc/asdf.texinfo asdf.texinfo
	cp asdf-upstream/README README
	cd asdf-upstream ; make archive
	cp asdf-upstream/LICENSE LICENSE 

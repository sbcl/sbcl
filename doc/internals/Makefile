.PHONY: top info clean all

all: info pdf

top:
	sh make-top.sh

info: top
	makeinfo sbcl-internals.texinfo

pdf: top
	texi2pdf sbcl-internals.texinfo

clean: 
	rm -f *.include *.info *.pdf *~ *.cp *.fn *.ky *.log *.pg *.toc \
		*.tp *.vr *.aux


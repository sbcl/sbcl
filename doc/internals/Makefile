ifeq ($(TEXI2PDF),)
  TEXI2PDF:=texi2dvi -p
endif

.PHONY: top clean html all

all: sbcl-internals.pdf sbcl-internals.info

top:
	sh make-top.sh

info: top
	makeinfo sbcl-internals.texinfo

%.eps: %.dot
	dot -Tps -Gsize="5,5" -Gratio=compress -Gconcentrate=true $< > $@

%.png: %.dot
	dot -Tpng -Gsize="5,5" -Gratio=compress -Gconcentrate=true $< > $@

%.txt: %.dot
	# FIXME.
	dot -Tcanon $< > $@

%.pdf: %.eps
	epstopdf $< > $@

sbcl-internals.pdf: top $(patsubst %.dot,%.pdf,$(wildcard *.dot)) *.texinfo
	$(TEXI2PDF) sbcl-internals.texinfo

sbcl-internals.info: top $(patsubst %.dot,%.txt,$(wildcard *.dot)) *.texinfo

html: html-stamp

html-stamp: top $(patsubst %.dot,%.png,$(wildcard *.dot)) *.texinfo
	makeinfo --html sbcl-internals.texinfo 
	# FIXME
	cp -f *.png sbcl-internals
	touch html-stamp
clean: 
	rm -rf *.include *.info *.pdf *~ *.cp *.fn *.ky *.log *.pg *.toc \
		*.tp *.vr *.aux *.eps *.png *.dvi *.ps *.txt *.fns \
		html-stamp sbcl-internals/


DOCFILES:=$(shell echo *.texinfo)
ROOTFILE:=sbcl.texinfo
TMPFILES:=sbcl.aux sbcl.cp sbcl.fn sbcl.ky sbcl.log sbcl.pg sbcl.toc sbcl.tp sbcl.vr

PSFILE=sbcl.ps
PDFFILE=sbcl.pdf
DVIFILE=sbcl.dvi
INFOFILE=sbcl.info
HTMLDIR=sbcl


ifeq ($(MAKEINFO),)
  MAKEINFO:=makeinfo
endif

ifeq ($(TEXI2PDF),)
  TEXI2PDF:=texi2pdf
endif

ifeq ($(DVIPS),)
  DVIPS:=dvips
endif

.PHONY: all
all: ps pdf info html

.PHONY: dist
dist: html pdf




.PHONY: html
html: html-stamp

html-stamp: $(DOCFILES)
	@rm -rf $(HTMLDIR)
	$(MAKEINFO) --html $(ROOTFILE)
	touch html-stamp


.PHONY: ps
ps: $(PSFILE)

$(PSFILE): $(DVIFILE)
	dvips -o $@ $<

$(DVIFILE): $(DOCFILES)
	texi2dvi $(ROOTFILE)


.PHONY: pdf
pdf: $(PDFFILE)

$(PDFFILE): $(DOCFILES)
	texi2pdf $(ROOTFILE)


.PHONY: info
info: $(INFOFILE)

$(INFOFILE): $(DOCFILES)
	$(MAKEINFO) $(ROOTFILE)



.PHONY: clean
clean: 
	rm -f *~ *.bak *.orig \#*\# .\#* texput.log
	rm -rf $(HTMLDIR)
	rm -f $(PSFILE) $(PDFFILE) $(DVIFILE) html-stamp
	rm -f $(TMPFILES)
	rm -f sbcl.info sbcl.info-*

.PHONY: distclean
distclean: clean


DOCFILES:=$(shell echo *.texinfo)
ROOTFILE:=sbcl.texinfo
TMPFILES:=sbcl.aux sbcl.cp sbcl.fn sbcl.ky sbcl.log sbcl.pg sbcl.toc sbcl.tp sbcl.vr

PSFILE=sbcl.ps
PDFFILE=sbcl.pdf
DVIFILE=sbcl.dvi
INFOFILE=sbcl.info
HTMLDIR=$(basename $(ROOTFILE))
# The value of DOCSTRINGDIR has to end with a slash or you lose (it's
# passed to Lisp's `pathname' function).
DOCSTRINGDIR="docstrings/"


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



# html documentation; output in $(HTMLDIR)
.PHONY: html
html: html-stamp

html-stamp: $(DOCFILES) docstrings
	@rm -rf $(HTMLDIR)
	$(MAKEINFO) -I $(DOCSTRINGDIR) --html $(ROOTFILE)
	touch html-stamp

# Postscript documentation
.PHONY: ps
ps: $(PSFILE)

$(PSFILE): $(DVIFILE)
	dvips -o $@ $<

$(DVIFILE): $(DOCFILES) docstrings
	texi2dvi -I $(DOCSTRINGDIR) $(ROOTFILE)

# PDF documentation
.PHONY: pdf
pdf: $(PDFFILE)

$(PDFFILE): $(DOCFILES) docstrings
	texi2pdf -I $(DOCSTRINGDIR) $(ROOTFILE)

# info docfiles
.PHONY: info
info: $(INFOFILE)

$(INFOFILE): $(DOCFILES) docstrings
	$(MAKEINFO) -I $(DOCSTRINGDIR) $(ROOTFILE)

# Texinfo docstring snippets; output hardcoded in docstrings/ for now.
.PHONY: docstrings
docstrings: docstrings-stamp

docstrings-stamp:
	DOCSTRINGDIR=$(DOCSTRINGDIR) sh docstrings.sh
	touch docstrings-stamp



.PHONY: clean
clean: 
	rm -f *~ *.bak *.orig \#*\# .\#* texput.log
	rm -rf $(HTMLDIR) $(DOCSTRINGDIR)
	rm -f $(PSFILE) $(PDFFILE) $(DVIFILE) html-stamp docstrings-stamp
	rm -f $(TMPFILES)
	rm -f sbcl.info sbcl.info-*

.PHONY: distclean
distclean: clean

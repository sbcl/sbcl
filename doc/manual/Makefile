
DOCFILES:=$(shell echo *.texinfo)
ROOTFILE:=sbcl.texinfo
TMPFILES:=sbcl.aux sbcl.cp sbcl.fn sbcl.ky sbcl.log sbcl.pg sbcl.toc sbcl.tp sbcl.vr

PSFILE=sbcl.ps
PDFFILE=sbcl.pdf
DVIFILE=sbcl.dvi
INFOFILE=sbcl.info
HTMLDIR=$(basename $(ROOTFILE))
# Place where generated documentation ends up. The value of
# DOCSTRINGDIR has to end with a slash or you lose (it's passed to
# Lisp's `pathname' function).
DOCSTRINGDIR="docstrings/"
# List of contrib modules that docstring docs will be created for.
# FIXME: should check test-passed and not load them.
MODULES=':sb-md5 :sb-rotate-byte :sb-grovel :sb-sprof'
# List of package names that docstring docs will be created for.
PACKAGES=":COMMON-LISP :SB-ALIEN :SB-DEBUG :SB-EXT :SB-GRAY :SB-MOP :SB-PROFILE :SB-THREAD :SB-MD5 :SB-ROTATE-BYTE :SB-SPROF"


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

# contrib-modules.texinfo includes contrib-doc-list.texi-temp
contrib-modules.texinfo: tempfiles-stamp

# Texinfo docstring snippets
.PHONY: docstrings
docstrings: tempfiles-stamp

tempfiles-stamp:
	DOCSTRINGDIR=$(DOCSTRINGDIR) PACKAGES=$(PACKAGES) MODULES=$(MODULES) sh make-tempfiles.sh && touch tempfiles-stamp


.PHONY: clean
clean: 
	rm -f *~ *.bak *.orig \#*\# .\#* texput.log *.fasl
	rm -rf $(HTMLDIR) $(DOCSTRINGDIR)
	rm -f contrib-docs.texi-temp
	rm -f $(PSFILE) $(PDFFILE) $(DVIFILE) html-stamp tempfiles-stamp
	rm -f $(TMPFILES) contrib-doc-list.texi-temp
	rm -f sbcl.info sbcl.info-*

.PHONY: distclean
distclean: clean

SBCLTEXI:=sbcl.texinfo
ASDFTEXI:=asdf.texinfo
DOCFILES:=*.texinfo $(ASDFTEXI)
TMPTYPES:=aux cps fns ky log pg toc tps vrs
TMPFILES:=$(foreach target,asdf sbcl,$(foreach type,$(TMPTYPES),$(target).$(type)))
PSFILES=sbcl.ps asdf.ps
PDFFILES=sbcl.pdf asdf.pdf
INFOFILES=sbcl.info asdf.info
HTMLDIRS=$(basename $(SBCLTEXI)) $(basename $(ASDFTEXI))
# Place where generated documentation ends up. The value of
# DOCSTRINGDIR has to end with a slash or you lose (it's passed to
# Lisp's `pathname' function).
DOCSTRINGDIR="docstrings/"
I_FLAGS=-I $(DOCSTRINGDIR) -I ../../contrib/
# List of contrib modules that docstring docs will be created for.
# FIXME: should check test-passed and not load them.
MODULES=':sb-md5 :sb-rotate-byte :sb-grovel :sb-sprof :sb-bsd-sockets'
# List of package names that docstring docs will be created for.
PACKAGES=":COMMON-LISP :SB-ALIEN :SB-DEBUG :SB-EXT :SB-GRAY :SB-MOP :SB-PROFILE :SB-THREAD :SB-MD5 :SB-ROTATE-BYTE :SB-SPROF :SB-BSD-SOCKETS"

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
all: asdf.texinfo ps pdf info html

.PHONY: dist
dist: html pdf

asdf.texinfo:
	rm -f asdf.texinfo
	ln -s ../../contrib/asdf/asdf.texinfo

# html documentation; output in $(HTMLDIRS)
.PHONY: html
html: html-stamp

html-stamp: $(DOCFILES) docstrings
	@rm -rf $(HTMLDIRS)
	$(MAKEINFO) $(I_FLAGS) --html $(SBCLTEXI)
	$(MAKEINFO) --html $(ASDFTEXI)
	touch html-stamp

# Postscript documentation
.PHONY: ps
ps: $(PSFILES)

%.ps: %.dvi
	dvips -o $@ $<

# DVI generation
%.dvi: %.texinfo $(DOCFILES) docstrings
	texi2dvi $(I_FLAGS) $<

# PDF documentation
.PHONY: pdf
pdf: $(PDFFILES)

%.pdf: %.texinfo $(DOCFILES) docstrings
	texi2pdf $(I_FLAGS) $<

# info docfiles
.PHONY: info
info: $(INFOFILES)

%.info: %.texinfo $(DOCFILES) docstrings
	$(MAKEINFO) $(I_FLAGS) $<

# Texinfo docstring snippets
.PHONY: docstrings
docstrings: tempfiles-stamp

tempfiles-stamp:
	DOCSTRINGDIR=$(DOCSTRINGDIR) PACKAGES=$(PACKAGES) MODULES=$(MODULES) sh make-tempfiles.sh && touch tempfiles-stamp


.PHONY: clean
clean: 
	rm -f *~ *.bak *.orig \#*\# .\#* texput.log *.fasl
	rm -rf $(HTMLDIRS) $(DOCSTRINGDIR)
	rm -f contrib-docs.texi-temp
	rm -f package-locks.texi-temp
	rm -f $(PSFILE) $(PDFFILE) $(DVIFILE) html-stamp tempfiles-stamp
	rm -f $(TMPFILES)
	rm -f sbcl.info sbcl.info-*

.PHONY: distclean
distclean: clean

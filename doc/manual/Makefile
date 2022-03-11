SBCLTEXI:=sbcl.texinfo
ASDFTEXI:=asdf.texinfo
DOCFILES:=*.texinfo $(ASDFTEXI)
TMPTYPES:=aux cp cps fn fns ky log pg toc tp tps vr vrs
TMPFILES:=$(foreach target,asdf sbcl,$(foreach type,$(TMPTYPES),$(target).$(type)))
PSFILES=sbcl.ps asdf.ps
PDFFILES=sbcl.pdf asdf.pdf
INFOFILES=sbcl.info asdf.info
HTMLDIRS=$(basename $(SBCLTEXI)) $(basename $(ASDFTEXI))
HTMLFILES=sbcl.html asdf.html
# Place where generated documentation ends up. The value of
# DOCSTRINGDIR has to end with a slash or you lose (it's passed to
# Lisp's `pathname' function).
DOCSTRINGDIR="docstrings/"
CONTRIB_SRC_DIR="../../contrib/"
I_FLAGS=-I $(DOCSTRINGDIR) -I $(CONTRIB_SRC_DIR)

# SBCL_SYSTEM is an optional argument to this make program. If this
# variable is set, its contents are used as the command line for
# invoking SBCL.

# When passing a non-standard SBCL_SYSTEM, be sure to set the
# environment variable SBCL_HOME to a useful value, as well.

ifeq ($(MAKEINFO),)
  MAKEINFO:=makeinfo
endif

ifeq ($(TEXI2PDF),)
  TEXI2PDF:=texi2dvi -p
endif

ifeq ($(DVIPS),)
  DVIPS:=dvips
endif

.PHONY: all
all: $(ASDFTEXI) ps pdf info html

.PHONY: dist
dist: html pdf

asdf.texinfo:
	rm -f asdf.texinfo
	ln -s ../../contrib/asdf/asdf.texinfo

version.texinfo:
	rm -f version.texinfo
	ln -s ../../contrib/asdf/version.texinfo

# html documentation; output in $(HTMLDIRS)
.PHONY: html
html: html-stamp

html-stamp: $(DOCFILES) docstrings
	@rm -rf $(HTMLDIRS)
	@rm -f $(HTMLFILES)
#	$(MAKEINFO) $(I_FLAGS) --html --css-include=style-multi.css $(SBCLTEXI)
#	$(MAKEINFO) --html --css-include=style-multi.css $(ASDFTEXI)
	$(MAKEINFO) $(I_FLAGS) --html --no-split --css-include=style-common.css --css-include=style-single.css $(SBCLTEXI)
	$(MAKEINFO) --html --no-split --css-include=style-common.css --css-include=style-single.css $(ASDFTEXI)
	touch html-stamp

# Postscript documentation
.PHONY: ps
ps: $(PSFILES)

%.ps: %.dvi
	dvips -q -o $@ $<

# DVI generation
%.dvi: %.texinfo $(DOCFILES) docstrings
	texi2dvi -q $(I_FLAGS) $<

# PDF documentation
.PHONY: pdf
pdf: $(PDFFILES)

%.pdf: %.texinfo $(DOCFILES) docstrings
	$(TEXI2PDF) -q $(I_FLAGS) $<

# info docfiles
.PHONY: info
info: $(INFOFILES)

%.info: %.texinfo $(DOCFILES) docstrings
	$(MAKEINFO) $(I_FLAGS) $<

# Texinfo docstring snippets
docstrings variables.texinfo: tempfiles-stamp
	sh make-tempfiles.sh "$(SBCL_SYSTEM)" "$(DOCSTRINGDIR)" && touch $(DOCSTRINGDIR)

tempfiles-stamp:
	touch tempfiles-stamp

.PHONY: clean
clean:
	rm -f *~ *.bak *.orig \#*\# .\#* texput.log *.fasl
	rm -rf $(HTMLDIRS) $(DOCSTRINGDIR)
	rm -f  $(HTMLFILES)
	rm -f contrib-docs.texi-temp
	rm -f package-locks.texi-temp
	rm -f variables.texinfo
	rm -f $(PSFILES) $(PDFFILES) html-stamp tempfiles-stamp
	rm -f $(TMPFILES) $(INDEXFILES)
	rm -f sbcl.info sbcl.info-* asdf.info

.PHONY: distclean
distclean: clean

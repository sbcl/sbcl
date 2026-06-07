SBCLTEXI:=sbcl.texinfo
ASDFTEXI:=asdf.texinfo
# The rest of the texinfo files are generated.
DOCFILES:=sbcl.texinfo backmatter.texinfo $(ASDFTEXI)
TMPTYPES:=aux cp cps fn fns ky log pg toc tp tps vr vrs
TMPFILES:=$(foreach target,asdf sbcl,$(foreach type,$(TMPTYPES),$(target).$(type)))
PSFILES=sbcl.ps asdf.ps
PDFFILES=sbcl.pdf asdf.pdf
INFOFILES=sbcl.info asdf.info
HTMLDIRS=$(basename $(SBCLTEXI)) $(basename $(ASDFTEXI))
HTMLFILES=sbcl.html asdf.html
CONTRIB_SRC_DIR="../../contrib/"
I_FLAGS=-I $(CONTRIB_SRC_DIR)

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

html-stamp: $(DOCFILES) generated-texinfo-files
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
%.dvi: %.texinfo $(DOCFILES) generated-texinfo-files
	texi2dvi -q $(I_FLAGS) $<

# PDF documentation
.PHONY: pdf
pdf: $(PDFFILES)

%.pdf: %.texinfo $(DOCFILES) generated-texinfo-files
	$(TEXI2PDF) -q $(I_FLAGS) $<

# info docfiles
.PHONY: info
info: $(INFOFILES)

%.info: %.texinfo $(DOCFILES) generated-texinfo-files
	$(MAKEINFO) $(I_FLAGS) $<

# Texinfo docstring snippets
generated-texinfo-files: generated-texinfo-stamp
	sh generate-texinfo.sh "$(SBCL_SYSTEM)"

generated-texinfo-stamp:
	touch generated-texinfo-stamp

.PHONY: clean
clean:
	rm -f *~ *.bak *.orig \#*\# .\#* texput.log *.fasl
	rm -rf $(HTMLDIRS)
	rm -f  $(HTMLFILES)
	rm -f contrib-docs.texi-temp
	rm -f package-locks.texi-temp
	rm -f variables.texinfo
	rm -f $(PSFILES) $(PDFFILES) html-stamp generated-texinfo-stamp
	rm -f $(TMPFILES) $(INDEXFILES)
	rm -f sbcl.info sbcl.info-* asdf.info

.PHONY: distclean
distclean: clean

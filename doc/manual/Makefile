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
CONTRIBDIR="../../contrib/"
I_FLAGS=-I $(DOCSTRINGDIR) -I $(CONTRIBDIR)
# List of contrib modules that docstring docs will be created for.
MODULES=':sb-md5 :sb-queue :sb-concurrency :sb-rotate-byte :sb-grovel \
         :sb-sprof :sb-bsd-sockets :sb-cover :sb-posix'
# List of package names that docstring docs will be created for.
PACKAGES=":COMMON-LISP :SB-ALIEN :SB-DEBUG :SB-EXT :SB-GRAY :SB-MOP \
	  :SB-PCL :SB-SYS \
          :SB-PROFILE :SB-THREAD :SB-MD5 :SB-QUEUE :SB-ROTATE-BYTE  \
          :SB-SPROF :SB-BSD-SOCKETS :SB-COVER :SB-POSIX :SB-CONCURRENCY"

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
	@rm -f $(HTMLFILES)
#	$(MAKEINFO) $(I_FLAGS) --html --css-include=style-multi.css $(SBCLTEXI)
#	$(MAKEINFO) --html --css-include=style-multi.css $(ASDFTEXI)
	$(MAKEINFO) $(I_FLAGS) --html --no-split --css-include=style-single.css $(SBCLTEXI)
	$(MAKEINFO) --html --no-split --css-include=style-single.css $(ASDFTEXI)
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
	$(TEXI2PDF) $(I_FLAGS) $<

# info docfiles
.PHONY: info
info: $(INFOFILES)

%.info: %.texinfo $(DOCFILES) docstrings
	$(MAKEINFO) $(I_FLAGS) $<

# Texinfo docstring snippets
# Note: assumes contrib module names are the same as the names of
# directories containing the modules, and that these directories
# are in $(CONTRIBDIR).
CONTRIB_FASLS=$(shell find $(shell echo $(MODULES) | sed "s|:|$(CONTRIBDIR)|g") -name '*.fasl')
docstrings: $(CONTRIB_FASLS) tempfiles-stamp
	for module in $(shell echo $(MODULES)); do \
		test -f $(CONTRIBDIR)/`echo $$module | tr -d :`/test-passed \
		|| { echo "The documented contrib $$module seems \
                           to have failed its tests." && exit 1; } \
	done
	DOCSTRINGDIR=$(DOCSTRINGDIR) \
	PACKAGES=$(PACKAGES) \
	MODULES=$(MODULES) \
	sh make-tempfiles.sh "$(SBCL_SYSTEM)" && touch $(DOCSTRINGDIR)

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

# Makefile for SBCL document generation

DOCFILE_BASE_DEFAULT:=user-manual
DOCFILE_EXT_DEFAULT:=xml

# Standard docfile processing as copied from UFFI
# Placed into public domain by author Kevin Rosenberg

SYSTEM:=$(shell uname)

ifeq ($(SYSTEM),Linux) 
  DEBIAN=$()
  SUSE=
  REDHAT=

  ifneq ($(shell expr "`cat /etc/issue`" : '.*Debian.*'),0)
    OS:=debian
    else
    ifneq ($(shell expr "`cat /etc/issue`" : '.*SuSE.*'),0)
      OS=suse
    else   
      ifneq ($(shell expr "`cat /etc/issue`" : 'Red Hat Linux release 9'),0)
        OS=redhat9
      endif
    endif
  endif
endif

ifeq ($(SYSTEM),Darwin)
  OS:=fink
endif

ifndef DOCFILE_BASE
DOCFILE_BASE=${DOCFILE_BASE_DEFAULT}
endif

ifndef DOCFILE_EXT
DOCFILE_EXT=${DOCFILE_EXT_DEFAULT}
endif

DOCFILE:=${DOCFILE_BASE}.${DOCFILE_EXT}
FOFILE:=${DOCFILE_BASE}.fo
PDFFILE:=${DOCFILE_BASE}.pdf
PSFILE:=${DOCFILE_BASE}.ps
DVIFILE:=${DOCFILE_BASE}.dvi
TXTFILE:=${DOCFILE_BASE}.txt
HTMLFILE:=${DOCFILE_BASE}.html
TMPFILES:=${DOCFILE_BASE}.aux ${DOCFILE_BASE}.out ${DOCFILE_BASE}.log
DOCFILES:=$(shell echo *.xml *.xsl)

ifeq ($(XSLTPROC),)
  XSLTPROC:=xsltproc
endif

CATALOG:=`pwd`/catalog-${OS}.xml
CHECK:=XML_CATALOG_FILES="$(CATALOG)" xmllint --noout --xinclude --postvalid $(DOCFILE) || exit 1

.PHONY: all
all: html 

.PHONY: dist
dist: html pdf

.PHONY: doc
doc: html pdf

.PHONY: check
check:
	@echo "Operating system detected: ${OS}"
	@$(CHECK)

.PHONY: html
html: html-stamp

html-stamp: $(DOCFILES) Makefile 
	@rm -rf html
	@mkdir html
	@XML_CATALOG_FILES="$(CATALOG)" $(XSLTPROC) --stringparam chunker.output.encoding ISO-8859-1 \
		 --xinclude --output html/ html_chunk.xsl $(DOCFILE)
	touch html-stamp

.PHONY: fo
fo: ${FOFILE}

${FOFILE}: $(DOCFILES) Makefile 
	@XML_CATALOG_FILES="$(CATALOG)" $(XSLTPROC) --xinclude --output $(FOFILE) fo.xsl $(DOCFILE)

.PHONY: pdf
pdf: ${PDFFILE}

${PDFFILE}: ${DOCFILES} Makefile
	@$(MAKE) fo
	@fop $(FOFILE) -pdf $(PDFFILE) > /dev/null

.PHONY: dvi
dvi: ${DVIFILE}

.PHONY: ps
ps: ${PSFILE}

${PSFILE}: ${DOCFILES} Makefile
	@$(MAKE) fo
	@fop $(FOFILE) -ps $(PSFILE) > /dev/null


.PHONY: txt
txt: ${TXTFILE}

${TXTFILE}: ${FOFILE}
	@XML_CATALOG_FILES="$(CATALOG)" $(XSLTPROC) --xinclude --output ${HTMLFILE} html.xsl $(DOCFILE)
	lynx -dump ${HTMLFILE} > ${TXTFILE}

.PHONY: clean
clean: 
	@rm -f *~ *.bak *.orig \#*\# .\#* texput.log
	@rm -rf html $(PSFILE) $(HTMLFILE) $(PDFFILE) html-stamp
	@rm -f $(TMPFILES) $(FOFILE)
	@rm -f  $(DVIFILE) $(TXTFILE)

.PHONY: distclean
distclean: clean

# Makefile for SBCL document generation

# This file is part of the SBCL system. It was copied from UFFI and
# placed into the public domain by the original author, Kevin
# Rosenberg.

DOCFILE_BASE_DEFAULT:=user-manual
DOCFILE_EXT_DEFAULT:=xml

SYSTEM:=$(shell uname)

ifeq ($(SYSTEM),Linux) 
  ifneq ($(shell expr "`cat /etc/issue`" : '.*Debian.*'),0)
    # Old Debian used /usr/share/sgml/docbook, new Debian uses
    # /usr/share/xml/docbook. 
    ifneq ($(shell expr "`ls -d /usr/share/xml`" : '.*/usr/share/xml.*'),0)
      OS:=debian
    else
      # Evidently it's not a new-style Debian DocBook setup, ergo:
      OS:=debian-old
    endif
  else
    ifneq ($(shell expr "`cat /etc/issue`" : '.*SuSE.*'),0)
      OS=suse
    else   
      ifneq ($(shell expr "`cat /etc/issue`" : 'Red Hat Linux release 9'),0)
        OS=redhat9
      else   
        ifneq ($(shell expr "`cat /etc/issue`" : '.*Yarrow.*'),0)
          OS=fedora1
        endif
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

CATALOG:=`pwd`/catalogs/catalog-${OS}.xml
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

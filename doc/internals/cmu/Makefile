LATEX=latex
PDFLATEX=pdflatex
DVIPS=dvips
DVIPSRES=1200
DVIPSOPTS=-t a4 -D $(DVIPSRES)
RM=rm
CP=cp
MV=mv

HTML_SPLIT_LEVEL=2

MASTERFILE=design

FILES=architecture.tex back.tex compiler-overview.tex compiler.tex debugger.tex \
      design.tex environment.tex fasl.tex front.tex glossary.tex interface.tex \
      interpreter.tex lowlev.tex middle.tex object.tex retargeting.tex \
      run-time.tex vm.tex 

FIGURES=

all : $(MASTERFILE).pdf

$(MASTERFILE).dvi : $(FILES) $(FIGURES)
	$(LATEX) $(MASTERFILE).tex

# $(MASTERFILE).aux $(MASTERFILE).toc : $(FILES) $(FIGURES)
# 		$(LATEX) $(MASTERFILE).tex
# 		$(RM) $(MASTERFILE).dvi

# $(MASTERFILE).dvi : $(FILES) $(MASTERFILE).aux $(MASTERFILE).toc $(FIGURES)
# 		$(LATEX) $(MASTERFILE).tex

# Three times should be enough for everyone(tm).
.PHONY: firsttime
firsttime: $(MASTERFILE).dvi
	$(LATEX) $(MASTERFILE).tex
	$(LATEX) $(MASTERFILE).tex


.PRECIOUS: %.aux %.toc %.tex


%.dvi : %.tex
	$(LATEX) $<
	@while ( grep Rerun $*.log > /dev/null ); do \
	   $(LATEX) $<; \
	done
	$(LATEX) $<

%.eps :	%.fig
	fig2dev -L ps $^ $@

%.pdf : %.tex
	$(PDFLATEX) $^
	@while ( grep 'Rerun to get cross' $*.log > /dev/null ); do \
	   $(PDFLATEX) $<; \
	done
	$(PDFLATEX) $<

%.ps : %.dvi
	$(DVIPS) $(DVIPSOPTS) -o $@ $*

.PHONY: html
html : $(MASTERFILE).dvi $(FILES) $(FIGURES)
	latex2html -local_icons -antialias_text -antialias -scalable_fonts \
        -split $(HTML_SPLIT_LEVEL) -transparent -white $(MASTERFILE).tex

clean distclean cvsclean :
	-$(RM) -f *.log *.toc *.aux *.dvi *~ *.bak *.ps *.pdf core *.eps
	-$(RM) -rf design

.PHONY : allformats
allformats: $(FILES) $(FIGURES)
	$(MAKE) firsttime LATEX=pslatex
	$(MAKE) design.ps ; $(MV) design.ps design-psfonts.ps
	$(MAKE) firsttime
	$(MAKE) design.ps DVIPSRES=1200 ; $(MV) design.ps design-1200.ps
	$(MAKE) design.ps DVIPSRES=600 ; $(MV) design.ps design-600.ps
	$(MAKE) design.ps DVIPSRES=300 ; $(MV) design.ps design-300.ps
	$(MAKE) design.pdf
	$(MAKE) html HTML_SPLIT_LEVEL=3 ; $(MV) design design-split3
	$(MAKE) html HTML_SPLIT_LEVEL=2 ; $(MV) design design-split2
	$(MAKE) html HTML_SPLIT_LEVEL=1 ; $(MV) design design-split1

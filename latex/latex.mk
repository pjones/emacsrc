# -*- mode: makefile-gmake -*-

############################################################################
LATEX_FILES ?= $(wildcard *.tex)
DOT_FILES   ?= $(wildcard *.dot)
LATEST_TEX  ?= $(shell ls -rt *.tex|tail -1)
ALSO_REMOVE  = aux glg glo gls ist xdy 4ct 4tc idv lg nav snm vrb
# SINGLE_PDF = Set to YES for generating a single PDF file

############################################################################
all: $(DOT_FILES:.dot=.pdf) $(LATEX_FILES:.tex=.pdf)

############################################################################
clean:
	@for f in $(LATEX_FILES); do latexmk -CA $$f > /dev/null 2>&1; done
	@for f in $(DOT_FILES:.dot=.pdf); do rm -f $$f; done
	@rm -f $(foreach f,$(ALSO_REMOVE),*.$(f))
	@rm -f $(LATEX_FILES:.tex=.tmp) $(LATEX_FILES:.tex=.html) \
	       $(LATEX_FILES:.tex=.css) $(LATEX_FILES:.tex=.xref)

############################################################################
define OPEN_FOR_LATEST_PDF
open: $(DOT_FILES:.dot=.pdf) $(LATEST_TEX:.tex=.pdf)
	@ open $(LATEST_TEX:.tex=.pdf)
endef

############################################################################
define OPEN_FOR_SINGLE_PDF
open: $(LATEX_FILES:.tex=.pdf)
	@ open $$<
endef

############################################################################
$(eval $(call $(if $(SINGLE_PDF),OPEN_FOR_SINGLE_PDF,OPEN_FOR_LATEST_PDF)))

############################################################################
.SUFFIXES: .tex .pdf .dot
.PHONY: open clean

############################################################################
%.pdf: %.tex
	latexmk -pdf $<

############################################################################
%.html: %.tex
	htlatex $< "xhtml"
	@ sed -i '' -e 's/®;/\&#174;/g' $@

############################################################################
%.pdf: %.dot
	dot -Tpdf -o $@ $<
	pdfcrop $@ $@_crop
	mv $@_crop $@

LATEX_FILES ?= $(wildcard *.tex)
DOT_FILES ?= $(wildcard *.dot)
LATEST_TEX = $(shell ls -rt *.tex|tail -1)
ALSO_REMOVE = glg glo gls ist xdy

all: $(DOT_FILES:.dot=.pdf) $(LATEX_FILES:.tex=.pdf)

clean:
	@for f in $(LATEX_FILES); do latexmk -C $$f; done
	@for f in $(DOT_FILES:.dot=.pdf); do rm -f $$f; done
	@rm -f $(foreach f,$(ALSO_REMOVE),*.$(f))

open: $(DOT_FILES:.dot=.pdf) $(LATEST_TEX:.tex=.pdf)
	@open $(LATEST_TEX:.tex=.pdf)

.SUFFIXES: .tex .pdf .dot

%.pdf: %.tex
	latexmk -pdf $<

%.pdf: %.dot
	dot -Tpdf -o $@ $<
	pdfcrop $@ $@_crop
	mv $@_crop $@

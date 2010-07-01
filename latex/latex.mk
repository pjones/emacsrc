LATEX_FILES ?= $(wildcard *.tex)
LATEST_TEX = $(shell ls -rt *.tex|tail -1)

all: $(LATEX_FILES:.tex=.pdf)

clean:
	@for f in $(LATEX_FILES); do latexmk -C $$f; done

open: $(LATEST_TEX:.tex=.pdf)
	@open $<

.SUFFIXES: .tex .pdf

%.pdf: %.tex
	latexmk -pdf $<

LATEX_FILES ?= $(wildcard *.tex)

all: $(LATEX_FILES:.tex=.pdf)

clean:
	@for f in $(LATEX_FILES); do latexmk -C $$f; done

open: all
	@open `ls -rt $(LATEX_FILES)|tail -1|sed 's/\.tex$$/.pdf/'`

.SUFFIXES: .tex .pdf

%.pdf: %.tex
	latexmk -pdf $<

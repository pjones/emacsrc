LATEX_FILES ?= $(wildcard *.tex)

all: $(LATEX_FILES:.tex=.pdf)

clean:
	@for f in $(LATEX_FILES); do latexmk -C $$f; done

.SUFFIXES: .tex .pdf

%.pdf: %.tex
	latexmk -pdf $<

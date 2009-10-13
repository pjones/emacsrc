LATEX_FILES ?= $(wildcard *.tex)

all: $(LATEX_FILES:.tex=.pdf)

clean:
	rm -f $(LATEX_FILES:.tex=.aux)
	rm -f $(LATEX_FILES:.tex=.log)
	rm -f $(LATEX_FILES:.tex=.out)
	rm -f $(LATEX_FILES:.tex=.toc)
	rm -f $(LATEX_FILES:.tex=.pdf)

.SUFFIXES: .tex .pdf

%.pdf: %.tex
	pdflatex $<
	pdflatex $<
	pdflatex $<

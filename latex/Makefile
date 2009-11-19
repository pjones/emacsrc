all: $(HOME)/.latexmkrc
	@(cd packages && $(MAKE))

$(HOME)/.latexmkrc: latexmkrc
	cp -p $< $@

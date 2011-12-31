.PHONEY: all

all: $(HOME)/.gitconfig $(HOME)/.gitignore
	@:

$(HOME)/.gitconfig: gitconfig
	cp -p $< $@

$(HOME)/.gitignore: gitignore
	cp -p $< $@

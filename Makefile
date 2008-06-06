DIRS = download packages emacs.d
DOTFILE = $(HOME)/.emacs

all: $(DOTFILE)
	@ for d in $(DIRS); do $(MAKE) -C $$d; done

uninstall:
	@ for d in $(DIRS); do $(MAKE) -C $$d $@; done

$(DOTFILE): dot.emacs.el
	cp $< $@

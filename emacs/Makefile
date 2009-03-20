DIRS = emacs.d download packages
DOTFILE = $(HOME)/.emacs

all: $(DOTFILE)

all uninstall:
	@ for d in $(DIRS); do $(MAKE) -C $$d $@ || exit 1; done

$(DOTFILE): dot.emacs.el
	cp $< $@

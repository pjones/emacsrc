DIRS = download packages lisp modes bin
DOTFILE = $(HOME)/.emacs

.PHONEY: all

all: $(DOTFILE)

all uninstall:
	@ for d in $(DIRS); do $(MAKE) -C $$d $@ || exit 1; done

$(DOTFILE): dot.emacs.el
	cp $< $@

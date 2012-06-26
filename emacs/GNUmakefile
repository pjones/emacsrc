DIRS = download third-party lisp modes themes bin
DOTFILE = $(HOME)/.emacs
MAKE_ARGS = --no-print-directory

.PHONEY: all

all: $(DOTFILE)

all uninstall:
	@ for d in $(DIRS); do $(MAKE) $(MAKE_ARGS) -C $$d $@ || exit 1; done

$(DOTFILE): dot.emacs.el
	cp $< $@

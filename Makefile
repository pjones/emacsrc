DIRS = zsh emacs

all:
	@ for d in $(DIRS); do $(MAKE) -C $$d $@ || exit 1; done

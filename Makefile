DIRS = zsh emacs git ruby screen

all:
	@ for d in $(DIRS); do $(MAKE) -C $$d $@ || exit 1; done

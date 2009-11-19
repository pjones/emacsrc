DIRS = emacs git gnus ruby screen zsh latex

all:
	@ for d in $(DIRS); do $(MAKE) -C $$d $@ || exit 1; done

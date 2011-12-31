DIRS = gnus.d

.PHONEY: all

all: $(HOME)/.gnus.el
	@ for d in $(DIRS); do $(MAKE) -C $$d $@ || exit 1; done

$(HOME)/.gnus.el: dot.gnus.el
	cp -p $< $@

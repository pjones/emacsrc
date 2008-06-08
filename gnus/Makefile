DIRS = gnus.d

all:
	@ for d in $(DIRS); do $(MAKE) -C $$d $@ || exit 1; done

all: $(HOME)/.gnus.el

$(HOME)/.gnus.el: dot.gnus.el
	cp -p $< $@

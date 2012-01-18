DEST = $(HOME)/.xmonad

.PHONEY: all

all: $(DEST) $(DEST)/xmonad.hs
	@:

$(DEST):
	mkdir -p $(DEST)

$(DEST)/xmonad.hs: xmonad.hs
	install -m 0640 xmonad.hs $@

DEST = $(HOME)/.xmonad

.PHONEY: all

all: $(DEST) $(DEST)/xmonad.hs
	@:

$(DEST):
	mkdir -p $(DEST)

$(DEST)/xmonad.hs: $(DEST) xmonad.hs
	install -m 0640 xmonad.hs $@

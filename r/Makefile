.PHONEY: all

all: install
	@:

install: $(HOME)/.Rprofile
	@:

$(HOME)/.Rprofile: profile.R
	install -m 0444 $< $@

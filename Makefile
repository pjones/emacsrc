# -*- mode: makefile-gmake -*-

################################################################################
DIRS = bin

################################################################################
ifeq (zsh,$(notdir $(shell which zsh)))
DIRS += zsh
endif

################################################################################
ifeq (emacs,$(notdir $(shell which emacs)))
DIRS += emacs
DIRS += gnus
endif

################################################################################
ifeq (conkeror,$(notdir $(shell which conkeror)))
DIRS += conkeror
endif

################################################################################
ifeq (texdoc,$(notdir $(shell which texdoc)))
DIRS += latex
endif

################################################################################
ifeq (screen,$(notdir $(shell which screen)))
DIRS += screen
endif

################################################################################
ifeq (sa-learn,$(notdir $(shell which sa-learn)))
DIRS += spamassassin
endif

################################################################################
ifeq (git,$(notdir $(shell which git)))
DIRS += git
endif

################################################################################
ifeq (mpd,$(notdir $(shell which mpd)))
DIRS += mpd
endif

################################################################################
ifeq (R,$(notdir $(shell which R)))
DIRS += r
endif

################################################################################
ifeq (ruby,$(notdir $(shell which ruby)))
DIRS += ruby
endif

################################################################################
ifeq (tmux,$(notdir $(shell which tmux)))
DIRS += tmux
endif

################################################################################
ifeq (startx,$(notdir $(shell which startx)))
DIRS += x
endif

################################################################################
ifeq (xmonad,$(notdir $(shell which xmonad)))
DIRS += xmonad
endif

################################################################################
ifeq (Darwin,$(shell uname))
DIRS += macosx
endif

################################################################################
.PHONEY: all install

################################################################################
all:
	@ for d in $(DIRS); do $(MAKE) --no-print-directory -C $$d $@ || exit 1; done

################################################################################
install: all

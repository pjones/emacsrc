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
ifeq (R,$(notdir $(shell which R)))
DIRS += r
endif

################################################################################
ifeq (ruby,$(notdir $(shell which ruby)))
DIRS += ruby
endif

################################################################################
ifeq (Darwin,$(shell uname))
DIRS += macosx
endif

################################################################################
all:
	@ for d in $(DIRS); do $(MAKE) -C $$d $@ || exit 1; done

################################################################################
install: all

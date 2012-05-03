################################################################################
include ../mk/init.mk
include ../mk/files.mk

################################################################################
DEST = $(HOME)/.zsh
HOST = $(shell hostname)
OS   = $(shell uname -s | tr '[:upper:]' '[:lower:]')

################################################################################
FILES  = $(wildcard lib/*.zsh)
FILES += $(wildcard app/*.zsh)
FILES += $(wildcard util/*.zsh)
FILES += $(wildcard func/*.zsh)
FILES += $(wildcard wids/*.zsh)

################################################################################
$(foreach f,$(FILES),$(eval $(call PMADE_INSTALL_FILE,$(f),$(DEST)/$(f))))
$(eval $(call PMADE_INSTALL_DOT,dot/zshrc))
$(eval $(call PMADE_INSTALL_DOT,dot/zshenv))

################################################################################
# Local (hostname based) Override
ifneq ($(wildcard local/$(HOST)),)
  $(eval $(call PMADE_INSTALL_FILE,local/$(HOST),$(DEST)/local))
endif

################################################################################
# OS (os name based) Override
ifneq ($(wildcard os/$(OS)),)
  $(eval $(call PMADE_INSTALL_FILE,os/$(OS),$(DEST)/os))
endif

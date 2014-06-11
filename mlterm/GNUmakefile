################################################################################
include ../mk/init.mk
include ../mk/files.mk

################################################################################
MLTERM_FILES = aafont color key main
MLTERM_DIR   = $(HOME)/.mlterm

################################################################################
$(foreach f,$(MLTERM_FILES),$(eval $(call PMADE_INSTALL_FILE,$(f),$(MLTERM_DIR)/$(f))))

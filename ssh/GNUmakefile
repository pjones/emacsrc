################################################################################
include ../mk/init.mk
include ../mk/files.mk

################################################################################
DIRS = $(HOME)/.ssh/master
HOST_CONFIG = $(HOME)/keys/ssh/$(HOSTNAME).config

################################################################################
all::
	@ mkdir -p $(DIRS)
	@ find $(HOME)/.ssh -type d -exec chmod 700 '{}' ';'
	@ find $(HOME)/.ssh -type f -exec chmod 600 '{}' ';'

################################################################################
ifneq ($(wildcard $(HOST_CONFIG)),"")
$(eval $(call PMADE_SYMLINK_FILE,$(HOST_CONFIG),~/.ssh/config))
else
$(eval $(call PMADE_INSTALL_FILE,config,~/.ssh/config))
endif

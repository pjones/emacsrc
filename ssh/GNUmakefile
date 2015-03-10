################################################################################
include ../mk/init.mk
include ../mk/files.mk

################################################################################
DIRS = $(HOME)/.ssh/master
HOST_CONFIG =  $(wildcard $(HOME)/keys/ssh/$(HOSTNAME).config)
HOST_PRV_KEY = $(wildcard $(HOME)/keys/ssh/$(HOSTNAME).id_[rd]sa)
HOST_PUB_KEY = $(wildcard $(HOME)/keys/ssh/$(HOSTNAME).id_[rd]sa.pub)

################################################################################
all::
	@ mkdir -p $(DIRS)
	@ find $(HOME)/.ssh -type d -exec chmod 700 '{}' ';'
	@ find $(HOME)/.ssh -type f -exec chmod 600 '{}' ';'

################################################################################
ifneq "$(HOST_CONFIG)" ""
$(eval $(call PMADE_SYMLINK_FILE,$(HOST_CONFIG),~/.ssh/config))
else
$(eval $(call PMADE_INSTALL_FILE,config,~/.ssh/config))
endif

################################################################################
ifneq "$(HOST_PRV_KEY)" ""
$(eval $(call PMADE_SYMLINK_FILE,$(HOST_PRV_KEY),~/.ssh/$(notdir $(HOST_PRV_KEY))))
endif

################################################################################
ifneq "$(HOST_PUB_KEY)" ""
$(eval $(call PMADE_SYMLINK_FILE,$(HOST_PUB_KEY),~/.ssh/$(notdir $(HOST_PUB_KEY))))
endif

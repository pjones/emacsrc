################################################################################
include ../mk/init.mk
include ../mk/files.mk

################################################################################
$(eval $(call PMADE_INSTALL_BIN,xinitrc,~/.xinitrc))
$(eval $(call PMADE_INSTALL_DOT,gtkrc-2.0.mine))
$(eval $(call PMADE_INSTALL_DOT,keynavrc))
$(eval $(call PMADE_INSTALL_FILE,user-dirs.conf,~/.config/user-dirs.conf))
$(eval $(call PMADE_INSTALL_FILE,xdefaults,~/.Xdefaults))

################################################################################
$(foreach f,$(wildcard urxvt/*),$(eval $(call PMADE_INSTALL_FILE,$(f),~/.$(f))))

################################################################################
HOSTNAME = $(shell hostname)

################################################################################
# $1: File name without hostname prefix.
define FILE_WITH_HOSTNAME
ifneq ($(wildcard $(HOSTNAME).$(1)),)
  $(call PMADE_INSTALL_FILE,$(HOSTNAME).$(1),~/.$(1))
else
  $(call PMADE_INSTALL_DOT,$(1))
endif
endef

################################################################################
$(eval $(call FILE_WITH_HOSTNAME,xmobarrc))
$(eval $(call FILE_WITH_HOSTNAME,xmodmaprc))

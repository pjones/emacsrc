################################################################################
include ../mk/init.mk
include ../mk/files.mk

################################################################################
$(eval $(call PMADE_INSTALL_DOT,gtkrc-2.0.mine))
$(eval $(call PMADE_INSTALL_DOT,keynavrc))
$(eval $(call PMADE_INSTALL_FILE,user-dirs.conf,~/.config/user-dirs.conf))
$(eval $(call PMADE_INSTALL_FILE,xdefaults,~/.Xdefaults))
$(eval $(call PMADE_INSTALL_FILE,feh.themes,~/.config/feh/themes))

################################################################################
$(foreach f,$(wildcard urxvt/*),$(eval $(call PMADE_INSTALL_FILE,$(f),~/.$(f))))

################################################################################
$(eval $(call PMADE_INSTALL_WITH_HOSTNAME,xmobarrc))
$(eval $(call PMADE_INSTALL_WITH_HOSTNAME,xmodmaprc))

################################################################################
# Xorg initialization.
XINITRC_EXTRA = $(shell find xinitrc.d -type f -name $(HOSTNAME)-\*)
$(eval $(call PMADE_INSTALL_BIN,xinitrc,~/.xinitrc))
$(foreach f,$(XINITRC_EXTRA),\
  $(eval $(call PMADE_INSTALL_BIN,$(f),~/.xinitrc.d/$(notdir $(f)))))

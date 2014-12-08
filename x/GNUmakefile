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
# Xorg initialization.
XINITRC_EXTRA  = $(shell find xinitrc.d -type f -name $(HOSTNAME)-\*)
XINITRC_EXTRA += $(shell find xinitrc.d -type f -regex 'xinitrc.d/[0-9]+-.*')

$(eval $(call PMADE_INSTALL_BIN,xinitrc,~/.xinitrc))
$(eval $(call PMADE_SYMLINK_FILE,~/.xinitrc,~/.xsession))

$(foreach f,$(XINITRC_EXTRA),\
  $(eval $(call PMADE_INSTALL_BIN,$(f),~/.xinitrc.d/$(notdir $(f)))))

# xkb symbol files.
$(foreach f,$(wildcard xkb/symbols/*),\
  $(eval $(call PMADE_INSTALL_FILE,$(f),~/.xkb/symbols/$(notdir $(f)))))

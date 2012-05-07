################################################################################
include ../mk/init.mk
include ../mk/files.mk

################################################################################
ifeq ($(shell hostname),hawkins)
  $(eval $(call PMADE_INSTALL_FILE,config/mpd.conf,~/.mpdconf))
endif

################################################################################
$(eval $(call PMADE_INSTALL_FILE,config/ncmpcpp.config,~/.ncmpcpp/config))
$(eval $(call PMADE_INSTALL_FILE,config/ncmpcpp.keys,~/.ncmpcpp/keys))

################################################################################
$(foreach f,$(wildcard bin/*),$(eval $(call PMADE_INSTALL_BIN,$(f),~/$(f))))

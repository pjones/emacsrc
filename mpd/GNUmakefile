################################################################################
include ../mk/init.mk
include ../mk/files.mk

################################################################################
$(eval $(call PMADE_INSTALL_WITH_HOSTNAME,config/mpd.conf,~/.mpdconf))
$(eval $(call PMADE_INSTALL_FILE,config/beets.yaml,~/.config/beets/config.yaml))

################################################################################
$(eval $(call PMADE_INSTALL_FILE,config/ncmpcpp.config,~/.ncmpcpp/config))
$(eval $(call PMADE_INSTALL_FILE,config/ncmpcpp.keys,~/.ncmpcpp/keys))

################################################################################
$(foreach f,$(wildcard bin/*),$(eval $(call PMADE_INSTALL_BIN,$(f),~/$(f))))

################################################################################
$(foreach l,$(wildcard lib/*),$(eval $(call PMADE_INSTALL_FILE,$(l),~/lib/sh/$(notdir $(l)))))

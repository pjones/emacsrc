################################################################################
include ../mk/init.mk
include ../mk/files.mk

################################################################################
DIRS  = $(HOME)/documents/music
DIRS += $(HOME)/documents/playlists
DIRS += $(HOME)/.mpd/data

################################################################################
all::
	@ mkdir -p $(DIRS)

################################################################################
$(eval $(call PMADE_INSTALL_WITH_HOSTNAME,config/mpd.conf,~/.mpdconf))

################################################################################
$(eval $(call PMADE_INSTALL_FILE,config/ncmpcpp.config,~/.ncmpcpp/config))
$(eval $(call PMADE_INSTALL_FILE,config/ncmpcpp.keys,~/.ncmpcpp/keys))
$(eval $(call PMADE_INSTALL_FILE,misc/streams,~/documents/playlists/streams))

################################################################################
$(foreach f,$(wildcard bin/*),$(eval $(call PMADE_INSTALL_BIN,$(f),~/$(f))))

################################################################################
$(foreach l,$(wildcard lib/*),$(eval $(call PMADE_INSTALL_FILE,$(l),~/lib/sh/$(notdir $(l)))))

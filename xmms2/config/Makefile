DEST_D = $(HOME)/.config/xmms2
DEST_F = $(DEST_D)/clients/nycli.conf

.PHONEY: all

all: $(DEST_F)
	@:

define INSTALL_template
$(1): $(notdir $(1))
	@ mkdir -p $(dir $(1))
	install -m 0644 $$< $$@
endef

$(foreach f,$(DEST_F),$(eval $(call INSTALL_template,$(f))))

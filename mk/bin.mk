# -*- mode: makefile-gmake -*-

################################################################################
SCRIPTS ?= $(shell ls | grep -iv makefile)
DEST    ?= $(HOME)/bin
MODE    ?= 0755

################################################################################
.PHONEY: install

################################################################################
define PMADE_INSTALL_SCRIPT
install: $(DEST)/$(1)
$(DEST)/$(1): $(1)
	@ mkdir -p $$(dir $(1))
	install -m $(MODE) $$< $$@
endef

################################################################################
$(foreach f,$(SCRIPTS),$(eval $(call PMADE_INSTALL_SCRIPT,$(f))))

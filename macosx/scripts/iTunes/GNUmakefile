# -*- mode: makefile-gmake -*-
SCRIPTS_FOR_BIN = $(wildcard it-*)

all: $(foreach i,$(SCRIPTS_FOR_BIN),$(HOME)/bin/$(i))

define INSTALL_INTO_BIN
$(HOME)/bin/$(1): $(1)
	install -m 0777 $(1) $(HOME)/bin
endef

$(foreach i,$(SCRIPTS_FOR_BIN),$(eval $(call INSTALL_INTO_BIN,$(i))))

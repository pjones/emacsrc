# -*- mode: makefile-gmake -*-

################################################################################
# $1: Local source file
# $2: Path to destination
define PMADE_INSTALL_FILE
all: $(2)
$(2): $(1)
	@ mkdir -p `dirname $(2)`
	install -m 0644 $(1) $(2)
endef

################################################################################
# $1: Local source file
# $2: Path to destination
define PMADE_INSTALL_BIN
all: $(2)
$(2): $(1)
	@ mkdir -p `dirname $(2)`
	install -m 0755 $(1) $(2)
endef

################################################################################
# $1: Local source file
define PMADE_INSTALL_DOT
all: ~/.$(notdir $(1))
$(HOME)/.$(notdir $(1)): $(1)
	@ mkdir -p `dirname $$@`
	install -m 0644 $$< $$@
endef

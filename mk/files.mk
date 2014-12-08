# -*- mode: makefile-gmake -*-

################################################################################
HOSTNAME = $(shell hostname)

################################################################################
# $1: Local source file
# $2: Path to destination
define PMADE_INSTALL_FILE
all:: $(2)
$(2): $(1)
	@ mkdir -p `dirname $(2)`
	install -m 0644 $(1) $(2)
endef

################################################################################
# $1: Name of file to create link to.
# $2: Name of the link itself.
define PMADE_SYMLINK_FILE
all:: $(2)
$(2): $(1)
	@ mkdir -p `dirname $(2)`
	ln -nfs $(1) $(2)
endef

################################################################################
# $1: Local source file
# $2: Path to destination
define PMADE_INSTALL_BIN
all:: $(2)
$(2): $(1)
	@ mkdir -p `dirname $(2)`
	install -m 0755 $(1) $(2)
endef

################################################################################
# $1: Local source file
define PMADE_INSTALL_DOT
all:: ~/.$(notdir $(1))
~/.$(notdir $(1)): $(1)
	@ mkdir -p `dirname $$@`
	install -m 0644 $$< $$@
endef

################################################################################
# $1: File name without hostname prefix.
# $2: Optional destination.
define PMADE_INSTALL_WITH_HOSTNAME
ifneq ($(wildcard $(dir $(1))$(HOSTNAME).$(notdir $(1))),)
  $(call PMADE_INSTALL_FILE,$(dir $(1))$(HOSTNAME).$(notdir $(1)),$(2))
else
  $(call PMADE_INSTALL_FILE,$(1),$(2))
endif
endef

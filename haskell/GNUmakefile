################################################################################
include ../mk/init.mk
include ../mk/files.mk
include ../mk/haskell.mk

################################################################################
PACKAGES  = hasktags/0.68.*
PACKAGES += hlint/1.8.*
PACKAGES += pandoc/1.11.*

################################################################################
# $1: Package and binary name.
# $2: Cabal version pattern.
define INSTALL_CABAL_PACKAGE

# Delegate most rules to PMADE_INSTALL_BIN.
$(call PMADE_INSTALL_BIN,cabal-dev/bin/$(1),$(HOME)/bin/$(1))

# This weird rule is needed so that we only call `cabal-dev' once
# instead of once for each binary to build.
cabal-dev/bin/$(1):
	cabal-dev install "$(1) == $(2)"
endef

################################################################################
define PKG_NAME
$(shell echo '$(1)' | sed 's|/.*$$||')
endef

################################################################################
define PKG_VER
$(shell echo '$(1)' | sed 's|^.*/||')
endef

################################################################################
define PKG_SPLIT_AND_CALL
endef

################################################################################
$(eval $(call PMADE_INSTALL_FILE,dot.ghci,~/.ghci))

################################################################################
$(foreach p,$(PACKAGES),$(eval \
  $(call INSTALL_CABAL_PACKAGE,$(call PKG_NAME,$(p)),$(call PKG_VER,$(p)))))

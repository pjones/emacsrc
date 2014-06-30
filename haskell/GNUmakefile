################################################################################
include ../mk/init.mk
include ../mk/files.mk
include ../mk/haskell.mk

################################################################################
# Each package is made up of two mandatory parts and one optional
# part.  Each part is separated with a forward slash.
#
# 1: The package name.
# 2: The package version.
# 3: Any other options to pass to cabal.  Pipes are replaced with spaces.
#
PACKAGES  = hasktags/0.68.*
PACKAGES += hlint/1.8.*
PACKAGES += c2hs/0.17.*
PACKAGES += alex/3.1.*
PACKAGES += happy/1.19.*
PACKAGES += pandoc/1.11.*
PACKAGES += xmobar/0.20.1/--flags="with_xft|with_mpd|with_alsa"

################################################################################
# $1: Package and binary name.
# $2: Cabal version pattern.
# $3: Flags.
define INSTALL_CABAL_PACKAGE

# Delegate most rules to PMADE_INSTALL_BIN.
$(call PMADE_INSTALL_BIN,.cabal-sandbox/bin/$(1),$(HOME)/bin/$(1))

# Install a specific package.
.cabal-sandbox/bin/$(1): cabal.sandbox.config
	cabal install "$(1) == $(2)" $(3)
endef

################################################################################
define PKG_NAME
$(shell echo '$(1)' | cut -d/ -f1)
endef

################################################################################
define PKG_VER
$(shell echo '$(1)' | cut -d/ -f2)
endef

################################################################################
define PKG_FLAGS
$(shell echo '$(1)' | cut -d/ -f3 | sed 's/|/ /g')
endef

################################################################################
# Configuration files.
$(eval $(call PMADE_INSTALL_FILE,dot.ghci,~/.ghci))

################################################################################
$(foreach p,$(PACKAGES),$(eval \
  $(call INSTALL_CABAL_PACKAGE,$(call PKG_NAME,$(p)),$(call PKG_VER,$(p)),$(call PKG_FLAGS,$(p)))))

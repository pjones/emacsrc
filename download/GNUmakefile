################################################################################
PACKAGE_VERSIONS = gnus/m0-11

################################################################################
# Packages I no longer use.
# ifeq (texdoc,$(notdir $(shell which texdoc)))
# PACKAGE_VERSIONS += auctex/11.85
# endif

################################################################################
DEST = $(HOME)/.emacs.d/download
PACKAGES = $(wildcard *.sh)

.PHONEY: all

all: $(DEST)
	@:

define PKG_template
$(DEST)/$(subst /,-,$(1)):
	sh -x $(subst /,,$(dir $(1))).sh $(notdir $(1)) $(if $(VERBOSE),,> /dev/null 2>&1)
	date > $$@
endef

all: $(foreach pkg,$(PACKAGE_VERSIONS),$(DEST)/$(subst /,-,$(pkg)))
$(foreach pkg,$(PACKAGE_VERSIONS),$(eval $(call PKG_template,$(pkg))))

$(DEST):
	mkdir -p $(DEST)

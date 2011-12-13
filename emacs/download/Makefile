PACKAGE_VERSIONS  = orgmode/release_7.8
PACKAGE_VERSIONS += rhtml/20090213
PACKAGE_VERSIONS += magit/1.0.0
PACKAGE_VERSIONS += auctex/11.85
PACKAGE_VERSIONS += gnus/n0-17

DEST = $(HOME)/.emacs.d/download
PACKAGES = $(wildcard *.sh)

all: $(DEST)

define PKG_template
$(DEST)/$(subst /,-,$(1)):
	sh -x $(subst /,,$(dir $(1))).sh $(notdir $(1)) $(if $(VERBOSE),,> /dev/null 2>&1)
	date > $$@
endef

all: $(foreach pkg,$(PACKAGE_VERSIONS),$(DEST)/$(subst /,-,$(pkg)))
$(foreach pkg,$(PACKAGE_VERSIONS),$(eval $(call PKG_template,$(pkg))))

$(DEST):
	mkdir -p $(DEST)



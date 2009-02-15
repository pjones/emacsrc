PACKAGE_VERSIONS  = orgmode/20090213
PACKAGE_VERSIONS += muse/20090215
PACKAGE_VERSIONS += rinari/20090213
PACKAGE_VERSIONS += rhtml/20090213
PACKAGE_VERSIONS += gnus/20090213
PACKAGE_VERSIONS += magit/20090213
PACKAGE_VERSIONS += apel/10.7
PACKAGE_VERSIONS += erc/5.3

DEST = $(HOME)/.emacs.d/download
PACKAGES = $(wildcard *.sh)

all: $(DEST)

define PKG_template
$(DEST)/$(subst /,-,$(1)):
	sh $(subst /,,$(dir $(1))).sh $(notdir $(1))
	date > $$@
endef

all: $(foreach pkg,$(PACKAGE_VERSIONS),$(DEST)/$(subst /,-,$(pkg)))
$(foreach pkg,$(PACKAGE_VERSIONS),$(eval $(call PKG_template,$(pkg))))

$(DEST):
	mkdir -p $(DEST)



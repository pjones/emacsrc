PACKAGE_VERSIONS  = orgmode/20100316
PACKAGE_VERSIONS += muse/20090215
PACKAGE_VERSIONS += rinari/20090411
PACKAGE_VERSIONS += rhtml/20090213
PACKAGE_VERSIONS += magit/20090906
PACKAGE_VERSIONS += apel/10.7
PACKAGE_VERSIONS += erc/5.3
PACKAGE_VERSIONS += epg/0.0.16
PACKAGE_VERSIONS += auctex/11.85
PACKAGE_VERSIONS += ess/5.8
#PACKAGE_VERSIONS += gnus/20090213

DEST = $(HOME)/.emacs.d/download
PACKAGES = $(wildcard *.sh)

all: $(DEST)

define PKG_template
$(DEST)/$(subst /,-,$(1)):
	sh $(subst /,,$(dir $(1))).sh $(notdir $(1)) > /dev/null 2>&1
	date > $$@
endef

all: $(foreach pkg,$(PACKAGE_VERSIONS),$(DEST)/$(subst /,-,$(pkg)))
$(foreach pkg,$(PACKAGE_VERSIONS),$(eval $(call PKG_template,$(pkg))))

$(DEST):
	mkdir -p $(DEST)



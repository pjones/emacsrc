PACKAGE_VERSIONS  = orgmode/6.04c
PACKAGE_VERSIONS += apel/10.7
PACKAGE_VERSIONS += erc/5.3
PACKAGE_VERSIONS += muse/3.12
PACKAGE_VERSIONS += rails/git-head
PACKAGE_VERSIONS += rhtml/svn-trunk

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



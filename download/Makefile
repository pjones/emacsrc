PACKAGE_VERSIONS  = orgmode/6.04c
PACKAGE_VERSIONS += apel/10.7

DEST = $(HOME)/.emacs.d/download
PACKAGES = $(wildcard *.sh)

all: $(DEST)

define PKG_template
$(DEST)/$(subst /,-,$(1)).installed:
	sh $(subst /,,$(dir $(1))).sh $(notdir $(1))
	date > $$@
endef

all: $(foreach pkg,$(PACKAGE_VERSIONS),$(DEST)/$(subst /,-,$(pkg)).installed)
$(foreach pkg,$(PACKAGE_VERSIONS),$(eval $(call PKG_template,$(pkg))))

$(DEST):
	mkdir -p $(DEST)



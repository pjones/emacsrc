PACKAGE_VERSIONS  = orgmode/6.06a
PACKAGE_VERSIONS += apel/10.7
PACKAGE_VERSIONS += erc/5.3
PACKAGE_VERSIONS += muse/3.12
PACKAGE_VERSIONS += rhtml/svn-trunk
PACKAGE_VERSIONS += gnus/git-head
PACKAGE_VERSIONS += rinari/master
PACKAGE_VERSIONS += rails/delete
#PACKAGE_VERSIONS += magit/52abed7629f23ebff56799c557a4ef0299df032c

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



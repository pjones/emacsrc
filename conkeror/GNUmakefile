FILES = $(shell ls | egrep -v 'Makefile')
DEST  = $(HOME)/.conkerorrc

.PHONEY: all

all: $(DEST) $(foreach f,$(FILES),$(DEST)/$(f))
	@:

$(DEST):
	mkdir -p $@

define INSTALL_FILE
$(DEST)/$(1): $(1)
	install -m 0640 $$< $$@
endef

$(foreach f,$(FILES),$(eval $(call INSTALL_FILE,$(f))))

DEST = $(HOME)/.zsh
HOST = $(shell hostname)
OS   = $(shell uname -s | tr '[:upper:]' '[:lower:]')

.PHONEY: all
all:
	@:

define DIR_template
all: $(1)
$(1):
	mkdir -p $(1)
endef

define COPY_template
all: $(1)
$(1): $(2)
	install -m 0640 $(2) $(1)
endef

define COMPILE_template
all: $(1).zwc
$(1).zwc: $(2)
	zsh -c 'zcompile -U $(2)'
	install -m 0644 $(2).zwc $(1).zwc
	@ rm -f $(2).zwc
endef

# Top level directory
$(eval $(call DIR_template,$(DEST)/lib))
$(foreach f,$(wildcard lib/*.zsh),$(eval $(call COPY_template,$(DEST)/lib/$(notdir $(f)),$(f))))

# App files
$(eval $(call DIR_template,$(DEST)/app))
$(foreach f,$(wildcard app/*.zsh),$(eval $(call COPY_template,$(DEST)/app/$(notdir $(f)),$(f))))

# Util files
$(eval $(call DIR_template,$(DEST)/util))
$(foreach f,$(wildcard util/*.zsh),$(eval $(call COPY_template,$(DEST)/util/$(notdir $(f)),$(f))))

# Function files
FUNC_DEST  = $(DEST)/func
FUNC_FILES = $(wildcard func/*)
$(eval $(call DIR_template,$(FUNC_DEST)))
$(foreach f,$(FUNC_FILES),$(eval $(call COPY_template,$(FUNC_DEST)/$(notdir $(f)),$(f))))

# Widget Files
WID_DEST  = $(DEST)/wids
WID_FILES = $(wildcard wids/*)
$(eval $(call DIR_template,$(WID_DEST)))
$(foreach f,$(WID_FILES),$(eval $(call COPY_template,$(WID_DEST)/$(notdir $(f)),$(f))))

# Local (hostname based) Override
ifneq (,$(wildcard local/$(HOST)))
all: $(DEST)/local
$(DEST)/local: local/$(HOST)
	cp -p $< $@
endif

# OS (os name based) Override
ifneq (,$(wildcard os/$(OS)))
all: $(DEST)/os
$(DEST)/os: os/$(OS)
	cp -p $< $@
endif

# The main startup files
all: $(HOME)/.zshrc
$(HOME)/.zshrc: dot/zshrc
	install -m 0640 $< $@

all: $(HOME)/.zshenv
$(HOME)/.zshenv: dot/zshenv
	install -m 0640 $< $@

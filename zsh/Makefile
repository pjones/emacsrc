DEST = $(HOME)/.zsh
HOST = $(shell hostname)

define DIR_template
all: $(1)
$(1):
	mkdir -p $(1)
endef

define COPY_template
all: $(1)
$(1): $(2)
	cp -p $(2) $(1)
endef

# Top level directory
$(eval $(call DIR_template,$(DEST)))
$(foreach f,$(wildcard main/*),$(eval $(call COPY_template,$(DEST)/$(notdir $(f)),$(f))))

# Files in bin
BIN_DEST  = $(HOME)/bin
BIN_FILES = $(wildcard bin/*)
$(eval $(call DIR_template,$(BIN_DEST)))
$(foreach f,$(BIN_FILES),$(eval $(call COPY_template,$(BIN_DEST)/$(notdir $(f)),$(f))))

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

# The main startup file
all: $(HOME)/.zshrc
$(HOME)/.zshrc:
	echo "source $(HOME)/.zsh/zshrc" > $@

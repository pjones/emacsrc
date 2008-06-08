DEST = $(HOME)/.zsh
HOST = $(shell hostname)

define COPY_template
all: $(1)
$(1): $(2)
	cp -p $(2) $(1)
endef

# Files in bin
BIN_DEST  = $(HOME)/bin
BIN_FILES = $(wildcard bin/*)
$(foreach f,$(BIN_FILES),$(eval $(call COPY_template,$(BIN_DEST)/$(notdir $(f)),$(f))))

# Function files
FUNC_DEST  = $(DEST)/func
FUNC_FILES = $(wildcard func/*)
$(foreach f,$(FUNC_FILES),$(eva $(call COPY_template,$(FUNC_DEST)/$(notdir $(f)),$(f))))

# Widget Files
WID_DEST  = $(DEST)/wids
WID_FILES = $(wildcard wids/*)
$(foreach f,$(WID_FILES),$(eval $(call COPY_template,$(WID_DEST)/$(notdir $(f)),$(f))))

# Main Files
$(foreach f,$(wildcard main/*),$(eval $(call COPY_template,$(DEST)/$(notdir $(f)),$(f))))

# Local (hostname based) Override
all: $(DEST)/local
$(DEST)/local: local/$(HOST)
	cp -p $< $@

# The main startup file
all: $(HOME)/.zshrc
$(HOME)/.zshrc:
	echo "source $(HOME)/.zsh/zshrc" > $@

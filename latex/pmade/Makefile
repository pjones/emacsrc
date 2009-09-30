DEST = $(HOME)/Library/texmf/tex

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

$(eval $(call DIR_template,$(DEST)))
$(foreach f,$(wildcard *.sty),$(eval $(call COPY_template,$(DEST)/$(notdir $(f)),$(f))))

DEST = $(HOME)/texmf/tex/latex

.PHONEY: all

define DIR_template
all: $(1)
$(1):
	mkdir -p $(1)
endef

define COPY_template
all: $(DEST)/$(basename $(1))/$(1)
$(DEST)/$(basename $(1))/$(1): $(1)
	@ mkdir -p $$(dir $$@)
	@ cp -p $$< $$@
	@ echo install $$<
endef

$(eval $(call DIR_template,$(DEST)))
$(foreach f,$(wildcard *.sty),$(eval $(call COPY_template,$(f))))

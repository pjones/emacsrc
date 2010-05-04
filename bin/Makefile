FILES = $(shell ls | grep -v Makefile)

all: $(HOME)/bin $(foreach f,$(FILES),$(HOME)/bin/$(f))

$(HOME)/bin:
	mkdir -p $@

define COPY_template
$(HOME)/bin/$(1): $(1) $(HOME)/bin
	install -m 0770 $(1) $(HOME)/bin/$(1)
endef

$(foreach f,$(FILES),$(eval $(call COPY_template,$(f))))

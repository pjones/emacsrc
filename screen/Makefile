all: $(HOME)/.screenrc

$(HOME)/.screenrc: screenrc
	cp -p $< $@

$(HOME)/.screen:
	mkdir $@

define SESSIONS_template
all: $(HOME)/.screen/$(1)
$(HOME)/.screen/$(1): sessions/$(1)
	cp $$< $$@
endef

$(foreach f,$(wildcard sessions/*),$(eval $(call SESSIONS_template,$(notdir $(f)))))

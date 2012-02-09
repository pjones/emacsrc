FILES = $(shell ls | egrep -v 'Makefile|xdefaults|urxvt|user-dirs')

.PHONEY: all

all: $(foreach f,$(FILES),$(HOME)/.$(f)) $(HOME)/.Xdefaults $(HOME)/.config/user-dirs.conf
	@:

define COPY_template
$(HOME)/.$(2): $(1)
	install -m 0640 $$< $$@
	@ if [ $(1) = 'xinitrc' ]; then chmod 750 $$@; fi
endef

$(foreach f,$(FILES),$(eval $(call COPY_template,$(f),$(f))))
$(eval $(call COPY_template,xdefaults,Xdefaults))
$(eval $(call COPY_template,user-dirs.conf,config/user-dirs.conf))

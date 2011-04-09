HOST = $(shell hostname)

all:
	$(MAKE) -C scripts/iTunes all

ifneq (,$(wildcard crontab/$(HOST)))
all: $(HOME)/.crontab-stamp
$(HOME)/.crontab-stamp: crontab/$(HOST)
	crontab crontab/$(HOST)
	touch $@
endif

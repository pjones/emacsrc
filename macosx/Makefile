HOST = $(shell hostname)

all:

ifneq (,$(wildcard crontab/$(HOST)))
all:
	crontab crontab/$(HOST)
endif

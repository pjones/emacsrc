################################################################################
SCRIPTS = $(shell find . -type f | grep -iv makefile | grep -v realpath)

################################################################################
ifneq ($(notdir $(shell which realpath 2>/dev/null)),realpath)
  SCRIPTS += realpath
endif

################################################################################
include ../mk/init.mk
include ../mk/bin.mk

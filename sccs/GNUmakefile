################################################################################
include ../mk/init.mk
include ../mk/files.mk

################################################################################
PROG_TEST = if which $1 > /dev/null 2>&1; then echo "YES"; else echo "NO"; fi

################################################################################
# Git:
ifeq "$(shell $(call PROG_TEST,git))" "YES"
$(eval $(call PMADE_INSTALL_DOT,gitconfig))
$(eval $(call PMADE_INSTALL_DOT,gitignore))
$(eval $(call PMADE_INSTALL_DOT,mrconfig))
endif

################################################################################
# Darcs:
ifeq "$(shell $(call PROG_TEST,darcs))" "YES"
DARCS_DEST = $(HOME)/.darcs
$(eval $(call PMADE_INSTALL_FILE,darcs.author,$(DARCS_DEST)/author))
endif

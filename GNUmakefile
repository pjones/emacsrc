################################################################################
DIRS = bin
MAKE_FLAGS = --no-print-directory

################################################################################
# Add a directory to the list of directories to run make inside of if
# a tool is found in PATH.
#
# $1: The directory to add.
# $2: The tool that should be in PATH (defaults to $1)
define MAYBE_ADD_DIRECTORY
ifeq ($(if $(2),$(2),$(1)),$(notdir $(shell which $(if $(2),$(2),$(1)))))
DIRS += $(1)
endif
endef

################################################################################
# Simple tests for tools with matching directories in this repo.
TOOLS = zsh emacs conkeror git mpd ruby tmux xmonad
$(foreach t,$(TOOLS),$(eval $(call MAYBE_ADD_DIRECTORY,$(t))))

################################################################################
# More complicated tests.
$(eval $(call MAYBE_ADD_DIRECTORY,haskell,ghci))
$(eval $(call MAYBE_ADD_DIRECTORY,latex,texdoc))
$(eval $(call MAYBE_ADD_DIRECTORY,spamassassin,sa-learn))
$(eval $(call MAYBE_ADD_DIRECTORY,r,R))
$(eval $(call MAYBE_ADD_DIRECTORY,x,startx))

################################################################################
# MacOS X specific directory.
ifeq (Darwin,$(shell uname))
DIRS += macosx
endif

################################################################################
.PHONEY: all install

################################################################################
all:
	@ for d in $(DIRS); do \
	  $(MAKE) $(MAKE_FLAGS) -C $$d $@ || exit 1; \
	done

################################################################################
install: all

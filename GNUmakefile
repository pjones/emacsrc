################################################################################
include mk/init.mk

################################################################################
MAKEFLAGS = --no-print-directory

################################################################################
# Add a directory to the list of directories to run make inside of if
# a tool is found in PATH.
#
# $1: The directory to add.
# $2: The tool that should be in PATH.
define MAYBE_ADD_DIRECTORY
all::
	@ if which $(2) > /dev/null 2>&1; then \
            echo "==> $(1)"; $(MAKE) -C $(1); \
          fi
endef

################################################################################
# If using Nix, install packages first.
$(eval $(call MAYBE_ADD_DIRECTORY,nix,nix-env))

################################################################################
# Now everyone else.
$(eval $(call MAYBE_ADD_DIRECTORY,bin,sh))
$(eval $(call MAYBE_ADD_DIRECTORY,conkeror,conkeror))
$(eval $(call MAYBE_ADD_DIRECTORY,git,git))
$(eval $(call MAYBE_ADD_DIRECTORY,gnus,emacs))
$(eval $(call MAYBE_ADD_DIRECTORY,haskell,ghc))
$(eval $(call MAYBE_ADD_DIRECTORY,latex,texdoc))
$(eval $(call MAYBE_ADD_DIRECTORY,misc,sh))
$(eval $(call MAYBE_ADD_DIRECTORY,mlterm,mlterm))
$(eval $(call MAYBE_ADD_DIRECTORY,mpd,mpd))
$(eval $(call MAYBE_ADD_DIRECTORY,r,R))
$(eval $(call MAYBE_ADD_DIRECTORY,ruby,ruby))
$(eval $(call MAYBE_ADD_DIRECTORY,spamassassin,sa-learn))
$(eval $(call MAYBE_ADD_DIRECTORY,tmux,tmux))
$(eval $(call MAYBE_ADD_DIRECTORY,x,X))
$(eval $(call MAYBE_ADD_DIRECTORY,zsh,zsh))

################################################################################
# MacOS X specific directory.
ifeq (Darwin,$(shell uname))
$(eval $(call MAYBE_ADD_DIRECTORY,macos,sh))
endif

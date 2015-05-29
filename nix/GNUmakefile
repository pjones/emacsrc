################################################################################
include ../mk/init.mk
include ../mk/files.mk

################################################################################
NIX_REMOVE ?= NO # FIXME: change back to YES
NIXPKGS_DEST = $(HOME)/.nixpkgs

################################################################################
PKG_FILES = $(shell find pkgs -type f -name \*.nix)
NIXPKGS_CONFIG_DEST = $(NIXPKGS_DEST)/config.nix

################################################################################
NIX_ENV_TOUCH = $(HOME)/.cache/nixpkgs/last-install.date
ENV_SRC_FILES = $(shell find envs -type f -name \*.nix)

################################################################################
CHANNEL_FILE = $(HOME)/.nix-defexpr/channels/nixpkgs/default.nix
CHANNEL_URL = http://nixos.org/channels/nixpkgs-unstable

################################################################################
# Just in case NIX_PATH doesn't include nixpkgs from a channel,
# probably because nix-channel command below hasn't run yet.
NIX_PATH := nixpkgs=$(CHANNEL_FILE)$(if $(NIX_PATH),:$(NIX_PATH))
export NIX_PATH

################################################################################
ifeq "$(NIX_REMOVE)" "YES"
  NIX_ENV_OPTIONS = -ir
else
  NIX_ENV_OPTIONS = -i
endif

################################################################################
all:: $(NIX_ENV_TOUCH) $(NIXPKGS_CONFIG_DEST)

################################################################################
$(NIX_ENV_TOUCH): $(PKG_FILES)
	nix-env -f hosts/$(HOSTNAME).nix $(NIX_ENV_OPTIONS)
	@ mkdir -p `dirname $@`
	@ date > $@

################################################################################
$(CHANNEL_FILE):
	nix-channel --add $(CHANNEL_URL) nixpkgs
	nix-channel --update

################################################################################
$(NIXPKGS_CONFIG_DEST): $(notdir $(NIXPKGS_CONFIG_DEST)) $(CHANNEL_FILE) $(PKG_FILES)
	mkdir -p $(dir $@)
	install -m 0644 $< $@

################################################################################
$(foreach f,$(ENV_SRC_FILES),\
  $(eval $(call PMADE_INSTALL_FILE,$(f),$(NIXPKGS_DEST)/$(f))))

################################################################################
include ../mk/init.mk
include ../mk/files.mk

################################################################################
NIX_REMOVE ?= NO # FIXME: change back to YES

################################################################################
NIX_FILES = $(shell find . -type f -name \*.nix)
NIXPKGS_CONFIG_DEST = $(HOME)/.nixpkgs/config.nix
NIX_ENV_TOUCH = $(HOME)/nixpkgs/last-install.date
CHANNEL_FILE = $(HOME)/.nix-defexpr/channels/nixpkgs/default.nix
CHANNEL_URL = http://nixos.org/channels/nixpkgs-unstable

################################################################################
# Just in case NIX_PATH doesn't include nixpkgs from a channel,
# probably because nix-channel command below hasn't run yet.
NIX_PATH := nixpkgs=$(CHANNEL_FILE)$(if $(NIX_PATH),:$(NIX_PATH))
export NIX_PATH

################################################################################
ifeq "$(NIX_REMOVE)" "YES"
  NIX_ENV_OPTIONS = "-ir"
else
  NIX_ENV_OPTIONS = "-i"
endif

################################################################################
all:: $(NIX_ENV_TOUCH) $(NIXPKGS_CONFIG_DEST)

################################################################################
$(NIX_ENV_TOUCH): $(NIX_FILES)
	nix-env -f hosts/$(HOSTNAME).nix $(NIX_ENV_OPTIONS)
	@ mkdir -p `dirname $@`
	@ date > $@

################################################################################
$(CHANNEL_FILE):
	nix-channel --add $(CHANNEL_URL) nixpkgs
	nix-channel --update

################################################################################
$(NIXPKGS_CONFIG_DEST): $(notdir $(NIXPKGS_CONFIG_DEST)) $(CHANNEL_FILE) $(NIX_FILES)
	mkdir -p $(dir $@)
	install -m 0644 $< $@

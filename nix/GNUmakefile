################################################################################
include ../mk/init.mk
include ../mk/files.mk

################################################################################
NIX_FILES = $(shell find . -type f -name \*.nix)
NIXPKGS_CONFIG_DEST = $(HOME)/.nixpkgs/config.nix
CHANNEL_FILE = $(HOME)/.nix-defexpr/channels/nixpkgs/default.nix
CHANNEL_URL = http://nixos.org/channels/nixpkgs-unstable

################################################################################
# Just in case NIX_PATH doesn't include nixpkgs from a channel,
# probably because nix-channel command below hasn't run yet.
NIX_PATH = nixpkgs=$(CHANNEL_FILE)$(if $(NIX_PATH),:$(NIX_PATH))
export NIX_PATH

################################################################################
all:: $(NIXPKGS_CONFIG_DEST)

################################################################################
$(CHANNEL_FILE):
	nix-channel --add $(CHANNEL_URL) nixpkgs
	nix-channel --update

################################################################################
$(NIXPKGS_CONFIG_DEST): $(notdir $(NIXPKGS_CONFIG_DEST)) $(CHANNEL_FILE) $(NIX_FILES)
	mkdir -p $(dir $@)
	install -m 0644 $< $@
	nix-env -f hosts/$(HOSTNAME).nix -ir

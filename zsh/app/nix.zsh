################################################################################
# Functions for making Nix and NixOS a bit easier.
if [ -d /etc/nix ]; then

  ##############################################################################
  alias nxs='nix-shell --command zsh'

  ##############################################################################
  function _nix-inside-shell () {
    if [[ -n $NIX_BUILD_TOP ]]; then
      return 0
    else
      return 1
    fi
  }

  ##############################################################################
  # Try to build a package from a file, instead of an attribute.
  function nix-build-package () {
    zparseopts -D -E -- p=here
    pkg=$1

    # Figure out which nixpkgs to use:
    if [[ -n $here && -r default.nix ]]; then
      # Use local nixpkgs
      nixpkgs="\"$(pwd)\""

    elif [[ -n $here ]]; then
      echo "error: $(pwd) doesn't look like a nixpkgs clone"
      return 1

    else
      nixpkgs="<nixpkgs>"
    fi

    # Now run nix-build:
    if [[ -n $pkg && -r $pkg ]]; then
      echo "building with ${nixpkgs}..."
      nix-build -E "with import $nixpkgs {}; callPackage $pkg {}"

    elif [[ -n $pkg ]]; then
      echo "error: whoa, $pkg doesn't seem to exist"
      return 2;

    else
      echo "error: whoa, you forgot to give the package dir name"
      return 2;
    fi
  }

################################################################################
else

  ##############################################################################
  function _nix-inside-shell () {return 1}
fi

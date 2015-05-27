################################################################################
# Functions for making Nix and NixOS a bit easier.
if [ -d /etc/nix ]; then

  ##############################################################################
  function _nix-inside-shell () {
    if [[ -n $NIX_BUILD_TOP ]]; then
      return 0
    else
      return 1
    fi
  }

  ##############################################################################
  # Nix shell with a local clone of nixpkgs.
  function nixpkgs-shell () {
    typeset -a options
    options=()

    # Automatically add a binary cache when on my home network:
    if iwgetid | cut -d: -f2 | grep -q "515"; then
      options=($options --option extra-binary-caches http://10.0.1.10:8080)
    fi

    export NIX_PATH=nixpkgs=$HOME/develop/oss/nixpkgs:$NIX_PATH
    nix-shell $options --command zsh "$@"
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

  ##############################################################################
  # Tool to help build Haskell projects using Nix.
  function nix-hs-build () {
    ( HOME="$(mktemp -d)" # For cabal-install.
      if [ ! -d .cabal-sandbox ]; then
        cabal sandbox init
        cabal sandbox add-source vendor/themoviedb
        cabal sandbox add-source vendor/byline
        cabal install --only-dependencies
      fi

      cabal configure -fmaintainer
      cabal build || exit 1
    ) && hlint src
  }

  ##############################################################################
  # Create a `shell.nix` file for a Haskell project.
  function nix-hs-derivation () {
    nixpkgs-shell -p haskellPackages.cabal2nix \
        --command 'cabal2nix --shell $PWD > shell.nix'
  }

################################################################################
else

  ##############################################################################
  function _nix-inside-shell () {return 1}
fi

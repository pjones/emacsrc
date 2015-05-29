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
  # Run a Nix command with the correct options and environment for a
  # local copy of nixpkgs.
  function _nix-with-nixpkgs () {
    command=$1
    shift

    typeset -a options
    options=()

    # Automatically add a binary cache when on my home network:
    if iwgetid | cut -d: -f2 | grep -q "515"; then
      options=($options --option extra-binary-caches http://10.0.1.10:8080)
    fi

    export NIX_PATH=nixpkgs=$HOME/develop/oss/nixpkgs:$NIX_PATH
    $command $options "$@"
  }

  ##############################################################################
  # Nix shell with a local clone of nixpkgs.
  function nixpkgs-shell () {
    _nix-with-nixpkgs nix-shell --command $(which zsh) "$@"
  }

  ##############################################################################
  # Nix build with a local clone of nixpkgs.
  function nixpkgs-build () {
    _nix-with-nixpkgs nix-build "$@"
  }

  ##############################################################################
  # Create a `default.nix` file for a Haskell project.
  function nix-hs-derivation () {
    nixpkgs-shell -p haskellPackages.cabal2nix \
        --command "cabal2nix $PWD -fmaintainer $@ > default.nix"
  }

  ##############################################################################
  function nix-hs-shell () {
    nix-hs-derivation
    override=~/.nixpkgs/envs/dev/haskell-cabal.nix
    nixpkgs-shell -I pwd=$PWD --pure "$@" $override
  }

  ##############################################################################
  # Build a Haskell project with the hsbuild.sh tool.
  function nix-hs-build () {
    nix-hs-shell --command  $(which hsbuild.sh) "$@"
  }

################################################################################
else

  ##############################################################################
  function _nix-inside-shell () {return 1}
fi

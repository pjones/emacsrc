if [ `hostname` = "cltc" ]; then

  ##############################################################################
  function _cltc-app {
    name=$1

    if [ -n "$shellHook" ]; then
      echo "ERROR: you're already inside nix-shell"
      exit 1
    fi

    ( export DEV_ENV=$name
      cd ~/develop/scors/$name
      nix-shell --command zsh ~/develop/scors/nix/${name}.nix
    )
  }

  ##############################################################################
  function cltc-phoenix {
    _cltc-app cltc-phoenix
  }

  ##############################################################################
  function cltc-claims {
    _cltc-app cltc-claims
  }

fi

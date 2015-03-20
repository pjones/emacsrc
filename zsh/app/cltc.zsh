if [ `hostname` = "cltc" ]; then

  ##############################################################################
  function _cltc-app {
    name=$1
    dir=$HOME/develop/scors/$name

    if [ -n "$shellHook" ]; then
      echo "ERROR: you're already inside nix-shell"
      exit 1
    fi

    if [ ! -d $dir ]; then
      ( cd `dirname $dir`
        git clone --recursive git@github.ors.sc.gov:ors/${name}.git
      )
    fi

    ( export DEV_ENV=$name
      cd $dir
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

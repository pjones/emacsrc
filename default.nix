{ pkgs ? (import <nixpkgs> {}).pkgs }:

let
  ##############################################################################
  # Custom build of Emacs with the settings I like.
  emacs = pkgs.emacs.override {
    withX       = true;
    withGTK2    = false;
    withGTK3    = false;
    imagemagick = pkgs.imagemagick;
  };

  ##############################################################################
  # Function for selecting which packages go into Emacs.
  emacsWithPackages = (pkgs.emacsPackagesNgGen emacs).emacsWithPackages;

  ##############################################################################
  # Access to the trivial Emacs builder so we can override some packages.
  pkgBuilder = import <nixpkgs/pkgs/build-support/emacs/trivial.nix> {
    inherit (pkgs) lib stdenv texinfo;
    inherit emacs;
  };

  ##############################################################################
  # Access to the MELPA Emacs builder.
  melpaBuild = import <nixpkgs/pkgs/build-support/emacs/melpa.nix> {
    inherit (pkgs) lib stdenv fetchurl texinfo;
    inherit emacs;
  };

  ##############################################################################
  # Latest version of passmm:
  passmmLatest = melpaPackages: melpaBuild {
    pname = "passmm";
    version = "20170113.837";
    src = pkgs.fetchgit {
      url = "git://git.devalot.com/passmm.git";
      rev = "2e0cd4e8ef7e6017dbc295664c925d32d6fdc688";
      sha256 = "0va9rg8nsi561x78qffr2piwprk9pq9k6zghg9yd4ldh834ccp6z";
    };
    recipeFile = pkgs.fetchurl {
      url = "https://raw.githubusercontent.com/milkypostman/melpa/8ae2a1e10375f9cd55d19502c9740b2737eba209/recipes/passmm";
      sha256 = "0p6qps9ww7s6w5x7p6ha26xj540pk4bjkr629lcicrvnfr5jsg4b";
      name = "passmm";
    };
    packageRequires = [ melpaPackages.password-store ];
  };

  ##############################################################################
  # Latest version of xelb:
  xelbLatest = elpaPkgs: pkgBuilder {
    pname = "xelb";
    version = "git";
    src = pkgs.fetchFromGitHub {
      owner = "ch11ng";
      repo  = "xelb";
      rev   = "8ac915ba1a836d3752ee8de4eb125da7af4dd299";
      sha256 = "1rd2702kygb8g0kpnnr48in6qywad52b1davigkv5p9wmrvm75jd";
    };
    packageRequires = [ elpaPkgs.cl-generic emacs ];
  };

  ##############################################################################
  # Latest version of exwm:
  exwmLatest = elpaPkgs: pkgBuilder {
    pname = "exwm";
    version = "git";
    src = pkgs.fetchFromGitHub {
      owner  = "ch11ng";
      repo   = "exwm";
      rev    = "df8de921132520cccf4236906bcd37aec83fa0ce";
      sha256 = "01j3s7l4hls5hbsci3af65p0pp8sy3cc5wy9snmz7jadzqg14ll5";
    };
    packageRequires = [ (xelbLatest elpaPkgs) ];
  };

  ##############################################################################
  # The derivation for building Emacs with packages and my config.
  pjones = pkgs.stdenv.mkDerivation rec {
    name = "emacsrc";
    src = ./.;

    phases = [ "installPhase" "fixupPhase" ];

    buildInputs = [ emacs' # Customized Emacs
                    pkgs.gitAndTools.gitFull # Needed to compile magit config
                    pkgs.imagemagick # For image-mode and eimp-mode
                  ];

    propagatedUserEnvPkgs = [ emacs' ];

    installPhase = ''
      mkdir -p "$out/bin" "$out/emacs.d"

      export loadpathel="$out/emacs.d/lisp/loadpath.el"
      export emacspath="${emacs'}/bin"

      substituteAll ${src}/dot.emacs.el "$out/dot.emacs.el"
      cp -r ${src}/lisp ${src}/modes "$out/emacs.d/"
      chmod u+w "$out"/emacs.d/*

      for f in ${src}/bin/*; do
        substituteAll "$f" "$out/bin/$(basename "$f")"
        chmod 0555 "$out/bin/$(basename "$f")"
      done

      for f in $(find "$out/emacs.d" -type f -name "*.el"); do
        emacs -Q --quick --batch -f package-initialize \
          --load "$loadpathel" -f batch-byte-compile "$f"
      done
    '';
  };

  ##############################################################################
  # List of Emacs packages I use.
  emacs' =
    emacsWithPackages (epkgs: (with epkgs.melpaStablePackages; [
      alect-themes
      beginend
      bm
      browse-kill-ring
      circe
      circe-notifications
      company
      company-ghc
      company-quickhelp
      company-statistics
      dante
      deft
      dim
      flycheck
      git-annex
      goto-chg
      graphviz-dot-mode
      haskell-mode
      helm
      helm-circe # https://github.com/lesharris/helm-circe
      htmlize
      hydra
      ialign
      indium
      inf-ruby
      js2-mode
      magit
      magit-annex
      markdown-mode
      nix-mode
      no-littering
      noccur
      org-tree-slide
      projectile
      resize-window
      ruby-end
      shackle
      switch-window
      yaml-mode

    ]) ++ (with epkgs.melpaPackages; [
      # No stable versions yet :(
      dired-filter
      dired-narrow
      dired-sidebar
      dired-subtree
      eimp
      elscreen # https://github.com/knu/elscreen
      god-mode
      helm-elscreen
      helm-hoogle # https://github.com/jwiegley/helm-hoogle
      highlight-indent-guides
      http
      mu4e-query-fragments
      rotate
      scad-mode

    ]) ++ (with epkgs.elpaPackages; [
      rainbow-mode

    ]) ++ (with epkgs.orgPackages; [
      org

    ]) ++ (with epkgs; [
      pdf-tools

    ]) ++ (with pkgs; [
      (exwmLatest epkgs.elpaPackages)
      (passmmLatest epkgs.melpaPackages)
      mu

    ]));

################################################################################
# It all boils down to the derivation.
in pjones

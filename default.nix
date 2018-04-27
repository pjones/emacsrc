{ pkgs ? (import <nixpkgs> {}).pkgs }:

let
  emacs = pkgs.emacs.override {
    withX    = true;
    withGTK2 = false;
    withGTK3 = false;
  };

  emacsWithPackages = (pkgs.emacsPackagesNgGen emacs).emacsWithPackages;

  pjones = pkgs.stdenv.mkDerivation rec {
    name = "emacsrc";
    src = ./.;

    phases = [ "installPhase" ];
    buildInputs = [ emacs'
                    pkgs.gitAndTools.gitFull # needed to compile magit config
                  ];

    installPhase = ''
      mkdir -p "$out/bin" "$out/emacs.d"

      export loadpathel="$out/emacs.d/lisp/loadpath.el"
      substituteAll ${src}/dot.emacs.el "$out/dot.emacs.el"

      cp -r ${src}/lisp ${src}/modes "$out/emacs.d/"
      cp -r ${src}/bin/* "$out/bin/"
      chmod u+w "$out"/emacs.d/*
      chmod 0555 "$out"/bin/*

      for f in ${emacs'}/bin/*; do
        ln -nfs "$f" "$out/bin/"
      done

      for f in $(find "$out/emacs.d" -type f -name "*.el"); do
        emacs -Q --quick --batch -f package-initialize \
          --load "$loadpathel" -f batch-byte-compile "$f"
      done
    '';
  };

  emacs' =
    emacsWithPackages (epkgs: (with epkgs.melpaPackages; [
      beginend
      bm
      circe
      circe-notifications
      color-theme-sanityinc-tomorrow
      company
      company-ghc
      company-quickhelp
      company-statistics
      dante
      deft

      # FIXME:
      # dictionary broken :(

      dired-filter
      dired-narrow
      dired-sidebar
      dired-subtree
      flx
      flx-ido
      flycheck
      git-annex
      god-mode
      google-contacts
      goto-chg
      graphviz-dot-mode
      indium
      haskell-mode
      highlight-indent-guides
      htmlize
      http
      hydra
      ido-completing-read-plus
      idomenu
      inf-ruby
      js2-mode
      magit
      magit-annex
      markdown-mode
      nix-mode
      noccur
      no-littering
      org-tree-slide
      passmm
      projectile
      ruby-end
      scad-mode
      smex
      smtpmail-multi
      switch-window
      yaml-mode

    ]) ++ (with epkgs.elpaPackages; [
      rainbow-mode

    ]) ++ (with epkgs.orgPackages; [
      org

    ]) ++ (with epkgs; [
      pdf-tools

    ]));

in pjones

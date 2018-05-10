{ pkgs ? (import <nixpkgs> {}).pkgs }:

let
  emacs = pkgs.emacs.override {
    withX       = true;
    withGTK2    = false;
    withGTK3    = false;
    imagemagick = pkgs.imagemagick;
  };

  emacsWithPackages = (pkgs.emacsPackagesNgGen emacs).emacsWithPackages;

  pjones = pkgs.stdenv.mkDerivation rec {
    name = "emacsrc";
    src = ./.;

    phases = [ "installPhase" "fixupPhase" ];

    buildInputs = [ emacs' # Customized Emacs
                    pkgs.gitAndTools.gitFull # Needed to compile magit config
                    pkgs.imagemagick # For image-mode and eimp-mode
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

      # FIXME:
      # dictionary broken :(

      flx
      flx-ido
      flycheck
      git-annex
      goto-chg
      graphviz-dot-mode
      haskell-mode
      htmlize
      hydra
      ialign
      ido-completing-read-plus
      idomenu
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
      passmm
      projectile
      resize-window
      ruby-end
      shackle
      smex
      switch-window
      yaml-mode

    ]) ++ (with epkgs.melpaPackages; [
      # No stable versions yet :(
      dired-filter
      dired-narrow
      dired-sidebar
      dired-subtree
      eimp
      god-mode
      google-contacts
      highlight-indent-guides
      http
      mu4e-query-fragments
      rotate
      scad-mode

    ]) ++ (with epkgs.elpaPackages; [
      exwm
      rainbow-mode

    ]) ++ (with epkgs.orgPackages; [
      org

    ]) ++ (with epkgs; [
      pdf-tools

    ]) ++ (with pkgs; [
      mu

    ]));

in pjones

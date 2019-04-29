{ pkgs ? (import <nixpkgs> {}).pkgs }:

let
  ##############################################################################
  # Custom build of Emacs with the settings I like:
  emacs = pkgs.emacs.override {
    withX       = true;
    withGTK2    = false;
    withGTK3    = true;
    imagemagick = pkgs.imagemagick;
  };

  ##############################################################################
  # Emacs + all of the packages I need:
  emacsAndPackages = pkgs.callPackage ./nix/packages.nix { };

in

################################################################################
# The actual Nix package definition:
pkgs.stdenv.mkDerivation rec {
  name = "emacsrc";
  src = ./.;

  phases = [ "installPhase" "fixupPhase" ];

  buildInputs = [ emacsAndPackages
                  pkgs.gitAndTools.gitFull # Needed to compile magit config
                  pkgs.imagemagick # For image-mode and eimp-mode
                ];

  propagatedUserEnvPkgs = [ emacsAndPackages ];

  installPhase = ''
    mkdir -p "$out/bin" "$out/emacs.d"

    export loadpathel="$out/emacs.d/lisp/loadpath.el"
    export emacspath="${emacsAndPackages}/bin"

    substituteAll ${src}/dot.emacs.el "$out/dot.emacs.el"
    cp -r ${src}/lisp ${src}/modes ${src}/hydras "$out/emacs.d/"
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
}

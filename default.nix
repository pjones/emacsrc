{ sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs { }
}:
let
  ##############################################################################
  # Custom build of Emacs with the settings I like:
  emacs = pkgs.emacs.override {
    withX = true;
    withGTK2 = false;
    withGTK3 = true;
  };

  ##############################################################################
  # Emacs + all of the packages I need:
  emacsAndPackages = pkgs.callPackage ./nix/packages.nix { };

  ##############################################################################
  # Use the latest version:
  nixpkgs-fmt = import sources.nixpkgs-fmt { };

  ################################################################################
  # The actual Nix package definition:
in
pkgs.stdenv.mkDerivation rec {
  name = "emacsrc";
  src = ./.;

  phases = [ "installPhase" "fixupPhase" ];

  buildInputs = [
    emacsAndPackages # Emacs!
    nixpkgs-fmt # Formatting nix files
    pkgs.gitAndTools.gitFull # Needed to compile magit config
    pkgs.gnuplot # For org-mode
    pkgs.graphviz # For org-roam
    pkgs.imagemagick # For image-mode and eimp-mode
    pkgs.netcat # For bin/e
    pkgs.python3 # For treemacs
    pkgs.shellcheck # Lint shell scripts
    pkgs.shfmt # Format shell scripts
    pkgs.sqlite # For org-roam
    pkgs.wmctrl # For bin/e
  ];

  propagatedUserEnvPkgs = buildInputs;

  installPhase = with pkgs.lib;
    let path = concatMapStringsSep ":" (p: "${p}/bin") buildInputs;
    in
    ''
      mkdir -p "$out/bin" "$out/emacs.d"

      export path="${path}"
      export loadpathel="$out/emacs.d/lisp/loadpath.el"

      substituteAll ${src}/dot.emacs.el "$out/dot.emacs.el"
      cp -r ${src}/lisp ${src}/modes ${src}/snippets "$out/emacs.d/"
      chmod u+w "$out"/emacs.d/*

      cp -r "${src}/share" "$out/"
      chmod -R u+r "$out/share"

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

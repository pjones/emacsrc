{ sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs { }
}:
let
  ##############################################################################
  # Emacs + all of the packages I need:
  emacsAndPackages = pkgs.callPackage ./nix/packages.nix { };

in
pkgs.stdenv.mkDerivation rec {
  name = "emacsrc";
  phases = [ "unpackPhase" "installPhase" "fixupPhase" ];

  src = with pkgs.lib;
    cleanSourceWith {
      src = ./.;
      filter = name: _type:
        let baseName = baseNameOf (toString name);
        in !(baseName == "nix" || baseName == "test");
    };

  buildInputs = [
    emacsAndPackages # Emacs!
    pkgs.gitAndTools.gitFull # Needed to compile magit config
    pkgs.gnuplot # For org-mode
    pkgs.imagemagick # For image-mode and eimp-mode
    pkgs.netcat # For bin/e
    pkgs.neuron-notes # For neuron-mode
    pkgs.nixpkgs-fmt # Formatting nix files
    pkgs.sage # for sage-shell-mode
    pkgs.shellcheck # Lint shell scripts
    pkgs.shfmt # Format shell scripts
    pkgs.wmctrl # For bin/e
  ];

  propagatedUserEnvPkgs = buildInputs;

  installPhase = with pkgs.lib;
    let path = makeBinPath buildInputs;
    in
    ''
      mkdir -p "$out/bin" "$out/emacs.d"

      export path="${path}"
      export loadpathel="$out/emacs.d/lisp/loadpath.el"

      substituteAll dot.emacs.el "$out/dot.emacs.el"
      cp -r lisp modes scripts snippets "$out/emacs.d/"
      chmod u+w "$out"/emacs.d/*

      cp -r share "$out/"
      chmod -R u+r "$out/share"

      for f in bin/*; do
        substituteAll "$f" "$out/bin/$(basename "$f")"
        chmod 0555 "$out/bin/$(basename "$f")"
      done

      for f in $(find "$out/emacs.d" -type f -name "*.el"); do
        echo "==> Compile" "$(sed -E 's|/nix/store/[^/]+/||' <<<"$f")"
        emacs --quick --batch \
          --load "$loadpathel" \
          --funcall batch-byte-compile \
          "$f" 2> >(grep -Ev "^(Loading |\[Treemacs)" >&2 || :)
      done
    '';
}

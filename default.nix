{ pkgs ? import <nixpkgs> { }
}:
let
  ##############################################################################
  # Emacs + all of the packages I need:
  emacsAndPackages = pkgs.callPackage ./nix/packages.nix { };

  ##############################################################################
  inherit (pkgs) lib;

in
pkgs.stdenv.mkDerivation rec {
  name = "emacsrc";

  src = with pkgs.lib;
    cleanSourceWith {
      src = ./.;
      filter = name: _type:
        let baseName = baseNameOf (toString name);
        in !(baseName == "nix" || baseName == "test" || baseName == "out");
    };

  enableParallelBuilding = true;
  makeFlags = [ "PREFIX=$(out)" ];

  buildInputs = [
    emacsAndPackages # Emacs!
  ] ++ lib.optionals (builtins.elem pkgs.system lib.platforms.x86) [
    # Tools for desktop systems:
    pkgs.black # For formatting Python code
    pkgs.gitAndTools.gitFull # Needed to compile magit config
    pkgs.gnuplot # For org-mode
    pkgs.imagemagick # For image-mode and eimp-mode
    pkgs.netcat # For bin/e
    pkgs.neuron-notes # For neuron-mode
    pkgs.nixpkgs-fmt # Formatting nix files
    pkgs.shellcheck # Lint shell scripts
    pkgs.shfmt # Format shell scripts
  ] ++ lib.optionals (builtins.elem pkgs.system lib.platforms.linux) [
    # Tools that only work on Linux:
    pkgs.wmctrl # For bin/e
  ] ++ lib.optionals (pkgs.system == "x86_64-linux") [
    # Broken except for Linux on x86:
    pkgs.sage # for sage-shell-mode
  ];

  propagatedUserEnvPkgs = buildInputs;

  postInstall = with pkgs.lib;
    let path = makeBinPath buildInputs; in
    ''
      export path="${path}"
      export loadpathel="$out/emacs.d/lisp/loadpath.el"
      substituteAllInPlace "$out/emacs.d/dot.emacs.el"
      for f in $out/bin/*; do substituteAllInPlace "$f"; done
    '';
}

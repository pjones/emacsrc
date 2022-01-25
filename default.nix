{ pkgs ? import <nixpkgs> { }
, inputs ? { }
}:
let
  ##############################################################################
  # Emacs + all of the packages I need:
  emacsAndPackages = pkgs.callPackage ./nix/packages.nix { inherit inputs; };

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
        in !(baseName == "test" || baseName == "out");
    };

  enableParallelBuilding = true;
  makeFlags = [ "PREFIX=$(out)" ];

  buildInputs = [
    emacsAndPackages # Emacs!
    pkgs.imagemagick # For image-mode
  ] ++ lib.optionals pkgs.stdenv.isLinux [
    pkgs.netcat # For bin/e
    pkgs.wmctrl # For bin/e
  ];

  # Ensure 'Emacs' makes it into the closure:
  propagatedUserEnvPkgs = [ emacsAndPackages ];

  postInstall = with pkgs.lib;
    let path = makeBinPath buildInputs; in
    ''
      export path="${path}"
      export loadpathel="$out/emacs.d/lisp/loadpath.el"
      substituteAllInPlace "$out/emacs.d/dot.emacs.el"
      for f in $out/bin/*; do substituteAllInPlace "$f"; done
    '';
}

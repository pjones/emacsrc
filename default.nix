{
  pkgs ? import <nixpkgs> { },
  inputs ? { },
  emacs ? pkgs.emacs,
}:
let
  ##############################################################################
  # Emacs + all of the packages I need:
  emacsAndPackages = pkgs.callPackage ./nix/packages.nix { inherit emacs inputs; };

  ##############################################################################
  inherit (pkgs) lib;

  ##############################################################################
  # Packages to put into the developer shell and the user environment:
  extraPackages = [
    (pkgs.nuspell.withDicts (
      dicts: with dicts; [
        en_US
        de_DE
      ]
    ))
    pkgs.enchant
    pkgs.nixd # I do a lot of Nix programming.
  ]
  ++ lib.optionals pkgs.stdenv.isLinux [ pkgs.dict ];

in
pkgs.stdenv.mkDerivation rec {
  name = "emacsrc";

  src =
    with pkgs.lib;
    cleanSourceWith {
      src = ./.;
      filter =
        name: _type:
        let
          baseName = baseNameOf (toString name);
        in
        !(baseName == "test" || baseName == "out");
    };

  enableParallelBuilding = true;
  makeFlags = [ "PREFIX=$(out)" ];

  buildInputs =
    extraPackages
    ++ [
      emacsAndPackages # Emacs!
      pkgs.git # For Magit
      pkgs.imagemagick # For image-mode
    ]
    ++ lib.optionals pkgs.stdenv.isLinux [
      pkgs.netcat # For bin/e
    ];

  # Packages to push into the user's PATH:
  propagatedUserEnvPkgs = [ emacsAndPackages ] ++ extraPackages;

  # Get directly to Emacs from the outside:
  passthru.emacs = emacsAndPackages;

  postInstall =
    let
      path = lib.makeBinPath buildInputs;
    in
    ''
      export path="${path}"
      export loadpathel="$out/emacs.d/lisp/loadpath.el"
      substituteAllInPlace "$out/emacs.d/dot.emacs.el"
      for f in $out/bin/*; do substituteAllInPlace "$f"; done
    '';

  meta = {
    description = "Peter's Emacs Configuration";
    longDescription = "Emacs configuration and scripts.";
    homepage = "https://github.com/pjones/emacsrc/";
    license = lib.licenses.bsd3;
    maintainers = with lib.maintainers; [ pjones ];
    platforms = lib.platforms.all;
  };
}

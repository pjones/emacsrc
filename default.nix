{ pkgs ? import <nixpkgs> { }
, inputs ? { }
, emacs ? pkgs.emacs
}:
let
  ##############################################################################
  # Emacs + all of the packages I need:
  emacsAndPackages = pkgs.callPackage ./nix/packages.nix {
    inherit emacs inputs;
  };

  ##############################################################################
  inherit (pkgs) lib;

  ##############################################################################
  hunspellDicts = with pkgs.hunspellDicts; [ en_US de_DE ];

  ##############################################################################
  # Packages to put into the developer shell and the user environment:
  extraPackages = [
    (pkgs.nuspellWithDicts hunspellDicts)
    pkgs.enchant
  ]
  ++ hunspellDicts
  ++ lib.optionals pkgs.stdenv.isLinux [
    pkgs.dict
  ];

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

  buildInputs = extraPackages ++ [
    emacsAndPackages # Emacs!
    pkgs.git # For Magit
    pkgs.imagemagick # For image-mode
    pkgs.mpv # For EMMS
  ] ++ lib.optionals pkgs.stdenv.isLinux [
    pkgs.netcat # For bin/e
    inputs.desktop-scripts.packages.${pkgs.system}.default # for bin/e
  ];

  # Packages to push into the user's PATH:
  propagatedUserEnvPkgs = [
    emacsAndPackages
  ] ++ extraPackages;

  postInstall =
    let path = lib.makeBinPath buildInputs;
    in
    ''
      export path="${path}"
      export loadpathel="$out/emacs.d/lisp/loadpath.el"
      substituteAllInPlace "$out/emacs.d/dot.emacs.el"
      for f in $out/bin/*; do substituteAllInPlace "$f"; done
    '' + (lib.concatMapStringsSep "\n"
      (dict: ''
        mkdir -p "$out"/share/enchant/nuspell
        for file in ${dict}/share/hunspell/*; do
          ln -s "$file" "$out"/share/enchant/nuspell/
        done
      '')
      hunspellDicts);
}

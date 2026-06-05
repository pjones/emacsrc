{
  stdenvNoCC,
  lib,
  emacs,
  enchant,
  netcat,
  nuspell,
}:
let
  # Tools that are needed by the scripts in this package, but don't
  # need to be in the user's PATH.
  neededTools = [
  ]
  ++ lib.optionals stdenvNoCC.isLinux [
    netcat # For bin/e
  ];
in
stdenvNoCC.mkDerivation (finalAttrs: {
  name = "emacsrc";

  src =
    with lib;
    cleanSourceWith {
      src = ../.;
      filter =
        name: _type:
        let
          baseName = baseNameOf (toString name);
        in
        !(baseName == "test" || baseName == "out");
    };

  enableParallelBuilding = true;
  makeFlags = [ "PREFIX=$(out)" ];

  buildInputs = [
    emacs
  ];

  # Packages to push into the user's PATH:
  propagatedUserEnvPkgs = [
    (nuspell.withDicts (
      dicts: with dicts; [
        en_US
        de_DE
      ]
    ))
    emacs
    enchant
  ];

  postPatch = ''
    export loadpathel="$out/emacs.d/lisp/loadpath.el"
    substituteAllInPlace "init.el"
  '';

  postInstall =
    let
      path = lib.makeBinPath (finalAttrs.propagatedUserEnvPkgs ++ neededTools);
    in
    ''
      export path="${path}"
      export loadpathel="$out/emacs.d/lisp/loadpath.el"
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
})

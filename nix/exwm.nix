{ self
, pkgs
, melpaBuild
, ...
}:

melpaBuild {
  pname = "exwm";
  version = "0.21.20190102";

  src = pkgs.fetchFromGitHub {
    owner  = "ch11ng";
    repo   = "exwm";
    rev    = "404c94568d581fb66fca5e2524c908d63188388";
    sha256 = "1081aqlp7g6h7r1lrlkab97npn5w9kmnafh2lmcb9hskp0ydfiym";
  };

  recipe = pkgs.writeText "recipe" ''
    (exwm :repo "ch11ng/exwm" :fetcher github)
  '';

  packageRequires = [
    self.xelb
  ];
}

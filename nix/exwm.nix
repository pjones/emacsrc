{ self
, pkgs
, melpaBuild
, ...
}:

melpaBuild {
  pname = "exwm";
  version = "0.19.20181003";

  src = pkgs.fetchFromGitHub {
    owner  = "ch11ng";
    repo   = "exwm";
    rev    = "472f7cb82b67b98843f10c12e6bda9b8ae7262bc";
    sha256 = "19gflsrb19aijf2xcw7j2m658qad21nbwziw38s1h2jw66vhk8dj";
  };

  recipe = pkgs.writeText "recipe" ''
    (exwm :repo "ch11ng/exwm" :fetcher github)
  '';

  packageRequires = [
    self.xelb
  ];
}

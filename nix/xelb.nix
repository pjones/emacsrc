{ pkgs
, melpaBuild
, self
, ...
}:

melpaBuild {
  pname = "xelb";
  version = "0.16.20190102";

  src = pkgs.fetchFromGitHub {
    owner = "ch11ng";
    repo  = "xelb";
    rev   = "407cce8dd8b0e621449c6c3015c303539f53819";
    sha256 = "0pf20jbhszv5p0ncd0y3smx0c2im3xslid4r62whr55x1mq27mr9";
  };

  recipe = pkgs.writeText "recipe" ''
    (xelb :repo "ch11ng/xelb" :fetcher github)
  '';

  packageRequires = [
    self.cl-generic
  ];
}

{ pkgs
, melpaBuild
, ...
}:

melpaBuild {
  pname = "polymode";
  ename = "polymode";
  version = "20190102.1110";
  src = pkgs.fetchFromGitHub {
    owner = "polymode";
    repo = "polymode";
    rev = "c2d950a46c2851a94b7f7c506c572de08acdfd53";
    sha256 = "0r14dga0bxdl4zwwgpvk185axi64w9fsn301slv009756mkbysni";
  };
  recipe = pkgs.fetchurl {
    url = "https://raw.githubusercontent.com/milkypostman/melpa/3058351c4500fdcbe7f40b4c96ac8d6de9bbeb1d/recipes/polymode";
    sha256 = "15i9masklpy4iwskc7dzqjhb430ggn0496z4wb1zjj0b9xx4wj66";
    name = "recipe";
  };
  packageRequires = [ ];
  meta = {
    homepage = "https://melpa.org/#/polymode";
    license = pkgs.lib.licenses.free;
  };
}

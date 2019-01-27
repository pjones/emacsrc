{ super
, self
, pkgs
, melpaBuild
}:

melpaBuild {
  pname = "exwm-nw";
  version = "0.1";

  src = pkgs.fetchgit {
    url = "git://git.devalot.com/exwm-nw.git";
    rev = "ada2cf7874a925d2b83b9735de25594e75520219";
    sha256 = "0kbbry6fdx4wagn1nhah9cshp7p1yl7fnp35rnvy24k06nibds1a";
  };

  recipe = pkgs.writeText "recipe" ''
    (exwm-nw :repo "pjones/exwm-nw" :fetcher github)
  '';

  packageRequires = [
    self.exwm
  ];
}

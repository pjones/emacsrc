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
    rev = "4d47a3118f748713371d57195b61e5dda6fbfbf2";
    sha256 = "1xazj9plldzc867bm80cr3qwb2ndyzjf98s4rhkk6i3idwj37hpa";
  };

  recipe = pkgs.writeText "recipe" ''
    (exwm-nw :repo "pjones/exwm-nw" :fetcher github)
  '';

  packageRequires = [
    self.exwm
  ];
}

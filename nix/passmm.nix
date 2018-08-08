{ super
, self
, pkgs
, melpaBuild
}:

melpaBuild {
  pname = "passmm";
  version = "20180624.0";

  src = pkgs.fetchgit {
    url = "git://git.devalot.com/passmm.git";
    rev = "898709c63130d6c0422af544ebac64eae04d24ac";
    sha256 = "0skawcv6iksydmw3hprff19ajj4n2yi3wwcc8wlh2gkncwnkkg9k";
  };

  recipeFile = pkgs.fetchurl {
    url = "https://raw.githubusercontent.com/milkypostman/melpa/8ae2a1e10375f9cd55d19502c9740b2737eba209/recipes/passmm";
    sha256 = "0p6qps9ww7s6w5x7p6ha26xj540pk4bjkr629lcicrvnfr5jsg4b";
    name = "passmm";
  };

  packageRequires = [ self.password-store ];
}

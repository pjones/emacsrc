{ super
, self
, pkgs
, melpaBuild
}:

melpaBuild {
  pname = "passmm";
  version = "20180624.0";

  src = pkgs.fetchgit {
    url = "https://code.devalot.com/open/passmm.git";
    rev = "b25a92048c788a8477cc5ffe14c0c4a4df19d79a";
    sha256 = "1jg2rs010fmw10ld0bfl6x7af3v9yqfy9ga5ixmam3qpilc8c4fw";
  };

  recipe = pkgs.fetchurl {
    url = "https://raw.githubusercontent.com/milkypostman/melpa/8ae2a1e10375f9cd55d19502c9740b2737eba209/recipes/passmm";
    sha256 = "0p6qps9ww7s6w5x7p6ha26xj540pk4bjkr629lcicrvnfr5jsg4b";
    name = "passmm";
  };

  packageRequires = [ self.password-store ];
}
